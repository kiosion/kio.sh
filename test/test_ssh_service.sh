#!/usr/bin/env bash
set -euo pipefail

image_name=${1:-kio-ssh}
container_name="kio-ssh-integration-${RANDOM}"

print_container_logs() {
  docker logs "$container_name" 2>&1 || true
}

remove_container() {
  docker rm -f "$container_name" >/dev/null 2>&1 || true
}

fail_test() {
  printf 'SSH integration test failed: %s\n' "$1" >&2
  print_container_logs >&2
  exit 1
}

trap remove_container EXIT

docker run --detach --name "$container_name" \
  --publish 127.0.0.1::2222 \
  --publish 127.0.0.1::2224 \
  "$image_name" >/dev/null

proxy_port=$(docker port "$container_name" 2222/tcp)
proxy_port=${proxy_port##*:}
direct_port=$(docker port "$container_name" 2224/tcp)
direct_port=${direct_port##*:}

ssh_common_options=(
  -o BatchMode=yes
  -o ConnectTimeout=5
  -o LogLevel=ERROR
  -o StrictHostKeyChecking=no
  -o UserKnownHostsFile=/dev/null
)

for _ in $(seq 1 30); do
  if ssh-keyscan -p "$direct_port" 127.0.0.1 >/dev/null 2>&1; then
    break
  fi
  sleep 1
done
ssh-keyscan -p "$direct_port" 127.0.0.1 >/dev/null 2>&1 || fail_test "SSH did not become ready"

docker exec "$container_name" haproxy -c -f /etc/haproxy/haproxy.cfg >/dev/null || fail_test "HAProxy configuration is invalid"
docker exec "$container_name" /usr/sbin/sshd -t || fail_test "OpenSSH configuration is invalid"

listing=$(ssh -p "$direct_port" "${ssh_common_options[@]}" integration-user@127.0.0.1 ls) || fail_test "an arbitrary username could not run the listing command"
[[ -n "$listing" ]] || fail_test "the listing command returned no posts"

if ssh -p "$direct_port" "${ssh_common_options[@]}" root@127.0.0.1 ls >/dev/null 2>&1; then
  fail_test "root authentication succeeded"
fi

if forced_output=$(ssh -p "$direct_port" "${ssh_common_options[@]}" integration-user@127.0.0.1 'printf command-bypass' 2>&1); then
  fail_test "an unrecognized command succeeded"
fi
[[ "$forced_output" != *command-bypass* ]] || fail_test "the requested command bypassed ForceCommand"

if sftp -b /dev/null -P "$direct_port" \
  -o BatchMode=yes \
  -o ConnectTimeout=5 \
  -o LogLevel=ERROR \
  -o StrictHostKeyChecking=no \
  -o UserKnownHostsFile=/dev/null \
  integration-user@127.0.0.1 >/dev/null 2>&1; then
  fail_test "SFTP succeeded"
fi

if ssh -p "$direct_port" "${ssh_common_options[@]}" \
  -o ExitOnForwardFailure=yes \
  -R 0:127.0.0.1:8080 \
  integration-user@127.0.0.1 ls >/dev/null 2>&1; then
  fail_test "TCP forwarding succeeded"
fi

if ssh -p "$proxy_port" "${ssh_common_options[@]}" integration-user@127.0.0.1 ls >/dev/null 2>&1; then
  fail_test "the public listener accepted a connection without a PROXY header"
fi

exec 3<>"/dev/tcp/127.0.0.1/$proxy_port"
printf 'PROXY TCP4 192.0.2.1 192.0.2.2 41001 22\r\n' >&3
IFS= read -r -t 5 first_banner <&3 || fail_test "the first proxied connection received no SSH banner"
[[ "$first_banner" == SSH-* ]] || fail_test "the first proxied connection received an invalid SSH banner"

exec 4<>"/dev/tcp/127.0.0.1/$proxy_port"
printf 'PROXY TCP4 192.0.2.1 192.0.2.2 41002 22\r\n' >&4
IFS= read -r -t 5 second_banner <&4 || fail_test "the second proxied connection received no SSH banner"
[[ "$second_banner" == SSH-* ]] || fail_test "the second proxied connection received an invalid SSH banner"

exec 5<>"/dev/tcp/127.0.0.1/$proxy_port"
printf 'PROXY TCP4 192.0.2.1 192.0.2.2 41003 22\r\n' >&5
IFS= read -r -t 5 third_banner <&5 || fail_test "the third proxied connection received no SSH banner"
[[ "$third_banner" == SSH-* ]] || fail_test "the third proxied connection received an invalid SSH banner"

exec 6<>"/dev/tcp/127.0.0.1/$proxy_port"
printf 'PROXY TCP4 192.0.2.1 192.0.2.2 41004 22\r\n' >&6
if IFS= read -r -t 2 fourth_banner <&6 && [[ "$fourth_banner" == SSH-* ]]; then
  fail_test "the fourth connection from one source address was accepted"
fi

printf 'SSH integration tests passed\n'
