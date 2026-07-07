#!/bin/sh
# Generate host key on first boot only; mount /etc/ssh/keys as a volume
# to persist between redeploys
set -e
mkdir -p /etc/ssh/keys /run/sshd
# utmp lets sshd's login accounting succeed
touch /run/utmp
if [ ! -f /etc/ssh/keys/ssh_host_ed25519_key ]; then
  ssh-keygen -t ed25519 -N '' -f /etc/ssh/keys/ssh_host_ed25519_key
fi
# Label daemons' output so the interleaved container log is followable
prefix() { while IFS= read -r line; do printf '[%s] %s\n' "$1" "$line"; done; }

trap 'kill 0' INT TERM

# static "connect via ssh" page for browsers
busybox httpd -f -vv -p 8080 -h /var/www -u blog 2>&1 | prefix http &

/usr/sbin/sshd -D -e 2>&1 | prefix sshd &
wait
