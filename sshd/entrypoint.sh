#!/bin/sh
# Generate the host key on first boot only; mount /etc/ssh/keys as a
# volume so visitors never see a host-key-changed warning after redeploys.
set -e
mkdir -p /etc/ssh/keys /run/sshd
# utmp lets sshd's login accounting succeed (silences the logout()
# warning). It's fixed-slot -- reused per pty, never grows -- and the
# append-only siblings (wtmp/btmp/lastlog) stay absent, so sshd skips
# them: no log files accumulate.
touch /run/utmp
if [ ! -f /etc/ssh/keys/ssh_host_ed25519_key ]; then
  ssh-keygen -t ed25519 -N '' -f /etc/ssh/keys/ssh_host_ed25519_key
fi
# Label each daemon's output so the interleaved container log is
# followable ("[http] ...", "[sshd] ..."). Timestamps come from the
# log collector (docker logs -t / fly logs).
prefix() { while IFS= read -r line; do printf '[%s] %s\n' "$1" "$line"; done; }

# The labeling pipelines mean the shell (not sshd) receives stop
# signals; forward them to everything so Ctrl+C/docker stop is instant.
trap 'kill 0' INT TERM

# static "connect via ssh" page for browsers; TLS terminates at the
# proxy (Fly edge), so plain HTTP is all we serve. Runs as blog,
# foreground + -v so request lines reach the container log.
busybox httpd -f -vv -p 8080 -h /var/www -u blog 2>&1 | prefix http &

/usr/sbin/sshd -D -e 2>&1 | prefix sshd &
wait
