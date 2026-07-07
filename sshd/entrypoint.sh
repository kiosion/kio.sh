#!/bin/sh
# Generate the host key on first boot only; mount /etc/ssh/keys as a
# volume so visitors never see a host-key-changed warning after redeploys.
set -e
mkdir -p /etc/ssh/keys /run/sshd
if [ ! -f /etc/ssh/keys/ssh_host_ed25519_key ]; then
  ssh-keygen -t ed25519 -N '' -f /etc/ssh/keys/ssh_host_ed25519_key
fi
exec /usr/sbin/sshd -D -e
