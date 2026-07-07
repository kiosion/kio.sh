# kio.dev over SSH

Browse the site as a TUI: `ssh shell.kio.dev`. Any username (or none), no
password, jump right in :)

Tabbed pages, navigable via keyboard (`1`/`2`/`3` or `Tab` to switch pages,
`j`/`k`/arrows, `enter`, `q`) and mouse (click tabs/posts, scroll wheel) both
work.

## How it works

- Stock OpenSSH `sshd` handles crypto, auth, and pty allocation.
- `sshd/nss_ato.c` maps any unknown username onto the `blog` account with an
  empty password.
- `blog` account is inert: `ForceCommand` runs the TUI binary; no shell, no
  forwarding, no tunnels.
- `kio-tui` is an ordinary terminal app built with brick; site content is
  embedded into the binary at compile time.

## Local development

Needs GHC + cabal; no SSH required while iterating:

```sh
cabal run kio-tui
```

## Docker

Build from the **repo root** (the image needs `src/content`):

```sh
docker build -f ssh/Dockerfile -t kio-ssh .
docker run -p 2222:22 -v kio-ssh-keys:/etc/ssh/keys kio-ssh
ssh -p 2222 anything@localhost
```

The `/etc/ssh/keys` volume persists the host key across rebuilds so visitors
never see host-key-changed warnings.

## Deploying + DNS

Any host that accepts raw TCP works (small VPS, Fly.io machine, etc.).
Netlify/serverless cannot host this — SSH needs a long-lived TCP listener.
Cloudflare's proxy only forwards HTTP(S), so add a **DNS-only (grey-cloud)**
`A`/`AAAA` record `ssh.kio.dev` pointing at the host; `kio.dev` stays proxied to
Netlify untouched. (`ssh kio.dev` on the apex would require Cloudflare
Spectrum.)
