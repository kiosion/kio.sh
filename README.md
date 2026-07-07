# kio.sh

[kio.dev](https://kio.dev), served over SSH, as a little easter-egg and quick
hack project:

```sh
ssh kio.sh              # interactive
ssh kio.sh ls           # list posts
ssh kio.sh cat <slug>   # print one as markdown
```

Stock OpenSSH handles the crypto, auth, and pty; a tiny NSS module
(`sshd/nss_ato.c`) maps every username onto one inert `blog` user whose
`ForceCommand` is the TUI; a Haskell +
[brick](https://github.com/jtdaugherty/brick) app.

## Building

```sh
make content && cabal run kio-tui   # run locally with no ssh layer
make dev                            # or the full container on :2222
```

## To-do

- a11y: `f`-style link-hint mode so keyboard-only visitors can follow inline links
- try: `makeLenses` for `St` to shrink the record-update lambdas in `Events`
- other: vi/vim navigation fun-to-have's
