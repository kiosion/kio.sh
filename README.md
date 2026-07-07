# kio.dev over SSH

As a little easter-egg and fun project, this site's available via SSH at
`ssh kio.sh`.

Stock OpenSSH does all the crypto/auth/pty work; a tiny NSS module
(`sshd/nss_ato.c`) maps every username onto the inert `blog` user, whose
`ForceCommand` is the Haskell TUI using brick. Site content is embedded into the
binary at compile.

```text
                +@@@@@@@@@*
                 =@@@@@@@@@%.
                  :@@@@@@@@@@-
                   .%@@@@@@@@@@@@@@@@@@@@@@@@=
                     #@@@@@@@@@@@@@@@@@@@@@@@@*
                      .......-@@@@@@@@@@@@@@@@@%.
                       .......:@@@@@@@@@@@@@@@@@@:
                     *@@@@@@@@@*.......:%@@@@@@@@@
                   .%@@@@@@@@@=          *@@@@@@%.
                  -@@@@@@@@@@:            +@@@@#
                 =@@@@@@@@@%.              -@@*
                +@@@@@@@@@#                 .:    :::::::::.
                        :@%.                    .%@@@@@@@@@=
                       -@@@@:                  :@@@@@@@@@@-
                      +@@@@@@=                =@@@@@@@@@%.
                     #@@@@@@@@*              *@@@@@@@@@#
                     +@@@@@@@@@#-::::::::::-#@@@@@@@@@+
                      -@@@@@@@@=+@@@@@@@@@@@@:
                       .%@@@@@:  -@@@@@@@@@@@@=
                        .#@@%.    .%@@@@@@@@@@@*
                          +*        #@@@@@@@@@@@*

                        Hey, my name's Maxim.
        Security & infra engineer based in New York, NY. · hi, yourname

                     home  ·  thoughts  ·  etc

              h/l select · enter open · q quit · ? keys
```

## Dev

Local, no SSH (GHC + cabal via ghcup):

```sh
cd ssh && cabal run kio-tui
```

Full container (build from the repo root; the image embeds `src/content`):

```sh
make -C ./ssh dev        # build + run fg on :2222, Ctrl+C to stop
ssh localhost -p 2222    # connect
```

## To-do

- Fly setup: `fly launch --no-deploy`, create the `ssh_keys` volume,
  allocate a dedicated IPv4, connect the repo (see `fly.toml`)
- `f`-style link-hint mode so keyboard-only visitors can follow inline links
- Optional: `makeLenses` for `St` to shrink the record-update lambdas in
  `Events`
- Other vi/vim navigation fun-to-have's
