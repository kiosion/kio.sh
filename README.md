# kio.dev over SSH

As a little easter-egg and fun project, this site's available via SSH at
`ssh kio.sh`.

Stock OpenSSH does all the crypto/auth/pty work; a tiny NSS module
(`sshd/nss_ato.c`) maps every username onto the inert `blog` user, whose
`ForceCommand` is the Haskell TUI using brick.

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

Local, without SSH layer (GHC + cabal via ghcup):

```sh
make content && cabal run kio-tui
```

Full container:

```sh
make dev                 # build + run fg on :2222, Ctrl+C to stop
ssh localhost -p 2222    # connect
```

## To-do

- a11y: `f`-style link-hint mode so keyboard-only visitors can follow inline links
- try: `makeLenses` for `St` to shrink the record-update lambdas in `Events`
- other: vi/vim navigation fun-to-have's
