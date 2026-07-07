# kio.sh

[kio.dev](https://kio.dev), but over SSH — a little easter-egg. My site as a
terminal app you can just `ssh` into:

```sh
ssh kio.sh              # interactive
ssh kio.sh ls           # list posts
ssh kio.sh cat <slug>   # print one as markdown
```

Stock OpenSSH handles the crypto, auth, and pty; a tiny NSS module
(`sshd/nss_ato.c`) maps every username onto one inert `blog` user whose
`ForceCommand` is the TUI — a Haskell + [brick](https://github.com/jtdaugherty/brick)
app with the site content baked in at build. Browsers that wander in get an
HTTP man page instead.

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

## Hacking

Content is the real thing from [kio.dev](https://github.com/kiosion/kio.dev),
pulled in at build:

```sh
make content && cabal run kio-tui   # run it locally, no ssh layer
make dev                            # or the full container on :2222
```

## To-do

- a11y: `f`-style link-hint mode so keyboard-only visitors can follow inline links
- try: `makeLenses` for `St` to shrink the record-update lambdas in `Events`
- other: vi/vim navigation fun-to-have's
