# Build context is the repo root: docker build -t kio-ssh .
FROM haskell:9.6-slim AS build
WORKDIR /build
COPY kio-tui.cabal ./
# Best-effort dependency layer so content/code edits don't recompile brick & co.
RUN cabal update && (cabal build --only-dependencies || true)
COPY sshd/nss_ato.c ./
RUN mkdir -p /out && gcc -shared -fPIC -O2 -o /out/libnss_ato.so.2 nss_ato.c
# Content's source of truth is the kio.dev repo; pull just src/content at build
# time (sparse, blobless) so publishing a post there needs no change here.
RUN apt-get update && apt-get install -y --no-install-recommends git ca-certificates \
  && rm -rf /var/lib/apt/lists/* \
  && git clone --depth 1 --filter=blob:none --sparse \
       https://github.com/kiosion/kio.dev.git /tmp/kio.dev \
  && git -C /tmp/kio.dev sparse-checkout set src/content \
  && cp -a /tmp/kio.dev/src/content content \
  && rm -rf /tmp/kio.dev
COPY logo.txt ./
COPY app app
# cabal install builds from an sdist copy, which breaks the relative
# content embed paths -- build in place instead.
RUN cabal build exe:kio-tui && cp "$(cabal list-bin kio-tui)" /out/kio-tui && strip /out/kio-tui
# Non-tty run prints the plain listing, forcing every post's frontmatter
# parse -- fails the image build (not a visitor's session) on a bad post.
RUN /out/kio-tui </dev/null >/dev/null

FROM debian:bookworm-slim
RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    # ncurses-term: terminfo for modern terminals (kitty, ghostty,
    # alacritty, wezterm, foot, ...) that ncurses-base lacks;
    # busybox serves the static "connect via ssh" page for browsers
    openssh-server libgmp10 libtinfo6 ncurses-base ncurses-term busybox \
  && rm -rf /var/lib/apt/lists/* \
  # ForceCommand runs through the user's shell, so it must be a real one
  && useradd -m -s /bin/sh blog \
  && passwd -d blog
COPY --from=build /out/kio-tui /usr/local/bin/kio-tui
COPY --from=build /out/libnss_ato.so.2 /usr/lib/libnss_ato.so.2
COPY sshd/sshd_config /etc/ssh/sshd_config
COPY sshd/entrypoint.sh /entrypoint.sh
# landing page for browsers, with the logo injected from its one source
# and the site's mono font served alongside it
COPY http/index.html /var/www/index.html
COPY http/fonts/CommitMono-Regular.woff2 /var/www/CommitMono-Regular.woff2
COPY http/fonts/CommitMono-Bold.woff2 /var/www/CommitMono-Bold.woff2
COPY logo.txt /tmp/logo.txt
RUN sed -i -e '/@LOGO@/r /tmp/logo.txt' -e '/@LOGO@/d' /var/www/index.html \
  && rm /tmp/logo.txt \
  # legit crawlers fetch this constantly; a real file beats recurring 404s
  && printf 'User-agent: *\nAllow: /\n' > /var/www/robots.txt
RUN chmod +x /entrypoint.sh \
  && ldconfig \
  && sed -i \
    -e 's/^passwd:.*/passwd:         files ato/' \
    -e 's/^shadow:.*/shadow:         files ato/' \
    /etc/nsswitch.conf
VOLUME /etc/ssh/keys
EXPOSE 2222
CMD ["/entrypoint.sh"]
