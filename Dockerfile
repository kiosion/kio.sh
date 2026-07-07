# Build from the repo root so compile-time content embedding can see
# src/content: docker build -f ssh/Dockerfile -t kio-ssh .
FROM haskell:9.6-slim AS build
WORKDIR /build/ssh
COPY ssh/kio-tui.cabal ./
# Best-effort dependency layer so content/code edits don't recompile brick & co.
RUN cabal update && (cabal build --only-dependencies || true)
COPY ssh/sshd/nss_ato.c ./
RUN mkdir -p /out && gcc -shared -fPIC -O2 -o /out/libnss_ato.so.2 nss_ato.c
COPY src/content /build/src/content
COPY ssh/app app
# cabal install builds from an sdist copy, which breaks the relative
# ../src/content embed paths -- build in place instead.
RUN cabal build exe:kio-tui && cp "$(cabal list-bin kio-tui)" /out/kio-tui

FROM debian:bookworm-slim
RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    openssh-server libgmp10 libtinfo6 ncurses-base \
  && rm -rf /var/lib/apt/lists/* \
  # ForceCommand runs through the user's shell, so it must be a real one
  && useradd -m -s /bin/sh blog \
  && passwd -d blog
COPY --from=build /out/kio-tui /usr/local/bin/kio-tui
COPY --from=build /out/libnss_ato.so.2 /usr/lib/libnss_ato.so.2
COPY ssh/sshd/sshd_config /etc/ssh/sshd_config
COPY ssh/sshd/entrypoint.sh /entrypoint.sh
RUN chmod +x /entrypoint.sh \
  && ldconfig \
  && sed -i 's/^passwd:.*/passwd:         files ato/' /etc/nsswitch.conf
VOLUME /etc/ssh/keys
EXPOSE 22
CMD ["/entrypoint.sh"]
