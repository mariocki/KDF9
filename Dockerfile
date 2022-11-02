# syntax = docker/dockerfile:1.4.0

FROM debian:sid-slim as build-stage

RUN apt-get update && apt-get install -y --no-install-recommends \
  apt-transport-https \
  autoconf \
  automake \
  build-essential \
  ca-certificates \
  git-core \
  gnat \
  bison

WORKDIR /build

ADD http://date.jsontest.com skipcache

RUN git clone --depth 1 https://github.com/mariocki/KDF9.git

WORKDIR KDF9

RUN make all

RUN mkdir -p /build/kdf-install

RUN make install DESTDIR=/build/kdf-install

WORKDIR /build/

RUN mkdir -p kdf-install/DEBIAN

RUN echo "Package: kdf-install" >> kdf-install/DEBIAN/control

RUN echo "Version: 1.0" >> kdf-install/DEBIAN/control

RUN echo $'Section: base\n\
Priority: optional\n\
Architecture: amd64\n\
Depends: libgnat-11, file\n\
Maintainer: Your Name <you@email.com>\n\
Description: KDF9 Emulator from https://github.com/mariocki/KDF9' >> kdf-install/DEBIAN/control

RUN dpkg-deb --build kdf-install

FROM debian:sid-slim as kdf9

COPY --from=build-stage /build/kdf-install.deb /kdf-install.deb

RUN apt-get update && apt-get -y upgrade && apt install -y libgnat-11 file

RUN dpkg -i /kdf-install.deb

RUN kdf9_setup

WORKDIR /root/.kdf9

ENTRYPOINT /bin/bash
