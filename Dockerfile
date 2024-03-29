# syntax = docker/dockerfile:1.4.0

FROM debian:stable-slim as build-stage

RUN apt-get update && apt-get install -y --no-install-recommends \
  apt-transport-https \
  build-essential \
  ca-certificates \
  git-core \
  gnat \
  bison

ADD http://date.jsontest.com skipcache

ADD . /build/

WORKDIR /build/

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
Depends: libgnat-10, file\n\
Maintainer: Your Name <you@email.com>\n\
Description: KDF9 Emulator from https://github.com/mariocki/KDF9' >> kdf-install/DEBIAN/control

RUN dpkg-deb --build kdf-install

FROM debian:stable-slim as kdf9

COPY --from=build-stage /build/kdf-install.deb /kdf-install.deb

RUN apt-get update && apt-get -y upgrade && apt install -y libgnat-10 file vim

RUN dpkg -i /kdf-install.deb

WORKDIR /root

ENTRYPOINT /bin/bash
