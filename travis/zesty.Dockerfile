FROM ubuntu:zesty

RUN apt-get update && \
    apt-get -y install software-properties-common && \
    add-apt-repository -y ppa:hvr/ghc && \
    apt-get update && \
    apt-get -y install happy-1.19.5 alex-3.1.7 libgirepository1.0-dev libgtksourceview-3.0-dev libwebkit2gtk-4.0-dev libtinfo-dev

RUN mkdir /build
WORKDIR /build

