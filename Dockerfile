ARG CSE507_UBUNTU_TAG=rolling

FROM --platform=linux/amd64 ubuntu:${CSE507_UBUNTU_TAG} AS base
ENV RACKET_VERSION=8.10
ENV RACKET_INSTALLER_URL=https://download.racket-lang.org/installers/${RACKET_VERSION}/racket-minimal-${RACKET_VERSION}-x86_64-linux-natipkg.sh
ENV SSL_CERT_DIR=/etc/ssl/certs
ENV SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt
RUN apt-get update -y \
    && DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
        build-essential \
        ca-certificates \
        curl \
        git \
        racket \
        sqlite3 \
        sudo \
    && apt-get clean \
    && mv /home/ubuntu /home/cse507 \
    && groupmod -n cse507 ubuntu \
    && usermod -d /home/cse507 -c CSE507 -l cse507 ubuntu \
    && echo "cse507 ALL=(ALL:ALL) NOPASSWD: ALL" >> /etc/sudoers \
    && yes | raco pkg install rosette
USER cse507
WORKDIR /home/cse507
RUN git clone git@github.com:arminbiere/cadical.git \
    && cadical/configure -l \
    && cadical/make

FROM --platform=linux/amd64 base AS latest
USER cse507
WORKDIR /home/cse507/cse507
COPY --chown=cse507:cse507 /.git/ .git
RUN git reset --hard

FROM --platform=linux/amd64 latest AS tested

