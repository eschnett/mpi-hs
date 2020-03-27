# How to build this Docker image:
#     docker build . --tag eschnett/mpi-hs
#     docker push eschnett/mpi-hs

# Use Ubuntu
FROM ubuntu:19.10

WORKDIR /home

# Install system packages and stack
RUN apt-get update && \
    apt-get --yes --no-install-recommends install \
        build-essential \
        ca-certificates \
        git \
        libmpich-dev \
        wget && \
    wget -qO- https://get.haskellstack.org/ | sh && \
    rm -rf /var/lib/apt/lists/*
