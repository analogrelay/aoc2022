FROM debian:11

# Dependencies for Alire
RUN apt-get update && apt-get install -y \
    gnat \
    gprbuild \
    build-essential \
    git \
    curl \
    && rm -Rf /var/lib/apt/lists/*

RUN mkdir /work \
    && cd /work \
    && git clone --recurse-submodules https://github.com/alire-project/alire.git \
    && cd alire \
    && gprbuild -j0 -P alr_env \
    && cp bin/alr /usr/local/bin \
    && cd / \
    && rm -Rf /work

# Install Ada Toolchain
RUN alr toolchain --install gnat_native=12.2.1 --install gprbuild=22.0.1