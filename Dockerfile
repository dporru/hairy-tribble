FROM debian:jessie
MAINTAINER Daan Porru <daan@porru.nl>

# Update package list and install GHC and Cabal requirements.
RUN apt-get -y update &&\
    apt-get -y install build-essential curl zlib1g-dev libgmp3-dev libedit2 wget

# Install GHC 7.8.4, newer version of GHC comes with an incompatible version of Base.
RUN wget https://www.haskell.org/ghc/dist/7.8.4/ghc-7.8.4-x86_64-unknown-linux-deb7.tar.bz2 &&\
    tar xvfj ghc-7.8.4-x86_64-unknown-linux-deb7.tar.bz2 &&\
    cd ghc-7.8.4 &&\
    ./configure &&\
    make install &&\
    ghc --version &&\
    cd .. &&\
    rm -fr ghc-7.8.4-x86_64-unknown-linux-deb7.tar.bz2 ghc-7.8.4

# Install Cabal install 1.22.3.0
RUN wget http://hackage.haskell.org/package/cabal-install-1.22.3.0/cabal-install-1.22.3.0.tar.gz &&\
    tar xvfz cabal-install-1.22.3.0.tar.gz &&\
    cd cabal-install-1.22.3.0 && ./bootstrap.sh &&\
    cd .. &&\
    rm -fr cabal-install-1.22.3.0*

# Add cabal binaries to the PATH
ENV PATH /root/.cabal/bin:$PATH
ENV LANG C.UTF-8

# Install app-specific requirements.
RUN apt-get update -y &&\
    apt-get install -y git libtinfo-dev texlive-xetex

# Add user ph.
RUN chmod a+rx /root &&\
    groupadd -g 9000 ph &&\
    useradd -mg 9000 ph &&\
    chown ph:ph /home/ph &&\
    mkdir -p /hairy-tribble/TCache &&\
    chown -R ph:ph /hairy-tribble

# Run the rest of the statements as user ph.
USER ph

# Expose port 8000 and set workdir for CMD command.
EXPOSE 8000
WORKDIR /hairy-tribble

# Expose /hairy-tribble as a volume for development.
VOLUME ["/hairy-tribble", "/hairy-tribble/TCache"]

# Install happy.
RUN cabal update &&\
    cabal install happy

# Download enhanced version of TCache.
RUN cd /home/ph &&\
    git clone https://github.com/ariep/TCache.git &&\
    cd TCache &&\
    git checkout 5c14e879fa62642d4ea5ebedcf54f9e5edc5999c

# Copy Cabal install file and only install dependecies.
COPY ./ph.cabal /hairy-tribble/ph.cabal
RUN cd /home/ph &&\
    cabal sandbox init &&\
    cabal sandbox add-source /home/ph/TCache &&\
    cabal sandbox add-source /hairy-tribble &&\
    cabal update &&\
    touch /hairy-tribble/LICENSE &&\
    cabal install --only-dependencies ph &&\
    rm /hairy-tribble/LICENSE

# Now do actual build.
ADD ./src/ /hairy-tribble/src/
RUN cd /home/ph &&\
    touch /hairy-tribble/LICENSE &&\
	cabal install ph &&\
	rm /hairy-tribble/LICENSE

# Put sanbox binaries in path.
ENV PATH /home/ph/.cabal-sandbox/bin:$PATH

# Add rest framework files.
ADD ./rest-gen-files/ /hairy-tribble/rest-gen-files/

# Add web-client files.
ADD ./client/ /hairy-tribble/client/

# Run rest when this container is started.
CMD ["rest"]

