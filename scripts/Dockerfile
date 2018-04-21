FROM debian:stretch

# We install the `libtinfo-dev` package to work around a strange bug in Stack.
# Without it, we get this error when running `stack install brittany` below:
#   <command line>: can't load .so/.DLL for: libtinfo.so
#   (libtinfo.so: cannot open shared object file: No such file or directory)
RUN \
  DEBIAN_FRONTEND=noninteractive apt-get -y update && \
  DEBIAN_FRONTEND=noninteractive apt-get -y install \
    'build-essential=12.3' \
    'libtinfo-dev=6.0+*' \
    'python-pip=9.0.*' \
    'ruby=1:2.3.*' \
    'texlive-full=2016.*' && \
  rm -rf /var/lib/apt/lists/* && \
  rm -rf /usr/share/doc

# Without this, Ruby will assume that files are encoded as ASCII.
RUN echo 'export LANG="C.UTF-8"' >> ~/.profile

RUN \
  DEBIAN_FRONTEND=noninteractive apt-get -y update && \
  DEBIAN_FRONTEND=noninteractive apt-get -y install \
    'camlp5=6.16-*' \
    'curl=7.52.*' \
    'ocaml=4.02.*' && \
  rm -rf /var/lib/apt/lists/* && \
  rm -rf /usr/share/doc && \
  curl -fsSLo coq.tar.gz https://github.com/coq/coq/archive/V8.8.0.tar.gz && \
  tar -xzf coq.tar.gz && \
  rm coq.tar.gz && \
  cd coq-8.8.0 && \
  ./configure -prefix /usr/local && \
  make && \
  make install && \
  cd .. && \
  rm -rf coq-8.8.0 && \
  DEBIAN_FRONTEND=noninteractive apt-get -y purge --auto-remove \
    camlp5 \
    curl \
    ocaml

RUN pip install 'awscli==1.14.*'

RUN gem install octokit --version '~> 4.8.0'

# The Stack installation script apparently uses apt-get.
RUN \
  DEBIAN_FRONTEND=noninteractive apt-get -y update && \
  DEBIAN_FRONTEND=noninteractive apt-get -y install 'curl=7.52.*' && \
  curl -sSL https://get.haskellstack.org/ | sh && \
  rm -rf /var/lib/apt/lists/* && \
  rm -rf /usr/share/doc && \
  DEBIAN_FRONTEND=noninteractive apt-get -y purge --auto-remove curl

RUN useradd --user-group --create-home user

USER user:user

WORKDIR /home/user

# Stack installs executables in $HOME/.local/bin.
RUN \
  mkdir -p "$HOME/.local/bin" && \
  echo 'export PATH="$HOME/.local/bin:$PATH"' >> ~/.profile

RUN stack setup --resolver lts-10.4

# The `mkdir` command is needed to work around a bug in brittany. See
# https://github.com/lspitzner/brittany/issues/115 for details.
RUN mkdir "$HOME/.config" && stack install brittany hlint
