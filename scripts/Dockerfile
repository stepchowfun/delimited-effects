FROM debian:stretch

RUN \
  DEBIAN_FRONTEND=noninteractive apt-get -y update && \
  DEBIAN_FRONTEND=noninteractive apt-get -y install \
    'build-essential=12.3' \
    'libreadline-dev=7.0-3' \
    'python-pip=9.0.1-2' \
    'ruby=1:2.3.3' \
    'texlive-full=2016.20170123-5' && \
  rm -rf /var/lib/apt/lists/* && \
  rm -rf /usr/share/doc

RUN \
  DEBIAN_FRONTEND=noninteractive apt-get -y update && \
  DEBIAN_FRONTEND=noninteractive apt-get -y install \
    'camlp5=6.16-1' \
    'curl=7.52.1-5+deb9u7' \
    'ocaml=4.02.3-9' && \
  rm -rf /var/lib/apt/lists/* && \
  rm -rf /usr/share/doc && \
  curl -fsSLo coq.tar.gz https://github.com/coq/coq/archive/V8.8.2.tar.gz && \
  tar -xzf coq.tar.gz && \
  rm coq.tar.gz && \
  cd coq-8.8.2 && \
  ./configure -prefix /usr/local && \
  make && \
  make install && \
  cd .. && \
  rm -rf coq-8.8.2 && \
  DEBIAN_FRONTEND=noninteractive apt-get -y purge --auto-remove \
    camlp5 \
    curl \
    ocaml

RUN pip install 'awscli==1.16.40'

RUN gem install octokit --version '4.13.0'

# The Stack installation script apparently uses apt-get.
RUN \
  DEBIAN_FRONTEND=noninteractive apt-get -y update && \
  DEBIAN_FRONTEND=noninteractive apt-get -y install \
    'curl=7.52.1-5+deb9u7' && \
  curl -sSL https://get.haskellstack.org/ | sh && \
  rm -rf /var/lib/apt/lists/* && \
  rm -rf /usr/share/doc && \
  DEBIAN_FRONTEND=noninteractive apt-get -y purge --auto-remove curl

RUN \
  DEBIAN_FRONTEND=noninteractive apt-get -y update && \
  DEBIAN_FRONTEND=noninteractive apt-get -y install \
    'curl=7.52.1-5+deb9u7' && \
  curl -LSfs \
    https://raw.githubusercontent.com/stepchowfun/tagref/master/install.sh | \
    sh && \
  rm -rf /var/lib/apt/lists/* && \
  rm -rf /usr/share/doc && \
  DEBIAN_FRONTEND=noninteractive apt-get -y purge --auto-remove curl

RUN useradd --user-group --create-home user

USER user:user

WORKDIR /home/user

# Without this, Ruby will assume files are encoded as ASCII.
RUN echo 'export LANG="C.UTF-8"' >> ~/.profile

# Stack installs executables in $HOME/.local/bin.
RUN \
  mkdir -p "$HOME/.local/bin" && \
  echo 'export PATH="$HOME/.local/bin:$PATH"' >> ~/.profile

RUN stack setup --resolver lts-12.14

RUN stack install hindent hlint
