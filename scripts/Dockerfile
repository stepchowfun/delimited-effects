FROM debian:stretch

RUN \
  DEBIAN_FRONTEND=noninteractive apt-get -y update && \
  DEBIAN_FRONTEND=noninteractive apt-get -y install \
    build-essential=12.3 \
    coq=8.6-4 \
    curl=7.52.1-5+deb9u3 \
    python-pip=9.0.1-2 \
    texlive-full=2016.20170123-5 && \
  rm -rf /var/lib/apt/lists/* && \
  rm -rf /usr/share/doc

RUN pip install awscli==1.14.28

# The Stack installation script apparently uses apt-get.
RUN \
  DEBIAN_FRONTEND=noninteractive apt-get -y update && \
  curl -sSL https://get.haskellstack.org/ | sh && \
  rm -rf /var/lib/apt/lists/* && \
  rm -rf /usr/share/doc

RUN useradd --user-group --create-home user

USER user:user

WORKDIR /home/user

# Stack installs executables in $HOME/.local/bin.
RUN \
  mkdir -p "$HOME/.local/bin" && \
  echo 'export PATH="$HOME/.local/bin:$PATH"' >> ~/.profile

RUN stack setup --resolver lts-10.3

# The `mkdir` command is needed to work around a bug in brittany.
# See https://github.com/lspitzner/brittany/issues/115 for details.
RUN mkdir "$HOME/.config" && stack install brittany
