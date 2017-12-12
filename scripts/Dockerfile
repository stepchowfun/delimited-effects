FROM debian:stretch

# This command will execute the following actions:
#   - Update package lists.
#   - Install the packages
#       Make,
#       Coq,
#       cURL,
#       pip, for installing the AWS CLI below, and
#       LaTeX.
#   - Install the AWS CLI.
#   - Install Stack.
#   - Install GHC. The resolver specified in this command must match the one in
#       implementation/stack.yaml to ensure that we install the version of GHC
#       that the implementation uses.
#   - Free up some space by removing the package lists, now that the packages
#       are installed.
#   - Free up 2G of disk space by deleting documentation for the packages.
#   - Create a user.
RUN \
  DEBIAN_FRONTEND=noninteractive apt-get -y update && \
  DEBIAN_FRONTEND=noninteractive apt-get -y install \
    build-essential \
    coq=8.6-4 \
    curl \
    python-pip \
    texlive-full && \
  pip install awscli && \
  curl -sSL https://get.haskellstack.org/ | sh && \
  stack --resolver lts-9.8 setup && \
  rm -rf /var/lib/apt/lists/* && \
  rm -rf /usr/share/doc && \
  useradd --user-group --create-home user

# Set the default user.
USER user:user

# Set the default working directory to be the user's home directory.
WORKDIR /home/user
