#!/usr/bin/env bash
set -eu -o pipefail

# This script builds the PDF and uploads it to S3.
# It also installs the necessary dependencies.

# Usage:
#   TRAVIS_BRANCH=master \
#   AWS_DEFAULT_REGION=us-east-1 \
#   AWS_ACCESS_KEY_ID=AKIAIOSFODNN7EXAMPLE \
#   AWS_SECRET_ACCESS_KEY=wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY \
#   ./deploy.sh

DEBIAN_FRONTEND=noninteractive sudo apt-get -y update
DEBIAN_FRONTEND=noninteractive sudo apt-get install -y python-pip texlive-full
sudo pip install awscli
make
if [ "$TRAVIS_PULL_REQUEST" = 'false' ]; then
  if [ "$TRAVIS_BRANCH" = 'master' ]; then
    aws s3 cp --acl public-read main.pdf 's3://stephan-misc/paper/latest.pdf'
  else
    aws s3 cp --acl public-read main.pdf "s3://stephan-misc/paper/branch-$TRAVIS_BRANCH.pdf"
  fi
fi
