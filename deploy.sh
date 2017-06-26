#!/usr/bin/env bash
set -eu -o pipefail

# This script builds the PDF and uploads it to S3.
# It also installs the necessary dependencies.

# Usage:
#   TRAVIS_BRANCH=master \
#   TRAVIS_PULL_REQUEST=false \
#   AWS_DEFAULT_REGION=us-east-1 \
#   AWS_ACCESS_KEY_ID=AKIAIOSFODNN7EXAMPLE \
#   AWS_SECRET_ACCESS_KEY=wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY \
#   ./deploy.sh

# Build the PDF, check the Coq scripts, and lint the source.
make docker-build

# Upload the PDF to S3.
if [ "$TRAVIS_PULL_REQUEST" = 'false' ]; then
  if [ "$TRAVIS_BRANCH" = 'master' ]; then
    docker run \
      -e "AWS_DEFAULT_REGION=$AWS_DEFAULT_REGION" \
      -e "AWS_ACCESS_KEY_ID=$AWS_ACCESS_KEY_ID" \
      -e "AWS_SECRET_ACCESS_KEY=$AWS_SECRET_ACCESS_KEY" \
      -it stephanmisc/delimited-effects:build aws s3 cp --acl public-read \
      /root/delimited-effects/main.pdf \
      s3://stephan-misc/paper/latest.pdf
  else
    docker run \
      -e "AWS_DEFAULT_REGION=$AWS_DEFAULT_REGION" \
      -e "AWS_ACCESS_KEY_ID=$AWS_ACCESS_KEY_ID" \
      -e "AWS_SECRET_ACCESS_KEY=$AWS_SECRET_ACCESS_KEY" \
      -it stephanmisc/delimited-effects:build aws s3 cp --acl public-read \
      /root/delimited-effects/main.pdf \
      "s3://stephan-misc/paper/branch-$TRAVIS_BRANCH.pdf"
  fi
fi
