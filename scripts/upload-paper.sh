#!/usr/bin/env bash
set -eu -o pipefail

# This script uploads the PDF to S3. The destination path determined by
# environment variables from Travis.

# Usage:
#   AWS_ACCESS_KEY_ID=AKIAIOSFODNN7EXAMPLE \
#   AWS_DEFAULT_REGION=us-east-1 \
#   AWS_SECRET_ACCESS_KEY=wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY \
#   PDF_PATH=/home/user/repo/main.pdf \
#   TRAVIS_BRANCH=master \
#   TRAVIS_PULL_REQUEST=123 \
#   TRAVIS_PULL_REQUEST_BRANCH=foo \
#   ./upload-paper.sh

S3_PREFIX='s3://stephan-misc/paper'

if [ "$TRAVIS_PULL_REQUEST" = 'false' ]; then
  if [ "$TRAVIS_BRANCH" = 'master' ]; then
    S3_SUFFIX='latest.pdf'
  else
    exit 0
  fi
else
  S3_SUFFIX="branch-$TRAVIS_PULL_REQUEST_BRANCH.pdf"
fi

aws s3 cp --acl public-read "$PDF_PATH" "$S3_PREFIX/$S3_SUFFIX"
