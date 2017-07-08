#!/usr/bin/env bash
set -eu -o pipefail

# This script uploads `main.pdf` to S3.

# Usage:
#   TRAVIS_BRANCH=master \
#   AWS_DEFAULT_REGION=us-east-1 \
#   AWS_ACCESS_KEY_ID=AKIAIOSFODNN7EXAMPLE \
#   AWS_SECRET_ACCESS_KEY=wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY \
#   ./deploy.sh

if [ "$TRAVIS_BRANCH" = 'master' ]; then
  S3_DESTINATION='s3://stephan-misc/paper/latest.pdf'
else
  S3_DESTINATION="s3://stephan-misc/paper/branch-$TRAVIS_BRANCH.pdf"
fi

docker run \
  --rm \
  -e "AWS_DEFAULT_REGION=$AWS_DEFAULT_REGION" \
  -e "AWS_ACCESS_KEY_ID=$AWS_ACCESS_KEY_ID" \
  -e "AWS_SECRET_ACCESS_KEY=$AWS_SECRET_ACCESS_KEY" \
  -v $(pwd):/root \
  stephanmisc/delimited-effects:deps \
  sh -c \
    "cd /root && \
    aws s3 cp --acl public-read \
      /root/main.pdf \
      $S3_DESTINATION"
