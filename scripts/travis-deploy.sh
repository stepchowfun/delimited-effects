#!/usr/bin/env bash
set -eu -o pipefail

# This script uploads `/home/user/repo/main.pdf` to S3 if certain conditions
# are met. It is intended to be run from inside the Docker container.

# Usage:
#   AWS_ACCESS_KEY_ID=AKIAIOSFODNN7EXAMPLE \
#   AWS_DEFAULT_REGION=us-east-1 \
#   AWS_SECRET_ACCESS_KEY=wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY \
#   TRAVIS_BRANCH=master \
#   TRAVIS_DEPLOY=true \
#   TRAVIS_PULL_REQUEST=123 \
#   TRAVIS_PULL_REQUEST_BRANCH=foo \
#   ./travis-deploy.sh

S3_PREFIX='s3://stephan-misc/paper'

if [ "$TRAVIS_DEPLOY" = 'true' ]; then
  echo 'Deploying...'

  if [ "$TRAVIS_PULL_REQUEST" = 'false' ]; then
    echo 'Not a pull request build. Checking branch...'

    if [ "$TRAVIS_BRANCH" = 'master' ]; then
      echo 'Master. Continuing...'
      S3_SUFFIX='latest.pdf'
    else
      echo 'Non-master. Aborting.'
      exit 0
    fi
  else
    echo 'Pull request build.'
    S3_SUFFIX="branch-$TRAVIS_PULL_REQUEST_BRANCH.pdf"
  fi

  echo 'Uploading...'
  aws s3 cp --acl public-read /home/user/repo/main.pdf "$S3_PREFIX/$S3_SUFFIX"
fi

echo 'Done.'
