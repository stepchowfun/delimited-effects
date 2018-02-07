#!/usr/bin/env ruby

# This script uploads the PDF of the paper to S3 and/or posts a link to it in
# the pull request, if applicable.
#
# Usage:
#   AWS_ACCESS_KEY_ID=AKIAIOSFODNN7EXAMPLE \
#   AWS_DEFAULT_REGION=us-east-1 \
#   AWS_SECRET_ACCESS_KEY=wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY \
#   GITHUB_TOKEN=4a68631afb82bala9f9c49892e0e3c82eaa7ef66 \
#   TRAVIS_BRANCH=master \
#   TRAVIS_PULL_REQUEST=123 \
#   TRAVIS_PULL_REQUEST_BRANCH=foo \
#   TRAVIS_REPO_SLUG=owner_name/repo_name \
#   ./deploy.rb

require 'octokit'

S3_PREFIX = 's3://stephan-misc/paper'

# Determine if and where we should upload the PDF to S3.
s3_suffix =
  if ENV['TRAVIS_PULL_REQUEST'] == 'false'
    ENV['TRAVIS_BRANCH'] == 'master' ? 'latest.pdf' : nil
  else
    "branch-#{ENV['TRAVIS_PULL_REQUEST_BRANCH']}.pdf"
  end

# If we need to upload the PDF, do it now.
if s3_suffix
  raise "Failed to upload the PDF to S3." if !system(
    "aws s3 cp --acl public-read " \
      "/home/user/repo/main.pdf " \
      "'#{S3_PREFIX}/#{s3_suffix}'"
  )
end

# For pull requests, post a message with a link to the PDF.
if ENV['TRAVIS_PULL_REQUEST'] != 'false'
  Octokit.configure do |c|
    c.access_token = ENV['GITHUB_TOKEN']
    c.auto_paginate = true
  end

  body = "[Here](https://s3.amazonaws.com/stephan-misc/paper/branch-" \
    "#{ENV['TRAVIS_PULL_REQUEST_BRANCH']}.pdf) is a link to the PDF " \
    "generated from this PR. This is an automated message."

  if !Octokit.issue_comments(
    ENV['TRAVIS_REPO_SLUG'],
    ENV['TRAVIS_PULL_REQUEST']
  ).any? { |comment| comment.body == body }
    Octokit.add_comment(
      ENV['TRAVIS_REPO_SLUG'],
      ENV['TRAVIS_PULL_REQUEST'],
      body
    )
  end
end
