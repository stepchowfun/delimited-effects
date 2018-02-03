#!/usr/bin/env ruby

# This script posts a link to the build artifact in a pull request.

# Usage:
#   GITHUB_TOKEN=4a68631afb82bala9f9c49892e0e3c82eaa7ef66 \
#   TRAVIS_BRANCH=foo \
#   TRAVIS_PULL_REQUEST=123 \
#   TRAVIS_REPO_SLUG=owner_name/repo_name \
#   ./post-artifact-link.rb

exit if ENV['TRAVIS_PULL_REQUEST'] == 'false'

require 'octokit'

Octokit.configure do |c|
  c.access_token = ENV['GITHUB_TOKEN']
  c.auto_paginate = true
end

body = "[Here](https://s3.amazonaws.com/stephan-misc/paper/branch-" \
  "#{ENV['TRAVIS_BRANCH']}.pdf) is a link to the PDF generated from this PR."

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
