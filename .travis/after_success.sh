#!/bin/bash

set -ex

# Inspired by https://gist.github.com/domenic/ec8b0fc8ab45f39403dd
if [ "x$DOCS" = "xdeploy" ]; then
  SOURCE_BRANCH="master"
  TARGET_BRANCH="gh-pages"

  if [ "$TRAVIS_PULL_REQUEST" != "false" -o "$TRAVIS_BRANCH" != "$SOURCE_BRANCH" ]; then
    echo "Skipping deploy"
    exit 0
  fi

  # Save some useful information
  REPO=`git config remote.origin.url`
  SSH_REPO=${REPO/https:\/\/github.com\//git@github.com:}
  SHA=`git rev-parse --verify HEAD`

  # Clone the existing gh-pages for this repo into out/
  # Create a new empty branch if gh-pages doesn't exist yet (should only happen on first deply)
  git clone $REPO out
  cd out
  git checkout $TARGET_BRANCH || (git checkout --orphan $TARGET_BRANCH && git rm -f $(git ls-files))
  cd ..

  # Clean out existing contents
  rm -rf out/* || exit 0

  # Copy over the docs
  cp -r .stack-work/install/x86_64-linux/*/*/doc/* out/
  
  # Now let's go have some fun with the cloned repo
  cd out
  git config user.name "Travis CI"
  git config user.email "$COMMIT_AUTHOR_EMAIL"

  # Commit the "changes", i.e. the new version.
  # The delta will show diffs between new and old versions.
  git add -A .

  # If there are no changes to the compiled out (e.g. this is a README update) then just bail.
  if git diff --cached --quiet; then
      echo "No changes to the output on this push; exiting."
      exit 0
  fi

  git commit --quiet -m "Deploy to GitHub Pages: ${SHA}"

  # Get the deploy key by using Travis's stored variables to decrypt deploy_key.enc
  ENCRYPTED_KEY_VAR="encrypted_${ENCRYPTION_LABEL}_key"
  ENCRYPTED_IV_VAR="encrypted_${ENCRYPTION_LABEL}_iv"
  ENCRYPTED_KEY=${!ENCRYPTED_KEY_VAR}
  ENCRYPTED_IV=${!ENCRYPTED_IV_VAR}
  openssl aes-256-cbc -K $ENCRYPTED_KEY -iv $ENCRYPTED_IV -in ../.travis/deploy_key.enc -out ../deploy_key -d
  chmod 600 ../deploy_key
  eval `ssh-agent -s`
  ssh-add deploy_key

  # Now that we're all set up, we can push.
  git push $SSH_REPO $TARGET_BRANCH
fi