#!/bin/sh
# based on: http://sleepycoders.blogspot.se/2013/03/sharing-travis-ci-generated-files.html

# Only do it if not acting on a pull request.
if [ "$TRAVIS_PULL_REQUEST" == "false" ]; then
  # Go to home and setup git
  cd $HOME
  git config --global user.email "travis@travis-ci.org"
  git config --global user.name "Travis"

  # Using token clone gh-pages branch. Pipe to /dev/null to avoid printing the decrypted key.
  git clone --quiet --branch=gh-pages https://${GH_TOKEN}@github.com/krischer/mtspec.git  gh-pages > /dev/null

  # Go there, and overwrite everything with the freshly built contents.
  cd gh-pages
  rm -rf *
  cp -Rf $TRAVIS_BUILD_DIR/doc/html_doc/* .

  # add, commit and push files
  git add -f .
  git commit -m "Travis build $TRAVIS_BUILD_NUMBER pushed to gh-pages"
  git push -fq origin gh-pages > /dev/null
fi
