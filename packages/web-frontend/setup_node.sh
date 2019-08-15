#!/bin/bash

rm -rfd "./.nvm"

export NVM_DIR="./.nvm" && (
  git clone https://github.com/nvm-sh/nvm.git "$NVM_DIR"
  cd "$NVM_DIR"
  git checkout `git describe --abbrev=0 --tags --match "v[0-9]*" $(git rev-list --tags --max-count=1)`
) && \. "$NVM_DIR/nvm.sh"

chmod u+x "$NVM_DIR/nvm.sh"

\. "$NVM_DIR/nvm.sh"
nvm install 8.0.0
nvm use 8.0.0

yarn install
yarn build

exit 0