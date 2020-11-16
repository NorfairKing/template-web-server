#!/usr/bin/env bash

export DEVELOPMENT=True

stack install foo-bar-web-server \
  --file-watch --watch-all \
  --no-nix-pure \
  --exec='./scripts/restart-foo-bar-web-server.sh'

