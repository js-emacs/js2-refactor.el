#!/bin/sh -e
cask exec ecukes "$@"
cask exec buttercup -l test/test-helper.el  -L . "$@"
