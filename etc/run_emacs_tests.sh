#!/bin/bash

if [ $# -ge 1 ]; then
  emacs --no-init-file --load .emacs_test.el --eval '(ensime-run-one-test "'"$*"'")'
else
  emacs --no-init-file --load .emacs_test.el  --eval '(ensime-run-all-tests)'
fi
