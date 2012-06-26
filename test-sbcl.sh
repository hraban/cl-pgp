#!/bin/sh

# Run all FiveAM unit tests under SBCL

SBCL=sbcl

$SBCL --eval "(asdf:operate 'asdf:load-op \"cl-pgp\")" \
      --eval "(5am:run! 'cl-pgp::cl-pgp)" \
      --eval "(sb-ext:quit)"
