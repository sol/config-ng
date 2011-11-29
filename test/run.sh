#!/bin/bash
cd "`dirname $0`"
runhaskell -hide-all-packages\
    -packagebase\
    -packagecontainers\
    -packagetext\
    -packageattoparsec\
    -packagestringbuilder\
    -packageHUnit\
    -packageQuickCheck\
    -packagehspec\
    -i../src Spec.hs $*
