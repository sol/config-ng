#!/bin/bash
cd "`dirname $0`"

runhaskell -hide-all-packages\
    -packagebase\
    -packagecontainers\
    -packagetext\
    -packagestringbuilder\
    -packageHUnit\
    -packageQuickCheck\
    -packagehspec\
    -packageparsec\
    -i../src Spec.hs $*

runhaskell -hide-all-packages\
    -packagebase\
    -packagecontainers\
    -packagetext\
    -packagestringbuilder\
    -packageHUnit\
    -packageQuickCheck\
    -packagehspec\
    -packageattoparsec -DUSE_ATTOPARSEC\
    -i../src Spec.hs $*
