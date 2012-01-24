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
    -packagehspec-shouldbe\
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
    -packagehspec-shouldbe\
    -packageattoparsec -DUSE_ATTOPARSEC\
    -i../src Spec.hs $*
