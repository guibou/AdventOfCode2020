#!/usr/bin/env bash
echo -e "infixl 7 +++\n(+++) = (+)\ninfixl 7 ***\n(***) = (*)\nmain = print $ sum$ [$(cat content/day18 | sed "s/+/+++/g" | sed "s/*/***/g" | sed "s/^/    /" | sed "s/$/,/")0]" | runhaskell
echo -e "infixl 8 +++\n(+++) = (+)\ninfixl 7 ***\n(***) = (*)\nmain = print $ sum$ [$(cat content/day18 | sed "s/+/+++/g" | sed "s/*/***/g" | sed "s/^/    /" | sed "s/$/,/")0]" | runhaskell
