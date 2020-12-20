#!/usr/bin/env bash
time cabal run alltests -- --match "works" | grep 'Day\|Finished'
