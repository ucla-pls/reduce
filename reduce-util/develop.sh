#/usr/bin/env bash
ghcid -c 'stack ghci reduce-util --test --ghci-options "-fno-code -fno-break-on-exception -fno-break-on-error -v1 -ferror-spans -j"' -T ":main $@"
