#!/usr/bin/env sh

for i in {0..255}; do
    printf "\x1b[38;5;%smcolour%s\x1b[0m\n" $i $i
done
