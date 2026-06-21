#!/usr/bin/env bash
set -e
B=$(basename "$1" | cut -f 1 -d '.')
/Users/stefantomaschko/Desktop/MyCode/sonnet/sonnet-bs/sonnetc "$1" >/dev/null 2>&1
gcc -Wall -Wextra -Wpedantic -Werror /Users/stefantomaschko/Desktop/MyCode/sonnet/sonnet-bs/build/"$B".s /Users/stefantomaschko/Desktop/MyCode/sonnet/sonnet-bs/stdlib/*.c -o /Users/stefantomaschko/Desktop/MyCode/sonnet/sonnet-bs/build/"$B" >/dev/null 2>&1
exec /Users/stefantomaschko/Desktop/MyCode/sonnet/sonnet-bs/build/"$B"