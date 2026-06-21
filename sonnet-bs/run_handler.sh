#!/usr/bin/env bash
set -e

DIR=$(dirname "$1")
B=$(basename "$1" | cut -f 1 -d '.')

/Users/stefantomaschko/Desktop/MyCode/sonnet/sonnet-bs/sonnetc "$1" >/dev/null 2>&1

gcc -Wall -Wextra -Wpedantic -Werror \
	"$DIR/build/$B.s" \
	/Users/stefantomaschko/Desktop/MyCode/sonnet/sonnet-bs/stdlib/*.c \
	-o "$DIR/build/$B" >/dev/null 2>&1

exec "$DIR/build/$B"