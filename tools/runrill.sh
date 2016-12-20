#!/bin/sh

set -e

rillc $1 -o $1.out >&2
./$1.out
