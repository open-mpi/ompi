#!/usr/bin/env bash

echo "Running clang-format on code base..."

files=($(git ls-tree -r main --name-only | grep -v -E '3rd-party/|contrib/' | grep -e '.*\.[ch]$' | xargs grep -E -L -- "-*- fortran -*-|-*- f90 -*-"))

for file in "${files[@]}" ; do
    if test "$1" = "-d" ; then
	echo "Would have formatted: ${file}"
    else
	clang-format --style=file --verbose -i "${file}"
    fi
done

echo "Done"
