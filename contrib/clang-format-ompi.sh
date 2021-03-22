#!/usr/bin/env bash

echo "Running clang-format on code base..."

files=($(git ls-tree -r master --name-only | grep -v '3rd-party/' | grep -v 'contrib' | grep -e '.*\.[ch]$' | xargs grep -L -- "-*- fortran -*-"))

for file in "${files[@]}" ; do
    if test "$1" = "-d" ; then
	echo "Would have formatted: ${file}"
    else
	clang-format --style=file --verbose -i "${file}"
    fi
done

echo "Done"
