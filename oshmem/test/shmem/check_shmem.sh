#!/bin/sh
#
# Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
#
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$

resolve_tool()
{
    tool=$1
    path=$(command -v "$tool" 2>/dev/null) || return 1

    case $path in
        /*) printf '%s\n' "$path" ;;
        *) printf '%s/%s\n' "$(pwd -P)" "$path" ;;
    esac
}

shmemcc_path=$(resolve_tool "$SHMEMCC") || {
    echo "SKIP: installed shmemcc not found: $SHMEMCC"
    exit 77
}
oshrun_path=$(resolve_tool "$OSHRUN") || {
    echo "SKIP: installed oshrun not found: $OSHRUN"
    exit 77
}

tmpdir=$(mktemp -d "${TMPDIR:-/tmp}/check-shmem.XXXXXX") || {
    echo "SKIP: could not create a temporary directory"
    exit 77
}
trap 'rm -rf "$tmpdir"' EXIT HUP INT TERM

if [ -z "$TESTS" ]; then
    TESTS=$(cd "$srcdir" && ls *.c 2>/dev/null | sed 's/\.c$//')
fi

pass=0
skip=0
fail=0

for name in $TESTS; do
    src="$srcdir/$name.c"
    np=$(sed -n 's/^.*NP:[ ]*\([0-9][0-9]*\).*$/\1/p' "$src" | head -n 1)
    np=${np:-2}

    if ! "$shmemcc_path" -g "$src" -o "$tmpdir/$name" >"$tmpdir/$name.log" 2>&1; then
        cat "$tmpdir/$name.log"
        echo "FAIL: $name (compile)"
        fail=$((fail + 1))
        continue
    fi

    "$oshrun_path" --oversubscribe --timeout "$OSHRUN_TIMEOUT" -np "$np" \
        "$tmpdir/$name" >"$tmpdir/$name.log" 2>&1
    rc=$?
    case $rc in
        0)
            echo "PASS: $name (np=$np)"
            pass=$((pass + 1))
            ;;
        77)
            echo "SKIP: $name (np=$np)"
            skip=$((skip + 1))
            ;;
        *)
            cat "$tmpdir/$name.log"
            echo "FAIL: $name (np=$np, exit $rc)"
            fail=$((fail + 1))
            ;;
    esac
done

echo "shmem tests: $pass passed, $skip skipped, $fail failed"
[ 0 -eq "$fail" ]
