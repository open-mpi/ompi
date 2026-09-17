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

run_static_mapping()
{
    printf 'SUBTEST=static-anonymous-mapping\n'
    "$srcdir/run-static-anonymous-mapping.sh"
    rc=$?
    printf 'SUBTEST=static-anonymous-mapping STATUS=%s\n' "$rc"

    case $rc in
        0) echo "PASS: static anonymous mapping" ;;
        77) echo "SKIP: static anonymous mapping (runner exit 77)" ;;
        *)
            echo "FAIL: static anonymous mapping (runner exit $rc)"
            static_failures=$((static_failures + 1))
            ;;
    esac
}

run_layout_case()
{
    label=$1
    key_exchange=$2
    shift 2
    output="$tmpdir/$label-key-exchange-$key_exchange.log"

    printf 'CASE=%s KEY_EXCHANGE=%s\n' "$label" "$key_exchange"
    printf 'LAUNCH: %s --timeout %s --mca memheap_base_key_exchange %s %s\n' \
           "$oshrun_path" "$OSHRUN_TIMEOUT" "$key_exchange" "$*"
    "$oshrun_path" --timeout "$OSHRUN_TIMEOUT" \
        --mca memheap_base_key_exchange "$key_exchange" "$@" \
        >"$output" 2>&1
    rc=$?
    cat "$output"
    printf 'CASE=%s KEY_EXCHANGE=%s STATUS=%s\n' \
           "$label" "$key_exchange" "$rc"

    case $label in
        equal-small)
            if [ 0 -ne "$rc" ]; then
                echo "FAIL: equal small/small initialization did not succeed"
                layout_failures=$((layout_failures + 1))
            fi
            ;;
        unequal-small-large)
            if [ 251 -ne "$rc" ]; then
                echo "FAIL: unequal small/large initialization did not report the controlled SHMEM abort (status 251)"
                layout_failures=$((layout_failures + 1))
            elif ! grep -E 'OSHMEM memheap layout mismatch: local PE [0-9]+, remote PE [0-9]+, segment [0-9]+, field (type|size|hints|static_offset), local \{type=[0-9]+,size=[0-9]+,hints=-?[0-9]+,static_offset=[0-9]+\}, remote \{type=[0-9]+,size=[0-9]+,hints=-?[0-9]+,static_offset=[0-9]+\}; remote keys cannot be associated safely$' "$output" >/dev/null; then
                echo "FAIL: unequal small/large initialization lacked the structural layout-mismatch diagnostic"
                layout_failures=$((layout_failures + 1))
            fi
            ;;
    esac
}

case ${OSHRUN_NP-} in
    2) ;;
    *)
        echo "FAIL: OSHRUN_NP must be exactly 2 for peer-layout validation"
        exit 1
        ;;
esac
if [ "${OSHRUN_TIMEOUT-}" != 60 ]; then
    echo "FAIL: OSHRUN_TIMEOUT must be exactly 60 seconds for peer-layout validation"
    exit 1
fi

shmemcc_path=$(resolve_tool "$SHMEMCC") || {
    echo "SKIP: installed shmemcc not found: $SHMEMCC"
    exit 77
}
oshrun_path=$(resolve_tool "$OSHRUN") || {
    echo "SKIP: installed oshrun not found: $OSHRUN"
    exit 77
}

tmpdir=$(mktemp -d "${TMPDIR:-/tmp}/check-memheap.XXXXXX") || {
    echo "SKIP: could not create a temporary directory"
    exit 77
}
trap 'rm -rf "$tmpdir"' EXIT HUP INT TERM

static_failures=0
layout_failures=0

printf 'SHMEMCC=%s\n' "$shmemcc_path"
printf 'OSHRUN=%s\n' "$oshrun_path"
printf 'LD_LIBRARY_PATH=%s\n' "${LD_LIBRARY_PATH-}"
run_static_mapping

printf 'COMPILE: %s -g %s -o layout_small\n' \
       "$shmemcc_path" "$srcdir/layout_small.c"
"$shmemcc_path" -g "$srcdir/layout_small.c" -o layout_small || exit $?
printf 'COMPILE: %s -g %s -o layout_large\n' \
       "$shmemcc_path" "$srcdir/layout_large.c"
"$shmemcc_path" -g "$srcdir/layout_large.c" -o layout_large || exit $?

if command -v ldd >/dev/null 2>&1; then
    echo "LDD_LAYOUT_SMALL_BEGIN"
    ldd ./layout_small
    echo "LDD_LAYOUT_SMALL_END"
    echo "LDD_LAYOUT_LARGE_BEGIN"
    ldd ./layout_large
    echo "LDD_LAYOUT_LARGE_END"
fi

run_layout_case equal-small 1 -n "$OSHRUN_NP" \
    --map-by "ppr:$OSHRUN_NP:node" ./layout_small
run_layout_case equal-small 0 -n "$OSHRUN_NP" \
    --map-by "ppr:$OSHRUN_NP:node" ./layout_small
run_layout_case unequal-small-large 1 --map-by "ppr:$OSHRUN_NP:node" \
    -n 1 ./layout_small : -n 1 ./layout_large
run_layout_case unequal-small-large 0 --map-by "ppr:$OSHRUN_NP:node" \
    -n 1 ./layout_small : -n 1 ./layout_large

if [ 0 -ne "$static_failures" ]; then
    echo "FAIL: one or more static anonymous mapping subtests failed"
fi
if [ 0 -ne "$layout_failures" ]; then
    echo "FAIL: one or more memheap layout subtests failed"
fi
if [ 0 -ne "$static_failures" ] || [ 0 -ne "$layout_failures" ]; then
    exit 1
fi

echo "PASS: memheap layout validation"
exit 0
