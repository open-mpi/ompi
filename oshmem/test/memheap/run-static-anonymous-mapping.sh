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

printf 'SHMEMCC=%s\n' "$shmemcc_path"
printf 'OSHRUN=%s\n' "$oshrun_path"
printf 'COMPILE: %s -g %s -o static_anonymous_mapping\n' \
       "$shmemcc_path" "$srcdir/static_anonymous_mapping.c"
"$shmemcc_path" -g "$srcdir/static_anonymous_mapping.c" -o static_anonymous_mapping || exit $?

if command -v ldd >/dev/null 2>&1; then
    echo "LD_LIBRARY_PATH=${LD_LIBRARY_PATH-}"
    echo "LDD_STATIC_ANONYMOUS_MAPPING_BEGIN"
    ldd ./static_anonymous_mapping
    echo "LDD_STATIC_ANONYMOUS_MAPPING_END"
fi

printf 'LAUNCH: %s --timeout %s -n %s --map-by ppr:%s:node ./static_anonymous_mapping\n' \
       "$oshrun_path" "$OSHRUN_TIMEOUT" "$OSHRUN_NP" "$OSHRUN_NP"
exec "$oshrun_path" --timeout "$OSHRUN_TIMEOUT" -n "$OSHRUN_NP" \
     --map-by "ppr:$OSHRUN_NP:node" ./static_anonymous_mapping
