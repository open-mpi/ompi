#!/bin/sh

perl="$1"
srcdir="$2"
destdir="$3"
ret=0

if test "$perl" = "" -o "$srcdir" = "" -o "$destdir" = "" ; then
    echo "ERROR: invalid argument to generate-all-asm.sh"
    echo "usage: generate-all-asm.sh [PERL] [SRCDIR] [DESTDIR]"
    exit 1
fi

for asmarch in `grep -v '^#' "$srcdir/asm-data.txt" | cut -f1 | xargs` ; do
    if test ! -f "${srcdir}/base/${asmarch}.asm" ; then
        echo "WARNING: Skipping missing assembly arch ${asmarch}"
        continue
    fi

    for asmformat in `grep $asmarch "$srcdir/asm-data.txt" | cut -f2 | xargs` ; do
        echo "--> Generating assembly for $asmarch $asmformat"
        output="`grep \"$asmarch.*$asmformat\" $srcdir/asm-data.txt | cut -f3`"
        $perl generate-asm.pl "$asmarch" "$asmformat" "$srcdir/base" "$destdir/generated/atomic-$output.s"
        if test "$?" != "0" ; then
            echo "WARNING: Failed to generate assembly for $asmarch $asmformat"
            ret=1
        fi
    done
done

exit $ret
