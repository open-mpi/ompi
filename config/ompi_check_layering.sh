#!/bin/sh
#
# Copyright (c) 2026 Jeffrey M. Squyres.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#
# Enforce Open MPI's library layering rule for libopen_mpi.
#
# libopen_mpi is the *lower* layer; libmpi and libmpi_abi are thin
# front-ends that sit on top of it:
#
#     +----------------+----------------+
#     |  libmpi_abi    |     libmpi     |
#     +----------------+----------------+
#     |          libopen_mpi            |
#     +---------------------------------+
#
# The public MPI entry points (MPI_*, PMPI_*, MPIX_*, PMPIX_*) and the
# Open MPI extensions (OMPI_*) are defined in the upper layer.  If code
# in libopen_mpi calls one of them, the dependency runs the wrong way and
# creates a circular library dependency.  Back-end code must call the
# internal ompi_* routines instead.
#
# This script fails the build if libopen_mpi has any *undefined*
# reference to such a symbol.
#
# This script -- not a linker flag -- is the authoritative check:
#
# 1. On macOS, the linker catches nothing.  Libtool's darwin configuration
#    links every shared library with "-undefined dynamic_lookup", which
#    overrides ld64's default of erroring on undefined symbols.  A
#    layering violation therefore links cleanly on macOS, and only this
#    script will notice it.
#
# 2. Static (--disable-shared) builds have no link step for the archive,
#    so no linker flag can catch the violation there, on any platform.
#
# 3. A linker flag stops complaining the moment someone "fixes" the error
#    by adding libmpi to libopen_mpi's link line -- which is precisely
#    the regression we want to prevent.  The undefined reference is still
#    recorded in the library, so this script still catches it.
#
# On Linux, -Wl,--no-undefined (see config/ompi_setup_layering_checks.m4)
# additionally fails the link itself, which is a faster and more pointed
# error; this script is the backstop that makes the guarantee identical on
# every platform.
#
# Usage: ompi_check_layering.sh <nm-command> <library> [<library>...]
#        Nonexistent files and symlinks are silently skipped, so callers
#        may pass a superset of the possible library names.
#

if test $# -lt 2; then
    echo "Usage: $0 <nm-command> <library> [<library>...]" >&2
    exit 2
fi

nm="$1"
shift

# Symbols that libopen_mpi must never *reference*: the public MPI APIs
# and the Open MPI extensions.  Note the optional leading underscore,
# which Mach-O prepends to C symbol names.
forbidden_re='^_?(P?MPIX?_|OMPI_)'

status=0

for lib in "$@"; do
    # Skip things that were not built in this configuration, and skip
    # symlinks so that we do not report the same library twice (libtool
    # creates e.g. libopen_mpi.dylib -> libopen_mpi.0.dylib).
    if test ! -f "$lib" || test -L "$lib"; then
        continue
    fi

    # Collect symbols from BOTH the regular and the dynamic symbol table,
    # and merge the results.  Do not try to pick just one:
    #
    # - The regular table (plain nm) is what a static archive has, and is
    #   also present in a normal, unstripped shared library.
    # - The dynamic table (nm -D) is the only one left in a *stripped* ELF
    #   shared library.
    #
    # Selecting between them is a trap: GNU nm -D on a static archive
    # neither fails nor prints nothing -- it exits 0 and prints one header
    # line per member object with no symbols at all.  A "try -D, fall back
    # if it fails or is empty" strategy therefore silently parses those
    # headers, finds no undefined symbols, and passes every static archive.
    # (Mach-O's nm rejects -D outright, so it would fall back correctly and
    # hide the bug on macOS.)  Merging both tables avoids the whole issue.
    syms=`{ $nm "$lib" 2>/dev/null; $nm -D "$lib" 2>/dev/null; }`
    if test -z "$syms"; then
        continue
    fi

    # Both GNU and Mach-O nm mark an undefined symbol with the type
    # letter "U" and print it as "<blank> U <name>"; a defined symbol is
    # printed as "<address> <type> <name>".
    undefined=`echo "$syms" | awk 'NF == 2 && $1 == "U" { print $2 }' | sort -u`
    defined=`echo "$syms" | awk 'NF == 3 { print $3 }' | sort -u`

    # A static archive reports one object file's references to another
    # object file in the *same* archive as undefined.  Those are internal
    # to libopen_mpi and are not layering violations, so subtract
    # everything the library itself defines.
    candidates=`echo "$undefined" | grep -E "$forbidden_re"`
    if test -z "$candidates"; then
        continue
    fi

    defined_file=`mktemp "${TMPDIR:-/tmp}/ompi_layering.XXXXXX"` || exit 2
    echo "$defined" > "$defined_file"
    violations=`echo "$candidates" | grep -v -x -F -f "$defined_file"`
    rm -f "$defined_file"

    if test -n "$violations"; then
        status=1
        cat >&2 <<EOF

=====================================================================
ERROR: Open MPI layering violation in `basename "$lib"`

libopen_mpi references the following public MPI / Open MPI extension
symbols, which are defined in libmpi (and libmpi_abi) -- i.e., in the
layer *above* libopen_mpi:

EOF
        echo "$violations" | sed -e 's/^/    /' >&2
        cat >&2 <<EOF

libopen_mpi is the lower layer and must not call up into libmpi; doing
so creates a circular library dependency.  Call the internal ompi_*
routine instead of the public MPI_* entry point.

Do NOT "fix" this by linking libmpi into libopen_mpi -- that is the
circular dependency, and it will break the MPI Forum ABI library
(libmpi_abi), which deliberately does not contain the extensions.

If the lower layer genuinely needs to reach upward, hand a callback
function pointer down from libmpi (see
ompi_mpi_instance_register_mpiext_init() in ompi/instance/instance.c).
=====================================================================

EOF
    fi
done

exit $status
