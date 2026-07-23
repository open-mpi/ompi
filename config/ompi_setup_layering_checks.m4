dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2026 Jeffrey M. Squyres.  All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

# OMPI_SETUP_LAYERING_CHECKS()
# ---------------------------
# Find a linker flag that turns an unresolved symbol in a shared library
# into a hard link error, and AC_SUBST it as OMPI_NO_UNDEFINED_LDFLAGS.
#
# This enforces Open MPI's library layering rule: libopen_mpi is the lower
# layer, and libmpi / libmpi_abi are thin front-ends above it.  If back-end
# code in libopen_mpi calls a public MPI_* entry point, the reference is
# unresolved when libopen_mpi is linked (libmpi is deliberately not on its
# link line), and we want that to fail loudly and immediately.
#
# Platform notes:
#
# - GNU ld / lld (Linux): shared objects are allowed to contain undefined
#   symbols by default, so we must ask for -Wl,--no-undefined explicitly.
#
# - FreeBSD: The --no-undefined flag is problematic because the environ
#   symbol (used extensively in Open MPI) is provided by the executable's
#   C runtime startup objects (e.g., crt1.o/Scrt1.o), not by libc.so.
#   This causes link failures when building shared libraries. We skip the
#   flag on FreeBSD and rely on config/ompi_check_layering.sh instead.
#
# - macOS (ld64): ld64 rejects --no-undefined as an unknown option, so no
#   flag is found here, and there is deliberately no macOS equivalent.
#   Although ld64 *by itself* errors on undefined symbols in a dylib,
#   Libtool's darwin configuration explicitly overrides that:
#
#       allow_undefined_flag="-undefined dynamic_lookup -no_fixup_chains"
#
#   so every Libtool-linked shared library on macOS permits undefined
#   symbols, and the linker will NOT catch a layering violation.  (Forcing
#   -Wl,-undefined,error back on is not a good answer: it fights Libtool,
#   and ld64 warns "-undefined error is deprecated" on every single link,
#   while Open MPI strives to build with zero warnings.)
#
# So on macOS and FreeBSD the linker gives us nothing here, and even on
# Linux a linker flag is not a sufficient guarantee: it does nothing for
# static (--disable-shared) archives, which have no link step, and it
# falls silent if someone puts libmpi on libopen_mpi's link line.  Hence
# config/ompi_check_layering.sh is the authoritative check, and is what
# actually enforces the rule on macOS and FreeBSD.  This flag simply
# fails faster and closer to the mistake on the platforms that support it.
AC_DEFUN([OMPI_SETUP_LAYERING_CHECKS],[
    OPAL_VAR_SCOPE_PUSH([ompi_layering_save_LDFLAGS ompi_layering_flag ompi_layering_check_freebsd])

    OMPI_NO_UNDEFINED_LDFLAGS=
    ompi_layering_save_LDFLAGS="$LDFLAGS"

    AC_MSG_CHECKING([for linker flag to reject undefined symbols in shared libraries])
    
    dnl Check if we're on FreeBSD where --no-undefined causes problems with environ
    case "${host}" in
    *freebsd*)
        ompi_layering_check_freebsd=yes
        ;;
    *)
        ompi_layering_check_freebsd=no
        ;;
    esac

    AS_IF([test "$ompi_layering_check_freebsd" = "yes"],
          [AC_MSG_RESULT([skipping on FreeBSD (environ symbol issue)])],
          [for ompi_layering_flag in "-Wl,--no-undefined"; do
               LDFLAGS="$ompi_layering_save_LDFLAGS $ompi_layering_flag"
               AC_LINK_IFELSE([AC_LANG_PROGRAM([[]], [[]])],
                              [OMPI_NO_UNDEFINED_LDFLAGS="$ompi_layering_flag"],
                              [])
               AS_IF([test -n "$OMPI_NO_UNDEFINED_LDFLAGS"], [break])
           done
           LDFLAGS="$ompi_layering_save_LDFLAGS"])

    dnl Be careful not to imply that no flag is *needed* when none is
    dnl available (e.g., macOS, FreeBSD, where Libtool links with
    dnl "-undefined dynamic_lookup" or environ causes issues): on those
    dnl platforms the linker does not enforce the layering rule at all, and
    dnl config/ompi_check_layering.sh is the only thing that does.
    AS_IF([test "$ompi_layering_check_freebsd" = "no"],
          [AS_IF([test -n "$OMPI_NO_UNDEFINED_LDFLAGS"],
                 [AC_MSG_RESULT([$OMPI_NO_UNDEFINED_LDFLAGS])],
                 [AC_MSG_RESULT([none available (relying on config/ompi_check_layering.sh)])])])

    AC_SUBST(OMPI_NO_UNDEFINED_LDFLAGS)

    OPAL_VAR_SCOPE_POP
])dnl
