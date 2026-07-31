# -*- shell-script -*-
#
# Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# OPAL_FIND_COUNT_TYPE()
# ----------------------
# Determine the C integer type that backs opal_count_t, and expose it (and
# its maximum value and size) to OPAL via the OPAL_COUNT_TYPE / OPAL_COUNT_MAX
# / OPAL_COUNT_SIZE preprocessor symbols.
#
# This is the SINGLE source of truth for the count type: when the OMPI project
# is built, OMPI_FIND_MPI_AINT_COUNT_OFFSET derives MPI_Count from the same
# OPAL_COUNT_* shell variables rather than recomputing them, so opal_count_t
# and MPI_Count are guaranteed to be the same type (a build-time
# _Static_assert in OMPI enforces this).  Because OPAL must build standalone
# (with or without OMPI), the computation lives here at the OPAL level and runs
# unconditionally.
#
# opal_count_t is a SIGNED integer type that must be:
#   - no larger than size_t -- the BTL/PML/convertor infrastructure relies on
#     sizeof(count) <= sizeof(size_t) (e.g., a 32-bit size_t on a 64-bit ABI;
#     see the historical Open MPI ticket 4205); and
#   - at least as large as ptrdiff_t (the type of MPI_Aint), since a count can
#     describe a datatype extent.
#
# We deliberately do NOT pick the widest available type: we pick the widest of
# { long long, long, int } that still fits within a size_t.  We do not consider
# int64_t/int32_t, because naming them would force <stdint.h> into mpi.h, which
# must stay self-contained.
AC_DEFUN([OPAL_FIND_COUNT_TYPE], [
    AC_REQUIRE([AC_PROG_CC])

    AC_MSG_CHECKING([for the type backing opal_count_t])

    OPAL_COUNT_TYPE=unknown
    if test $ac_cv_sizeof_long_long -le $ac_cv_sizeof_size_t && \
       test $ac_cv_sizeof_long_long -ge $ac_cv_sizeof_ptrdiff_t; then
        OPAL_COUNT_TYPE="long long"
        OPAL_COUNT_SIZE=$ac_cv_sizeof_long_long
    elif test $ac_cv_sizeof_long -le $ac_cv_sizeof_size_t && \
         test $ac_cv_sizeof_long -ge $ac_cv_sizeof_ptrdiff_t; then
        OPAL_COUNT_TYPE=long
        OPAL_COUNT_SIZE=$ac_cv_sizeof_long
    elif test $ac_cv_sizeof_int -le $ac_cv_sizeof_size_t && \
         test $ac_cv_sizeof_int -ge $ac_cv_sizeof_ptrdiff_t; then
        OPAL_COUNT_TYPE=int
        OPAL_COUNT_SIZE=$ac_cv_sizeof_int
    fi

    if test "$OPAL_COUNT_TYPE" = "unknown"; then
        AC_MSG_RESULT([not found])
        AC_MSG_ERROR([*** Unable to find a suitable type for opal_count_t])
    fi

    if test $OPAL_COUNT_SIZE -eq 8 ; then
        OPAL_COUNT_MAX="0x7fffffffffffffffll"
    elif test $OPAL_COUNT_SIZE -eq 4 ; then
        OPAL_COUNT_MAX="0x7fffffffl"
    elif test $OPAL_COUNT_SIZE -eq 2 ; then
        OPAL_COUNT_MAX="0x7fff"
    else
        AC_MSG_RESULT([$OPAL_COUNT_TYPE (size: $OPAL_COUNT_SIZE)])
        AC_MSG_ERROR([*** Configure cannot handle an opal_count_t of size $OPAL_COUNT_SIZE -- contact Open MPI developers])
    fi

    AC_DEFINE_UNQUOTED([OPAL_COUNT_TYPE], [$OPAL_COUNT_TYPE],
                       [Type backing opal_count_t (and MPI_Count when OMPI is built)])
    AC_DEFINE_UNQUOTED([OPAL_COUNT_SIZE], [$OPAL_COUNT_SIZE],
                       [Size (in bytes) of opal_count_t])
    AC_DEFINE_UNQUOTED([OPAL_COUNT_MAX], [$OPAL_COUNT_MAX],
                       [Maximum value representable in an opal_count_t])

    AC_MSG_RESULT([$OPAL_COUNT_TYPE (size: $OPAL_COUNT_SIZE)])
])
