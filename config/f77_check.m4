dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2004-2005 The Trustees of Indiana University.
dnl                         All rights reserved.
dnl Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
dnl                         All rights reserved.
dnl Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
dnl                         University of Stuttgart.  All rights reserved.
dnl Copyright (c) 2004-2005 The Regents of the University of California.
dnl                         All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

AC_DEFUN([OMPI_F77_CHECK],[
# Determine a bunch of things about each Fortran datatype:
# - whether the compiler supports it or not (sometimes an error if it
#   does not exist, sometimes not)
# - what the size of it is
# - if it equals the expected size (if there is an expected size)
# - what its alignment is
# - what the C type corresponding to it is (sometimes an error if one
#   cannot be found, sometimes not)
#
# Arguments
# 1. Fortran type name
# 2. All-caps version of #define name
# 3. All-lower version of #define name
# 4. Required to have a corresponding C type or not (yes or no)
# 5. Space-delineated list of C types to search (:'s are replaced with
#    spaces, e.g., "long:long" is changed to a single type, "long
#    long")
# 6. What the expected size is (or -1 if no expected size)

ofc_c_required="$4"
ofc_c_search_types="$5"
ofc_expected_size="$6"

# Some defaults

ofc_have_type=0
ofc_type_size=$ac_cv_sizeof_int
ofc_type_alignment=$ac_cv_sizeof_int
ofc_c_type=ompi_fortran_bogus_type_t

# Only check if we actually want the F77 bindings / have a F77
# compiler.  This allows us to call this macro, even if there is no
# F77 compiler.  If we have no f77 compiler, then just set a bunch of
# defaults.

if test "$OMPI_WANT_F77_BINDINGS" = "1"; then
    OMPI_F77_CHECK_TYPE([$1], [ofc_have_type])
else
    AC_MSG_CHECKING([if FORTRAN compiler supports $1])
    AC_MSG_RESULT([skipped])
fi

if test "$ofc_have_type" = "1"; then

    # What is the size of this type?

    OMPI_F77_GET_SIZEOF([$1], [ofc_type_size])
    if test "$ofc_expected_size" != "-1" -a "$ofc_type_size" != "$ofc_expected_size"; then
        AC_MSG_WARN([*** Fortran $1 does not have expected size!])
        AC_MSG_WARN([*** Expected $ofc_expected_size, got $ofc_type_size])
        AC_MSG_WARN([*** Disabling MPI support for Fortran $1])
        ofc_have_type=0
    else

        # Look for a corresponding C type (will abort by itself if the
        # type isn't found and we need it)

        if test "$ofc_c_search_types" != ""; then
            OMPI_FIND_TYPE([Fortran $1], [$ofc_c_search_types], 
                           [$ofc_c_required], [$ofc_type_size], [ofc_c_type])
            if test -z "$ofc_c_type"; then
                ofc_have_type=0
            fi
        fi

        # Get the alignment of the type

        if test "$ofc_have_type" = "1"; then
            OMPI_F77_GET_ALIGNMENT([$1], [ofc_type_alignment])
        fi
    fi
fi

# We always need these defines -- even if we don't have a given type,
# there are some places in the code where we have to have *something*.

AC_DEFINE_UNQUOTED([OMPI_HAVE_FORTRAN_$2], [$ofc_have_type], 
                   [Whether we have FORTRAN $1 or not])
AC_DEFINE_UNQUOTED([OMPI_SIZEOF_FORTRAN_$2], [$ofc_type_size], 
                   [Size of FORTRAN $1])
AC_DEFINE_UNQUOTED([OMPI_ALIGNMENT_FORTRAN_$2], [$ofc_type_alignment], 
                   [Alignment of FORTRAN $1])
if test "$ofc_c_search_types" != ""; then
    AC_DEFINE_UNQUOTED([ompi_fortran_$3_t], [$ofc_c_type], 
                       [C type corresponding to FORTRAN $1])
fi

# Save some in shell variables for later use (e.g., need
# OMPI_SIZEOF_FORTRAN_INTEGER in OMPI_F77_GET_FORTRAN_HANDLE_MAX)

OMPI_FORTRAN_$2_C_TYPE=$ofc_c_type
OMPI_SIZEOF_FORTRAN_$2=$ofc_type_size

# Clean up

unset ofc_have_type ofc_type_size ofc_type_alignment ofc_c_type ofc_bogus
unset ofc_fortran_required ofc_c_required ofc_c_search_types ofc_expected_size
])dnl
