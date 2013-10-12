dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2013      Mellanox Technologies, Inc.
dnl                         All rights reserved.
dnl
dnl Copyright (c) 2013 Cisco Systems, Inc.  All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

AC_DEFUN([OSHMEM_CONFIGURE_OPTIONS],[
ompi_show_subtitle "OSHMEM Configuration options"

AC_SUBST(OSHMEM_LIBSHMEM_EXTRA_LIBS)
AC_SUBST(OSHMEM_LIBSHMEM_EXTRA_LDFLAGS)

#
# Enable compatibility mode
#
AC_MSG_CHECKING([if want SGI/Quadrix compatibility mode])
AC_ARG_ENABLE(oshmem-compat,
        AC_HELP_STRING([--enable-oshmem-compat],
            [enable compatibility mode (default: enabled)]))
if test "$enable_oshmem_compat" != "no"; then
    AC_MSG_RESULT([yes])
    OSHMEM_SPEC_COMPAT=1
else
    AC_MSG_RESULT([no])
    OSHMEM_SPEC_COMPAT=0
fi
AC_DEFINE_UNQUOTED([OSHMEM_SPEC_COMPAT], [$OSHMEM_SPEC_COMPAT],
                   [Whether user wants OSHMEM in compatibility mode or not])



#
# Do we want to disable OSHMEM parameter checking at run-time?
#
AC_MSG_CHECKING([if want SHMEM API parameter checking])
AC_ARG_WITH(shmem-param-check,
    AC_HELP_STRING([--shmem-param-check(=VALUE)],
                   [behavior of SHMEM function parameter checking.  Valid values are: always, never.  If --with-shmem-param-check is specified with no VALUE argument, it is equivalent to a VALUE of "always"; --without-shmem-param-check is equivalent to "never" (default: always).]))
shmem_param_check=1
if test "$with_shmem_param_check" = "no" -o \
    "$with_shmem_param_check" = "never"; then
    shmem_param_check=0
    AC_MSG_RESULT([never])
elif test "$with_shmem_param_check" = "yes" -o \
    "$with_shmem_param_check" = "always" -o \
    -z "$with_shmem_param_check"; then
    shmem_param_check=1
    AC_MSG_RESULT([always])
else
    AC_MSG_RESULT([unknown])
    AC_MSG_WARN([*** Unrecognized --with-shmem-param-check value])
    AC_MSG_WARN([*** See "configure --help" output])
    AC_MSG_WARN([*** Defaulting to "runtime"])
fi
AC_DEFINE_UNQUOTED(OSHMEM_PARAM_CHECK, $shmem_param_check,
    [Whether we want to check SHMEM parameters always or never])


#
#  OSHMEM profiling support
#
AC_MSG_CHECKING([if want pshmem_])
AC_ARG_ENABLE(oshmem-profile,
    AC_HELP_STRING([--enable-oshmem-profile],
                   [enable OSHMEM profiling (default: enabled)]))
if test "$enable_oshmem_profile" != "no"; then
    AC_MSG_RESULT([yes])
    oshmem_profiling_support=1
else
    AC_MSG_RESULT([no])
    oshmem_profiling_support=0
fi
AM_CONDITIONAL(OSHMEM_PROFILING, test "$oshmem_profiling_support" = 1)

# Whether to build the OpenShmem fortran support or not For the
# moment, use the same value as was derived from --enable-mpi-fortra.
# *This seems wrong*; someone should somehow unify these two
# options... but the implications are complicated.
#
# Option 1: make --enable-fortran that governs both MPI and shmem.
# This has 2 implications:
# - --enable-mpi-fortran needs to be maintained for at least the
#   1.7/1.8 series
# - what to do with --enable-mpi-cxx?  It should be made consistent --
#   so make it --enable-cxx?
# 
# Option 2: make separate --enable-oshmem-fortran.  This seems sucky,
# though, because oshmem Fortran depends on a lot of MPI Fortran
# infrastructure.  If it isin't there, then oshmem Fortran can't
# built.
#
# Option 3: ...? (something better than option 1/2?)
AC_MSG_CHECKING([if want to build SHMEM fortran bindings])
OSHMEM_WANT_FORTRAN_BINDINGS=$OMPI_WANT_FORTRAN_BINDINGS
AM_CONDITIONAL(OSHMEM_WANT_FORTRAN_BINDINGS,
    [test $OSHMEM_WANT_FORTRAN_BINDINGS -eq 1])
AS_IF([test $OSHMEM_WANT_FORTRAN_BINDINGS -eq 1],
    [AC_MSG_RESULT([yes])],
    [AC_MSG_RESULT([no])])
])


AC_DEFUN([OSHMEM_SETUP_CFLAGS],[


OMPI_C_COMPILER_VENDOR([oshmem_c_vendor])

#
# OSHMEM force warnings as errors
#
#
# Since SHMEM libraries are not fully ISO99 C compliant
# -pedantic and -Wundef raise a bunch of warnings, so
# we just strip them off for this component
AC_MSG_WARN([Removed -pedantic and -Wundef from CFLAGS for OSHMEM])

oshmem_CFLAGS="$CFLAGS"

# Strip off problematic arguments
oshmem_CFLAGS="`echo $oshmem_CFLAGS | sed 's/-pedantic//g'`"
oshmem_CFLAGS="`echo $oshmem_CFLAGS | sed 's/-Wundef//g'`"
oshmem_CFLAGS="`echo $oshmem_CFLAGS | sed 's/-Wno-long-double//g'`"
CFLAGS="$oshmem_CFLAGS"

case "$oshmem_c_vendor" in
    gnu)
        OSHMEM_CFLAGS=" -Werror"
        ;;
    intel)
        # we want specifically the warning on format string conversion
        OSHMEM_CFLAGS=" -Werror "
        ;;
esac

AC_SUBST([OSHMEM_CFLAGS])

OPAL_CHECK_PMI([pmi_oshmem], [pmi_oshmem_happy="yes"], [pmi_oshmem_happy="no"])

AC_SUBST([pmi_oshmem_CPPFLAGS])
AC_SUBST([pmi_oshmem_LDFLAGS])
AC_SUBST([pmi_oshmem_LIBS])
 
AS_IF(
    [test "$pmi_oshmem_happy" = "yes"],
    [
	OSHMEM_CFLAGS="$OSHMEM_CFLAGS $pmi_oshmem_CPPFLAGS"
	OSHMEM_LDFLAGS="$OSHMEM_LDFLAGS $pmi_oshmem_LDFLAGS $pmi_oshmem_LIBS"
    ])

AC_SUBST([OSHMEM_CFLAGS])
AC_SUBST([OSHMEM_LDFLAGS])


OMPI_CHECK_OPENFABRICS([oshmem_verbs],
                        [oshmem_verbs_happy="yes"],
                        [oshmem_verbs_happy="no"])

# substitute in the things needed to build MEMHEAP BASE
AC_SUBST([oshmem_verbs_CFLAGS])
AC_SUBST([oshmem_verbs_CPPFLAGS])
AC_SUBST([oshmem_verbs_LDFLAGS])
AC_SUBST([oshmem_verbs_LIBS])

# If we have the oshmem_verbs stuff available, find out what we've got
AS_IF(
    [test "$oshmem_verbs_happy" = "yes"],
    [
        OSHMEM_LIBSHMEM_EXTRA_LDFLAGS="$OSHMEM_LIBSHMEM_EXTRA_LDFLAGS $oshmem_verbs_LDFLAGS"
        OSHMEM_LIBSHMEM_EXTRA_LIBS="$OSHMEM_LIBSHMEM_EXTRA_LIBS $oshmem_verbs_LIBS"

        # ibv_reg_shared_mr was added in MOFED 1.8
        oshmem_have_mpage=0

        oshmem_verbs_save_CPPFLAGS="$CPPFLAGS"
        oshmem_verbs_save_LDFLAGS="$LDFLAGS"
        oshmem_verbs_save_LIBS="$LIBS"

        CPPFLAGS="$CPPFLAGS $oshmem_verbs_CPPFLAGS"
        LDFLAGS="$LDFLAGS $oshmem_verbs_LDFLAGS"
        LIBS="$LIBS $oshmem_verbs_LIBS"

        AC_CHECK_DECLS([IBV_ACCESS_ALLOCATE_MR,IBV_ACCESS_SHARED_MR_USER_READ],
               [oshmem_have_mpage=2], [],
               [#include <infiniband/verbs.h>])

        CPPFLAGS="$oshmem_verbs_save_CPPFLAGS"
        LDFLAGS="$oshmem_verbs_save_LDFLAGS"
        LIBS="$oshmem_verbs_save_LIBS"

        AC_DEFINE_UNQUOTED(MPAGE_ENABLE, $oshmem_have_mpage,
            [Whether we can use M-PAGE supported since MOFED 1.8])
    ])
])dnl
