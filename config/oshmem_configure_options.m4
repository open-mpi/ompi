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
# Disable Open SHMEM?
#
AC_ARG_ENABLE([oshmem],
  [AC_HELP_STRING([--disable-oshmem],
     [Disable building the OpenSHMEM interface])])

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
AC_MSG_CHECKING([if want OSHMEM API parameter checking])
AC_ARG_WITH(oshmem-param-check,
    AC_HELP_STRING([--oshmem-param-check(=VALUE)],
                   [behavior of OSHMEM API function parameter checking.  Valid values are: always, never.  If --with-oshmem-param-check is specified with no VALUE argument, it is equivalent to a VALUE of "always"; --without-oshmem-param-check is equivalent to "never" (default: always).]))
shmem_param_check=1
if test "$with_oshmem_param_check" = "no" -o \
    "$with_oshmem_param_check" = "never"; then
    shmem_param_check=0
    AC_MSG_RESULT([never])
elif test "$with_oshmem_param_check" = "yes" -o \
    "$with_oshmem_param_check" = "always" -o \
    -z "$with_oshmem_param_check"; then
    shmem_param_check=1
    AC_MSG_RESULT([always])
else
    AC_MSG_RESULT([unknown])
    AC_MSG_WARN([*** Unrecognized --with-oshmem-param-check value])
    AC_MSG_WARN([*** See "configure --help" output])
    AC_MSG_WARN([*** Defaulting to "runtime"])
fi
AC_DEFINE_UNQUOTED(OSHMEM_PARAM_CHECK, $shmem_param_check,
    [Whether we want to check OSHMEM parameters always or never])


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

#
# Fortran bindings
#
AC_MSG_CHECKING([if want to build OSHMEM fortran bindings])
AC_ARG_ENABLE(oshmem-fortran,
AC_HELP_STRING([--enable-oshmem-fortran],
               [enable OSHMEM Fortran bindings (default: enabled if Fortran compiler found)]))
AS_IF([test $OMPI_WANT_FORTRAN_BINDINGS -eq 1],
[if test "$enable_oshmem_fortran" != "no"; then
# If no OMPI FORTRAN, bail
#   AS_IF([test $OMPI_BUILD_FORTRAN_MPIFH_BINDINGS -eq 0],
   AS_IF([test $OMPI_WANT_FORTRAN_BINDINGS -eq 0],
               [AC_MSG_RESULT([bad value OMPI_WANT_FORTRAN_BINDINGS: ($OMPI_WANT_FORTRAN_BINDINGS)])
                AC_MSG_WARN([Your explicit request to --enable-oshmem-fortran can only be satisfied if fortran support is enabled in OMPI. You are seeing this message for one of two reasons:
                    1. OMPI fortran support has been explicitly disabled via --disable-mpi-fortran, in which case you cannot --enable-oshmem-fortran. Configure will abort because you, a human, have explicitly asked for something that cannot be provided. 
                    2. OMPI fortran support is implicitly not being built because no fortran compiler could be found on your system. Configure will abort because you, a human, have explicitly asked for something that cannot be provided.])
                AC_MSG_ERROR([Cannot continue])])
    AC_MSG_RESULT([yes])
    OSHMEM_FORTRAN_BINDINGS=1
else
    AC_MSG_RESULT([no])
    OSHMEM_FORTRAN_BINDINGS=0
fi],
[AC_MSG_RESULT([no]) 
    OSHMEM_FORTRAN_BINDINGS=0])

AM_CONDITIONAL(OSHMEM_WANT_FORTRAN_BINDINGS,
    [test $OSHMEM_FORTRAN_BINDINGS -eq 1])
])

############################################################################

AC_DEFUN([OSHMEM_SETUP_CFLAGS],[


OMPI_C_COMPILER_VENDOR([oshmem_c_vendor])

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
