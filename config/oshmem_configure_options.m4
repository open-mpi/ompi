dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2012      Mellanox Technologies, Inc.
dnl                         All rights reserved.
dnl
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
# OSHMEM support
#
AC_MSG_CHECKING([if want OSHMEM support])
AC_ARG_WITH([oshmem],
    [AC_HELP_STRING([--with-oshmem],
                    [Build with OSHMEM support (default=no)])])
if test "$with_oshmem" = "no"; then
    AC_MSG_RESULT([no])
    oshmem_with_support=0
else
    AC_MSG_RESULT([yes])
    oshmem_with_support=1
fi
AM_CONDITIONAL(OSHMEM_SUPPORT, test "$oshmem_with_support" = 1)
AC_DEFINE_UNQUOTED([OSHMEM_ENABLED], [$oshmem_with_support],
                   [Whether user wants OSHMEM support or not])

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
    oshmem_progiling_support=1
else
    AC_MSG_RESULT([no])
    oshmem_progiling_support=0
fi
AM_CONDITIONAL(OSHMEM_PROFILING, test "$oshmem_progiling_support" = 1)
#AC_DEFINE_UNQUOTED([OSHMEM_PROFILING], [$oshmem_progiling_support],
#                   [Whether user wants OSHMEM profiling])

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
        OSHMEM_CFLAGS=" -Werror "
        OSHMEM_TEST_CFLAGS="$CFLAGS -Wall -Wundef  -Werror "
        ;;
    intel)
        # we want specifically the warning on format string conversion
        OSHMEM_CFLAGS=" -Werror "
        OSHMEM_TEST_CFLAGS="$CFLAGS -Wall -Werror -wd188,981,1419,810"
        ;;
esac

AC_SUBST([OSHMEM_CFLAGS])
AC_SUBST([OSHMEM_TEST_CFLAGS])



OMPI_CHECK_OPENFABRICS([openib],
                        [openib_happy="yes"],
                        [openib_happy="no"])

# substitute in the things needed to build MEMHEAP BASE
AC_SUBST([openib_CFLAGS])
AC_SUBST([openib_CPPFLAGS])
AC_SUBST([openib_LDFLAGS])
AC_SUBST([openib_LIBS])

# If we have the openib stuff available, find out what we've got
AS_IF(
    [test "$openib_happy" = "yes"],
    [
        OSHMEM_LIBSHMEM_EXTRA_LDFLAGS="$OSHMEM_LIBSHMEM_EXTRA_LDFLAGS $openib_LDFLAGS"
        OSHMEM_LIBSHMEM_EXTRA_LIBS="$OSHMEM_LIBSHMEM_EXTRA_LIBS $openib_LIBS"

        # ibv_reg_shared_mr was added in MOFED 1.8
        oshmem_have_mpage=0

        openib_save_CPPFLAGS="$CPPFLAGS"
        openib_save_LDFLAGS="$LDFLAGS"
        openib_save_LIBS="$LIBS"

        CPPFLAGS="$CPPFLAGS $openib_CPPFLAGS"
        LDFLAGS="$LDFLAGS $openib_LDFLAGS"
        LIBS="$LIBS $openib_LIBS"

        AC_CHECK_DECLS([IBV_ACCESS_ALLOCATE_MR],
               [oshmem_have_mpage=1], [],
               [#include <infiniband/verbs.h>])

        AC_CHECK_LIB([ibverbs], [ibv_reg_shared_mr], [oshmem_have_mpage=2])

        CPPFLAGS="$openib_save_CPPFLAGS"
        LDFLAGS="$openib_save_LDFLAGS"
        LIBS="$openib_save_LIBS"

        AC_DEFINE_UNQUOTED(MPAGE_ENABLE, $oshmem_have_mpage,
            [Whether we can use M-PAGE supported since MOFED 1.8])
    ])
])dnl

