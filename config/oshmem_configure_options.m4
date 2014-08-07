dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2013      Mellanox Technologies, Inc.
dnl                         All rights reserved.
dnl Copyright (c) 2013-2014 Cisco Systems, Inc.  All rights reserved.
dnl Copyright (c) 2014      Intel, Inc. All rights reserved
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
AC_MSG_CHECKING([if want oshmem])
AC_ARG_ENABLE([oshmem],
              [AC_HELP_STRING([--enable-oshmem],
                              [Enable building the OpenSHMEM interface (available on Linux only, where it is enabled by default)])],
              [oshmem_arg_given=yes],
              [oshmem_arg_given=no])
if test "$oshmem_arg_given" = "yes"; then
    if test "$enable_oshmem" = "yes"; then
        AC_MSG_RESULT([yes])
        if test "$opal_found_linux" != "yes"; then
            AC_MSG_WARN([OpenSHMEM support was requested, but currently])
            AC_MSG_WARN([only supports Linux.])
            AC_MSG_ERROR([Cannot continue])
        fi
    fi
    AC_MSG_RESULT([no])
else
    if test "$opal_found_linux" = "yes"; then
        enable_oshmem=yes
        AC_MSG_RESULT([yes])
    else
        enable_oshmem=no
        AC_MSG_RESULT([not supported on this platform])
    fi
fi

#
# Enable compatibility mode
#
AC_MSG_CHECKING([if want SGI/Quadrics compatibility mode])
AC_ARG_ENABLE(oshmem-compat,
        AC_HELP_STRING([--enable-oshmem-compat],
            [enable compatibility mode (default: enabled)]))
if test "$enable_oshmem" != "no" -a "$enable_oshmem_compat" != "no"; then
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
                   [behavior of OSHMEM API function parameter checking.  Valid values are: always, never.  If --with-oshmem-param-check is specified with no VALUE argument, it is equivalent to a VALUE of "always"; --without-oshmem-param-check is equivalent to "never" (default: never).]))
shmem_param_check=0
if test "$enable_oshmem" != "no"; then
    if test "$with_oshmem_param_check" = "no" -o \
        "$with_oshmem_param_check" = "never" -o \
        -z "$with_oshmem_param_check"; then
        shmem_param_check=0
        AC_MSG_RESULT([never])
    elif test "$with_oshmem_param_check" = "yes" -o \
        "$with_oshmem_param_check" = "always"; then
        shmem_param_check=1
        AC_MSG_RESULT([always])
    else
        AC_MSG_RESULT([unknown])
        AC_MSG_WARN([*** Unrecognized --with-oshmem-param-check value])
        AC_MSG_WARN([*** See "configure --help" output])
        AC_MSG_WARN([*** Defaulting to "runtime"])
    fi
else
    shmem_param_check=0
    AC_MSG_RESULT([no])
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
if test "$enable_oshmem" != "no" -a "$enable_oshmem_profile" != "no"; then
    _OMPI_C_WEAK_SYMBOLS([
        AC_MSG_RESULT([yes])
        oshmem_profiling_support=1
        ], [
        AC_MSG_RESULT([no])
        AC_MSG_WARN([Weak symbols not supported by compiler.
                     Profiling will be disabled.])
        oshmem_profiling_support=0]
    )
else
    AC_MSG_RESULT([no])
    oshmem_profiling_support=0
fi
AM_CONDITIONAL(OSHMEM_PROFILING, test $oshmem_profiling_support -eq 1)

#
# Fortran bindings
#
AC_MSG_CHECKING([if want to build OSHMEM fortran bindings])
AC_ARG_ENABLE(oshmem-fortran,
AC_HELP_STRING([--enable-oshmem-fortran],
               [enable OSHMEM Fortran bindings (default: enabled if Fortran compiler found)]))
if test "$enable_oshmem" != "no" -a "$enable_oshmem_fortran" != "no"; then
# If no OMPI FORTRAN, bail
   AS_IF([test $OMPI_WANT_FORTRAN_BINDINGS -eq 0 -a "$enable_oshmem_fortran" = "yes"],
               [AC_MSG_RESULT([bad value OMPI_WANT_FORTRAN_BINDINGS: ($OMPI_WANT_FORTRAN_BINDINGS)])
                AC_MSG_WARN([Your request to --enable-oshmem-fortran can only be satisfied if fortran support is enabled in OMPI.
You see this message because OMPI fortran support has been explicitly disabled via --disable-mpi-fortran and OSHMEM fortran support was explicitly enabled with --enable-oshmem-fortran.
Configure will abort because you, a human, have asked for something that cannot be provided.])
                AC_MSG_ERROR([Cannot continue])])
    if test $OMPI_WANT_FORTRAN_BINDINGS -eq 1; then
        AC_MSG_RESULT([yes])
    else
        AC_MSG_RESULT([no])
    fi
else
    enable_oshmem_fortran=no
    AC_MSG_RESULT([no])
fi

#
# We can't set am_conditional here since it's yet unknown if there is valid Fortran compiler avaliable
#

]) dnl
