dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2004-2006 High Performance Computing Center Stuttgart, 
dnl                         University of Stuttgart.  All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

dnl we only want one :)
m4_define(MCA_memchecker_CONFIGURE_MODE, STOP_AT_FIRST)

AC_DEFUN([MCA_memchecker_CONFIG],[


    AC_MSG_CHECKING([if the memchecker mca should be enabled])
    AC_ARG_ENABLE(memchecker,
        AC_HELP_STRING([--enable-memchecker],
                       [Enable memory and buffer checks (default: disabled)]))
    if test "$enable_memchecker" = "yes"; then
        AC_MSG_RESULT([yes])
        WANT_MEMCHECKER=1
    else
        AC_MSG_RESULT([no])
        WANT_MEMCHECKER=0
    fi
    AC_DEFINE_UNQUOTED([OMPI_WANT_MEMCHECKER],
                       [$WANT_MEMCHECKER],
                       [if the memory and buffer checking should be enabled])
    AM_CONDITIONAL([OMPI_WANT_MEMCHECKER],
                   [test "$WANT_MEMCHECKER" = "1"])

    memchecker_base_found=0

    # first, compile all the components
    MCA_CONFIGURE_FRAMEWORK($1, $2, 1)

    AC_DEFINE_UNQUOTED([OMPI_MEMCHECKER_HAVE_COMPONENT], [$memchecker_base_found],
        [Whether any opal memchecker mca components were found])
])
