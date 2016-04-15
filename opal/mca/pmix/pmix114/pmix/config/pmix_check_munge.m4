# -*- shell-script -*-
#
# Copyright (c) 2015      Intel, Inc. All rights reserved
# Copyright (c) 2015      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_munge_CONFIG([action-if-found], [action-if-not-found])
# --------------------------------------------------------------------
AC_DEFUN([PMIX_MUNGE_CONFIG],[

    PMIX_VAR_SCOPE_PUSH([pmix_munge_dir pmix_munge_libdir])

    AC_ARG_WITH([munge],
                [AC_HELP_STRING([--with-munge=DIR],
                                [Search for munge headers and libraries in DIR ])])

    AC_ARG_WITH([munge-libdir],
                [AC_HELP_STRING([--with-munge-libdir=DIR],
                                [Search for munge libraries in DIR ])])

    pmix_munge_support=0
    if test "$with_munge" != "no"; then
        AC_MSG_CHECKING([for munge in])
        if test ! -z "$with_munge" && test "$with_munge" != "yes"; then
            if test -d $with_munge/include/munge; then
                pmix_munge_dir=$with_munge/include/munge
            else
                pmix_munge_dir=$with_munge
            fi
            if test -d $with_munge/lib; then
                pmix_munge_libdir=$with_munge/lib
            elif test -d $with_munge/lib64; then
                pmix_munge_libdir=$with_munge/lib64
            else
                AC_MSG_RESULT([Could not find $with_munge/lib or $with_munge/lib64])
                AC_MSG_ERROR([Can not continue])
            fi
            AC_MSG_RESULT([$pmix_munge_dir and $pmix_munge_libdir])
        else
            AC_MSG_RESULT([(default search paths)])
            pmix_munge_dir=
        fi
        AS_IF([test ! -z "$with_munge_libdir" && test "$with_munge_libdir" != "yes"],
              [pmix_munge_libdir="$with_munge_libdir"])

        PMIX_CHECK_PACKAGE([pmix_munge],
                           [munge.h],
                           [munge],
                           [munge_encode],
                           [-lmunge],
                           [$pmix_munge_dir],
                           [$pmix_munge_libdir],
                           [pmix_munge_support=1],
                           [pmix_munge_support=0])
        if test $pmix_munge_support == "1"; then
            CPPFLAGS="$pmix_munge_CPPFLAGS $CPPFLAGS"
            LIBS="$LIBS -lmunge"
            LDFLAGS="$pmix_munge_LDFLAGS $LDFLAGS"
        fi
    fi

    if test ! -z "$with_munge" && test "$with_munge" != "no" && test "$pmix_munge_support" != "1"; then
        AC_MSG_WARN([MUNGE SUPPORT REQUESTED AND NOT FOUND.])
        AC_MSG_ERROR([CANNOT CONTINUE])
    fi

    AC_MSG_CHECKING([will munge support be built])
    if test "$pmix_munge_support" != "1"; then
        AC_MSG_RESULT([no])
    else
        AC_MSG_RESULT([yes])
    fi

    AC_DEFINE_UNQUOTED([PMIX_HAVE_MUNGE], [$pmix_munge_support],
                       [Whether we have munge support or not])

    PMIX_VAR_SCOPE_POP
])dnl
