# -*- shell-script -*-
#
# Copyright (c) 2015      Intel, Inc. All rights reserved
# Copyright (c) 2015      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2015      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_sasl_CONFIG([action-if-found], [action-if-not-found])
# --------------------------------------------------------------------
AC_DEFUN([PMIX_SASL_CONFIG],[

    PMIX_VAR_SCOPE_PUSH([pmix_sasl_dir pmix_sasl_libdir])

    AC_ARG_WITH([sasl],
                [AC_HELP_STRING([--with-sasl=DIR],
                                [Search for sasl headers and libraries in DIR ])],
               [], [with_sasl=no])

    AC_ARG_WITH([sasl-libdir],
                [AC_HELP_STRING([--with-sasl-libdir=DIR],
                                [Search for sasl libraries in DIR ])])

    pmix_sasl_support=0
    if test "$with_sasl" != "no"; then
        AC_MSG_CHECKING([for sasl in])
        if test ! -z "$with_sasl" && test "$with_sasl" != "yes"; then
            pmix_sasl_dir=$with_sasl/include/sasl
            if test -d $with_sasl/lib; then
                pmix_sasl_libdir=$with_sasl/lib
            elif test -d $with_sasl/lib64; then
                pmix_sasl_libdir=$with_sasl/lib64
            else
                AC_MSG_RESULT([Could not find $with_sasl/lib or $with_sasl/lib64])
                AC_MSG_ERROR([Can not continue])
            fi
            AC_MSG_RESULT([$pmix_sasl_dir and $pmix_sasl_libdir])
        else
            AC_MSG_RESULT([(default search paths)])
            pmix_sasl_dir=
        fi
        AS_IF([test ! -z "$with_sasl_libdir" && test "$with_sasl_libdir" != "yes"],
              [pmix_sasl_libdir="$with_sasl_libdir"])

        PMIX_CHECK_PACKAGE([pmix_sasl],
                           [sasl/sasl.h],
                           [sasl2],
                           [sasl_server_init],
                           [-lsasl2],
                           [$pmix_sasl_dir],
                           [$pmix_sasl_libdir],
                           [pmix_sasl_support=1],
                           [pmix_sasl_support=0])
        if test $pmix_sasl_support == "1"; then
            CPPFLAGS="$pmix_sasl_CPPFLAGS $CPPFLAGS"
            LIBS="$LIBS -lsasl2"
            LDFLAGS="$pmix_sasl_LDFLAGS $LDFLAGS"
        fi
    fi

    if test ! -z "$with_sasl" && test "$with_sasl" != "no" && test "$pmix_sasl_support" != "1"; then
        AC_MSG_WARN([SASL SUPPORT REQUESTED AND NOT FOUND.])
        AC_MSG_ERROR([CANNOT CONTINUE])
    fi

    AC_MSG_CHECKING([will sasl support be built])
    if test "$pmix_sasl_support" != "1"; then
        AC_MSG_RESULT([no])
    else
        AC_MSG_RESULT([yes])
    fi

    AC_DEFINE_UNQUOTED(PMIX_HAVE_SASL, [$pmix_sasl_support],
                       [Whether we have sasl support or not])

    PMIX_VAR_SCOPE_POP
])dnl
