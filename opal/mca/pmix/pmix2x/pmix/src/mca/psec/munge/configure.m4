# -*- shell-script -*-
#
# Copyright (c) 2015-2016 Intel, Inc. All rights reserved
# Copyright (c) 2015      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_psec_munge_CONFIG([action-if-found], [action-if-not-found])
# --------------------------------------------------------------------
AC_DEFUN([MCA_pmix_psec_munge_CONFIG],[
    AC_CONFIG_FILES([src/mca/psec/munge/Makefile])

    PMIX_VAR_SCOPE_PUSH([psec_munge_support psec_munge_dir psec_munge_libdir save_cpp save_ld])

    AC_ARG_WITH([munge],
                [AC_HELP_STRING([--with-munge=DIR],
                                [Search for munge headers and libraries in DIR ])])

    AC_ARG_WITH([munge-libdir],
                [AC_HELP_STRING([--with-munge-libdir=DIR],
                                [Search for munge libraries in DIR ])])

    psec_munge_support=0
    if test ! -z "$with_munge" && test "$with_munge" != "no"; then
        AC_MSG_CHECKING([for munge in])
        if test -n "$with_munge" && test "$with_munge" != "yes"; then
            if test -d $with_munge/include/munge; then
                psec_munge_dir=$with_munge/include/munge
            else
                psec_munge_dir=$with_munge
            fi
            if test -d $with_munge/lib; then
                psec_munge_libdir=$with_munge/lib
            elif -d $with_munge/lib64; then
                psec_munge_libdir=$with_munge/lib64
            else
                AC_MSG_RESULT([Could not find $with_munge/lib or $with_munge/lib64])
                AC_MSG_ERROR([Can not continue])
            fi
            AC_MSG_RESULT([$psec_munge_dir and $psec_munge_libdir])
        else
            AC_MSG_RESULT([(default search paths)])
            psec_munge_dir=
        fi
        AS_IF([test -n "$with_munge_libdir" && test "$with_munge_libdir" != "yes"],
              [psec_munge_libdir="$with_munge_libdir"])

        save_cpp=$CPPFLAGS
        save_ld=$LDFLAGS

        PMIX_CHECK_PACKAGE([psec_munge],
                           [munge.h],
                           [munge],
                           [munge_encode],
                           [-lmunge],
                           [$psec_munge_dir],
                           [$psec_munge_libdir],
                           [psec_munge_support=1],
                           [psec_munge_support=0])

        CPPFLAGS=$save_cpp
        LDFLAGS=$save_ld
    fi

    if test -n "$with_munge" && test "$with_munge" != "no" && test "$psec_munge_support" != "1"; then
        AC_MSG_WARN([MUNGE SUPPORT REQUESTED AND NOT FOUND.])
        AC_MSG_ERROR([CANNOT CONTINUE])
    fi

    AC_MSG_CHECKING([will munge support be built])
    AS_IF([test "$psec_munge_support" != "1"],
          [AC_MSG_RESULT([no])
           $2],
          [AC_MSG_RESULT([yes])
           $1])

    # set build flags to use in makefile
    AC_SUBST([psec_munge_CPPFLAGS])
    AC_SUBST([psec_munge_LDFLAGS])
    AC_SUBST([psec_munge_LIBS])

    PMIX_VAR_SCOPE_POP
])dnl
