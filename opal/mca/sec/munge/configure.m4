# -*- shell-script -*-
#
# Copyright (c) 2015      Intel, Inc. All rights reserved
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# MCA_sec_munge_CONFIG([action-if-found], [action-if-not-found])
# --------------------------------------------------------------------
AC_DEFUN([MCA_opal_sec_munge_CONFIG],[
    AC_CONFIG_FILES([opal/mca/sec/munge/Makefile])

    OPAL_VAR_SCOPE_PUSH([sec_munge_support sec_munge_dir sec_munge_libdir save_cpp save_ld])

    AC_ARG_WITH([munge],
                [AC_HELP_STRING([--with-munge=DIR],
                                [Search for munge headers and libraries in DIR ])])

    AC_ARG_WITH([munge-libdir],
                [AC_HELP_STRING([--with-munge-libdir=DIR],
                                [Search for munge libraries in DIR ])])

    sec_munge_support=0
    if test "$with_munge" != "no"; then
        AC_MSG_CHECKING([for munge in])
        if test ! -z "$with_munge" -a "$with_munge" != "yes"; then
            if test -d $with_munge/include/munge; then
                sec_munge_dir=$with_munge/include/munge
            else
                sec_munge_dir=$with_munge
            fi
            if test -d $with_munge/lib; then
                sec_munge_libdir=$with_munge/lib
            elif -d $with_munge/lib64; then
                sec_munge_libdir=$with_munge/lib64
            else
                AC_MSG_RESULT([Could not find $with_munge/lib or $with_munge/lib64])
                AC_MSG_ERROR([Can not continue])
            fi
            AC_MSG_RESULT([$sec_munge_dir and $sec_munge_libdir])
        else
            AC_MSG_RESULT([(default search paths)])
            sec_munge_dir=
        fi
        AS_IF([test ! -z "$with_munge_libdir" && test "$with_munge_libdir" != "yes"],
              [sec_munge_libdir="$with_munge_libdir"])

        save_cpp=$CPPFLAGS
        save_ld=$LDFLAGS

        OPAL_CHECK_PACKAGE([sec_munge],
                           [munge.h],
                           [munge],
                           [munge_encode],
                           [-lmunge],
                           [$sec_munge_dir],
                           [$sec_munge_libdir],
                           [sec_munge_support=1],
                           [sec_munge_support=0])

        CPPFLAGS=$save_cpp
        LDFLAGS=$save_ld
    fi

    if test ! -z "$with_munge" && test "$with_munge" != "no" && test "$sec_munge_support" != "1"; then
        AC_MSG_WARN([MUNGE SUPPORT REQUESTED AND NOT FOUND.])
        AC_MSG_ERROR([CANNOT CONTINUE])
    fi

    AC_MSG_CHECKING([will munge support be built])
    AS_IF([test "$sec_munge_support" != "1"],
          [AC_MSG_RESULT([no])
           $2],
          [AC_MSG_RESULT([yes])
           $1])
    
    # set build flags to use in makefile
    AC_SUBST([sec_munge_CPPFLAGS])
    AC_SUBST([sec_munge_LDFLAGS])
    AC_SUBST([sec_munge_LIBS])

    OPAL_VAR_SCOPE_POP
])dnl
