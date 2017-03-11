# -*- shell-script -*-
#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2009-2016 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2016      Los Alamos National Security, LLC. All rights
#                         reserved.
# Copyright (c) 2017      Intel, Inc.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# ORTE_CHECK_MOAB(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
AC_DEFUN([ORTE_CHECK_MOAB],[
    if test -z "$orte_check_moab_happy" ; then
        OPAL_VAR_SCOPE_PUSH([orte_check_moab_$1_save_CPPFLAGS orte_check_moab_$1_save_LDFLAGS orte_check_moab_$1_save_LIBS])

        AC_ARG_WITH([moab],
                    [AC_HELP_STRING([--with-moab],
                                    [Build MOAB scheduler component (default: yes)])])
        OPAL_CHECK_WITHDIR([moab], [$with_moab], [mapi.h])
        AC_ARG_WITH([moab-libdir],
                    [AC_HELP_STRING([--with-moab-libdir=DIR],
                    [Search for Moab libraries in DIR])])
        OPAL_CHECK_WITHDIR([moab-libdir], [$with_moab_libdir], [libmoab.*])

        orte_check_moab_happy="yes"
        AS_IF([test "$with_moab" = "no"],
              [orte_check_moab_happy=no])


        AS_IF([test $orte_check_moab_happy = yes],
              [AC_MSG_CHECKING([looking for moab in])
               AS_IF([test "$with_moab" != "yes"],
                     [orte_moab_dir=$with_moab
                      AC_MSG_RESULT([($orte_moab_dir)])],
                     [AC_MSG_RESULT([(default search paths)])])
               AS_IF([test ! -z "$with_moab_libdir" && \
                      test "$with_moab_libdir" != "yes"],
                           [orte_moab_libdir=$with_moab_libdir])
              ])

        orte_check_moab_$1_save_CPPFLAGS=$CPPFLAGS
        orte_check_moab_$1_save_LDFLAGS=$LDFLAGS
        orte_check_moab_$1_save_LIBS=$LIBS

        AS_IF([test $orte_check_moab_happy = yes],
              [OPAL_CHECK_PACKAGE([orte_check_moab],
                                  [mapi.h],
                                  [cmoab],
                                  [MCCJobGetRemainingTime],
                                  [],
                                  [$orte_moab_dir],
                                  [$orte_moab_libdir],
                                  [],
                                  [orte_check_moab_happy=no])])

        CPPFLAGS=$orte_check_moab_$1_save_CPPFLAGS
        LDFLAGS=$orte_check_moab_$1_save_LDFLAGS
        LIBS=$orte_check_moab_$1_save_LIBS

        OPAL_SUMMARY_ADD([[Resource Managers]],[[Moab]],[$1],[$orte_check_moab_happy])
        OPAL_VAR_SCOPE_POP
    fi

    if test $orte_check_moab_happy = yes ; then
        $1_CPPFLAGS="[$]$1_CPPFLAGS $orte_check_moab_CPPFLAGS"
        $1_LIBS="[$]$1_LIBS $orte_check_moab_LIBS"
        $1_LDFLAGS="[$]$1_LDFLAGS $orte_check_moab_LDFLAGS"

        AC_SUBST($1_CPPFLAGS)
        AC_SUBST($1_LDFLAGS)
        AC_SUBST($1_LIBS)
    fi

    AS_IF([test "$orte_check_moab_happy" = "yes"],
          [$2],
          [$3])
])
