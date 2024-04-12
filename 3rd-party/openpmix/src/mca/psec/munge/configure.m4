# -*- shell-script -*-
#
# Copyright (c) 2015-2016 Intel, Inc. All rights reserved
# Copyright (c) 2015      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
# Copyright (c) 2022      Amazon.com, Inc. or its affiliates.
#                         All Rights reserved.
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

    AC_ARG_WITH([munge],
                [AS_HELP_STRING([--with-munge=DIR],
                                [Search for munge headers and libraries in DIR ])])
    AC_ARG_WITH([munge-libdir],
                [AS_HELP_STRING([--with-munge-libdir=DIR],
                                [Search for munge libraries in DIR ])])

    OAC_CHECK_PACKAGE([munge],
                      [psec_munge],
                      [munge.h],
                      [munge],
                      [munge_encode],
                      [psec_munge_support=1],
                      [psec_munge_support=0])

    if test -n "$with_munge" && test "$with_munge" != "no" && test "$psec_munge_support" != "1"; then
        AC_MSG_WARN([MUNGE SUPPORT REQUESTED AND NOT FOUND.])
        AC_MSG_ERROR([CANNOT CONTINUE])
    fi

    AS_IF([test "$psec_munge_support" != "1"],
          [$2],
          [$1])

    PMIX_SUMMARY_ADD([External Packages], [munge], [], [${psec_munge_SUMMARY}])

    # set build flags to use in makefile
    AC_SUBST([psec_munge_CPPFLAGS])
    AC_SUBST([psec_munge_LDFLAGS])
    AC_SUBST([psec_munge_LIBS])
])dnl
