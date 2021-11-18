# -*- shell-script -*-
#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2020 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2006 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2006      QLogic Corp. All rights reserved.
# Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2011-2018 Los Alamos National Security, LLC.
#                         All rights reserved.
# Copyright (c) 2018      Research Organization for Information Science
#                         and Technology (RIST).  All rights reserved.
# Copyright (c) 2019      Triad National Security, LLC. All rights
#                         reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# OPAL_CHECK_UCX(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
# check if UCX support can be found.  sets prefix_{CPPFLAGS,
# LDFLAGS, LIBS} as needed and runs action-if-found if there is
# support, otherwise executes action-if-not-found

AC_DEFUN([MCA_opal_btl_uct_CONFIG],[
    AC_CONFIG_FILES([opal/mca/btl/uct/Makefile])

    OMPI_CHECK_UCX([btl_uct],
                   [btl_uct_happy="yes"],
                   [btl_uct_happy="no"])
dnl
dnl check UCT version.  UCT API can change at any given release
dnl so we only allow compiling against ones we know work.
dnl
    AC_ARG_ENABLE([uct-version-check],
       [AS_HELP_STRING([--enable-uct-version-check],
           [enable UCT version check (default: enabled)])])
    AC_MSG_CHECKING([check uct version])
    if test "$enable_uct_version_check" != "no"; then
        AC_MSG_RESULT([yes])
    else
        AC_MSG_RESULT([no])
    fi

    min_allowed_uct_major=1
    min_allowed_uct_minor=9
    if test "$btl_uct_happy" = "yes" && test "$enable_uct_version_check" != "no"; then
        AC_MSG_CHECKING([UCT version compatibility])
        OPAL_VAR_SCOPE_PUSH([CPPFLAGS_save])
        CPPFLAGS_save="$CPPFLAGS"
        CPPFLAGS="$CPPFLAGS $btl_uct_CPPFLAGS"
        AC_PREPROC_IFELSE([AC_LANG_PROGRAM([#include <uct/api/version.h>
                                            #if (UCT_VERNO_MAJOR < $min_allowed_uct_major)
                                            #error "UCT MAJOR VERNO < $min_allowed_uct_major"
                                            #endif
                                            #if (UCT_VERNO_MAJOR == $min_allowed_uct_major) &&\
                                                (UCT_VERNO_MINOR <  $min_allowed_uct_minor)
                                            #error "UCT MINOR VERNO < $min_allowed_uct_minor"
                                            #endif], [])],
                           [AC_MSG_RESULT([UCT version compatible])],
                           [AC_MSG_RESULT([UCT version not compatible - need UCX $min_allowed_uct_major.$min_allowed_uct_minor or newer])
                            btl_uct_happy="no"])
        CPPFLAGS="$CPPFLAGS_save"
        OPAL_VAR_SCOPE_POP
    fi

    if test "$btl_uct_happy" = "yes" ; then
        OPAL_VAR_SCOPE_PUSH([CPPFLAGS_save])

        CPPFLAGS_save="$CPPFLAGS"
        CPPFLAGS="$CPPFLAGS $btl_uct_CPPFLAGS"

        AC_CHECK_DECLS([UCT_PROGRESS_THREAD_SAFE, UCT_CB_FLAG_SYNC], [], [], [[#include <uct/api/uct.h>]])

        CPPFLAGS="$CPPFLAGS_save"
        OPAL_VAR_SCOPE_POP
    fi

    AS_IF([test "$btl_uct_happy" = "yes"],
          [$1
           btl_uct_LIBS="$btl_uct_LIBS -luct"
          ],
          [$2])

    # substitute in the things needed to build ucx
    AC_SUBST([btl_uct_CPPFLAGS])
    AC_SUBST([btl_uct_LDFLAGS])
    AC_SUBST([btl_uct_LIBS])
])dnl
