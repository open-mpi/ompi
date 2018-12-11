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
# Copyright (c) 2004-2006 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2006      QLogic Corp. All rights reserved.
# Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2011-2018 Los Alamos National Security, LLC.
#                         All rights reserved.
# Copyright (c) 2018      Research Organization for Information Science
#                         and Technology (RIST).  All rights reserved.
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
