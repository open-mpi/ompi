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
# Copyright (c) 2012 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# MCA_oob_ud_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_orte_oob_ud_CONFIG],[
    # We need to know if we have verbs support
    AC_REQUIRE([OPAL_CHECK_VERBS_DIR])

    AC_CONFIG_FILES([orte/mca/oob/ud/Makefile])

    # JMS Still have problems with AC_ARG ENABLE not yet having been
    # called or CHECK_WITHDIR'ed.

    orte_oob_ud_check_save_CPPFLAGS=$CPPFLAGS
    orte_oob_ud_check_save_LDFLAGS=$LDFLAGS
    orte_oob_ud_check_save_LIBS=$LIBS

    OMPI_CHECK_PACKAGE([orte_oob_ud],
                       [infiniband/verbs.h],
                       [ibverbs],
                       [ibv_open_device],
                       [],
                       [$opal_verbs_dir],
                       [$opal_verbs_libdir],
                       [orte_oob_ud_check_happy=yes],
                       [orte_oob_ud_check_happy=no])

    CPPFLAGS=$orte_oob_ud_check_save_CPPFLAGS
    LDFLAGS=$orte_oob_ud_check_save_LDFLAGS
    LIBS=$orte_oob_ud_check_save_LIBS

    AS_IF([test "$orte_oob_ud_check_happy" = "yes"],
          [$1],
          [AS_IF([test "$opal_want_verbs" = "yes"],
                 [AC_MSG_WARN([--with-verbs specified, but cannot build this component])
                  AC_MSG_ERROR([Cannot continue])
                 ])
           $2])

    # substitute in the things needed to build this component
    AC_SUBST([orte_oob_ud_CFLAGS])
    AC_SUBST([orte_oob_ud_CPPFLAGS])
    AC_SUBST([orte_oob_ud_LDFLAGS])
    AC_SUBST([orte_oob_ud_LIBS])
])dnl
