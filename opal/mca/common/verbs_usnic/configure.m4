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
# Copyright (c) 2007-2015 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
# Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

#
# This component must be linked statically into libopen-pal because it
# registers a provider for libibverbs at run time, and there's no
# libibverbs API to *un*register a plugin.  Hence, we can't allow this
# code to be dlclosed/removed from the process.  Hence: it must be
# compiled statically into libopen-pal.
#
AC_DEFUN([MCA_opal_common_verbs_usnic_COMPILE_MODE], [
    AC_MSG_CHECKING([for MCA component $2:$3 compile mode])
    $4="static"
    AC_MSG_RESULT([$$4])
])

# MCA_opal_common_verbs_usnic_CONFIG([action-if-can-compile],
#                      [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_opal_common_verbs_usnic_CONFIG],[
    AC_CONFIG_FILES([opal/mca/common/verbs_usnic/Makefile])
    common_verbs_usnic_happy="no"

    OPAL_CHECK_OPENFABRICS([common_verbs_usnic],
                           [common_verbs_usnic_happy="yes"])

    AS_IF([test "$common_verbs_usnic_happy" = "yes"],
          [$1],
          [$2])

    # substitute in the things needed to build openib
    AC_SUBST([common_verbs_usnic_CPPFLAGS])
    AC_SUBST([common_verbs_usnic_LDFLAGS])
    AC_SUBST([common_verbs_usnic_LIBS])
])dnl
