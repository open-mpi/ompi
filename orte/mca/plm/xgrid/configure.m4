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
# Copyright (c) 2009-2010 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# MCA_orte_plm_xgrid_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_orte_plm_xgrid_CONFIG],[
    AC_CONFIG_FILES([orte/mca/plm/xgrid/Makefile])

    ORTE_CHECK_XGRID([plm_xgrid], [plm_xgrid_good=1], [plm_xgrid_good=0])

    # For very dumb reasons involving linking, it's near impossible
    # to build the XGrid components as static libraries.  Disable if that's
    # the case.
    AS_IF([test "$plm_xgrid_good" = "0"], [$2],
          [AS_IF([test "$compile_mode" = "dso"],
                  [ # plm_xgrid_LDFLAGS will be set by ORTE_CHECK_XGRID
                   plm_xgrid_WRAPPER_EXTRA_LDFLAGS="$plm_xgrid_LDFLAGS"
                   $1],
                  [AC_MSG_WARN([XGrid components must be built as DSOs.  Disabling])
                   $2])])

    # set build flags to use in makefile
    AC_SUBST([plm_xgrid_OBJCFLAGS])
    AC_SUBST([plm_xgrid_LDFLAGS])
])dnl
