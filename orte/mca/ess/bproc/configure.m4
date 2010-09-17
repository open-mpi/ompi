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
AC_DEFUN([MCA_orte_ess_bproc_PRIORITY], [30])

# MCA_ess_bproc_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_orte_ess_bproc_CONFIG],[
    AC_CONFIG_FILES([orte/mca/ess/bproc/Makefile])

    ORTE_CHECK_BPROC([ess_bproc], [ess_bproc_good=1], 
                     [ess_bproc_good=1], [ess_bproc_good=0])
    # if check worked, set wrapper flags if so.  
    # Evaluate succeed / fail
    AS_IF([test "$ess_bproc_good" = "1"],
          [ess_bproc_WRAPPER_EXTRA_LDFLAGS="$ess_bproc_LDFLAGS"
           ess_bproc_WRAPPER_EXTRA_LIBS="$ess_bproc_LIBS"
           $1],
          [$2])
    # set build flags to use in makefile
    AC_SUBST([ess_bproc_CPPFLAGS])
    AC_SUBST([ess_bproc_LDFLAGS])
    AC_SUBST([ess_bproc_LIBS])
])dnl
