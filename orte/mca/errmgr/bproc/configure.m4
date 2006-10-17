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
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# MCA_errmgr_bproc_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_errmgr_bproc_CONFIG],[
    OMPI_CHECK_BPROC([errmgr_bproc], [errmgr_bproc_good=1], 
                     [errmgr_bproc_good=1], [errmgr_bproc_good=0])

    # if check worked, set wrapper flags if so.  
    # Evaluate succeed / fail
    AS_IF([test "$errmgr_bproc_good" = "1"], 
          [errmgr_bproc_WRAPPER_EXTRA_LDFLAGS="$errmgr_bproc_LDFLAGS"
           errmgr_bproc_WRAPPER_EXTRA_LIBS="$errmgr_bproc_LIBS"
           $1],
          [$2])

    # set build flags to use in makefile
    AC_SUBST([errmgr_bproc_CPPFLAGS])
    AC_SUBST([errmgr_bproc_LDFLAGS])
    AC_SUBST([errmgr_bproc_LIBS])
])dnl
