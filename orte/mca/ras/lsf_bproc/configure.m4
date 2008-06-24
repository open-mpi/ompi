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

# MCA_ras_lsf_bproc_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_ras_lsf_bproc_CONFIG],[
    OMPI_CHECK_BPROC([ras_lsf_bproc], [ras_lsf_bproc_good=1], 
                     [ras_lsf_bproc_good=1], [ras_lsf_bproc_good=0])

    # if check worked, set wrapper flags if so.  
    # Evaluate succeed / fail
    AS_IF([test "$ras_lsf_bproc_good" = "1"],
          [ras_lsf_bproc_WRAPPER_EXTRA_LDFLAGS="$ras_lsf_bproc_LDFLAGS"
           ras_lsf_bproc_WRAPPER_EXTRA_LIBS="$ras_lsf_bproc_LIBS"
           $1],
          [$2])

    # set build flags to use in makefile
    AC_SUBST([ras_lsf_bproc_CPPFLAGS])
    AC_SUBST([ras_lsf_bproc_LDFLAGS])
    AC_SUBST([ras_lsf_bproc_LIBS])
])dnl
