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

# MCA_plm_slurmd_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_plm_slurmd_CONFIG],[
    OMPI_CHECK_SLURM([plm_slurmd], [plm_slurmd_good=1], [plm_slurmd_good=0])
         
    # if check worked, set wrapper flags if so.  
    # Evaluate succeed / fail
    AS_IF([test "$plm_slurmd_good" = "1"],
          [plm_slurmd_WRAPPER_EXTRA_LDFLAGS="$plm_slurmd_LDFLAGS"
           plm_slurmd_WRAPPER_EXTRA_LIBS="$plm_slurmd_LIBS"
           $1],
          [$2])

    # set build flags to use in makefile
    AC_SUBST([plm_slurmd_CPPFLAGS])
    AC_SUBST([plm_slurmd_LDFLAGS])
    AC_SUBST([plm_slurmd_LIBS])
])dnl
