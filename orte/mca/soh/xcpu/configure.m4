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

# MCA_soh_xcpu_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_soh_xcpu_CONFIG],[
    OMPI_CHECK_XCPU([soh_xcpu], [soh_xcpu_good=1], [soh_xcpu_good=0])
    # if xcpu is present and working, soh_xcpu_good=1.  
    # Evaluate succeed / fail

    AS_IF([test "$soh_xcpu_good" = "1"],
          [soh_xcpu_WRAPPER_EXTRA_LDFLAGS="$soh_xcpu_LDFLAGS"
           soh_xcpu_WRAPPER_EXTRA_LIBS="$soh_xcpu_LIBS"
           $1],
          [$2])

    # set build flags to use in makefile
    AC_SUBST([soh_xcpu_CPPFLAGS])
    AC_SUBST([soh_xcpu_LDFLAGS])
    AC_SUBST([soh_xcpu_LIBS])
])dnl
