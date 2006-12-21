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

# MCA_sds_xcpu_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_sds_xcpu_CONFIG],[
    OMPI_CHECK_XCPU([sds_xcpu], [sds_xcpu_good=1], [sds_xcpu_good=0])

    # if check worked, set wrapper flags if so.  
    # Evaluate succeed / fail
    AS_IF([test "$sds_xcpu_good" = "1"],
          [sds_xcpu_WRAPPER_EXTRA_LDFLAGS="$sds_xcpu_LDFLAGS"
           sds_xcpu_WRAPPER_EXTRA_LIBS="$sds_xcpu_LIBS"
           $1],
          [$2])
    # set build flags to use in makefile
    AC_SUBST([sds_xcpu_CPPFLAGS])
    AC_SUBST([sds_xcpu_LDFLAGS])
    AC_SUBST([sds_xcpu_LIBS])
])dnl
