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
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# MCA_pls_xcpu_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_pls_xcpu_CONFIG],[
    OMPI_CHECK_XCPU([pls_xcpu], [pls_xcpu_good=1], [pls_xcpu_good=0])
         
    # if check worked, set wrapper flags.  
    # Evaluate succeed / fail
    AS_IF([test "$pls_xcpu_good" = "1"],
          [pls_xcpu_WRAPPER_EXTRA_LDFLAGS="$pls_xcpu_LDFLAGS"
           pls_xcpu_WRAPPER_EXTRA_LIBS="$pls_xcpu_LIBS"
           $1],
          [$2])

    # set build flags to use in makefile
    AC_SUBST([pls_xcpu_CPPFLAGS])
    AC_SUBST([pls_xcpu_LDFLAGS])
    AC_SUBST([pls_xcpu_LIBS])
])dnl
