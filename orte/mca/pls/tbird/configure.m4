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

# MCA_pls_tm_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_pls_tm_CONFIG],[
    OMPI_CHECK_TM([pls_tm], [pls_tm_good=1], [pls_tm_good=0])
         
    # if check worked, set wrapper flags if so.  
    # Evaluate succeed / fail
    AS_IF([test "$pls_tm_good" = "1"],
          [pls_tm_WRAPPER_EXTRA_LDFLAGS="$pls_tm_LDFLAGS"
           pls_tm_WRAPPER_EXTRA_LIBS="$pls_tm_LIBS"
           $1],
          [$2])

    # set build flags to use in makefile
    AC_SUBST([pls_tm_CPPFLAGS])
    AC_SUBST([pls_tm_LDFLAGS])
    AC_SUBST([pls_tm_LIBS])
])dnl
