# -*- shell-script -*-
#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
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

# MCA_soh_bproc_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_soh_bproc_CONFIG],[
    OMPI_CHECK_BPROC([soh_bproc], [soh_bproc_good=1], 
                     [soh_bproc_good=0], [soh_bproc_good=0])

    #BPROC_API_VERSION was added in bproc 4.0.0, and this component
    #will only compile with >= bproc 4.0.0
    AS_IF([test "$soh_bproc_good" = "1"],
       [AC_MSG_CHECKING(for BPROC_API_VERSION)
        AC_TRY_COMPILE([#include<bproc.h>],
            [int foo = BPROC_API_VERSION;], 
            have_bproc_api_ver_msg=yes soh_bproc_good=1,
            have_bproc_api_ver_msg=no  soh_bproc_good=0)
       AC_MSG_RESULT([$have_bproc_api_ver_msg])])

    # if check worked, set wrapper flags if so.  
    # Evaluate succeed / fail
    AS_IF([test "$soh_bproc_good" = "1"],
          [soh_bproc_WRAPPER_EXTRA_LDFLAGS="$soh_bproc_LDFLAGS"
           soh_bproc_WRAPPER_EXTRA_LIBS="$soh_bproc_LIBS"
           $1],
          [$2])

    # set build flags to use in makefile
    AC_SUBST([soh_bproc_CPPFLAGS])
    AC_SUBST([soh_bproc_LDFLAGS])
    AC_SUBST([soh_bproc_LIBS])
])dnl
