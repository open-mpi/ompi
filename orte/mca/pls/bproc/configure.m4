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

AC_DEFUN([MCA_pls_bproc_CONFIG],[
    OMPI_CHECK_BPROC([pls_bproc], [pls_bproc_good=1], 
                     [pls_bproc_good=0])

    AS_IF([test "$pls_bproc_good" = "0"], [$2],
          [pls_bproc_WRAPPER_EXTRA_LDFLAGS="$pls_bproc_LDFLAGS"
           pls_bproc_WRAPPER_EXTRA_LIBS="$pls_bproc_LIBS"
           $1])

    AC_SUBST([pls_bproc_OBJCFLAGS])
    AC_SUBST([pls_bproc_LDFLAGS])
    AC_SUBST([pls_bproc_LIBS])
])dnl
