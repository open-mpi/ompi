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

AC_DEFUN([MCA_pls_bproc_seed_CONFIG],[
    OMPI_CHECK_BPROC([pls_bproc_seed], [pls_bproc_seed_good=1], 
                     [pls_bproc_seed_good=0])

    # For very dumb reasons involving linking, it's near impossible
    # to build the XGrid components as static libraries.  Disable if that's
    # the case.
    AS_IF([test "$pls_bproc_seed_good" = "0" -a "$OMPI_WANT_DIST" = "no"], [$2],
          [pls_bproc_seed_WRAPPER_EXTRA_LDFLAGS="$pls_bproc_seed_LDFLAGS"
           pls_bproc_seed_WRAPPER_EXTRA_LIBS="$pls_bproc_seed_LIBS"
           $1])

    AC_SUBST([pls_bproc_seed_OBJCFLAGS])
    AC_SUBST([pls_bproc_seed_LDFLAGS])
    AC_SUBST([pls_bproc_seed_LIBS])
])dnl
