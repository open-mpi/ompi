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

AC_DEFUN([MCA_ras_lsf_bproc_CONFIG],[
    OMPI_CHECK_BPROC([ras_lsf_bproc], [ras_lsf_bproc_good=1], [ras_lsf_bproc_good=0])

    # For very dumb reasons involving linking, it's near impossible
    # to build the XGrid components as static libraries.  Disable if that's
    # the case.
    AS_IF([test "$ras_lsf_bproc_good" = "0" -a "$OMPI_WANT_DIST" = "no"], [$2],
          [ras_lsf_bproc_WRAPPER_EXTRA_LDFLAGS="$ras_lsf_bproc_LDFLAGS"
           ras_lsf_bproc_WRAPPER_EXTRA_LIBS="$ras_lsf_bproc_LIBS"
           $1])

    AC_SUBST([ras_lsf_bproc_OBJCFLAGS])
    AC_SUBST([ras_lsf_bproc_LDFLAGS])
    AC_SUBST([ras_lsf_bproc_LIBS])
])dnl
