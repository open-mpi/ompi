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

AC_DEFUN([MCA_ras_bjs_CONFIG],[
    OMPI_CHECK_BPROC([ras_bjs], [ras_bjs_good=1], [ras_bjs_good=0])

    # For very dumb reasons involving linking, it's near impossible
    # to build the XGrid components as static libraries.  Disable if that's
    # the case.
    AS_IF([test "$ras_bjs_good" = "0"], [$2],
          [ras_bjs_WRAPPER_EXTRA_LDFLAGS="$ras_bjs_LDFLAGS"
           ras_bjs_WRAPPER_EXTRA_LIBS="$ras_bjs_LIBS"
           $1])

    AC_SUBST([ras_bjs_OBJCFLAGS])
    AC_SUBST([ras_bjs_LDFLAGS])
    AC_SUBST([ras_bjs_LIBS])
])dnl
