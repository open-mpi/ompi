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

AC_DEFUN([MCA_pls_xgrid_CONFIG],[
    OMPI_CHECK_XGRID([pls_xgrid], [pls_xgrid_good=1], [pls_xgrid_good=0])

    # For very dumb reasons involving linking, it's near impossible
    # to build the XGrid components as static libraries.  Disable if that's
    # the case.
    AS_IF([test "$pls_xgrid_good" = "0"], [$2],
          [AS_IF([test "$compile_mode" = "dso"],
                  [ # pls_xgrid_LDFLAGS will be set by OMPI_CHECK_XGRID
                   pls_xgrid_WRAPPER_EXTRA_LDFLAGS="$pls_xgrid_LDFLAGS"
                   $1],
                  [AC_MSG_WARN([XGrid components must be built as DSOs.  Disabling])
                   $2])])

    AC_SUBST([pls_xgrid_OBJCFLAGS])
    AC_SUBST([pls_xgrid_LDFLAGS])
])dnl
