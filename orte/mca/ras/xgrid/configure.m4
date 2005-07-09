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

AC_DEFUN([MCA_ras_xgrid_CONFIG],[
    OMPI_CHECK_XGRID([ras_xgrid], [ras_xgrid_good=1], [ras_xgrid_good=0])
         
    # don't need to set any flags - there's no XGrid-using code in this
    # component.  For very dumb reasons involving linking, it's near impossible
    # to build the XGrid components as static libraries.  Disable if that's
    # the case.
    AS_IF([test "$ras_xgrid_good" = "0" -a "$OMPI_WANT_DIST" = "no"], [$2],
          [AS_IF([test "$compile_mode" = "dso"], [$1], 
                 [AC_MSG_WARN([XGrid components must be built as DSOs.  Disabling])
                  $2])])
])dnl
