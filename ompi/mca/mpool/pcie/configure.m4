# -*- shell-script -*-
#
# Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
#                         reserved. 
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#


# MCA_mpool_pcie_CONFIG(action-if-can-compile, 
#                        [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_mpool_pcie_CONFIG],[
    OMPI_CHECK_PCIE([mpool_pcie],
                     [mpool_pcie_happy="yes"],
                     [mpool_pcie_happy="no"])

    AS_IF([test "$mpool_pcie_happy" = "yes"],
          [mpool_pcie_WRAPPER_EXTRA_LDFLAGS="$mpool_pcie_LDFLAGS"
           mpool_pcie_WRAPPER_EXTRA_LIBS="$mpool_pcie_LIBS"
           $1],
          [$2])

    # substitute in the things needed to build pcie
    AC_SUBST([mpool_pcie_CPPFLAGS])
    AC_SUBST([mpool_pcie_LDFLAGS])
    AC_SUBST([mpool_pcie_LIBS])
])dnl
