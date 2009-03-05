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


# MCA_btl_pcie_CONFIG(action-if-can-compile, 
#                        [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_btl_pcie_CONFIG],[
    OMPI_CHECK_PCIE([btl_pcie],
                     [btl_pcie_happy="yes"],
                     [btl_pcie_happy="no"])

    AS_IF([test "$btl_pcie_happy" = "yes"],
          [btl_pcie_WRAPPER_EXTRA_LDFLAGS="$btl_pcie_LDFLAGS"
           btl_pcie_WRAPPER_EXTRA_LIBS="$btl_pcie_LIBS"
           $1],
          [$2])

    # substitute in the things needed to build pcie
    AC_SUBST([btl_pcie_CPPFLAGS])
    AC_SUBST([btl_pcie_LDFLAGS])
    AC_SUBST([btl_pcie_LIBS])
])dnl
