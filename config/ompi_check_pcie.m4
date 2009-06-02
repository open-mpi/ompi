# -*- shell-script -*-
#
# Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
#                         reserved. 
# Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# OMPI_CHECK_PCIE(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
AC_DEFUN([OMPI_CHECK_PCIE],[
    OMPI_VAR_SCOPE_PUSH(ompi_check_pcie_$1_CPPFLAGS ompi_check_pcie_happy)
    AC_ARG_WITH([pcie],
        [AC_HELP_STRING([--with-pcie(=DIR)],
             [Build PCIE (Axon) support, optionally adding DIR to the compiler search path.  Specifically, the test will add -IDIR to the compiler command line, and will attempt to compile #include <linux/axon_ioctl.h>.])])
    OMPI_CHECK_WITHDIR([pcie], [$with_pcie], [linux/axon_ioctl.h])

    ompi_check_pcie_$1_save_CPPFLAGS="$CPPFLAGS"
    ompi_check_pcie_happy="no"

    AS_IF([test "$with_pcie" != "no" -a "$with_pcie" != "yes" -a "$with_pcie" != "/usr" -a "$with_pcie" != "/usr/local"],
          [$1_CPPFLAGS="-I$with_pcie"])

    CPPFLAGS="$$1_CPPFLAGS $CPPFLAGS"
    AS_IF([test "$with_pcie" != "no"],
          [AC_CHECK_HEADER([linux/axon_ioctl.h],
                           [ompi_check_pcie_happy="yes"],
                           [ompi_check_pcie_happy="no"],
                           [AC_INCLUDES_DEFAULT
#include <linux/types.h>
])])

    CPPFLAGS="$ompi_check_pcie_$1_save_CPPFLAGS"

    AS_IF([test "$ompi_check_pcie_happy" = "yes" -a "$enable_progress_threads" = "yes"],
          [AC_MSG_WARN([PCIE driver does not currently support progress threads.  Disabling BTL.])
           ompi_check_pcie_happy="no"])

    AS_IF([test "$ompi_check_pcie_happy" = "yes"],
          [$2],
          [AS_IF([test ! -z "$with_pcie" -a "$with_pcie" != "no"],
                 [AC_MSG_ERROR([PCIe support requested but not found.  Aborting])])
           $3])
    AC_SUBST([$1_CPPFLAGS])
    OMPI_VAR_SCOPE_POP
])
