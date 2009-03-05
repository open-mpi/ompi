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

# OMPI_CHECK_PCIE(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
AC_DEFUN([OMPI_CHECK_PCIE],[
    AC_ARG_WITH([pcie],
        [AC_HELP_STRING([--with-pcie(=DIR)],
             [Build PCIE (QLogic InfiniPath PCIE) support, searching for libraries in DIR])])
    AC_ARG_WITH([pcie-libdir],
        [AC_HELP_STRING([--with-pcie-libdir=DIR],
             [Search for PCIE (QLogic InfiniPath PCIE) libraries in DIR])])

    ompi_check_pcie_$1_save_CPPFLAGS="$CPPFLAGS"
    ompi_check_pcie_$1_save_LDFLAGS="$LDFLAGS"
    ompi_check_pcie_$1_save_LIBS="$LIBS"

    ompi_check_pcie_happy="yes"

    AS_IF([test "$with_pcie" != "no"],
          [AS_IF([test ! -z "$with_pcie" -a "$with_pcie" != "yes"],
                 [ompi_check_pcie_dir="$with_pcie"])
           AS_IF([test ! -z "$with_pcie_libdir" -a "$with_pcie_libdir" != "yes"],
                 [ompi_check_pcie_libdir="$with_pcie_libdir"])
           OMPI_CHECK_PACKAGE([$1],
                              [axon_ioctl.h],
			      [],
                              [$ompi_check_pcie_dir],
                              [$ompi_check_pcie_libdir],
                              [ompi_check_pcie_happy="yes"],
                              [ompi_check_pcie_happy="no"])],
          [ompi_check_pcie_happy="no"])


    CPPFLAGS="$ompi_check_pcie_$1_save_CPPFLAGS"
    LDFLAGS="$ompi_check_pcie_$1_save_LDFLAGS"
    LIBS="$ompi_check_pcie_$1_save_LIBS"

    AS_IF([test "$ompi_check_pcie_happy" = "yes" -a "$enable_progress_threads" = "yes"],
          [AC_MSG_WARN([PCIE driver does not currently support progress threads.  Disabling BTL.])
           ompi_check_pcie_happy="no"])

    AS_IF([test "$ompi_check_pcie_happy" = "yes"],
          [$2],
          [AS_IF([test ! -z "$with_pcie" -a "$with_pcie" != "no"],
                 [AC_MSG_ERROR([PCIe support requested but not found.  Aborting])])
           $3])
])
