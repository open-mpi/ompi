# -*- shell-script -*-
#
#
# Copyright (c) 2011 Mellanox Technologies. All rights reserved.
# Copyright (c) 2013 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#


# MCA_coll_hcoll_CONFIG([action-if-can-compile],
#                      [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_ompi_coll_hcoll_CONFIG],[
    AC_CONFIG_FILES([ompi/mca/coll/hcoll/Makefile])

    OMPI_CHECK_HCOLL([coll_hcoll],
                     [coll_hcoll_happy="yes"],
                     [coll_hcoll_happy="no"])

    AS_IF([test "$coll_hcoll_happy" = "yes"],
          [coll_hcoll_WRAPPER_EXTRA_LDFLAGS="$coll_hcoll_LDFLAGS"
           coll_hcoll_CPPFLAGS="$coll_hcoll_CPPFLAGS"
           coll_hcoll_WRAPPER_EXTRA_CPPFLAGS="$coll_hcoll_CPPFLAGS"
           coll_hcoll_WRAPPER_EXTRA_LIBS="$coll_hcoll_LIBS"
           $1],
          [$2])

    # substitute in the things needed to build hcoll
    AC_SUBST([coll_hcoll_CFLAGS])
    AC_SUBST([coll_hcoll_CPPFLAGS])
    AC_SUBST([coll_hcoll_LDFLAGS])
    AC_SUBST([coll_hcoll_LIBS])
])dnl

