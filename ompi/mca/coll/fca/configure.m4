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


# MCA_coll_fca_CONFIG([action-if-can-compile],
#                      [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_ompi_coll_fca_CONFIG],[
    AC_CONFIG_FILES([ompi/mca/coll/fca/Makefile])

    OMPI_CHECK_FCA([coll_fca],
                     [coll_fca_happy="yes"],
                     [coll_fca_happy="no"])

    AS_IF([test "$coll_fca_happy" = "yes"],
          [$1],
          [$2])

    # substitute in the things needed to build fca
    AC_SUBST([coll_fca_CFLAGS])
    AC_SUBST([coll_fca_CPPFLAGS])
    AC_SUBST([coll_fca_LDFLAGS])
    AC_SUBST([coll_fca_LIBS])
])dnl

