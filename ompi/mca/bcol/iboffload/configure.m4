# -*- shell-script -*-
#
# Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
# Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_ompi_bcol_iboffload_CONFIG([should_build])
# ------------------------------------------
# AC_DEFUN([MCA_ompi_bcol_iboffload_POST_CONFIG], [
# ])


# MCA_ompi_bcol_iboffload_CONFIG([action-if-can-compile], 
#                      [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_ompi_bcol_iboffload_CONFIG],[
    AC_CONFIG_FILES([ompi/mca/bcol/iboffload/Makefile])
    bcol_ofa_happy="no"
    bcol_mlnx_ofed_happy="no"

    OMPI_CHECK_OPENFABRICS([bcol_iboffload], [bcol_ofa_happy="yes"])
    OMPI_CHECK_MLNX_OPENFABRICS([bcol_iboffload], [bcol_mlnx_ofed_happy="yes"])

    AS_IF([test "$bcol_ofa_happy" = "yes" -a "$bcol_mlnx_ofed_happy" = "yes"],
          [bcol_iboffload_WRAPPER_EXTRA_LDFLAGS="$bcol_iboffload_LDFLAGS"
           bcol_iboffload_WRAPPER_EXTRA_LIBS="$bcol_iboffload_LIBS"
           $1],
          [$2])

    # substitute in the things needed to build iboffload
    AC_SUBST([bcol_iboffload_CFLAGS])
    AC_SUBST([bcol_iboffload_CPPFLAGS])
    AC_SUBST([bcol_iboffload_LDFLAGS])
    AC_SUBST([bcol_iboffload_LIBS])
])dnl
