# -*- shell-script -*-
#
# Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
# Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
# Copyright (c) 2015      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_ompi_sbgp_ibnet_CONFIG([should_build])
# ------------------------------------------
# AC_DEFUN([MCA_ompi_sbgp_ibnet_POST_CONFIG], [
# ])


# MCA_ompi_sbgp_ibnet_CONFIG([action-if-can-compile],
#                      [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_ompi_sbgp_ibnet_CONFIG],[
    AC_CONFIG_FILES([ompi/mca/sbgp/ibnet/Makefile])
    sbgp_ofa_happy="no"
    sbgp_mlnx_ofed_happy="no"

    OPAL_CHECK_OPENFABRICS([sbgp_ibnet], [sbgp_ofa_happy="yes"])
    OPAL_CHECK_MLNX_OPENFABRICS([sbgp_ibnet], [sbgp_mlnx_ofed_happy="yes"])

    AS_IF([test "$sbgp_ofa_happy" = "yes" && test "$sbgp_mlnx_ofed_happy" = "yes"],
          [$1],
          [$2])

    # substitute in the things needed to build iboffload
    AC_SUBST([sbgp_ibnet_CFLAGS])
    AC_SUBST([sbgp_ibnet_CPPFLAGS])
    AC_SUBST([sbgp_ibnet_LDFLAGS])
    AC_SUBST([sbgp_ibnet_LIBS])
])dnl
