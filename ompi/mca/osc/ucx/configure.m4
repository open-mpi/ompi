# -*- shell-script -*-
#
# Copyright (C) Mellanox Technologies Ltd. 2001-2017.  ALL RIGHTS RESERVED.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_ompi_osc_ucx_POST_CONFIG(will_build)
# ----------------------------------------
# Only require the tag if we're actually going to be built
AC_DEFUN([MCA_ompi_osc_ucx_POST_CONFIG], [
    AS_IF([test "$1" = "1"], [OMPI_REQUIRE_ENDPOINT_TAG([UCX])])
])dnl

# MCA_osc_ucx_CONFIG(action-if-can-compile,
#                        [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_ompi_osc_ucx_CONFIG],[
    AC_CONFIG_FILES([ompi/mca/osc/ucx/Makefile])

    OMPI_CHECK_UCX([osc_ucx],
                   [osc_ucx_happy="yes"],
                   [osc_ucx_happy="no"])

    AS_IF([test "$osc_ucx_happy" = "yes"],
          [$1],
          [$2])

    # substitute in the things needed to build ucx
    AC_SUBST([osc_ucx_CPPFLAGS])
    AC_SUBST([osc_ucx_LDFLAGS])
    AC_SUBST([osc_ucx_LIBS])
])dnl
