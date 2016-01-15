#
# Copyright (C) Mellanox Technologies Ltd. 2001-2015.  ALL RIGHTS RESERVED.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#


AC_DEFUN([MCA_ompi_pml_ucx_POST_CONFIG], [
    AS_IF([test "$1" = "1"], [OMPI_REQUIRE_ENDPOINT_TAG([PML])])
])

AC_DEFUN([MCA_ompi_pml_ucx_CONFIG], [
    AC_CONFIG_FILES([ompi/mca/pml/ucx/Makefile])

    OMPI_CHECK_UCX([pml_ucx],
                   [pml_ucx_happy="yes"],
                   [pml_ucx_happy="no"])

    AS_IF([test "$pml_ucx_happy" = "yes"],
          [$1],
          [$2])

    # substitute in the things needed to build ucx
    AC_SUBST([pml_ucx_CPPFLAGS])
    AC_SUBST([pml_ucx_LDFLAGS])
    AC_SUBST([pml_ucx_LIBS])
])
