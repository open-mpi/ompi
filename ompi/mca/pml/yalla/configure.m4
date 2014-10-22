#
# Copyright (C) Mellanox Technologies Ltd. 2001-2014.  ALL RIGHTS RESERVED.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#


AC_DEFUN([MCA_ompi_pml_yalla_POST_CONFIG], [
    AS_IF([test "$1" = "1"], [OMPI_REQUIRE_ENDPOINT_TAG([PML])])
])

AC_DEFUN([MCA_ompi_pml_yalla_CONFIG], [
    AC_CONFIG_FILES([ompi/mca/pml/yalla/Makefile])

    OMPI_CHECK_MXM([pml_yalla],
                   [pml_yalla_happy="yes"],
                   [pml_yalla_happy="no"])

    AS_IF([test "$pml_yalla_happy" = "yes"],
          [$1],
          [$2])

    # substitute in the things needed to build mxm
    AC_SUBST([pml_yalla_CPPFLAGS])
    AC_SUBST([pml_yalla_LDFLAGS])
    AC_SUBST([pml_yalla_LIBS])
])
