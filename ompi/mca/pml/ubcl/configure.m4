#
# Copyright (c) 2024 Bull SAS.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#


AC_DEFUN([MCA_ompi_pml_ubcl_POST_CONFIG], [
    AS_IF([test "$1" = "1"], [OMPI_REQUIRE_ENDPOINT_TAG([PML])])
])

AC_DEFUN([MCA_ompi_pml_ubcl_CONFIG], [
    AC_CONFIG_FILES([ompi/mca/pml/ubcl/Makefile])

    OMPI_CHECK_UBCL([pml_ubcl],
                   [pml_ubcl_happy="yes"],
                   [pml_ubcl_happy="no"])

    AC_REQUIRE([MCA_ompi_common_ubcl_CONFIG])
    AC_REQUIRE([MCA_opal_common_ubcl_CONFIG])
    AC_REQUIRE([OPAL_CHECK_CUDA])
    AC_REQUIRE([OPAL_CHECK_CUDART])

    AS_IF([test "$pml_ubcl_happy" = "yes"],
          [$1],
          [$2])

    # substitute in the things needed to build ubcl
    AC_SUBST([pml_ubcl_CPPFLAGS])
    AC_SUBST([pml_ubcl_LDFLAGS])
    AC_SUBST([pml_ubcl_LIBS])
])
