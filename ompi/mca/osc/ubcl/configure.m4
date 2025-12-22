# Copyright (c) 2025 Bull SAS.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#


AC_DEFUN([MCA_ompi_osc_ubcl_POST_CONFIG], [
    AS_IF([test "$1" = "1"], [OMPI_REQUIRE_ENDPOINT_TAG([PML])])
])

AC_DEFUN([MCA_ompi_osc_ubcl_CONFIG], [
    AC_CONFIG_FILES([ompi/mca/osc/ubcl/Makefile])

    AC_REQUIRE([MCA_ompi_common_ubcl_CONFIG])
    AC_REQUIRE([MCA_opal_common_ubcl_CONFIG])

    OMPI_CHECK_UBCL([osc_ubcl],
        [osc_ubcl_happy="yes"],
        [osc_ubcl_happy="no"])

    AS_IF([test "$osc_ubcl_happy" = "yes"],
        [$1],
        [$2])

# substitute in the things needed to build ubcl
    AC_SUBST([osc_ubcl_CPPFLAGS])
    AC_SUBST([osc_ubcl_LDFLAGS])
    AC_SUBST([osc_ubcl_LIBS])
])
