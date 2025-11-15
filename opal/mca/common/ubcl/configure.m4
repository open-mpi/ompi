# -*- shell-script -*-
#
# Copyright (c) 2024      Bull S.A.S. All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AC_DEFUN([MCA_opal_common_ubcl_CONFIG],[
    AC_CONFIG_FILES([opal/mca/common/ubcl/Makefile])

    OMPI_CHECK_UBCL([common_ubcl],
                    [common_ubcl_happy="yes"],
                    [common_ubcl_happy="no"])


    AS_IF([test "$common_ubcl_happy" = "yes"],
          [$1],
          [$2])

    # substitute in the things needed to build ubcl
    AC_SUBST([common_ubcl_CPPFLAGS])
    AC_SUBST([common_ubcl_LDFLAGS])
    AC_SUBST([common_ubcl_LIBS])
])dnl
