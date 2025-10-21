# -*- shell-script -*-
#
# Copyright (c) 2025-2026 Bull S.A.S. All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AC_DEFUN([MCA_ompi_common_ubcl_CONFIG],[
    AC_CONFIG_FILES([ompi/mca/common/ubcl/Makefile])

    OMPI_CHECK_UBCL([ompi_common_ubcl],
                    [common_ubcl_happy="yes"],
                    [common_ubcl_happy="no"])

    # common/ubcl let endusers provide a more recent version of UBCL
    # An mca parameter is exposed to select a specific UBCL path that is dlopen
    # at runtime. We don't want any previous linking on DSO files.
    AS_IF([test "$compile_mode" = "dso"],
          [common_ubcl_LDFLAGS=""],
          [AC_MSG_WARN([Only DSO mode of common/ubcl is tested])])

    AC_REQUIRE([MCA_opal_common_ubcl_CONFIG])

    AS_IF([test "$common_ubcl_happy" = "yes"],
          [$1],
          [$2])

    # substitute in the things needed to build ubcl
    AC_SUBST([common_ubcl_CPPFLAGS])
    AC_SUBST([common_ubcl_LDFLAGS])
    AC_SUBST([common_ubcl_LIBS])
])dnl
