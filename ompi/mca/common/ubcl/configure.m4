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

    # By default the tarball build goes through a 'make dist check' step.
    # It runs first a configure without any options and OMPI_CHECK_UBCL may find UBCL.
    # In that case, UBCL symbols will be included inside binaries such as ompi_info,
    # therefore UBCL linking flags are needed in static mode to resolve symbols.
    #
    # In dynamic mode, UBCL symbols are resolved at runtime: ompi components
    # are dlopen-ed lazily and UBCL components initialization starts with a dlopen
    # of libubcl.so to load symbols. If it fails UBCL components init returns an error and
    # Open MPI will search for other components. UBCL linking flags become optional.
    #
    # In dynamic mode, linking to the UBCL library is delayed. So common/ubcl can let
    # endusers provide a more recent version of UBCL.
    # This mode should be preferably selected for UBCL components.
    # An mca parameter is exposed to select a specific UBCL path that is dlopen
    # at runtime.
    AS_IF([test "$compile_mode" = "dso"],
          [common_ubcl_LDFLAGS=""],
          [AC_MSG_WARN([Only DSO mode of common/ubcl is tested (see --enable-mca-dso)])])

    AC_REQUIRE([MCA_opal_common_ubcl_CONFIG])

    AS_IF([test "$common_ubcl_happy" = "yes"],
          [$1],
          [$2])

    # substitute in the things needed to build ubcl
    AC_SUBST([ompi_common_ubcl_CPPFLAGS])
    AC_SUBST([ompi_common_ubcl_LDFLAGS])
    AC_SUBST([ompi_common_ubcl_LIBS])
])dnl
