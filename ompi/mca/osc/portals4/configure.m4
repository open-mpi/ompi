# -*- shell-script -*-
#
# Copyright (c) 2011      Sandia National Laboratories.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# MCA_ompi_osc_portals4_POST_CONFIG(will_build)
# ----------------------------------------
# Only require the tag if we're actually going to be built
AC_DEFUN([MCA_ompi_osc_portals4_POST_CONFIG], [
    AS_IF([test "$1" = "1"], [OMPI_REQUIRE_ENDPOINT_TAG([PORTALS4])])
])dnl

# MCA_osc_portals4_CONFIG(action-if-can-compile, 
#                        [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_ompi_osc_portals4_CONFIG],[
    AC_CONFIG_FILES([ompi/mca/osc/portals4/Makefile])

    OMPI_CHECK_PORTALS4([osc_portals4],
                     [osc_portals4_happy="yes"],
                     [osc_portals4_happy="no"])

    AS_IF([test "$osc_portals4_happy" = "yes"],
          [osc_portals4_WRAPPER_EXTRA_LDFLAGS="$osc_portals4_LDFLAGS"
           osc_portals4_WRAPPER_EXTRA_LIBS="$osc_portals4_LIBS"
           $1],
          [$2])

    # need to propogate CPPFLAGS to all of OMPI
    AS_IF([test "$DIRECT_osc" = "portals4"],
          [CPPFLAGS="$CPPFLAGS $osc_portals4_CPPFLAGS"])

    # substitute in the things needed to build portals4
    AC_SUBST([osc_portals4_CPPFLAGS])
    AC_SUBST([osc_portals4_LDFLAGS])
    AC_SUBST([osc_portals4_LIBS])
])dnl
