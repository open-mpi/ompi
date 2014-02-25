# -*- shell-script -*-
#
# Copyright (c) 2013      Sandia National Laboratories.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# MCA_ompi_coll_portals4_POST_CONFIG(will_build)
# ----------------------------------------
# Only require the tag if we're actually going to be built
AC_DEFUN([MCA_ompi_coll_portals4_POST_CONFIG], [
    AS_IF([test "$1" = "1"], [OMPI_REQUIRE_ENDPOINT_TAG([PORTALS4])])
])dnl

# MCA_coll_portals4_CONFIG(action-if-can-compile, 
#                        [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_ompi_coll_portals4_CONFIG],[
    AC_CONFIG_FILES([ompi/mca/coll/portals4/Makefile])

    OMPI_CHECK_PORTALS4([coll_portals4],
                     [coll_portals4_happy="yes"],
                     [coll_portals4_happy="no"])

    AS_IF([test "$coll_portals4_happy" = "yes"],
          [$1],
          [$2])

    AC_SUBST([coll_portals4_CPPFLAGS])
    AC_SUBST([coll_portals4_LDFLAGS])
    AC_SUBST([coll_portals4_LIBS])
])dnl
