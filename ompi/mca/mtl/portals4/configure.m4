# -*- shell-script -*-
#
# Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2013      Sandia National Laboratories.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# MCA_ompi_mtl_portals4_POST_CONFIG(will_build)
# ----------------------------------------
# Only require the tag if we're actually going to be built
AC_DEFUN([MCA_ompi_mtl_portals4_POST_CONFIG], [
    AS_IF([test "$1" = "1"], [OMPI_REQUIRE_ENDPOINT_TAG([PORTALS4])])
])dnl

# MCA_mtl_portals4_CONFIG(action-if-can-compile, 
#                        [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_ompi_mtl_portals4_CONFIG],[
    AC_CONFIG_FILES([ompi/mca/mtl/portals4/Makefile])

    OMPI_CHECK_PORTALS4([mtl_portals4],
                     [mtl_portals4_happy="yes"],
                     [mtl_portals4_happy="no"])

    AS_IF([test "$mtl_portals4_happy" = "yes"],
          [$1],
          [$2])

    # need to propogate CPPFLAGS to all of OMPI
    AS_IF([test "$DIRECT_mtl" = "portals4"],
          [mtl_portals4_WRAPPER_EXTRA_CPPFLAGS="$mtl_portals4_CPPFLAGS"
           CPPFLAGS="$CPPFLAGS $mtl_portals4_CPPFLAGS"])

    AC_ARG_ENABLE([mtl-portals4-flow-control],
       [AC_HELP_STRING([--enable-mtl-portals4-flow-control],
           [enable flow control for Portals 4 MTL (default: disabled)])])
    AC_MSG_CHECKING([whether to enable flow control])
    if test "$enable_mtl_portals4_flow_control" != "no"; then
        AC_MSG_RESULT([yes])
        mtl_portals4_flow_control_enabled=1
    else
        AC_MSG_RESULT([no])
        mtl_portals4_flow_control_enabled=0
    fi
    AC_DEFINE_UNQUOTED([OMPI_MTL_PORTALS4_FLOW_CONTROL],
        [$mtl_portals4_flow_control_enabled],
        [Enable flow control for Portals4 MTL])
    AM_CONDITIONAL([OMPI_MTL_PORTALS4_FLOW_CONTROL],
        [test "$mtl_portals4_flow_control_enabled" = "1"])

    # substitute in the things needed to build portals4
    AC_SUBST([mtl_portals4_CPPFLAGS])
    AC_SUBST([mtl_portals4_LDFLAGS])
    AC_SUBST([mtl_portals4_LIBS])
])dnl
