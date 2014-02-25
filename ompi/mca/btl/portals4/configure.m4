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
# Copyright (c) 2010      Sandia National Laboratories.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# MCA_btl_portals4_CONFIG(action-if-can-compile, 
#                        [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_ompi_btl_portals4_CONFIG],[
    AC_CONFIG_FILES([ompi/mca/btl/portals4/Makefile])

    OMPI_CHECK_PORTALS4([btl_portals4],
                     [btl_portals4_happy="yes"],
                     [btl_portals4_happy="no"])

    AS_IF([test "$btl_portals4_happy" = "yes"],
          [btl_portals4_WRAPPER_EXTRA_LDFLAGS="$btl_portals4_LDFLAGS"
           btl_portals4_WRAPPER_EXTRA_LIBS="$btl_portals4_LIBS"
           $1],
          [$2])

    # need to propogate CPPFLAGS to all of OMPI
    AS_IF([test "$DIRECT_btl" = "portals4"],
          [CPPFLAGS="$CPPFLAGS $btl_portals4_CPPFLAGS"])

    AC_ARG_ENABLE([btl-portals4-flow-control],
       [AC_HELP_STRING([--enable-btl-portals4-flow-control],
           [enable flow control for Portals 4 BTL (default: disabled)])])
    AC_MSG_CHECKING([whether to enable flow control])
    if test "$enable_btl_portals4_flow_control" != "yes"; then
        AC_MSG_RESULT([no])
        btl_portals4_flow_control_enabled=0
    else
        AC_MSG_RESULT([no])
        btl_portals4_flow_control_enabled=0
    fi
    AC_DEFINE_UNQUOTED([OMPI_BTL_PORTALS4_FLOW_CONTROL],
        [$btl_portals4_flow_control_enabled],
        [Enable flow control for Portals4 BTL])
    AM_CONDITIONAL([OMPI_BTL_PORTALS4_FLOW_CONTROL],
        [test "$btl_portals4_flow_control_enabled" = "1"])

    # substitute in the things needed to build portals4
    AC_SUBST([btl_portals4_CPPFLAGS])
    AC_SUBST([btl_portals4_LDFLAGS])
    AC_SUBST([btl_portals4_LIBS])
])dnl
