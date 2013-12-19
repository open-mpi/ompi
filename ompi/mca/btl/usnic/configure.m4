# -*- shell-script -*-
#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2006      Sandia National Laboratories. All rights
#                         reserved.
# Copyright (c) 2010-2013 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#


# MCA_btl_usnic_CONFIG([action-if-can-compile], 
#                      [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_ompi_btl_usnic_CONFIG],[
    AC_CONFIG_FILES([ompi/mca/btl/usnic/Makefile])

    OMPI_CHECK_OPENFABRICS([btl_usnic],
                        [btl_usnic_happy="yes"],
                        [btl_usnic_happy="no"])

    # We only want to build on Linux.  We also use the clock_gettime()
    # function, which conveniently only happens to exist on Linux.  So
    # just check for that.
    AS_IF([test "$btl_usnic_happy" = "yes"],
          [AC_CHECK_FUNC([clock_gettime],
                         [btl_usnic_happy=yes],
                         [btl_usnic_happy=no])
          ])

    # Do we have the IBV_TRANSPORT_USNIC / IBV_NODE_USNIC defines?
    # (note: if we have one, we have both)
    btl_usnic_have_ibv_usnic=0
    btl_usnic_have_ibv_event_gid_change=0
    AS_IF([test "$btl_usnic_happy" = "yes"],
          [AC_CHECK_DECL([IBV_NODE_USNIC], 
                         [btl_usnic_have_ibv_usnic=1],
                         [],
                         [ #include <infiniband/verbs.h> 
])

           AC_CHECK_DECL([IBV_EVENT_GID_CHANGE], 
                         [btl_usnic_have_ibv_event_gid_change=1],
                         [],
                         [ #include <infiniband/verbs.h> 
])
          ]
    )
    AC_DEFINE_UNQUOTED([BTL_USNIC_HAVE_IBV_USNIC], 
                       [$btl_usnic_have_ibv_usnic],
                       [Whether we have IBV_NODE_USNIC / IBV_TRANSPORT_USNIC or not])
    AC_DEFINE_UNQUOTED([BTL_USNIC_HAVE_IBV_EVENT_GID_CHANGE], 
                       [$btl_usnic_have_ibv_event_gid_change],
                       [Whether we have IBV_EVENT_GID_CHANGE or not])

    AS_IF([test "$btl_usnic_happy" = "yes"],
          [btl_usnic_WRAPPER_EXTRA_LDFLAGS="$btl_usnic_LDFLAGS"
           btl_usnic_WRAPPER_EXTRA_LIBS="$btl_usnic_LIBS"
           $1],
          [$2])

    # Substitute in the things needed to build USNIC
    AC_SUBST([btl_usnic_CPPFLAGS])
    AC_SUBST([btl_usnic_CFLAGS])
    AC_SUBST([btl_usnic_LDFLAGS])
    AC_SUBST([btl_usnic_LIBS])
])dnl
