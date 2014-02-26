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
# Copyright (c) 2010-2014 Cisco Systems, Inc.  All rights reserved.
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

    # We only want to build on 64 bit Linux.
    AS_IF([test "$btl_usnic_happy" = "yes"],
          [AC_CHECK_SIZEOF([void *])
           AC_MSG_CHECKING([for 64 bit Linux])
           case $host_os in
               *linux*)
                   AS_IF([test $ac_cv_sizeof_void_p -eq 8],
                         [btl_usnic_happy=yes],
                         [btl_usnic_happy=no])
                   ;;
               *)
                   btl_usnic_happy=no
                   ;;
           esac
           AC_MSG_RESULT([$btl_usnic_happy])
          ]
    )

    AS_IF([test "$btl_usnic_happy" = "yes"],
          [AC_CHECK_DECLS([IBV_EVENT_GID_CHANGE, ibv_event_type_str], [], [],
                          [#include <infiniband/verbs.h>
])
          ]
    )

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
