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
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#


# MCA_sds_portals_utcp_CONFIG(action-if-can-compile, 
#                        [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_sds_portals_utcp_CONFIG],[
    # save compiler flags so that we don't alter them for later
    # components.
    sds_portals_utcp_save_CPPFLAGS="$CPPFLAGS"
    sds_portals_utcp_save_LDFLAGS="$LDFLAGS"
    sds_portals_utcp_save_LIBS="$LIBS"

    # allow user a way to say where the Portals installation is
    AC_ARG_WITH(portals, 
        AC_HELP_STRING([--with-portals=DIR],
                       [Specify the installation directory of PORTALS]))

    AS_IF([test -n "$with_portals"],
          [AS_IF([test -d "$with_portals/include"],
                 [sds_portals_utcp_CPPFLAGS="-I$with_portals/include"
                  CPPFLAGS="$CPPFLAGS $sds_portals_utcp_CPPFLAGS"], [])
           AS_IF([test -d "$with_portals/lib"],
                 [sds_portals_utcp_LDFLAGS="-L$with_portals/lib"
                  LDFLAGS="$LDFLAGS $sds_portals_utcp_LDFLAGS"], [])])

    # Try to find all the portals libraries (this is not fun!)
    AC_ARG_WITH(portals-libs, 
        AC_HELP_STRING([--with-portals-libs=LIBS],
                       [Libraries to link with for portals]))
    if test -n "$with_portals_libs" ; then
        sds_portals_utcp_LIBS=""
        for lib in $with_portals_libs ; do
            sds_portals_utcp_LIBS="$sds_portals_utcp_LIBS -l$lib"
        done
    fi

    sds_portals_utcp_LIBS="-lp3utcp -lp3api -lp3lib -lp3rt -lp3utcp"

    # check for portals
    LIBS="$LIBS $sds_portals_utcp_LIBS"
    AC_MSG_CHECKING([for PtlGetRank])
    AC_LINK_IFELSE([AC_LANG_PROGRAM([#include <portals3.h>
#include <stdio.h>
#include <p3nal_utcp.h>
#include <p3rt/p3rt.h>
#include <p3api/debug.h>
FILE *utcp_api_out;
FILE *utcp_lib_out;],
                                    [unsigned int nprocs, rank;
int dummy;
PtlInit(&dummy);
PtlNIInit(PTL_IFACE_DEFAULT, PTL_PID_ANY, NULL, NULL, NULL);
PtlGetRank(PTL_INVALID_HANDLE, &rank, &nprocs);])],
                   [AC_MSG_RESULT([yes])
                    $1],
                   [AC_MSG_RESULT([no])
                    $2])

    # substitute in the things needed to build Portals
    AC_SUBST([sds_portals_utcp_CPPFLAGS])
    AC_SUBST([sds_portals_utcp_LDFLAGS])
    AC_SUBST([sds_portals_utcp_LIBS])

    # reset the flags for the next test
    CPPFLAGS="$sds_portals_utcp_save_CPPFLAGS"
    LDFLAGS="$sds_portals_utcp_save_LDFLAGS"
    LIBS="$sds_portals_utcp_save_LIBS"
])dnl
