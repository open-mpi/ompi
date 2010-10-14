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
# Copyright (c) 2004-2006 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2006      QLogic Corp. All rights reserved.
# Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# OMPI_CHECK_PORTALS4(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
# check if PORTALS4 support can be found.  sets prefix_{CPPFLAGS, 
# LDFLAGS, LIBS} as needed and runs action-if-found if there is
# support, otherwise executes action-if-not-found
AC_DEFUN([OMPI_CHECK_PORTALS4],[
    AC_ARG_WITH([portals4],
        [AC_HELP_STRING([--with-portals4(=DIR)],
             [Build Portals4 support, optionally adding DIR/include, DIR/lib, and DIR/lib64 to the search path for headers and libraries])])
    OMPI_CHECK_WITHDIR([portals4], [$with_portals4], [include/portals4.h])
    AC_ARG_WITH([portals4-libdir],
        [AC_HELP_STRING([--with-portals4-libdir=DIR],
             [Search for Portals4 libraries in DIR])])
    OMPI_CHECK_WITHDIR([portals4-libdir], [$with_portals4_libdir], [libportals.*])

    ompi_check_portals4_$1_save_CPPFLAGS="$CPPFLAGS"
    ompi_check_portals4_$1_save_LDFLAGS="$LDFLAGS"
    ompi_check_portals4_$1_save_LIBS="$LIBS"

    AS_IF([test "$with_portals4" != "no"],
          [AS_IF([test ! -z "$with_portals4" -a "$with_portals4" != "yes"],
                 [ompi_check_portals4_dir="$with_portals4"])
           AS_IF([test ! -z "$with_portals4_libdir" -a "$with_portals4_libdir" != "yes"],
                 [ompi_check_portals4_libdir="$with_portals4_libdir"])

           OMPI_CHECK_PACKAGE([$1],
                              [portals4.h],
                              [portals],
                              [PtlLEAppend],
			      [],
                              [$ompi_check_portals4_dir],
                              [$ompi_check_portals4_libdir],
                              [ompi_check_portals4_happy="yes"],
                              [ompi_check_portals4_happy="no"])],
          [ompi_check_portals4_happy="no"])

    CPPFLAGS="$ompi_check_portals4_$1_save_CPPFLAGS"
    LDFLAGS="$ompi_check_portals4_$1_save_LDFLAGS"
    LIBS="$ompi_check_portals4_$1_save_LIBS"

    AS_IF([test "$ompi_check_portals4_happy" = "yes"],
          [$2],
          [AS_IF([test ! -z "$with_portals4" -a "$with_portals4" != "no"],
                 [AC_MSG_ERROR([Portals4 support requested but not found.  Aborting])])
           $3])
])

