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
# Copyright (c) 2010-2011 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# Normal Autotools setup stuff

AC_INIT([openmpi_v1.5_btl_tcp2], [1.0], 
        [http://example.com/help], [openmpi_v1.5_btl_tcp2])
AC_CONFIG_AUX_DIR(config)
AC_CONFIG_MACRO_DIR(config)

AM_INIT_AUTOMAKE([foreign dist-bzip2 no-define 1.11])
AM_SILENT_RULES([yes])

AC_LANG([C])

AH_TOP([/* -*- c -*-
 * Automatically-generated header file from configure.
 */

#ifndef BTL_TCP2_CONFIG_H
#define BTL_TCP2_CONFIG_H
])
AH_BOTTOM([
#endif /* BTL_TCP2_CONFIG_H */
])
AM_CONFIG_HEADER([btl_tcp2_config.h])

CFLAGS_save="$CFLAGS"
AC_PROG_CC
CFLAGS=$CFLAGS_SAVE

# Allow the OMPI header and lib dirs to be specified in shell
# variables (e.g., CPPFLAGS and LDFLAGS) or via command line options.

# Use an existing Open MPI installation tree

AC_ARG_WITH([openmpi-install],
            [AS_HELP_STRING([--with-openmpi-install=DIR],
                            [Specify the --prefix directory used with a "--with-devel-headers" Open MPI installation])],
            [
             # Check for the header directories
             AC_MSG_CHECKING([for Open MPI install dir flags])
             flags="`$withval/bin/mpicc --showme:incdirs`"
             result=
             for f in $flags; do
                 result="-I$f $result"
             done
             CPPFLAGS="$CPPFLAGS $result"
             AC_MSG_RESULT([$result])

             # Check for the library directories
	     AC_MSG_CHECKING([for Open MPI install dir LD flags])
             flags="`$withval/bin/mpicc --showme:libdirs`"
             result=
             for f in $flags; do
                 result="-L$f $result"
             done
             LDFLAGS="$LDFLAGS $result"
             AC_MSG_RESULT([$result])

             # Check for the library files
             AC_MSG_CHECKING([for Open MPI install dir libs])
             flags="`$withval/bin/mpicc --showme:libs`"
             result=
             for f in $flags; do
                 result="-l$f $result"
             done
             LIBS="$LIBS $result"
             AC_MSG_RESULT([not found; double check --with-openmpi-install value])
            ])

# Use an existing Open MPI source tree (assuming that is configured
# and built already)

AC_ARG_WITH([openmpi-source],
            [AS_HELP_STRING([--with-openmpi-source=DIR],
                            [Specify the top directory for the Open MPI source tree])],
            [AC_MSG_CHECKING([for Open MPI source tree])
             AS_IF([test -d "$withval" -a -d "$withval/opal" -a -d "$withval/orte" -a -d "$withval/ompi" -a -f "$withval/VERSION" -a -f "$withval/Makefile.man-page-rules" -a -f "$withval/opal/include/opal_config.h"],
                    [AC_MSG_RESULT([$withval])],
                    [AC_MSG_RESULT([not found; double check --with-openmpi-source value])
                     AC_MSG_ERROR([Cannot continue])])
             CPPFLAGS="$CPPFLAGS -I$withval -I$withval/opal/include -I$withval/orte/include -I$withval/ompi/include"
             # Open MPI v1.4 libraries
             LDFLAGS="$LDFLAGS -L$withval/ompi/.libs -L$withval/orte/.libs -L$withval/opal/.libs"
             LIBS="$LIBS -lmpi -lopen-rte -lopen-pal"
            ])

# Check for the OMPI header files and libraries

AC_CHECK_HEADER([ompi_config.h], [],
                [AC_MSG_WARN([Cannot find ompi_config.h])
                 AC_MSG_ERROR([Cannot continue])])
AC_CHECK_HEADER([ompi/runtime/mpiruntime.h], [],
                [AC_MSG_WARN([Cannot find ompi/runtime/mpiruntime.h])
                 AC_MSG_ERROR([Cannot continue])])
AC_CHECK_FUNC([mca_btl_base_open], [],
              [AC_MSG_WARN([Could not find mca_btl_base_open])
               AC_MSG_ERROR([Cannot continue])])
AC_CHECK_FUNC([orte_show_help], [],
              [AC_MSG_WARN([Could not find orte_show_help])
               AC_MSG_ERROR([Cannot continue])])

# Check for types we need for this component

AC_CHECK_HEADERS([netinet/in.h])
AC_CHECK_TYPES([struct sockaddr_in], [], 
               [AC_MSG_WARN([No struct sockaddr_in])
                AC_MSG_ERROR([Cannot continue])], 
               [AC_INCLUDES_DEFAULT
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif])

LT_INIT([disable-static])

# Party on

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
