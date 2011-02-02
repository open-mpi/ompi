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
# Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# Normal Autotools setup stuff

AC_INIT([openmpi_btl_tcp2], [1.0.0], 
        [http://example.com/help], [openmpi_btl_tcp2])
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
             AC_MSG_RESULT([not found; double check --with-openmpi-install value])

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
            [

             # This works with v1.4 and v1.5, but not with trunk
             # because trunk has the new build opal/event stuff.  This
             # means that there is a variable file to be included that
             # is selected by the winning event component.  I don't
             # know yet how to -I this file properly.  In a
             # --with-devel-headers, this is easy -- I just slurp all
             # flags from the wrapper compiler.  But in a source tree
             # case, how do I know which component won and what -I's
             # to add?  In the current trunk, libevent207 provides a
             # nice include file that is relative to the root of the
             # source tree.  That's good.  But then that file ends up
             # including <event2/event-config.h>, which is most
             # definitely *not* relative to the top of the OMPI source
             # tree -- it's relative to the libevent207 tree.  This is
             # handled properly by libevent207's configure.m4 when
             # building in-tree, but when we're building
             # out-of-the-tree (like here), how the heck are we
             # supposed to know what -I's to add?  This is the problem
             # that needs to be solved.  Probably not *too* hard to
             # fix, but I'm outta time today...

             AC_MSG_CHECKING([for Open MPI source tree])
             AS_IF([test -d "$withval" -a -d "$withval/opal" -a -d "$withval/orte" -a -d "$withval/ompi" -a -f "$withval/VERSION" -a -f "$withval/Makefile.man-page-rules" -a -f "$withval/opal/include/opal_config.h" -a -x "$withval/config.status"],
                    [AC_MSG_RESULT([$withval])],
                    [AC_MSG_RESULT([not found; double check --with-openmpi-source value])
                     AC_MSG_ERROR([Cannot continue])])

             # Run the config.status in the source tree to extract the
             # CPPFLAGS, CFLAGS, LDFLAGS, and LIBS.
             AC_MSG_CHECKING([for Open MPI source tree flags])
             file=source-flags.sh
             rm -f $file.in
             cat > $file.in <<EOF
:
source_CPPFLAGS="@CPPFLAGS@"
source_CFLAGS="@CFLAGS@"
EOF
             $withval/config.status --file=${file}:$file.in > /dev/null
             AS_IF([test "$?" != "0"],
                   [AC_MSG_WARN([config.status from the Open MPI source tree did not run cleanly])
                    AC_MSG_WARN([May experience problems later in the build...])])

             # The flags will contain -I$(top_srcdir).  Filter that out
             sed -e 's/-I$(top_srcdir)//' source-flags.sh > source-flags-filtered
             cp -f source-flags-filtered source-flags.sh
             rm -f source-flags-filtered

             chmod +x $file
             . ./$file
             rm -f $file $file.in

             AC_MSG_RESULT([found])
             echo "  --> CPPFLAGS:" $source_CPPFLAGS
             echo "  --> CFLAGS:" $source_CFLAGS

             CPPFLAGS="$CPPFLAGS -I$withval -I$withval/opal/include -I$withval/orte/include -I$withval/ompi/include"

             # Open MPI v1.7 libraries
             LDFLAGS="$LDFLAGS -L$withval/ompi/.libs"
             # For OMPI v1.7 and later
             LIBS="$LIBS -lmpi"

             CPPFLAGS="$CPPFLAGS $source_CPPFLAGS"
             CFLAGS="$CFLAGS $source_CFLAGS"
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
