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
# Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#


# OMPI_CHECK_UDAPL(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
# check if uDAPL support can be found.  sets prefix_{CPPFLAGS, 
# LDFLAGS, LIBS} as needed and runs action-if-found if there is
# support, otherwise executes action-if-not-found
AC_DEFUN([OMPI_CHECK_UDAPL],[
    AC_ARG_WITH([udapl],
        [AC_HELP_STRING([--with-udapl(=DIR)],
             [Build uDAPL support, searching for libraries in DIR])])
    AC_ARG_WITH([udapl-libdir],
       [AC_HELP_STRING([--with-udapl-libdir=DIR],
             [Search for uDAPL libraries in DIR])])

    # Special case for OFED/Linux: the default /etc/dat.conf that
    # ships with OFED is broken in that it includes DAT providers that
    # are not guarnateed to work (e.g., it includes providers for ib0,
    # ib1, ib2, ib3, and bond0).  Usually, a sysadmin will need to
    # edit this file to configure it for the specific environment in
    # which it will be used.  Hence, if you run the udapl BTL on
    # Linux/OFED, you'll get a bunch of warning messages about the
    # providers that don't work.  However, on Linux/OFED, you don't
    # really want to use udapl anyway; you likely really want to use
    # the openib BTL (i.e., native verbs, not udapl).  

    # So after exploring many different scenarios, the least evil
    # solution seemed to be to disable building the udapl BTL on
    # Linux/OFED *unless the user specifically asks for it.* To be
    # specific: on Linux/OFED, if you do not specify
    # --with-udapl(=DIR), the udapl BTL will not be built.
    AS_IF([test -z "$with_udapl"],
          [case $host in
              *linux*) 
                  AC_MSG_WARN([On Linux and --with-udapl was not specified])
                  AC_MSG_WARN([Not building the udapl BTL])
                  with_udapl=no
                  ;;
           esac])

    AS_IF([test ! -z "$with_udapl" -a "$with_udapl" != "yes"],
          [ompi_check_udapl_dir="$with_udapl"])
    AS_IF([test ! -z "$with_udapl_libdir" -a "$with_udapl_libdir" != "yes"],
          [ompi_check_udapl_libdir="$with_udapl_libdir"])

    AS_IF([test "$with_udapl" != "no"],
          [ # check for pthreads and emit a warning that
            # things might go south...
           AS_IF([test "$HAVE_POSIX_THREADS" != "1"],
                 [AC_MSG_WARN([POSIX threads not enabled.  May not be able to link with udapl])])
    
           ompi_check_udapl$1_save_CFLAGS="$CFLAGS"
           ompi_check_udapl$1_save_CPPFLAGS="$CPPFLAGS"
    
           OMPI_CHECK_PACKAGE([$1],
	          [dat/udat.h],
                  [dat],
                  [dat_registry_list_providers],
                  [],
                  [$ompi_check_udapl_dir],
                  [$ompi_check_udapl_libdir],
                  [ompi_check_udapl_happy="yes"],
                  [ompi_check_udapl_happy="no"])

           # if needed use -ldapl as well
           AS_IF([test "$ompi_check_udapl_happy" = "no"],
             [OMPI_CHECK_PACKAGE([$1],
	            [dat/udat.h],
                   [dat],
                   [dat_registry_list_providers],
                   [-ldapl],
                   [$ompi_check_udapl_dir],
                   [$ompi_check_udapl_libdir],
                   [ompi_check_udapl_happy="yes"],
                   [ompi_check_udapl_happy="no"])])

           CPPFLAGS="$ompi_check_udapl$1_save_CPPFLAGS"],
          [ompi_check_udapl_happy="no"])

    AS_IF([test "$ompi_check_udapl_happy" = "yes"],
          [$2],
          [AS_IF([test ! -z "$with_udapl" -a "$with_udapl" != "no"],
                 [AC_MSG_ERROR([uDAPL support requested but not found.  Aborting])])
           $3])
])

