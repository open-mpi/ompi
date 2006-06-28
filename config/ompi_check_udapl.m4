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

