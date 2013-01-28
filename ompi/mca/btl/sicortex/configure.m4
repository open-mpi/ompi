# -*- shell-script -*-
#
# Copyright (c) 2008-2009 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# OMPI_CHECK_SICORTEX(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
# check if Sicortex support can be found.  sets prefix_{CPPFLAGS,
# LDFLAGS, LIBS} as needed and runs action-if-found if there is
# support, otherwise executes action-if-not-found
AC_DEFUN([OMPI_CHECK_SICORTEX],[
    AC_ARG_WITH([sicortex],
                [AC_HELP_STRING([--with-sicortex(=DIR)],
                [Build Sicortex (scdma) support, searching for libraries in DIR])])
    AC_ARG_WITH([sicortex-libdir],
                [AC_HELP_STRING([--with-sicortex-libdir=DIR],
                [Search for Sicortex (scdma) libraries in DIR])])
    
    AS_IF([test "$with_sicortex" != "no"],
          [AS_IF([test ! -z "$with_sicortex" -a "$with_sicortex" != "yes"],
                 [ompi_check_sicortex_dir="$with_sicortex"])
           AS_IF([test ! -z "$with_sicortex_libdir" -a "$with_sicortex_libdir" != "yes"],
                 [ompi_check_sicortex_libdir="$with_sicortex_libdir"])

           OMPI_CHECK_PACKAGE([$1],
                              [sicortex/scdma.h],
                              [scdma],
                              [scdma_open],
                              [],
                              [$ompi_check_sicortex_dir],
                              [$ompi_check_sicortex_libdir],
                              [ompi_check_sicortex_happy="yes"],
                              [ompi_check_sicortex_happy="no"])
	    ],
            [ompi_check_sicortex_happy="no"])
    
    AS_IF([test "$ompi_check_sicortex_happy" = "yes"],
          [$2],
          [AS_IF([test ! -z "$with_sicortex" -a "$with_sicortex" != "no"],
                 [AC_MSG_ERROR([Sicortex (scdma) support requested but not found.  Aborting])])
           $3])
    ])

# MCA_btl_sicortex_CONFIG([action-if-can-compile],
#                      [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_btl_sicortex_CONFIG],[
    OMPI_CHECK_SICORTEX([btl_sicortex],
                     [btl_sicortex_happy="yes"],
                     [btl_sicortex_happy="no"])

    AS_IF([test "$btl_sicortex_happy" = "yes"],
          [btl_sicortex_WRAPPER_EXTRA_LDFLAGS="$btl_sicortex_LDFLAGS"
           btl_sicortex_WRAPPER_EXTRA_LIBS="$btl_sicortex_LIBS"
           $1],
          [$2])

    # substitute in the things needed to build sicortex
    AC_SUBST([btl_sicortex_CFLAGS])
    AC_SUBST([btl_sicortex_CPPFLAGS])
    AC_SUBST([btl_sicortex_LDFLAGS])
    AC_SUBST([btl_sicortex_LIBS])
])dnl

