# -*- shell-script -*-
#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
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


# MCA_<framework>_<component>_CONFIG([action-if-can-compile], 
#                                    [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_maffinity_libnuma_CONFIG],[
    AC_ARG_WITH([libnuma],
                [AC_HELP_STRING([--with-libnuma],
                                [Directory where the libnuma software is installed])])

    ompi_check_maffinity_libnuma_save_CPPFLAGS="$CPPFLAGS"
    ompi_check_maffinity_libnuma_save_LDFLAGS="$LDFLAGS"
    ompi_check_maffinity_libnuma_save_LIBS="$LIBS"

    # libnuma, by default, installs into lib64/.  So we're going to
    # assume that's the common case.  If this needs to be augmented
    # someday to look in lib/ as well, then so be it.
    AS_IF([test ! -z "$with_libnuma"], 
          [CPPFLAGS="$CPPFLAGS -I$with_libnuma/include"
           LDFLAGS="$LDFLAGS -L$with_libnuma/lib64"])
    AC_CHECK_HEADERS([numa.h],
                     [AC_CHECK_LIB([numa], 
                                   [numa_available],
                                   [ompi_check_maffinity_libnuma_happy="yes"],
                                   [ompi_check_maffinity_libnuma_happy="no"])],
                     [ompi_check_maffinity_libnuma_happy="no"])

    CPPFLAGS="$ompi_check_maffinity_libnuma_save_CPPFLAGS"
    LDFLAGS="$ompi_check_maffinity_libnuma_save_LDFLAGS"
    LIBS="$ompi_check_maffinity_libnuma_save_LIBS"

    AS_IF([test "$ompi_check_maffinity_libnuma_happy" = "yes"], 
          [AS_IF([test ! -z "$with_libnuma"], 
                 [maffinity_libnuma_CPPFLAGS="$maffinity_libnuma_CPPFLAGS -I$with_libnuma/include"
                  maffinity_libnuma_LDFLAGS="$maffinity_libnuma_LDFLAGS -L$with_libnuma/lib64"
                  maffinity_libnuma_LIBS="$maffinity_libnuma_LIBS -lnuma"
                  maffinity_libnuma_WRAPPER_EXTRA_LDFLAGS="$maffinity_libnuma_LDFLAGS"
                  maffinity_libnuma_WRAPPER_EXTRA_LIBS="$maffinity_libnuma_LIBS"])
           $1], 
          [$2])

    # substitute in the things needed to build gm
    AC_SUBST([maffinity_libnuma_CFLAGS])
    AC_SUBST([maffinity_libnuma_CPPFLAGS])
    AC_SUBST([maffinity_libnuma_LDFLAGS])
    AC_SUBST([maffinity_libnuma_LIBS])
])dnl
