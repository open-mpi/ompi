# -*- shell-script -*-
#
# Copyright (c) 2004-2010 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2006 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2009-2010 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2011      Oak Ridge National Labs.  All rights reserved.
# Copyright (c) 2014      Hochschule Esslingen.  All rights reserved.
#
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_crs_criu_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_opal_crs_criu_CONFIG],[
    AC_CONFIG_FILES([opal/mca/crs/criu/Makefile])

    AC_ARG_WITH([criu],
                [AC_HELP_STRING([--with-criu(=DIR)],
                                [Path to CRIU Installation])])
    OMPI_CHECK_WITHDIR([criu], [$with_criu], [include/criu/criu.h])
    AC_ARG_WITH([criu-libdir],
                [AC_HELP_STRING([--with-criu-libdir=DIR],
                                [Search for CRIU libraries in DIR])])
    OMPI_CHECK_WITHDIR([criu-libdir], [$with_criu_libdir], [libcriu.*])

    check_crs_criu_good="no"

    # If we do not want FT, don't compile this component
    #
    # If we wanted CRIU, but did not specify the FT option,
    # error out with a warning for the user
    AS_IF([test "$opal_want_ft_cr" = "0"],
          [$2
           check_crs_criu_good="no"
           AS_IF([test ! -z "$with_criu" -a "$with_criu" != "no"],
                 [AC_MSG_WARN([CRIU support requested, but FT support not requested. You need to specify the --with-ft=cr configure option.])
                  AC_MSG_ERROR([Aborting.])])
          ],
          [check_crs_criu_good="yes"])

    # If we do not want CRIU, then do not compile it
    AS_IF([test "$with_criu" = "no" -o "$check_crs_criu_good" = "no"],
          [$2
           check_crs_criu_good="no"],
          [check_crs_criu_good="yes"])

    # Defaults
    check_crs_criu_dir_msg="compiler default"
    check_crs_criu_libdir_msg="linker default"
    check_crs_criu_dir=""
    check_crs_criu_libdir=""

    # Determine the search paths for the headers and libraries
    AS_IF([test "$check_crs_criu_good" != "yes"], [$2],
          [AS_IF([test ! -z "$with_criu" -a "$with_criu" != "yes"],
                 [check_crs_criu_dir="$with_criu"
                  check_crs_criu_dir_msg="$with_criu (from --with-criu)"])
           AS_IF([test ! -z "$with_criu_libdir" -a "$with_criu_libdir" != "yes"],
                 [check_crs_criu_libdir="$with_criu_libdir"
                  check_crs_criu_libdir_msg="$with_criu_libdir (from --with-criu-libdir)"])
          ])

    AS_IF([test "$check_crs_criu_good" != "yes"], [$2],
          [AC_MSG_CHECKING([for CRIU dir])
           AC_MSG_RESULT([$check_crs_criu_dir_msg])
           AC_MSG_CHECKING([for CRIU library dir])
           AC_MSG_RESULT([$check_crs_criu_libdir_msg])
           OMPI_CHECK_PACKAGE([crs_criu_check],
                              [criu/criu.h],
                              [criu],
                              [criu_init_opts],
                              [],
                              [$check_crs_criu_dir],
                              [$check_crs_criu_libdir],
                              [check_crs_criu_good="yes"],
                              [check_crs_criu_good="no"])
          ])

    crs_criu_CFLAGS="$CFLAGS $crs_criu_check_CFLAGS"
    crs_criu_CPPFLAGS="$CPPFLAGS $crs_criu_check_CPPFLAGS"
    crs_criu_LDFLAGS="$LDFLAGS $crs_criu_check_LDFLAGS"
    crs_criu_LIBS="$LIBS $crs_criu_check_LIBS"

    AS_IF([test "$check_crs_criu_good" = "yes"],
          [ AC_SUBST([crs_criu_CFLAGS])
            AC_SUBST([crs_criu_CPPFLAGS])
            AC_SUBST([crs_criu_LDFLAGS])
            AC_SUBST([crs_criu_LIBS])
            $1],
          [AS_IF([test ! -z "$with_criu" -a "$with_criu" != "no"],
                 [AC_MSG_WARN([CRIU support requested but not found.  Perhaps you need to specify the location of the CRIU libraries.])
                  AC_MSG_ERROR([Aborting.])])
           $2])

])dnl
