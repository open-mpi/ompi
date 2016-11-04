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
# Copyright (c) 2009-2014 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2011      Oak Ridge National Labs.  All rights reserved.
# Copyright (c) 2014      Hochschule Esslingen.  All rights reserved.
# Copyright (c) 2015      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
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
    OPAL_VAR_SCOPE_PUSH([check_crs_criu_good check_crs_criu_dir_msg check_crs_criu_libdir_msg check_crs_criu_dir check_crs_criu_libdir])
    AC_CONFIG_FILES([opal/mca/crs/criu/Makefile])

    AC_ARG_WITH([criu],
                [AC_HELP_STRING([--with-criu(=DIR)],
                                [Path to CRIU Installation])])
    OPAL_CHECK_WITHDIR([criu], [$with_criu], [include/criu/criu.h])
    AC_ARG_WITH([criu-libdir],
                [AC_HELP_STRING([--with-criu-libdir=DIR],
                                [Search for CRIU libraries in DIR])])
    OPAL_CHECK_WITHDIR([criu-libdir], [$with_criu_libdir], [libcriu.*])

    # If we do not want FT or CRIU, don't compile this component
    AS_IF([test "$opal_want_ft_cr" = "1" && test "$with_criu" = "yes"],
          [check_crs_criu_good=yes],
          [check_crs_criu_good=no])

    # Defaults
    check_crs_criu_dir_msg="compiler default"
    check_crs_criu_libdir_msg="linker default"
    check_crs_criu_dir=""
    check_crs_criu_libdir=""

    # Determine the search paths for the headers and libraries
    AS_IF([test $check_crs_criu_good = yes],
          [AS_IF([test ! -z "$with_criu" && test "$with_criu" != "yes"],
                 [check_crs_criu_dir="$with_criu"
                  check_crs_criu_dir_msg="$with_criu (from --with-criu)"])
           AS_IF([test ! -z "$with_criu_libdir" && test "$with_criu_libdir" != "yes"],
                 [check_crs_criu_libdir="$with_criu_libdir"
                  check_crs_criu_libdir_msg="$with_criu_libdir (from --with-criu-libdir)"])
          ])

    AS_IF([test $check_crs_criu_good = yes],
          [AC_MSG_CHECKING([for CRIU dir])
           AC_MSG_RESULT([$check_crs_criu_dir_msg])
           AC_MSG_CHECKING([for CRIU library dir])
           AC_MSG_RESULT([$check_crs_criu_libdir_msg])
           OPAL_CHECK_PACKAGE([crs_criu_check],
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

    AS_IF([test $check_crs_criu_good = yes],
          [ AC_SUBST([crs_criu_CFLAGS])
            AC_SUBST([crs_criu_CPPFLAGS])
            AC_SUBST([crs_criu_LDFLAGS])
            AC_SUBST([crs_criu_LIBS])
            $1],
          [AS_IF([test ! -z "$with_criu" && test "$with_criu" != "no"],
                 [AC_MSG_WARN([CRIU support requested but not found.  Perhaps you need to enable FT support, or specify the location of the CRIU libraries...?])
                  AC_MSG_ERROR([Aborting.])])
           $2])

    OPAL_VAR_SCOPE_POP
])dnl
