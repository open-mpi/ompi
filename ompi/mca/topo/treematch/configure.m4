# -*- shell-script -*-
#
# Copyright (c) 2011-2015 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2011-2015 INRIA.  All rights reserved.
# Copyright (c) 2011-2015 Universite Bordeaux 1
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_ompi_topo_treematch_CONFIG([action-if-can-compile],
#                                [action-if-cant-compile])
# -------------------------------------------
AC_DEFUN([MCA_ompi_topo_treematch_CONFIG], [
    AC_REQUIRE([MCA_opal_hwloc_CONFIG_REQUIRE])

    AC_ARG_WITH([treematch],
          [AC_HELP_STRING([--with-treematch(=DIR)],
               [Build TreeMatch topology support, optionally adding DIR/include, DIR/lib, and DIR/lib64 to the search path for headers and libraries])],
               [],
               [with_treematch=yes])
     AC_ARG_WITH([treematch-include],
                 [AC_HELP_STRING([--with-treematch-include(=DIR)],
                                 ["Search for TreeMatch headers in DIR"])])
     AC_ARG_WITH([treematch-libdir],
                 [AC_HELP_STRING([--with-treematch-libdir(=DIR)],
                                 ["Search for TreeMatch libraries in DIR"])])

    treematch_files_local="no"
    ompi_check_treematch_dir=$srcdir
    ompi_check_treematch_libdir=""
    ompi_check_treematch_happy="no"

    AS_IF([test "x$with_treematch" != xno],
          [AC_MSG_CHECKING([TreeMatch headers])
           AS_IF([test "x$with_treematch_include" = x],
                 [AS_IF([test "x$with_treematch" = xyes],
                        [treematch_files_local="yes"
                         with_treematch_include=$OMPI_TOP_SRCDIR/ompi/mca/topo/treematch/treematch],
                        [with_treematch_include=$with_treematch/include])])
           AS_IF([test -f $with_treematch_include/tm_tree.h],
                 [AS_IF([test "x$with_treematch" = xyes],
                        [AC_MSG_RESULT([in the source])],
                        [AC_MSG_RESULT([user provided])])
                  opal_check_treematch_dir=$with_treematch_include
                  ompi_check_treematch_happy="yes"],
                 [AC_MSG_ERROR([missing tm_tree.h (${with_treematch}:${with_treematch_include})])])])

    AS_IF([test "$ompi_check_treematch_happy" = "yes"],
          [AC_MSG_CHECKING([TreeMatch library])
           OPAL_CHECK_WITHDIR([treematch], [$with_treematch_include], [tm_tree.h])
           AS_IF([test "x$with_treematch_libdir" = x],
                 [AS_IF([test "x$with_treematch" != xyes],
                        [with_treematch_libdir=$with_treematch/lib]
                        [with_treematch_libdir=$OMPI_TOP_SRCDIR/ompi/mca/topo/treematch/treematch])])
           AS_IF([test "x$treematch_files_local" = xno],
                 [OPAL_CHECK_WITHDIR([treematch-libdir], [$with_treematch_libdir], [libtreematch.*])
                  AS_IF([test "x$with_treematch" != xno -a "x$with_treematch" != xyes],
                        [AS_IF([test ! -z "$with_treematch" -a "$with_treematch" != "yes"],
                               [ompi_check_treematch_dir="$with_treematch"])
                         AS_IF([test ! -z "$with_treematch_libdir" -a "$with_treematch_libdir" != "yes"],
                               [ompi_check_treematch_libdir="$with_treematch_libdir"])
                         OPAL_CHECK_PACKAGE([topo_treematch],
                                            [tm_tree.h],
                                            [treematch],
                                            [build_tree],
                                            [],
                                            [$with_treematch_include],
                                            [$with_treematch_libdir],
                                            [ompi_check_treematch_happy="yes"],
                                            [ompi_check_treematch_happy="no"])],
                        [ompi_check_treematch_happy="no"])])])

    AS_IF([test "$ompi_check_treematch_happy" = "yes"],
          [$1],
          [AS_IF([test ! -z "$with_treematch" -a "$with_treematch" != "no"],
                 [AC_MSG_ERROR([TreeMatch support requested but not found.  Aborting])])
           $2])

    AC_CONFIG_FILES([ompi/mca/topo/treematch/Makefile])
    AM_CONDITIONAL(topo_treematch_local,
                   [test "x$treematch_files_local" = "xyes"])
])
