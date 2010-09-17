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
# Copyright (c) 2007-2010 Cisco Systems, Inc. All rights reserved.
#
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# Include hwloc m4 files
m4_include(opal/mca/paffinity/hwloc/hwloc/config/hwloc.m4)
m4_include(opal/mca/paffinity/hwloc/hwloc/config/hwloc_pkg.m4)
m4_include(opal/mca/paffinity/hwloc/hwloc/config/hwloc_check_attributes.m4)
m4_include(opal/mca/paffinity/hwloc/hwloc/config/hwloc_check_visibility.m4)

# MCA_paffinity_hwloc_POST_CONFIG()
# ---------------------------------
AC_DEFUN([MCA_opal_paffinity_hwloc_POST_CONFIG],[
    AM_CONDITIONAL([OPAL_PAFFINITY_HWLOC_INTERNAL],
                   [test "$paffinity_hwloc_location" = "internal"])

    HWLOC_DO_AM_CONDITIONALS
])dnl


# MCA_paffinity_hwloc_CONFIG([action-if-found], [action-if-not-found])
# --------------------------------------------------------------------
AC_DEFUN([MCA_opal_paffinity_hwloc_CONFIG],[
    AC_CONFIG_FILES([opal/mca/paffinity/hwloc/Makefile])

    OMPI_VAR_SCOPE_PUSH([HWLOC_VERSION opal_check_hwloc_happy opal_check_hwloc_save_CPPFLAGS opal_check_hwloc_save_LDFLAGS opal_check_hwloc_save_LIBS])

    # Allowing building using either the internal copy of
    # hwloc, or an external version.
    AC_ARG_WITH([hwloc],
        [AC_HELP_STRING([--with-hwloc(=DIR)],
             [Build hwloc support.  DIR can take one of three values: "internal", "external", or a valid directory name.  "internal" (or no DIR value) forces Open MPI to use its internal copy of hwloc.  "external" forces Open MPI to use an external installation of hwloc.  Supplying a valid directory name also forces Open MPI to use an external installation of hwloc, and adds DIR/include, DIR/lib, and DIR/lib64 to the search path for headers and libraries.])])

    # Default to building the internal copy.  After this,
    # paffinity_hwloc_location is guaranteed to be set to one of:
    # "internal", a directory name (i.e., whatever the user supplied),
    # or "no".
    paffinity_hwloc_location=$with_hwloc
    AS_IF([test -z "$paffinity_hwloc_location" -o "$paffinity_hwloc_location" = "yes"],
          [paffinity_hwloc_location=internal])

    # Check the DIR value if it's a directory
    case $paffinity_hwloc_location in
    no|internal|external) ;;
    *) OMPI_CHECK_WITHDIR([hwloc], [$paffinity_hwloc_location], [include/hwloc.h]) ;;
    esac
 
    AC_ARG_WITH([hwloc-libdir],
       [AC_HELP_STRING([--with-hwloc-libdir=DIR],
             [Search for hwloc libraries in DIR.  Should only be used if an external copy of hwloc is being used.])])
    AS_IF([test "$with_hwloc_libdir" = "internal" -a "$with_hwloc_libdir" != ""],
          [AC_MSG_WARN([Both --with-hwloc=internal and --with-hwloc-libdir=DIR])
           AC_MSG_WARN([were specified, which does not make sense.])
           AC_MSG_ERROR([Cannot continue])])
    OMPI_CHECK_WITHDIR([hwloc-libdir], [$with_hwloc_libdir], [libhwloc.*])

    opal_check_hwloc_save_CPPFLAGS=$CPPFLAGS
    opal_check_hwloc_save_LDFLAGS=$LDFLAGS
    opal_check_hwloc_save_LIBS=$LIBS

    AS_IF([test "$paffinity_hwloc_location" != "no"],
          [AC_MSG_CHECKING([where to look for hwloc])])

    # If we're building internal, run the hwloc configuration.
    AS_IF([test "$paffinity_hwloc_location" = "internal"],
          [# Main hwloc configuration
           AC_MSG_RESULT([internal copy])
           HWLOC_SET_SYMBOL_PREFIX([opal_paffinity_])
           HWLOC_SETUP_CORE([opal/mca/paffinity/hwloc/hwloc], 
                     [AC_MSG_CHECKING([whether hwloc configure succeeded])
                      AC_MSG_RESULT([yes])
                      HWLOC_VERSION="internal v`$srcdir/opal/mca/paffinity/hwloc/hwloc/config/hwloc_get_version.sh $srcdir/opal/mca/paffinity/hwloc/hwloc/VERSION`"
                      # Add flags to the wrappers for static builds
                      paffinity_hwloc_WRAPPER_EXTRA_LDFLAGS=$HWLOC_EMBEDDED_LDFLAGS
                      paffinity_hwloc_WRAPPER_EXTRA_LIBS=$HWLOC_EMBEDDED_LIBS
                      opal_check_hwloc_happy=yes], 
                     [AC_MSG_CHECKING([whether hwloc configure succeeded])
                      AC_MSG_RESULT([no])
                      opal_check_hwloc_happy=no])
          ])

    # If we are not building internal, then run all the normal checks
    AS_IF([test "$paffinity_hwloc_location" != "internal" -a "$paffinity_hwloc_location" != "no"],
          [AS_IF([test ! -z "$paffinity_hwloc_location" -a "$paffinity_hwloc_location" != "yes" -a "$paffinity_hwloc_location" != "external"],
                 [opal_check_hwloc_dir=$paffinity_hwloc_location
                  AC_MSG_RESULT([external install ($paffinity_hwloc_location)])],
                 [AC_MSG_RESULT([external install (default search paths)])])
           AS_IF([test ! -z "$with_hwloc_libdir" -a "$with_hwloc_libdir" != "yes"],
                 [opal_check_hwloc_libdir="$with_hwloc_libdir"])
           AS_IF([test "$paffinity_hwloc_location" = no],
                 [opal_check_hwloc_happy=no],
                 [opal_check_hwloc_happy=yes])

           HWLOC_VERSION=external
           OMPI_CHECK_PACKAGE([paffinity_hwloc],
                              [hwloc.h],
                              [hwloc],
                              [hwloc_topology_init],
                              [],
                              [$opal_check_hwloc_dir],
                              [$opal_check_hwloc_libdir],
                              [opal_check_hwloc_happy=yes],
                              [opal_check_hwloc_happy=no])
           ])

    CPPFLAGS=$opal_check_hwloc_save_CPPFLAGS
    LDFLAGS=$opal_check_hwloc_save_LDFLAGS
    LIBS=$opal_check_hwloc_save_LIBS

    AC_SUBST([paffinity_hwloc_CFLAGS])
    AC_SUBST([paffinity_hwloc_CPPFLAGS])
    AC_SUBST([paffinity_hwloc_LDFLAGS])
    AC_SUBST([paffinity_hwloc_LIBS])

    # Done!
    AS_IF([test "$opal_check_hwloc_happy" = "yes"],
          [AC_DEFINE_UNQUOTED([PAFFINITY_HWLOC_HWLOC_VERSION], 
                              ["$HWLOC_VERSION"], 
                              [Version of hwloc])
           $1],
          [AS_IF([test ! -z "$with_hwloc" -a "$with_hwloc" != "no"],
                 [AC_MSG_WARN([hwloc support requested (via --with-hwloc) but not found.])
                  AC_MSG_ERROR([Cannot continue.])])
           $2])
    OMPI_VAR_SCOPE_POP
])dnl
