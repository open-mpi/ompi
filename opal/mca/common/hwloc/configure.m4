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

# This configure.m4 script outputs several things:
#
# 1. The $opal_common_hwloc_support shell variable will be set to "yes"
# or "no".  Since the common framework is guaranteed to be processed
# by configure first (before all other frameworks), components that
# depend on hwloc can simply check the value of
# $opal_check_hwloc_support to know if this common framework will be
# built or not.
#
# 2. Similarly, OPAL_COMMON_HWLOC_SUPPORT is an AM_CONDITIONAL; it'll be
# true if $opal_common_hwloc_support is "yes".
#
# 3. Similarly, OPAL_COMMON_HWLOC_SUPPORT is AC_DEFINE'd to 0 or 1.
#
# 4. The following values are AC_SUBSTed and can be used in
# components' Makefile.ams:
#    opal_common_hwloc_CFLAGS
#    opal_common_hwloc_CPPFLAGS
#    opal_common_hwloc_LDFLAGS
#    opal_common_hwloc_LIBS
#

# Include hwloc m4 files
m4_include(opal/mca/common/hwloc/hwloc/config/hwloc.m4)
m4_include(opal/mca/common/hwloc/hwloc/config/hwloc_pkg.m4)
m4_include(opal/mca/common/hwloc/hwloc/config/hwloc_check_attributes.m4)
m4_include(opal/mca/common/hwloc/hwloc/config/hwloc_check_visibility.m4)

# MCA_common_hwloc_POST_CONFIG()
# ---------------------------------
AC_DEFUN([MCA_opal_common_hwloc_POST_CONFIG],[
    AM_CONDITIONAL([OPAL_COMMON_HWLOC_SUPPORT],
                   [test "$opal_common_hwloc_support" = "yes"])
    AM_CONDITIONAL([OPAL_COMMON_HWLOC_INTERNAL],
                   [test "$opal_common_hwloc_support" = "yes" -a "$opal_common_hwloc_location" = "internal"])

    HWLOC_DO_AM_CONDITIONALS
])dnl


# MCA_common_hwloc_CONFIG([action-if-found], [action-if-not-found])
# --------------------------------------------------------------------
AC_DEFUN([MCA_opal_common_hwloc_CONFIG],[
    AC_CONFIG_FILES([opal/mca/common/hwloc/Makefile])

    OPAL_VAR_SCOPE_PUSH([HWLOC_VERSION opal_common_hwloc_save_CPPFLAGS opal_common_hwloc_save_LDFLAGS opal_common_hwloc_save_LIBS opal_common_hwloc_support_value opal_common_hwloc_save_xml opal_common_hwloc_save_cairo])

    # Allowing building using either the internal copy of
    # hwloc, or an external version.
    AC_ARG_WITH([hwloc],
        [AC_HELP_STRING([--with-hwloc(=DIR)],
             [Build hwloc support.  DIR can take one of three values: "internal", "external", or a valid directory name.  "internal" (or no DIR value) forces Open MPI to use its internal copy of hwloc.  "external" forces Open MPI to use an external installation of hwloc.  Supplying a valid directory name also forces Open MPI to use an external installation of hwloc, and adds DIR/include, DIR/lib, and DIR/lib64 to the search path for headers and libraries.])])

    # Default to building the internal copy.  After this,
    # opal_common_hwloc_location is guaranteed to be set to one of:
    # "internal", a directory name (i.e., whatever the user supplied),
    # or "no".
    opal_common_hwloc_location=$with_hwloc
    AS_IF([test -z "$opal_common_hwloc_location" -o "$opal_common_hwloc_location" = "yes"],
          [opal_common_hwloc_location=internal])

    # Check the DIR value if it's a directory
    case $opal_common_hwloc_location in
    no|internal|external) ;;
    *) OMPI_CHECK_WITHDIR([hwloc], [$opal_common_hwloc_location], [include/hwloc.h]) ;;
    esac
 
    AC_ARG_WITH([hwloc-libdir],
       [AC_HELP_STRING([--with-hwloc-libdir=DIR],
             [Search for hwloc libraries in DIR.  Should only be used if an external copy of hwloc is being used.])])
    AS_IF([test "$with_hwloc_libdir" = "internal" -a "$with_hwloc_libdir" != ""],
          [AC_MSG_WARN([Both --with-hwloc=internal and --with-hwloc-libdir=DIR])
           AC_MSG_WARN([were specified, which does not make sense.])
           AC_MSG_ERROR([Cannot continue])])
    OMPI_CHECK_WITHDIR([hwloc-libdir], [$with_hwloc_libdir], [libhwloc.*])

    opal_common_hwloc_save_CPPFLAGS=$CPPFLAGS
    opal_common_hwloc_save_LDFLAGS=$LDFLAGS
    opal_common_hwloc_save_LIBS=$LIBS

    AS_IF([test "$opal_common_hwloc_location" != "no"],
          [AC_MSG_CHECKING([where to look for hwloc])])

    # If we're building internal, run the hwloc configuration.
    AS_IF([test "$opal_common_hwloc_location" = "internal"],
          [# Main hwloc configuration
           AC_MSG_RESULT([internal copy])
           HWLOC_SET_SYMBOL_PREFIX([opal_common_])

           # We don't want no stinkin' XML or graphical support
           opal_common_hwloc_save_xml=$enable_xml
           opal_common_hwloc_save_cairo=$enable_cairo
           enable_xml=no
           enable_cairo=no

           HWLOC_SETUP_CORE([opal/mca/common/hwloc/hwloc], 
                     [AC_MSG_CHECKING([whether hwloc configure succeeded])
                      AC_MSG_RESULT([yes])
                      HWLOC_VERSION="internal v`$srcdir/opal/mca/common/hwloc/hwloc/config/hwloc_get_version.sh $srcdir/opal/mca/common/hwloc/hwloc/VERSION`"

                      # Add flags to the wrappers for static builds.
                      # Note that we don't add the project name to the
                      # wrapper extra flags.  :-(
                      common_hwloc_WRAPPER_EXTRA_LIBS=$HWLOC_EMBEDDED_LIBS

                      opal_common_hwloc_LDFLAGS='$(HWLOC_EMBEDDED_LDFLAGS)'
                      opal_common_hwloc_LIBS='$(top_ompi_builddir)/opal/mca/common/hwloc/hwloc/src/libhwloc_embedded.la $(HWLOC_EMBEDDED_LIBS)'
                      opal_common_hwloc_support=yes], 
                     [AC_MSG_CHECKING([whether hwloc configure succeeded])
                      AC_MSG_RESULT([no])
                      opal_common_hwloc_support=no])

          # Restore some env variables, if necessary
          AS_IF([test -n "$opal_common_hwloc_save_xml"],
                [enable_xml=$opal_common_hwloc_save_xml])
          AS_IF([test -n "$opal_common_hwloc_save_cairo"],
                [enable_cairo=$opal_common_hwloc_save_cairo])
          ])

    # If we are not building internal, then run all the normal checks
    AS_IF([test "$opal_common_hwloc_location" != "internal" -a "$opal_common_hwloc_location" != "no"],
          [AS_IF([test ! -z "$opal_common_hwloc_location" -a "$opal_common_hwloc_location" != "yes" -a "$opal_common_hwloc_location" != "external"],
                 [opal_common_hwloc_dir=$opal_common_hwloc_location
                  AC_MSG_RESULT([external install ($opal_common_hwloc_location)])],
                 [AC_MSG_RESULT([external install (default search paths)])])
           AS_IF([test ! -z "$with_hwloc_libdir" -a "$with_hwloc_libdir" != "yes"],
                 [opal_common_hwloc_libdir="$with_hwloc_libdir"])
           AS_IF([test "$opal_common_hwloc_location" = no],
                 [opal_common_hwloc_support=no],
                 [opal_common_hwloc_support=yes])

           HWLOC_VERSION=external
           OMPI_CHECK_PACKAGE([opal_common_hwloc],
                              [hwloc.h],
                              [hwloc],
                              [hwloc_topology_init],
                              [],
                              [$opal_common_hwloc_dir],
                              [$opal_common_hwloc_libdir],
                              [opal_common_hwloc_support=yes],
                              [opal_common_hwloc_support=no])
           ])

    CPPFLAGS=$opal_common_hwloc_save_CPPFLAGS
    LDFLAGS=$opal_common_hwloc_save_LDFLAGS
    LIBS=$opal_common_hwloc_save_LIBS

    AC_SUBST([opal_common_hwloc_CFLAGS])
    AC_SUBST([opal_common_hwloc_CPPFLAGS])
    AC_SUBST([opal_common_hwloc_LDFLAGS])
    AC_SUBST([opal_common_hwloc_LIBS])

    # Done!
    AS_IF([test "$opal_common_hwloc_support" = "yes"],
          [AC_DEFINE_UNQUOTED([COMMON_HWLOC_HWLOC_VERSION], 
                              ["$HWLOC_VERSION"], 
                              [Version of hwloc])
           opal_common_hwloc_support_value=1
           $1],
          [AS_IF([test ! -z "$with_hwloc" -a "$with_hwloc" != "no"],
                 [AC_MSG_WARN([hwloc support requested (via --with-hwloc) but not found.])
                  AC_MSG_ERROR([Cannot continue.])])
           opal_common_hwloc_support_value=0
           $2])

    AC_DEFINE_UNQUOTED([OPAL_COMMON_HWLOC_SUPPORT], 
                       [$opal_common_hwloc_support_value],
                       [Whether opal/mca/common/hwloc was built or not])

    OPAL_VAR_SCOPE_POP
])dnl

