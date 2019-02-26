# -*- shell-script -*-
#
# Copyright (c) 2009-2017 Cisco Systems, Inc.  All rights reserved
# Copyright (c) 2014-2018 Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
#
# Copyright (c) 2018      Intel, Inc. All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

#
# Priority
#
AC_DEFUN([MCA_opal_hwloc_external_PRIORITY], [90])

#
# Force this component to compile in static-only mode
#
AC_DEFUN([MCA_opal_hwloc_external_COMPILE_MODE], [
    AC_MSG_CHECKING([for MCA component $2:$3 compile mode])
    $4="static"
    AC_MSG_RESULT([$$4])
])


# MCA_hwloc_external_POST_CONFIG()
# ---------------------------------
AC_DEFUN([MCA_opal_hwloc_external_POST_CONFIG],[
    OPAL_VAR_SCOPE_PUSH([opal_hwloc_external_basedir])

    # If we won, then do all the rest of the setup
    AS_IF([test "$1" = "1"],
          [AC_DEFINE_UNQUOTED([HWLOC_EXTERNAL_HWLOC_VERSION],
                              [external],
                              [Version of hwloc])

           # Set this variable so that the framework m4 knows what
           # file to include in opal/mca/hwloc/hwloc-internal.h
           opal_hwloc_external_basedir=opal/mca/hwloc/external
           opal_hwloc_base_include="$opal_hwloc_external_basedir/external.h"

           # Add some stuff to CPPFLAGS so that the rest of the source
           # tree can be built
           CPPFLAGS="$CPPFLAGS $opal_hwloc_external_CPPFLAGS"
           LDFLAGS="$LDFLAGS $opal_hwloc_external_LDFLAGS"
           LIBS="$LIBS $opal_hwloc_external_LIBS"

           # We have to do some extra indirection to get the
           # OPAL_HWLOC_WANT_VERBS_HELPER to work.  First, the
           # opal_hwloc_external_include file (set above), points to a
           # file here in this component. That file will include the
           # actual external hwloc.h file (via the
           # MCA_hwloc_external_header define).  And if
           # OPAL_HWLOC_WANT_VERBS_HELPER is set, that file will
           # include the external hwloc/openfabrics-verbs.h file (via
           # the MCA_hwloc_external_openfabrics_helper define).
           AS_IF([test "$opal_hwloc_dir" != ""],
                 [opal_hwloc_include="$opal_hwloc_dir/include/hwloc.h"
                  opal_hwloc_shmem_include="$opal_hwloc_dir/include/hwloc/shmem.h"
                  opal_hwloc_openfabrics_include="$opal_hwloc_dir/include/hwloc/openfabrics-verbs.h"],
                 [opal_hwloc_include="hwloc.h"
                  opal_hwloc_shmem_include="hwloc/shmem.h"
                  opal_hwloc_openfabrics_include="hwloc/openfabrics-verbs.h"])
           AC_DEFINE_UNQUOTED(MCA_hwloc_external_header,
                  ["$opal_hwloc_include"],
                  [Location of external hwloc header])
           AC_DEFINE_UNQUOTED(MCA_hwloc_external_shmem_header,
                  ["$opal_hwloc_shmem_include"],
                  [Location of external hwloc shmem header])
           AC_DEFINE_UNQUOTED(MCA_hwloc_external_openfabrics_header,
                  ["$opal_hwloc_openfabrics_include"],
                  [Location of external hwloc OpenFabrics header])
          ])
    OPAL_VAR_SCOPE_POP
])dnl


# MCA_hwloc_external_CONFIG([action-if-found], [action-if-not-found])
# --------------------------------------------------------------------
AC_DEFUN([MCA_opal_hwloc_external_CONFIG],[
    AC_CONFIG_FILES([opal/mca/hwloc/external/Makefile])

    OPAL_VAR_SCOPE_PUSH([opal_hwloc_external_CPPFLAGS_save opal_hwloc_external_CFLAGS_save opal_hwloc_external_LDFLAGS_save opal_hwloc_external_LIBS_save opal_hwloc_external_tmp opal_hwloc_external_lstopo opal_hwloc_summary_msg])

    AC_ARG_WITH([hwloc-libdir],
       [AC_HELP_STRING([--with-hwloc-libdir=DIR],
             [Search for hwloc libraries in DIR.  Should only be used if an external copy of hwloc is being used.])])

    # Make sure the user didn't specify --with-hwloc=internal and
    # --with-hwloc-libdir=whatever.
    AS_IF([test "$with_hwloc" = "internal" && \
           test "$with_hwloc_libdir" != ""],
          [AC_MSG_WARN([Both --with-hwloc=internal and --with-hwloc-libdir=DIR])
           AC_MSG_WARN([were specified, which does not make sense.])
           AC_MSG_ERROR([Cannot continue])])

    opal_hwloc_summary_msg="internal"
    # Try external support if needed
    AS_IF([test "$with_hwloc" != "internal"],
          [OPAL_CHECK_WITHDIR([hwloc-libdir], [$with_hwloc_libdir],
                              [libhwloc.*])

           AC_MSG_CHECKING([looking for external hwloc in])
           AS_IF([test "$with_hwloc" != "external" && \
                  test "$with_hwloc" != "yes"],
                 [opal_hwloc_dir=$with_hwloc
                  AC_MSG_RESULT([($opal_hwloc_dir)])],
                 [AC_MSG_RESULT([(default search paths)])])
           AS_IF([test ! -z "$with_hwloc_libdir" && \
                  test "$with_hwloc_libdir" != "yes"],
                 [opal_hwloc_libdir=$with_hwloc_libdir])

           opal_hwloc_external_CPPFLAGS_save=$CPPFLAGS
           opal_hwloc_external_CFLAGS_save=$CFLAGS
           opal_hwloc_external_LDFLAGS_save=$LDFLAGS
           opal_hwloc_external_LIBS_save=$LIBS

           OPAL_CHECK_PACKAGE([opal_hwloc_external],
                              [hwloc.h],
                              [hwloc],
                              [hwloc_topology_init],
                              [],
                              [$opal_hwloc_dir],
                              [$opal_hwloc_libdir],
                              [opal_hwloc_external_support=yes],
                              [opal_hwloc_external_support=no])

           AS_IF([test "$opal_hwloc_external_support" = "yes"],
                 [CPPFLAGS="$CPPFLAGS $opal_hwloc_external_CPPFLAGS"
                  LDFLAGS="$LDFLAGS $opal_hwloc_external_LDFLAGS"
                  LIBS="$LIBS $opal_hwloc_external_LIBS"

                  AC_MSG_CHECKING([if external hwloc version is 1.5 or greater])
                  AC_COMPILE_IFELSE(
                      [AC_LANG_PROGRAM([[#include <hwloc.h>]],
                          [[
#if HWLOC_API_VERSION < 0x00010500
#error "hwloc API version is less than 0x00010500"
#endif
                          ]])],
                      [AC_MSG_RESULT([yes])],
                      [AC_MSG_RESULT([no])
                       opal_hwloc_external_support=no])])

           # If external hwloc is not explicitly requested, check external version
           # is not lower than the internal one
           AS_IF([test "$opal_hwloc_external_support" = "yes"],
                 [AS_IF([test -z "$with_hwloc" || test "$with_hwloc" = "yes"],
                        [AC_MSG_CHECKING([if external hwloc version is 2.0 or greater])
                         AC_COMPILE_IFELSE(
                             [AC_LANG_PROGRAM([[#include <hwloc.h>]],
                                 [[
#if HWLOC_API_VERSION < 0x00020000
#error "hwloc API version is less than 0x00020000"
#endif
                                 ]])],
                             [AC_MSG_RESULT([yes])],
                             [AC_MSG_RESULT([no])
                              opal_hwloc_summary_msg="internal (external hlwoc version is less than internal version 2.0)"
                              AC_MSG_WARN([external hwloc version is less than internal version 2.0])
                              AC_MSG_WARN([using internal hwloc])
                              opal_hwloc_external_support=no])])])

           AS_IF([test "$opal_hwloc_external_support" = "yes"],
                 [AC_DEFINE_UNQUOTED([HWLOC_EXTERNAL_HWLOC_VERSION],
                                     [external],
                                     [Version of hwloc])

                  AC_CHECK_DECLS([HWLOC_OBJ_OSDEV_COPROC], [], [], [#include <hwloc.h>])
                  AC_CHECK_FUNCS([hwloc_topology_dup])

                  # See if the external hwloc supports XML
                  AC_MSG_CHECKING([if external hwloc supports XML])
                  AS_IF([test "$opal_hwloc_dir" != ""],
                        [opal_hwloc_external_lstopo="$opal_hwloc_dir/bin/lstopo"],
                        [OPAL_WHICH(lstopo, opal_hwloc_external_lstopo)])
                  opal_hwloc_external_tmp=`$opal_hwloc_external_lstopo --help | $GREP "Supported output file formats" | grep xml`
                  AS_IF([test "$opal_hwloc_external_tmp" = ""],
                        [opal_hwloc_external_enable_xml=0
                         AC_MSG_RESULT([no])],
                        [opal_hwloc_external_enable_xml=1
                         AC_MSG_RESULT([yes])])

                  AC_CHECK_HEADERS([infiniband/verbs.h])

                  # These flags need to get passed to the wrapper compilers
                  # (this is unnecessary for the internal/embedded hwloc)

                  # Finally, add some flags to the wrapper compiler if we're
                  # building with developer headers so that our headers can
                  # be found.
                  hwloc_external_WRAPPER_EXTRA_CPPFLAGS=$opal_hwloc_external_CPPFLAGS
                  hwloc_external_WRAPPER_EXTRA_LDFLAGS=$opal_hwloc_external_LDFLAGS
                  hwloc_external_WRAPPER_EXTRA_LIBS=$opal_hwloc_external_LIBS])

            CPPFLAGS=$opal_hwloc_external_CPPFLAGS_save
            CFLAGS=$opal_hwloc_external_CFLAGS_save
            LDFLAGS=$opal_hwloc_external_LDFLAGS_save
            LIBS=$opal_hwloc_external_LIBS_save
          ])

    # Done!
    AS_IF([test "$opal_hwloc_external_support" = "yes"],
          [opal_hwloc_summary_msg="external"
           $1],
          [# Abort is external hwloc was explicitly requested but cannot be built
           AS_IF([test "$with_hwloc" != internal &&
                  test -n "$with_hwloc"],
                 [AC_MSG_WARN([external hwloc cannot be built])
                  AC_MSG_ERROR([Cannot continue.])])
           $2])

    OPAL_SUMMARY_ADD([[Miscellaneous]],[[HWLOC support]], [], [$opal_hwloc_summary_msg])

    AC_SUBST(opal_hwloc_external_LDFLAGS)
    AC_SUBST(opal_hwloc_external_LIBS)

    OPAL_VAR_SCOPE_POP
])dnl
