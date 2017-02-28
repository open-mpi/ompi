# -*- shell-script -*-
#
# Copyright (c) 2009-2016 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2014-2017 Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
#
# Copyright (c) 2017      Intel, Inc. All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# --------------------------------------------------------------------
AC_DEFUN([OPAL_CHECK_HWLOC],[

    OPAL_VAR_SCOPE_PUSH([opal_hwloc_CPPFLAGS_save opal_hwloc_CFLAGS_save opal_hwloc_LDFLAGS_save opal_hwloc_LIBS_save opal_hwloc_support opal_hwloc_tmp opal_hwloc_lstopo])

    AC_ARG_WITH([hwloc],
                [AC_HELP_STRING([--with-hwloc=DIR],
                                [Where to find hwloc, if not in the standard location])])

    AC_ARG_WITH([hwloc-libdir],
       [AC_HELP_STRING([--with-hwloc-libdir=DIR],
             [Search for hwloc libraries in DIR])])

    # look for support unless expressly told not to
    AC_MSG_CHECKING([if have hwloc support])
    AS_IF([test "$with_hwloc" = "no"],
          [AC_MSG_RESULT([no])
           AC_MSG_WARN([HWLOC support is required])
           AC_MSG_WARN([Please install a recent version (at least 1.5)])
           AC_MSG_ERROR([Cannot continue])],
          [AC_MSG_RESULT([yes])
           OPAL_CHECK_WITHDIR([hwloc-libdir], [$with_hwloc_libdir],
                              [libhwloc.*])

           AC_MSG_CHECKING([looking for hwloc in])
           AS_IF([test ! -z "$with_hwloc" && \
                  test "$with_hwloc" != "yes"],
                 [opal_hwloc_dir=$with_hwloc
                  AC_MSG_RESULT([($opal_hwloc_dir)])],
                 [AC_MSG_RESULT([(default search paths)])])
           AS_IF([test ! -z "$with_hwloc_libdir" && \
                  test "$with_hwloc_libdir" != "yes"],
                 [opal_hwloc_libdir=$with_hwloc_libdir])

           opal_hwloc_CPPFLAGS_save=$CPPFLAGS
           opal_hwloc_CFLAGS_save=$CFLAGS
           opal_hwloc_LDFLAGS_save=$LDFLAGS
           opal_hwloc_LIBS_save=$LIBS

           OPAL_CHECK_PACKAGE([opal_hwloc],
                              [hwloc.h],
                              [hwloc],
                              [hwloc_topology_init],
                              [],
                              [$opal_hwloc_dir],
                              [$opal_hwloc_libdir],
                              [opal_hwloc_support=yes],
                              [opal_hwloc_support=no])

           CPPFLAGS=$opal_hwloc_CPPFLAGS_save
           CFLAGS=$opal_hwloc_CFLAGS_save
           LDFLAGS=$opal_hwloc_LDFLAGS_save
           LIBS=$opal_hwloc_LIBS_save
          ])

    # Done!
    AC_MSG_CHECKING([was hwloc support found])
    AS_IF([test "$opal_hwloc_support" != "yes"],
          [AC_MSG_RESULT([no])
           AC_MSG_WARN([HWLOC support was not found, but is required])
           AC_MSG_WARN([Please install a recent version (at least 1.5)])
           AC_MSG_ERROR([Cannot continue])],
          [AC_MSG_RESULT([yes])
           # See if hwloc supports XML
           AC_MSG_CHECKING([if hwloc supports XML])
           AS_IF([test "$opal_hwloc_dir" != ""],
                 [opal_hwloc_lstopo="$opal_hwloc_dir/bin/lstopo"],
                 [OPAL_WHICH(lstopo, opal_hwloc_lstopo)])
           opal_hwloc_tmp=`$opal_hwloc_lstopo --help | $GREP "Supported output file formats" | grep xml`
           AS_IF([test "$opal_hwloc_tmp" = ""],
                 [opal_hwloc_enable_xml=0
                  AC_MSG_RESULT([no])],
                 [opal_hwloc_enable_xml=1
                  AC_MSG_RESULT([yes])])

           AC_CHECK_HEADERS([infiniband/verbs.h])

           AC_MSG_CHECKING([if hwloc version is 1.5 or greater])
           AS_IF([test "$opal_hwloc_dir" != ""],
                 [opal_hwloc_external_CFLAGS_save=$CFLAGS
                  CFLAGS="-I$opal_hwloc_dir/include $opal_hwloc_CFLAGS_save"])
           AC_COMPILE_IFELSE(
               [AC_LANG_PROGRAM([[#include <hwloc.h>]],
                   [[
#if HWLOC_API_VERSION < 0x00010500
#error "hwloc API version is less than 0x00010500"
#endif
                   ]])],
               [AC_MSG_RESULT([yes])],
               [AC_MSG_RESULT([no])
                AC_MSG_ERROR([Cannot continue])])
           AC_MSG_CHECKING([if hwloc version is lower than 2.0])
           AS_IF([test "$opal_hwloc_dir" != ""],
                 [opal_hwloc_CFLAGS_save=$CFLAGS
                  CFLAGS="-I$opal_hwloc_dir/include $opal_hwloc_CFLAGS_save"])
           AC_COMPILE_IFELSE(
               [AC_LANG_PROGRAM([[#include <hwloc.h>]],
                   [[
#if HWLOC_API_VERSION >= 0x00020000
#error "hwloc API version is greater or equal than 0x00020000"
#endif
                   ]])],
               [AC_MSG_RESULT([yes])],
               [AC_MSG_RESULT([no])
                AC_MSG_ERROR([OMPI does not currently support hwloc v2 API
Cannot continue])])
           AC_CHECK_DECLS([HWLOC_OBJ_OSDEV_COPROC])
           AS_IF([test "$opal_hwloc_dir" != ""],
                 [CFLAGS=$opal_hwloc_CFLAGS_save])

           # These flags need to get passed to the wrapper compilers

           # Finally, add some flags to the wrapper compiler if we're
           # building with developer headers so that our headers can
           # be found.
           hwloc_WRAPPER_EXTRA_CPPFLAGS=$opal_hwloc_CPPFLAGS
           hwloc_WRAPPER_EXTRA_LDFLAGS=$opal_hwloc_LDFLAGS
           hwloc_WRAPPER_EXTRA_LIBS=$opal_hwloc_LIBS

           # Add some stuff to CPPFLAGS so that the rest of the source
           # tree can be built
           CPPFLAGS="$CPPFLAGS $opal_hwloc_CPPFLAGS"
           LDFLAGS="$LDFLAGS $opal_hwloc_LDFLAGS"
           LIBS="$LIBS $opal_hwloc_LIBS"
           AS_IF([test "$OPAL_TOP_BUILDDIR" != "$OPAL_TOP_SRCDIR"],
                 [CPPFLAGS="$CPPFLAGS -I$OPAL_TOP_BUILDDIR/opal/hwloc"])

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
                 opal_hwloc_openfabrics_include="$opal_hwloc_dir/include/hwloc/openfabrics-verbs.h"],
                [opal_hwloc_include="hwloc.h"
                 opal_hwloc_openfabrics_include="hwloc/openfabrics-verbs.h"])
          AC_DEFINE_UNQUOTED(hwloc_external_header,
                 ["$opal_hwloc_include"],
                 [Location of external hwloc header])
          AC_DEFINE_UNQUOTED(hwloc_external_openfabrics_header,
                 ["$opal_hwloc_openfabrics_include"],
                 [Location of external hwloc OpenFabrics header])])

    OPAL_SUMMARY_ADD([[Miscellaneous]],[[HWLOC support]],[opal_hwloc], [$opal_hwloc_support])

    OPAL_VAR_SCOPE_POP
])dnl
