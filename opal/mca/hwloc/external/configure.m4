# -*- shell-script -*-
#
# Copyright (c) 2009-2013 Cisco Systems, Inc.  All rights reserved. 
#
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
           # file to include in opal/mca/hwloc/hwloc.h
           opal_hwloc_external_basedir=opal/mca/hwloc/external
           opal_hwloc_base_include="$opal_hwloc_external_basedir/external.h"

           # Add some stuff to CPPFLAGS so that the rest of the source
           # tree can be built
           file=$opal_hwloc_external_basedir/hwloc
           CPPFLAGS="$CPPFLAGS $opal_hwloc_external_CPPFLAGS"
           LDFLAGS="$LDFLAGS $opal_hwloc_external_LDFLAGS"
           LIBS="$LIBS $opal_hwloc_external_LIBS"
           AS_IF([test "$OMPI_TOP_BUILDDIR" != "$OMPI_TOP_SRCDIR"],
                 [CPPFLAGS="$CPPFLAGS -I$OMPI_TOP_BUILDDIR/$file/include"])
           unset file

           # We have to do some extra indirection to get the
           # OPAL_HWLOC_WANT_VERBS_HELPER to work.  First, the
           # opal_hwloc_external_include file (set above), points to a
           # file here in this component. That file will include the
           # actual external hwloc.h file (via the
           # MCA_hwloc_external_header define).  And if
           # OPAL_HWLOC_WANT_VERBS_HELPER is set, that file will
           # include the external hwloc/openfabrics-verbs.h file (via
           # the MCA_hwloc_external_openfabrics_helper define).
           AC_DEFINE_UNQUOTED(MCA_hwloc_external_header,
                  ["$opal_hwloc_dir/include/hwloc.h"],
                  [Location of external hwloc header])
           AC_DEFINE_UNQUOTED(MCA_hwloc_external_openfabrics_header,
                  ["$opal_hwloc_dir/include/hwloc/openfabrics-verbs.h"],
                  [Location of external hwloc header])

           # These flags need to get passed to the wrapper compilers
           # (this is unnecessary for the internal/embedded hwloc)

           # Finally, add some flags to the wrapper compiler if we're
           # building with developer headers so that our headers can
           # be found.
           hwloc_external_WRAPPER_EXTRA_CPPFLAGS="$opal_hwloc_external_CPPFLAGS"
           hwloc_external_WRAPPER_EXTRA_LDFLAGS="$opal_hwloc_external_LDFLAGS"
           hwloc_external_WRAPPER_EXTRA_LIBS="$opal_hwloc_external_LIBS"
          ])
    OPAL_VAR_SCOPE_POP
])dnl


# MCA_hwloc_external_CONFIG([action-if-found], [action-if-not-found])
# --------------------------------------------------------------------
AC_DEFUN([MCA_opal_hwloc_external_CONFIG],[
    AC_CONFIG_FILES([opal/mca/hwloc/external/Makefile])

    OPAL_VAR_SCOPE_PUSH([opal_hwloc_external_CPPFLAGS_save opal_hwloc_external_CFLAGS_save opal_hwloc_external_LDFLAGS_save opal_hwloc_external_LIBS_save opal_hwloc_external_want opal_hwloc_external_tmp opal_hwloc_external_lstopo])

    AC_ARG_WITH([hwloc-libdir],
       [AC_HELP_STRING([--with-hwloc-libdir=DIR],
             [Search for hwloc libraries in DIR.  Should only be used if an external copy of hwloc is being used.])])

    # Make sure the user didn't specify --with-hwloc=internal and
    # --with-hwloc-libdir=whatever.
    AS_IF([test "$with_hwloc" = "internal" -a "$with_hwloc_libdir" != ""],
          [AC_MSG_WARN([Both --with-hwloc=internal and --with-hwloc-libdir=DIR])
           AC_MSG_WARN([were specified, which does not make sense.])
           AC_MSG_ERROR([Cannot continue])])

    # Do we want this external component? (slightly redundant logic,
    # but hopefully slightly more clear...)
    opal_hwloc_external_want=no
    AS_IF([test "$with_hwloc_libdir" != ""], [opal_hwloc_external_want=yes])
    AS_IF([test "$with_hwloc" = "external"], [opal_hwloc_external_want=yes])
    AS_IF([test "$with_hwloc" != "" -a "$with_hwloc" != "no" -a "$with_hwloc" != "internal"], [opal_hwloc_external_want=yes])
    AS_IF([test "$with_hwloc" = "no"], [opal_hwloc_external_want=no])

    # If we still want external support, try it
    AS_IF([test "$opal_hwloc_external_want" = "yes"],
          [OMPI_CHECK_WITHDIR([hwloc-libdir], [$with_hwloc_libdir], 
                              [libhwloc.*])

           AC_MSG_CHECKING([looking for external hwloc in])
           AS_IF([test "$with_hwloc" != "external" -a "$with_hwloc" != "yes"],
                 [opal_hwloc_dir=$with_hwloc
                  AC_MSG_RESULT([($opal_hwloc_dir)])],
                 [AC_MSG_RESULT([(default search paths)])])
           AS_IF([test ! -z "$with_hwloc_libdir" -a "$with_hwloc_libdir" != "yes"],
                 [opal_hwloc_libdir="$with_hwloc_libdir"])

           opal_hwloc_external_CPPFLAGS_save=$CPPFLAGS
           opal_hwloc_external_CFLAGS_save=$CFLAGS
           opal_hwloc_external_LDFLAGS_save=$LDFLAGS
           opal_hwloc_external_LIBS_save=$LIBS

           OMPI_CHECK_PACKAGE([opal_hwloc_external],
                              [hwloc.h],
                              [hwloc],
                              [hwloc_topology_init],
                              [],
                              [$opal_hwloc_dir],
                              [$opal_hwloc_libdir],
                              [opal_hwloc_external_support=yes],
                              [opal_hwloc_external_support=no])

           CPPFLAGS=$opal_hwloc_external_CPPFLAGS_save
           CFLAGS=$opal_hwloc_external_CFLAGS_save
           LDFLAGS=$opal_hwloc_external_LDFLAGS_save
           LIBS=$opal_hwloc_external_LIBS_save
          ])

    # Done!
    AS_IF([test "$opal_hwloc_external_support" = "yes"],
          [AC_DEFINE_UNQUOTED([HWLOC_EXTERNAL_HWLOC_VERSION], 
                              [external], 
                              [Version of hwloc])

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

           $1],
          [$2])

    AC_SUBST(opal_hwloc_external_LDFLAGS)
    AC_SUBST(opal_hwloc_external_LIBS)

    OPAL_VAR_SCOPE_POP
])dnl
