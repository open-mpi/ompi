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
AC_DEFUN([MCA_opal_hwloc_hwloc151_PRIORITY], [75])

#
# Force this component to compile in static-only mode
#
AC_DEFUN([MCA_opal_hwloc_hwloc151_COMPILE_MODE], [
    AC_MSG_CHECKING([for MCA component $2:$3 compile mode])
    $4="static"
    AC_MSG_RESULT([$$4])
])

# Include hwloc m4 files
m4_include(opal/mca/hwloc/hwloc151/hwloc/config/hwloc.m4)
m4_include(opal/mca/hwloc/hwloc151/hwloc/config/hwloc_pkg.m4)
m4_include(opal/mca/hwloc/hwloc151/hwloc/config/hwloc_check_attributes.m4)
m4_include(opal/mca/hwloc/hwloc151/hwloc/config/hwloc_check_visibility.m4)
m4_include(opal/mca/hwloc/hwloc151/hwloc/config/hwloc_check_vendor.m4)

# MCA_hwloc_hwloc151_POST_CONFIG()
# ---------------------------------
AC_DEFUN([MCA_opal_hwloc_hwloc151_POST_CONFIG],[
    OPAL_VAR_SCOPE_PUSH([opal_hwloc_hwloc151_basedir])

    # If we won, then do all the rest of the setup
    AS_IF([test "$1" = "1"], 
          [
           # Set this variable so that the framework m4 knows what
           # file to include in opal/mca/hwloc/hwloc.h
           opal_hwloc_hwloc151_basedir=opal/mca/hwloc/hwloc151
           opal_hwloc_base_include="$opal_hwloc_hwloc151_basedir/hwloc151.h"

           # Add some stuff to CPPFLAGS so that the rest of the source
           # tree can be built
           file=$opal_hwloc_hwloc151_basedir/hwloc
           CPPFLAGS="$CPPFLAGS -I$OMPI_TOP_SRCDIR/$file/include"
           AS_IF([test "$OMPI_TOP_BUILDDIR" != "$OMPI_TOP_SRCDIR"],
                 [$CPPFLAGS="$CPPFLAGS -I$OMPI_TOP_BUILDDIR/$file/include"])
           unset file

           # Finally, add some flags to the wrapper compiler if we're
           # building with developer headers so that our headers can
           # be found.
           AS_IF([test "$with_devel_headers" = "yes"],
               [OPAL_WRAPPER_EXTRA_CPPFLAGS="$OPAL_WRAPPER_EXTRA_CPPFLAGS "'-I${includedir}/openmpi/'"$opal_hwloc_hwloc151_basedir/hwloc/include"])

           OPAL_WRAPPER_EXTRA_LIBS="$OPAL_WRAPPER_EXTRA_LIBS $HWLOC_EMBEDDED_LIBS"
           HWLOC_DO_AM_CONDITIONALS
          ])
    OPAL_VAR_SCOPE_POP
])dnl


# MCA_hwloc_hwloc151_CONFIG([action-if-found], [action-if-not-found])
# --------------------------------------------------------------------
AC_DEFUN([MCA_opal_hwloc_hwloc151_CONFIG],[
    # Hwloc needs to know if we have Verbs support
    AC_REQUIRE([OPAL_CHECK_VERBS_DIR])

    AC_CONFIG_FILES([opal/mca/hwloc/hwloc151/Makefile])

    OPAL_VAR_SCOPE_PUSH([HWLOC_VERSION opal_hwloc_hwloc151_save_CPPFLAGS opal_hwloc_hwloc151_save_LDFLAGS opal_hwloc_hwloc151_save_LIBS opal_hwloc_hwloc151_save_cairo opal_hwloc_hwloc151_save_xml opal_hwloc_hwloc151_basedir opal_hwloc_hwloc151_file opal_hwloc_hwloc151_save_cflags])

    # default to this component not providing support
    opal_hwloc_hwloc151_basedir=opal/mca/hwloc/hwloc151
    opal_hwloc_hwloc151_support=no

    if test "$with_hwloc" = "internal" -o "$with_hwloc" = "" -o "$with_hwloc" = "yes"; then
        opal_hwloc_hwloc151_save_CPPFLAGS=$CPPFLAGS
        opal_hwloc_hwloc151_save_LDFLAGS=$LDFLAGS
        opal_hwloc_hwloc151_save_LIBS=$LIBS

        # Run the hwloc configuration - set the prefix to minimize
        # the chance that someone will use the internal symbols
        HWLOC_SET_SYMBOL_PREFIX([opal_hwloc151_])

        # save XML or graphical options
        opal_hwloc_hwloc151_save_cairo=$enable_cairo
        opal_hwloc_hwloc151_save_xml=$enable_xml

        # never enable hwloc's graphical option
        enable_cairo=no

        # Override -- disable hwloc's libxml2 support, but enable the
        # native hwloc XML support
        enable_libxml2=no
        enable_xml=yes

        # hwloc checks for compiler visibility, and its needs to do
        # this without "picky" flags.
        opal_hwloc_hwloc151_save_cflags=$CFLAGS
        CFLAGS=$OMPI_CFLAGS_BEFORE_PICKY
        HWLOC_SETUP_CORE([opal/mca/hwloc/hwloc151/hwloc], 
                  [AC_MSG_CHECKING([whether hwloc configure succeeded])
                   AC_MSG_RESULT([yes])
                   HWLOC_VERSION="internal v`$srcdir/$opal_hwloc_hwloc151_basedir/hwloc/config/hwloc_get_version.sh $srcdir/$opal_hwloc_hwloc151_basedir/hwloc/VERSION`"

                   # Build flags for our Makefile.am
                   opal_hwloc_hwloc151_LDFLAGS='$(HWLOC_EMBEDDED_LDFLAGS)'
                   opal_hwloc_hwloc151_LIBS='$(top_ompi_builddir)/'"$opal_hwloc_hwloc151_basedir"'/hwloc/src/libhwloc_embedded.la $(HWLOC_EMBEDDED_LIBS)'
                   opal_hwloc_hwloc151_support=yes

                   AC_DEFINE_UNQUOTED([HWLOC_HWLOC151_HWLOC_VERSION], 
                       ["$HWLOC_VERSION"], 
                       [Version of hwloc])

                   # Do we have verbs support?
                   CPPFLAGS_save=$CPPFLAGS
                   AS_IF([test "$opal_want_verbs" = "yes"],
                         [CPPFLAGS="-I$opal_verbs_dir/include $CPPFLAGS"])
                   AC_CHECK_HEADERS([infiniband/verbs.h])
                   CPPFLAGS=$CPPFLAGS_save
                  ],
                  [AC_MSG_CHECKING([whether hwloc configure succeeded])
                   AC_MSG_RESULT([no])
                   opal_hwloc_hwloc151_support=no])
        CFLAGS=$opal_hwloc_hwloc151_save_cflags

        # Restore some env variables, if necessary
        AS_IF([test -n "$opal_hwloc_hwloc151_save_cairo"],
              [enable_cairo=$opal_hwloc_hwloc151_save_cairo])
        AS_IF([test -n "$opal_hwloc_hwloc151_save_xml"],
              [enable_xml=$opal_hwloc_hwloc151_save_xml])
       
        CPPFLAGS=$opal_hwloc_hwloc151_save_CPPFLAGS
        LDFLAGS=$opal_hwloc_hwloc151_save_LDFLAGS
        LIBS=$opal_hwloc_hwloc151_save_LIBS

        AC_SUBST([opal_hwloc_hwloc151_CFLAGS])
        AC_SUBST([opal_hwloc_hwloc151_CPPFLAGS])
        AC_SUBST([opal_hwloc_hwloc151_LDFLAGS])
        AC_SUBST([opal_hwloc_hwloc151_LIBS])
    fi

    # Done!
    AS_IF([test "$opal_hwloc_hwloc151_support" = "yes"],
          [$1],
          [$2])

    OPAL_VAR_SCOPE_POP
])dnl
