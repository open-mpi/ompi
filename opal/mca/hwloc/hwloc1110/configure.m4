# -*- shell-script -*-
#
# Copyright (c) 2009-2014 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2014-2015 Intel, Inc. All rights reserved.
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
AC_DEFUN([MCA_opal_hwloc_hwloc1110_PRIORITY], [90])

#
# Force this component to compile in static-only mode
#
AC_DEFUN([MCA_opal_hwloc_hwloc1110_COMPILE_MODE], [
    AC_MSG_CHECKING([for MCA component $2:$3 compile mode])
    $4="static"
    AC_MSG_RESULT([$$4])
])

# Include hwloc m4 files
m4_include(opal/mca/hwloc/hwloc1110/hwloc/config/hwloc.m4)
m4_include(opal/mca/hwloc/hwloc1110/hwloc/config/hwloc_pkg.m4)
m4_include(opal/mca/hwloc/hwloc1110/hwloc/config/hwloc_check_attributes.m4)
m4_include(opal/mca/hwloc/hwloc1110/hwloc/config/hwloc_check_visibility.m4)
m4_include(opal/mca/hwloc/hwloc1110/hwloc/config/hwloc_check_vendor.m4)
m4_include(opal/mca/hwloc/hwloc1110/hwloc/config/hwloc_components.m4)

# MCA_hwloc_hwloc1110_POST_CONFIG()
# ---------------------------------
AC_DEFUN([MCA_opal_hwloc_hwloc1110_POST_CONFIG],[
    OPAL_VAR_SCOPE_PUSH([opal_hwloc_hwloc1110_basedir])

    # If we won, then do all the rest of the setup
    AS_IF([test "$1" = "1" && test "$opal_hwloc_hwloc1110_support" = "yes"],
          [
           # Set this variable so that the framework m4 knows what
           # file to include in opal/mca/hwloc/hwloc.h
           opal_hwloc_hwloc1110_basedir=opal/mca/hwloc/hwloc1110
           opal_hwloc_base_include="$opal_hwloc_hwloc1110_basedir/hwloc1110.h"

           # Add some stuff to CPPFLAGS so that the rest of the source
           # tree can be built
           file=$opal_hwloc_hwloc1110_basedir/hwloc
           CPPFLAGS="$CPPFLAGS -I$OPAL_TOP_SRCDIR/$file/include"
           AS_IF([test "$OPAL_TOP_BUILDDIR" != "$OPAL_TOP_SRCDIR"],
                 [CPPFLAGS="$CPPFLAGS -I$OPAL_TOP_BUILDDIR/$file/include"])
           unset file
          ])
    OPAL_VAR_SCOPE_POP

    # This must be run unconditionally
    HWLOC_DO_AM_CONDITIONALS
])dnl


# MCA_hwloc_hwloc1110_CONFIG([action-if-found], [action-if-not-found])
# --------------------------------------------------------------------
AC_DEFUN([MCA_opal_hwloc_hwloc1110_CONFIG],[
    # Hwloc needs to know if we have Verbs support
    AC_REQUIRE([OPAL_CHECK_VERBS_DIR])

    AC_CONFIG_FILES([opal/mca/hwloc/hwloc1110/Makefile])

    OPAL_VAR_SCOPE_PUSH([HWLOC_VERSION opal_hwloc_hwloc1110_save_CPPFLAGS opal_hwloc_hwloc1110_save_LDFLAGS opal_hwloc_hwloc1110_save_LIBS opal_hwloc_hwloc1110_save_cairo opal_hwloc_hwloc1110_save_xml opal_hwloc_hwloc1110_basedir opal_hwloc_hwloc1110_file opal_hwloc_hwloc1110_save_cflags CPPFLAGS_save LIBS_save])

    # default to this component not providing support
    opal_hwloc_hwloc1110_basedir=opal/mca/hwloc/hwloc1110
    opal_hwloc_hwloc1110_support=no

    if test "$with_hwloc" = "internal" -o "$with_hwloc" = "" -o "$with_hwloc" = "yes"; then
        opal_hwloc_hwloc1110_save_CPPFLAGS=$CPPFLAGS
        opal_hwloc_hwloc1110_save_LDFLAGS=$LDFLAGS
        opal_hwloc_hwloc1110_save_LIBS=$LIBS

        # Run the hwloc configuration - set the prefix to minimize
        # the chance that someone will use the internal symbols
        HWLOC_SET_SYMBOL_PREFIX([opal_hwloc1110_])

        # save XML or graphical options
        opal_hwloc_hwloc1110_save_cairo=$enable_cairo
        opal_hwloc_hwloc1110_save_xml=$enable_xml
        opal_hwloc_hwloc1110_save_static=$enable_static
        opal_hwloc_hwloc1110_save_shared=$enable_shared
        opal_hwloc_hwloc1110_save_plugins=$enable_plugins

        # never enable hwloc's graphical option
        enable_cairo=no

        # never enable hwloc's plugin system
        enable_plugins=no
        enable_static=yes
        enable_shared=no

        # Override -- disable hwloc's libxml2 support, but enable the
        # native hwloc XML support
        enable_libxml2=no
        enable_xml=yes

        # hwloc checks for compiler visibility, and its needs to do
        # this without "picky" flags.
        opal_hwloc_hwloc1110_save_cflags=$CFLAGS
        CFLAGS=$OPAL_CFLAGS_BEFORE_PICKY
        HWLOC_SETUP_CORE([opal/mca/hwloc/hwloc1110/hwloc],
                  [AC_MSG_CHECKING([whether hwloc configure succeeded])
                   AC_MSG_RESULT([yes])
                   HWLOC_VERSION="internal v`$srcdir/$opal_hwloc_hwloc1110_basedir/hwloc/config/hwloc_get_version.sh $srcdir/$opal_hwloc_hwloc1110_basedir/hwloc/VERSION`"

                   # Build flags for our Makefile.am
                   opal_hwloc_hwloc1110_LDFLAGS='$(HWLOC_EMBEDDED_LDFLAGS)'
                   opal_hwloc_hwloc1110_LIBS='$(OPAL_TOP_BUILDDIR)/'"$opal_hwloc_hwloc1110_basedir"'/hwloc/src/libhwloc_embedded.la $(HWLOC_EMBEDDED_LIBS)'
                   opal_hwloc_hwloc1110_support=yes

                   AC_DEFINE_UNQUOTED([HWLOC_HWLOC1110_HWLOC_VERSION],
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
                   opal_hwloc_hwloc1110_support=no])
        CFLAGS=$opal_hwloc_hwloc1110_save_cflags

        # Restore some env variables, if necessary
        AS_IF([test -n "$opal_hwloc_hwloc1110_save_cairo"],
              [enable_cairo=$opal_hwloc_hwloc1110_save_cairo])
        AS_IF([test -n "$opal_hwloc_hwloc1110_save_xml"],
              [enable_xml=$opal_hwloc_hwloc1110_save_xml])
        AS_IF([test -n "$opal_hwloc_hwloc1110_save_static"],
              [enable_static=$opal_hwloc_hwloc1110_save_static])
        AS_IF([test -n "$opal_hwloc_hwloc1110_save_shared"],
              [enable_shared=$opal_hwloc_hwloc1110_save_shared])
        AS_IF([test -n "$opal_hwloc_hwloc1110_save_plugins"],
              [enable_plugins=$opal_hwloc_hwloc1110_save_shared])

        CPPFLAGS=$opal_hwloc_hwloc1110_save_CPPFLAGS
        LDFLAGS=$opal_hwloc_hwloc1110_save_LDFLAGS
        LIBS=$opal_hwloc_hwloc1110_save_LIBS

        AC_SUBST([opal_hwloc_hwloc1110_CFLAGS])
        AC_SUBST([opal_hwloc_hwloc1110_CPPFLAGS])
        AC_SUBST([opal_hwloc_hwloc1110_LDFLAGS])
        AC_SUBST([opal_hwloc_hwloc1110_LIBS])

        # Finally, add some flags to the wrapper compiler so that our
        # headers can be found.
        hwloc_hwloc1110_WRAPPER_EXTRA_LDFLAGS="$HWLOC_EMBEDDED_LDFLAGS"
        hwloc_hwloc1110_WRAPPER_EXTRA_LIBS="$HWLOC_EMBEDDED_LIBS"
        hwloc_hwloc1110_WRAPPER_EXTRA_CPPFLAGS='-I${includedir}/openmpi/'"$opal_hwloc_hwloc1110_basedir/hwloc/include"
    fi

    # Done!
    AS_IF([test "$opal_hwloc_hwloc1110_support" = "yes"],
          [$1],
          [$2])

    OPAL_VAR_SCOPE_POP
])dnl
