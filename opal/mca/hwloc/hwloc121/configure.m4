# -*- shell-script -*-
#
# Copyright (c) 2009-2011 Cisco Systems, Inc.  All rights reserved. 
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
AC_DEFUN([MCA_opal_hwloc_hwloc121_PRIORITY], [60])

#
# Force this component to compile in static-only mode
#
AC_DEFUN([MCA_opal_hwloc_hwloc121_COMPILE_MODE], [
    AC_MSG_CHECKING([for MCA component $2:$3 compile mode])
    $4="static"
    AC_MSG_RESULT([$$4])
])

# Include hwloc m4 files
m4_include(opal/mca/hwloc/hwloc121/hwloc/config/hwloc.m4)
m4_include(opal/mca/hwloc/hwloc121/hwloc/config/hwloc_pkg.m4)
m4_include(opal/mca/hwloc/hwloc121/hwloc/config/hwloc_check_attributes.m4)
m4_include(opal/mca/hwloc/hwloc121/hwloc/config/hwloc_check_visibility.m4)

# MCA_hwloc_hwloc121_POST_CONFIG()
# ---------------------------------
AC_DEFUN([MCA_opal_hwloc_hwloc121_POST_CONFIG],[
    HWLOC_DO_AM_CONDITIONALS
])dnl


# MCA_hwloc_hwloc121_CONFIG([action-if-found], [action-if-not-found])
# --------------------------------------------------------------------
AC_DEFUN([MCA_opal_hwloc_hwloc121_CONFIG],[
    AC_CONFIG_FILES([opal/mca/hwloc/hwloc121/Makefile])

    OPAL_VAR_SCOPE_PUSH([HWLOC_VERSION opal_hwloc_hwloc121_save_CPPFLAGS opal_hwloc_hwloc121_save_LDFLAGS opal_hwloc_hwloc121_save_LIBS opal_hwloc_hwloc121_save_cairo opal_hwloc_hwloc121_save_xml opal_hwloc_hwloc121_basedir opal_hwloc_hwloc121_file])

    # default to this component not providing support
    opal_hwloc_hwloc121_basedir=opal/mca/hwloc/hwloc121
    opal_hwloc_hwloc121_support=no

    if test "$with_hwloc" = "internal" -o "$with_hwloc" = "" -o "$with_hwloc" = "yes"; then
        opal_hwloc_hwloc121_save_CPPFLAGS=$CPPFLAGS
        opal_hwloc_hwloc121_save_LDFLAGS=$LDFLAGS
        opal_hwloc_hwloc121_save_LIBS=$LIBS

        # Run the hwloc configuration - set the prefix to minimize
        # the chance that someone will use the internal symbols
        HWLOC_SET_SYMBOL_PREFIX([opal_hwloc121_])

        # save XML or graphical options
        opal_hwloc_hwloc121_save_cairo=$enable_cairo
        opal_hwloc_hwloc121_save_xml=$enable_xml

        # never enable hwloc's graphical option
        enable_cairo=no

        # check for xml option
        AC_ARG_ENABLE(hwloc-xml,
            AC_HELP_STRING([--enable-hwloc-xml],
                [enable xml support for hwloc (experimental)]))
        if test "$enable_hwloc_xml" = "yes"; then
            enable_xml=yes
            opal_hwloc_hwloc121_enable_xml=1
        else
            enable_xml=no
            opal_hwloc_hwloc121_enable_xml=0
        fi

        HWLOC_SETUP_CORE([opal/mca/hwloc/hwloc121/hwloc], 
                  [AC_MSG_CHECKING([whether hwloc configure succeeded])
                   AC_MSG_RESULT([yes])
                   HWLOC_VERSION="internal v`$srcdir/$opal_hwloc_hwloc121_basedir/hwloc/config/hwloc_get_version.sh $srcdir/$opal_hwloc_hwloc121_basedir/hwloc/VERSION`"

                   # Build flags for our Makefile.am
                   opal_hwloc_hwloc121_LDFLAGS='$(HWLOC_EMBEDDED_LDFLAGS)'
                   opal_hwloc_hwloc121_LIBS='$(top_ompi_builddir)/'"$opal_hwloc_hwloc121_basedir"'/hwloc/src/libhwloc_embedded.la $(HWLOC_EMBEDDED_LIBS)'
                   opal_hwloc_hwloc121_support=yes], 
                  [AC_MSG_CHECKING([whether hwloc configure succeeded])
                   AC_MSG_RESULT([no])
                   opal_hwloc_hwloc121_support=no])

        # Restore some env variables, if necessary
        AS_IF([test -n "$opal_hwloc_hwloc121_save_cairo"],
              [enable_cairo=$opal_hwloc_hwloc121_save_cairo])
        AS_IF([test -n "$opal_hwloc_hwloc121_save_xml"],
              [enable_xml=$opal_hwloc_hwloc121_save_xml])
       
        CPPFLAGS=$opal_hwloc_hwloc121_save_CPPFLAGS
        LDFLAGS=$opal_hwloc_hwloc121_save_LDFLAGS
        LIBS=$opal_hwloc_hwloc121_save_LIBS

        AC_SUBST([opal_hwloc_hwloc121_CFLAGS])
        AC_SUBST([opal_hwloc_hwloc121_CPPFLAGS])
        AC_SUBST([opal_hwloc_hwloc121_LDFLAGS])
        AC_SUBST([opal_hwloc_hwloc121_LIBS])
    fi

    # Done!
    AS_IF([test "$opal_hwloc_hwloc121_support" = "yes"],
          [AC_DEFINE_UNQUOTED([HWLOC_HWLOC121_HWLOC_VERSION], 
                              ["$HWLOC_VERSION"], 
                              [Version of hwloc])

           # Set these variables so that the framework m4 knows
           # what file to include in opal/mca/hwloc/hwloc.h
           opal_hwloc_hwloc121_include="$opal_hwloc_hwloc121_basedir/hwloc121.h"

           # Also pass some *_ADD_* flags upwards to the framework m4
           # for various compile/link flags that are needed a) to
           # build the rest of the source tree, and b) for the wrapper
           # compilers (in the --with-devel-headers case).
           opal_hwloc_hwloc121_file=$opal_hwloc_hwloc121_basedir/hwloc
           opal_hwloc_hwloc121_ADD_CPPFLAGS="-I$OMPI_TOP_SRCDIR/$opal_hwloc_hwloc121_file/include"
           AS_IF([test "$OMPI_TOP_BUILDDIR" != "$OMPI_TOP_SRCDIR"],
                 [opal_hwloc_hwloc121_ADD_CPPFLAGS="$opal_hwloc_hwloc121_ADD_CPPFLAGS -I$OMPI_TOP_BUILDDIR/$opal_hwloc_hwloc121_file/include"])
           if test "$with_devel_headers" = "yes" ; then
               opal_hwloc_hwloc121_ADD_WRAPPER_EXTRA_CPPFLAGS='-I${includedir}/openmpi/'"$opal_hwloc_hwloc121_basedir/hwloc/include"
               opal_hwloc_hwloc121_ADD_WRAPPER_EXTRA_LIBS=$HWLOC_EMBEDDED_LIBS
           fi

           $1],
          [$2])

    OPAL_VAR_SCOPE_POP
])dnl
