# -*- shell-script -*-
#
# Copyright (c) 2009-2018 Cisco Systems, Inc.  All rights reserved
# Copyright (c) 2014-2017 Intel, Inc. All rights reserved.
# Copyright (c) 2015-2018 Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# Copyright (c) 2016      Los Alamos National Security, LLC. All rights
#                         reserved.
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
AC_DEFUN([MCA_opal_hwloc_hwloc201_PRIORITY], [80])

#
# Force this component to compile in static-only mode
#
AC_DEFUN([MCA_opal_hwloc_hwloc201_COMPILE_MODE], [
    AC_MSG_CHECKING([for MCA component $2:$3 compile mode])
    $4="static"
    AC_MSG_RESULT([$$4])
])

# Include hwloc m4 files
m4_include(opal/mca/hwloc/hwloc201/hwloc/config/hwloc.m4)
m4_include(opal/mca/hwloc/hwloc201/hwloc/config/hwloc_pkg.m4)
m4_include(opal/mca/hwloc/hwloc201/hwloc/config/hwloc_check_attributes.m4)
m4_include(opal/mca/hwloc/hwloc201/hwloc/config/hwloc_check_visibility.m4)
m4_include(opal/mca/hwloc/hwloc201/hwloc/config/hwloc_check_vendor.m4)
m4_include(opal/mca/hwloc/hwloc201/hwloc/config/hwloc_components.m4)
m4_include(opal/mca/hwloc/hwloc201/hwloc/config/hwloc_internal.m4)
m4_include(opal/mca/hwloc/hwloc201/hwloc/config/netloc.m4)

# MCA_hwloc_hwloc201_POST_CONFIG()
# ---------------------------------
AC_DEFUN([MCA_opal_hwloc_hwloc201_POST_CONFIG],[
    OPAL_VAR_SCOPE_PUSH([opal_hwloc_hwloc201_basedir])

    # If we won, then do all the rest of the setup
    AS_IF([test "$1" = "1" && test "$opal_hwloc_hwloc201_support" = "yes"],
          [
           # Set this variable so that the framework m4 knows what
           # file to include in opal/mca/hwloc/hwloc-internal.h
           opal_hwloc_hwloc201_basedir=opal/mca/hwloc/hwloc201
           opal_hwloc_base_include="$opal_hwloc_hwloc201_basedir/hwloc201.h"

           # Add some stuff to CPPFLAGS so that the rest of the source
           # tree can be built
           file=$opal_hwloc_hwloc201_basedir/hwloc
           CPPFLAGS="-I$OPAL_TOP_SRCDIR/$file/include $CPPFLAGS"
           AS_IF([test "$OPAL_TOP_BUILDDIR" != "$OPAL_TOP_SRCDIR"],
                 [CPPFLAGS="-I$OPAL_TOP_BUILDDIR/$file/include $CPPFLAGS"])
           unset file
          ])
    OPAL_VAR_SCOPE_POP

    # This must be run unconditionally
    HWLOC_DO_AM_CONDITIONALS
])dnl


# MCA_hwloc_hwloc201_CONFIG([action-if-found], [action-if-not-found])
# --------------------------------------------------------------------
AC_DEFUN([MCA_opal_hwloc_hwloc201_CONFIG],[
    # Hwloc needs to know if we have Verbs support
    AC_REQUIRE([OPAL_CHECK_VERBS_DIR])

    AC_CONFIG_FILES([opal/mca/hwloc/hwloc201/Makefile])

    OPAL_VAR_SCOPE_PUSH([HWLOC_VERSION opal_hwloc_hwloc201_save_CPPFLAGS opal_hwloc_hwloc201_save_LDFLAGS opal_hwloc_hwloc201_save_LIBS opal_hwloc_hwloc201_save_cairo opal_hwloc_hwloc201_save_xml opal_hwloc_hwloc201_save_mode opal_hwloc_hwloc201_basedir opal_hwloc_hwloc201_file opal_hwloc_hwloc201_save_cflags CPPFLAGS_save LIBS_save opal_hwloc_external])

    # We know that the external hwloc component will be configured
    # before this one because of its priority.  This component is only
    # needed if the external component was not successful in selecting
    # itself.  Print out a message explaining this.
    AC_MSG_CHECKING([if hwloc external component succeeded])
    AS_IF([test "$opal_hwloc_external_support" = "yes"],
          [AC_MSG_RESULT([yes])
           AC_MSG_NOTICE([hwloc:external succeeded, so this component will be configured, but then will be skipped])],
          [AC_MSG_RESULT([no])
           AC_MSG_NOTICE([hwloc:external failed, so this component will be used])])

    # default to this component not providing support
    opal_hwloc_hwloc201_basedir=opal/mca/hwloc/hwloc201
    opal_hwloc_hwloc201_support=no

    opal_hwloc_hwloc201_save_CPPFLAGS=$CPPFLAGS
    opal_hwloc_hwloc201_save_LDFLAGS=$LDFLAGS
    opal_hwloc_hwloc201_save_LIBS=$LIBS

    # save XML or graphical options
    opal_hwloc_hwloc201_save_cairo=$enable_cairo
    opal_hwloc_hwloc201_save_xml=$enable_xml
    opal_hwloc_hwloc201_save_static=$enable_static
    opal_hwloc_hwloc201_save_shared=$enable_shared
    opal_hwloc_hwloc201_save_plugins=$enable_plugins
    opal_hwloc_hwloc201_save_mode=$hwloc_mode

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

    # ensure we are in "embedded" mode
    hwloc_mode=embedded

    # GL and OpenCL OS devices aren't used in OMPI
    enable_gl=no
    enable_opencl=no

    # Per https://github.com/open-mpi/ompi/pull/4257, ALWAYS
    # disable cuda support
    enable_cuda=no

    # Open MPI currently does not use hwloc's NVML support
    enable_nvml=no

    # hwloc checks for compiler visibility, and its needs to do
    # this without "picky" flags.
    opal_hwloc_hwloc201_save_cflags=$CFLAGS
    CFLAGS=$OPAL_CFLAGS_BEFORE_PICKY
    AS_IF([test -n "$opal_datatype_cuda_CPPFLAGS"],
          [CPPFLAGS="$CPPFLAGS $opal_datatype_cuda_CPPFLAGS"])

    # Only set the symbol prefix if this component is being used
    # (i.e., if the external component is not being used).
    AS_IF([test "$opal_hwloc_external_support" = "no"],
          [HWLOC_SET_SYMBOL_PREFIX([opal_hwloc201_])])

    # Do the bulk of the hwloc core setup
    HWLOC_SETUP_CORE([opal/mca/hwloc/hwloc201/hwloc],
              [AC_MSG_CHECKING([whether hwloc configure succeeded])
               AC_MSG_RESULT([yes])
               HWLOC_VERSION="internal v`$srcdir/$opal_hwloc_hwloc201_basedir/hwloc/config/hwloc_get_version.sh $srcdir/$opal_hwloc_hwloc201_basedir/hwloc/VERSION`"

               # Build flags for our Makefile.am
               opal_hwloc_hwloc201_LDFLAGS='$(HWLOC_EMBEDDED_LDFLAGS)'
               opal_hwloc_hwloc201_LIBS='$(OPAL_TOP_BUILDDIR)/'"$opal_hwloc_hwloc201_basedir"'/hwloc/hwloc/libhwloc_embedded.la $(HWLOC_EMBEDDED_LIBS)'
               opal_hwloc_hwloc201_support=yes

               AC_DEFINE_UNQUOTED([HWLOC_HWLOC201_HWLOC_VERSION],
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
               opal_hwloc_hwloc201_support=no])
    CFLAGS=$opal_hwloc_hwloc201_save_cflags

    # Restore some env variables, if necessary
    AS_IF([test -n "$opal_hwloc_hwloc201_save_cairo"],
          [enable_cairo=$opal_hwloc_hwloc201_save_cairo])
    AS_IF([test -n "$opal_hwloc_hwloc201_save_xml"],
          [enable_xml=$opal_hwloc_hwloc201_save_xml])
    AS_IF([test -n "$opal_hwloc_hwloc201_save_static"],
          [enable_static=$opal_hwloc_hwloc201_save_static])
    AS_IF([test -n "$opal_hwloc_hwloc201_save_shared"],
          [enable_shared=$opal_hwloc_hwloc201_save_shared])
    AS_IF([test -n "$opal_hwloc_hwloc201_save_plugins"],
          [enable_plugins=$opal_hwloc_hwloc201_save_shared])

    CPPFLAGS=$opal_hwloc_hwloc201_save_CPPFLAGS
    LDFLAGS=$opal_hwloc_hwloc201_save_LDFLAGS
    LIBS=$opal_hwloc_hwloc201_save_LIBS

    AC_SUBST([opal_hwloc_hwloc201_CFLAGS])
    AC_SUBST([opal_hwloc_hwloc201_CPPFLAGS])
    AC_SUBST([opal_hwloc_hwloc201_LDFLAGS])
    AC_SUBST([opal_hwloc_hwloc201_LIBS])

    # Finally, add some flags to the wrapper compiler so that our
    # headers can be found.
    hwloc_hwloc201_WRAPPER_EXTRA_LDFLAGS="$HWLOC_EMBEDDED_LDFLAGS"
    hwloc_hwloc201_WRAPPER_EXTRA_LIBS="$HWLOC_EMBEDDED_LIBS"
    hwloc_hwloc201_WRAPPER_EXTRA_CPPFLAGS='-I${pkgincludedir}/'"$opal_hwloc_hwloc201_basedir/hwloc/include"

    # If we are not building the internal hwloc, then indicate that
    # this component should not be built.  NOTE: we still did all the
    # above configury so that all the proper GNU Autotools
    # infrastructure is setup properly (e.g., w.r.t. SUBDIRS=hwloc in
    # this directory's Makefile.am, we still need the Autotools "make
    # distclean" infrastructure to work properly).
    AS_IF([test "$opal_hwloc_external_support" = "yes"],
          [AC_MSG_NOTICE([using an external hwloc; disqualifying this component])
           opal_hwloc_hwloc201_support=no],
          [AC_DEFINE([HAVE_DECL_HWLOC_OBJ_OSDEV_COPROC], [1])
           AC_DEFINE([HAVE_HWLOC_TOPOLOGY_DUP], [1])])

    # Done!
    AS_IF([test "$opal_hwloc_hwloc201_support" = "yes"],
          [$1],
          [$2])

    OPAL_VAR_SCOPE_POP
])dnl
