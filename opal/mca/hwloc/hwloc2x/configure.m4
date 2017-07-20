# -*- shell-script -*-
#
# Copyright (c) 2009-2017 Cisco Systems, Inc.  All rights reserved
# Copyright (c) 2014-2015 Intel, Inc. All rights reserved.
# Copyright (c) 2015-2017 Research Organization for Information Science
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
AC_DEFUN([MCA_opal_hwloc_hwloc2x_PRIORITY], [90])

#
# Force this component to compile in static-only mode
#
AC_DEFUN([MCA_opal_hwloc_hwloc2x_COMPILE_MODE], [
    AC_MSG_CHECKING([for MCA component $2:$3 compile mode])
    $4="static"
    AC_MSG_RESULT([$$4])
])

# MCA_hwloc_hwloc2x_POST_CONFIG()
# ---------------------------------
AC_DEFUN([MCA_opal_hwloc_hwloc2x_POST_CONFIG],[
    OPAL_VAR_SCOPE_PUSH([opal_hwloc_hwloc2x_basedir])

    # If we won, then do all the rest of the setup
    AS_IF([test "$1" = "1" && test "$opal_hwloc_hwloc2x_support" = "yes"],
          [
           # Set this variable so that the framework m4 knows what
           # file to include in opal/mca/hwloc/hwloc-internal.h
           opal_hwloc_hwloc2x_basedir=opal/mca/hwloc/hwloc2x
           opal_hwloc_base_include="$opal_hwloc_hwloc2x_basedir/hwloc2x.h"

           # Add some stuff to CPPFLAGS so that the rest of the source
           # tree can be built
           file=$opal_hwloc_hwloc2x_basedir/hwloc
           CPPFLAGS="-I$OPAL_TOP_SRCDIR/$file/include $CPPFLAGS"
           AS_IF([test "$OPAL_TOP_BUILDDIR" != "$OPAL_TOP_SRCDIR"],
                 [CPPFLAGS="-I$OPAL_TOP_BUILDDIR/$file/include $CPPFLAGS"])
           unset file
          ])
    OPAL_VAR_SCOPE_POP
])dnl


# MCA_hwloc_hwloc2x_CONFIG([action-if-found], [action-if-not-found])
# --------------------------------------------------------------------
AC_DEFUN([MCA_opal_hwloc_hwloc2x_CONFIG],[
    # Hwloc needs to know if we have Verbs support
    AC_REQUIRE([OPAL_CHECK_VERBS_DIR])

    AC_CONFIG_FILES([opal/mca/hwloc/hwloc2x/Makefile])

    OPAL_VAR_SCOPE_PUSH([HWLOC_VERSION opal_hwloc_hwloc2x_flags opal_hwloc_hwloc2x_save_CPPFLAGS opal_hwloc_hwloc2x_basedir opal_hwloc_hwloc2x_file opal_hwloc_future])

    # default to this component not providing support
    opal_hwloc_hwloc2x_basedir=opal/mca/hwloc/hwloc2x
    opal_hwloc_hwloc2x_support=no

    AS_IF([test "$with_hwloc" = "future"],
          [opal_hwloc_future="yes"],
          [opal_hwloc_future="no"])

    opal_hwloc_hwloc2x_save_CPPFLAGS=$CPPFLAGS

    # Run the hwloc configuration - if no external hwloc, then set the prefix
    # to minimize the chance that someone will use the internal symbols
    
    opal_hwloc_hwloc2x_flags="--enable-embedded-mode --with-hwloc-symbol-prefix=opal_hwloc2x_ --disable-cairo --disable-pugins --enable-static --enable-xml"
    AS_IF([test "$opal_check_cuda_happy" = "yes"],
          [CPPFLAGS="$CPPFLAGS $opal_datatype_cuda_CPPFLAGS",
           opal_hwloc_hwloc2x_flags="$opal_hwloc_hwloc2x_flags --enable-nvml CPPFLAGS=\"$CPPFLAGS\""]
          [opal_hwloc_hwloc2x_flags="$opal_hwloc_hwloc2x_flags --disable-nvml"])

    OPAL_CONFIG_SUBDIR([opal/mca/hwloc/hwloc2x/hwloc],
                       [$opal_hwloc_hwloc2x_flags],
                       [opal_hwloc_hwloc2x_support="yes"],
                       [opal_hwloc_hwloc2x_support="no"])

    CPPFLAGS=$opal_hwloc_hwloc2x_save_CPPFLAGS

    # If we are not building the internal hwloc, then indicate that
    # this component should not be built.  NOTE: we still did all the
    # above configury so that all the proper GNU Autotools
    # infrastructure is setup properly (e.g., w.r.t. SUBDIRS=hwloc in
    # this directory's Makefile.am, we still need the Autotools "make
    # distclean" infrastructure to work properly).
    AS_IF([test "$opal_hwloc_future" != "yes"],
          [AC_MSG_WARN([not using future hwloc; disqualifying this component])
           opal_hwloc_hwloc2x_support=no])

    # Done!
    AS_IF([test "$opal_hwloc_hwloc2x_support" = "yes"],
          [AC_DEFINE_UNQUOTED([HWLOC_SYM_PREFIX],[opal_hwloc2x_])
           AC_DEFINE_UNQUOTED([HWLOC_SYM_PREFIX_CAPS], [OPAL_HWLOC2X_])
           AC_DEFINE_UNQUOTED([HWLOC_SYM_TRANSFORM], [1])
           AC_DEFINE([HAVE_DECL_HWLOC_OBJ_OSDEV_COPROC], [1])
           $1],
          [$2])

    OPAL_VAR_SCOPE_POP
])dnl
