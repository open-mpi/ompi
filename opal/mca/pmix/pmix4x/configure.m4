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
# Copyright (c) 2011-2013 Los Alamos National Security, LLC.
#                         All rights reserved.
# Copyright (c) 2010-2017 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2013-2020 Intel, Inc.  All rights reserved.
# Copyright (c) 2015-2016 Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# Copyright (c) 2020      Amazon.com, Inc. or its affiliates.
#                         All Rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

#
# Priority
#
AC_DEFUN([MCA_opal_pmix_pmix4x_PRIORITY], [80])

#
# Force this component to compile in static-only mode
#
AC_DEFUN([MCA_opal_pmix_pmix4x_COMPILE_MODE], [
    AC_MSG_CHECKING([for MCA component $2:$3 compile mode])
    $4="static"
    AC_MSG_RESULT([$$4])
])

# MCA_pmix_pmix4x_POST_CONFIG()
# ---------------------------------
AC_DEFUN([MCA_opal_pmix_pmix4x_POST_CONFIG],[
    OPAL_VAR_SCOPE_PUSH([opal_pmix_pmix4x_basedir])

    # If we won, then do all the rest of the setup
    AS_IF([test "$1" = "1" && test "$opal_pmix_pmix4x_happy" = "1"],
          [
           # Set this variable so that the framework m4 knows what
           # file to include in opal/mca/pmix/pmix-internal.h
           opal_pmix_pmix4x_basedir=opal/mca/pmix/pmix4x
           opal_pmix_base_include="$opal_pmix_pmix4x_basedir/pmix4x.h"

           # Add some stuff to CPPFLAGS so that the rest of the source
           # tree can be built
           file=$opal_pmix_pmix4x_basedir/openpmix
           CPPFLAGS="-I$OPAL_TOP_SRCDIR/$file/include $CPPFLAGS"
           AS_IF([test "$OPAL_TOP_BUILDDIR" != "$OPAL_TOP_SRCDIR"],
                 [CPPFLAGS="-I$OPAL_TOP_BUILDDIR/$file/include $CPPFLAGS"])
           unset file
          ])
    OPAL_VAR_SCOPE_POP

    # This must be run unconditionally
    # PMIX_DO_AM_CONDITIONALS
])dnl


# MCA_pmix_pmix4x_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_opal_pmix_pmix4x_CONFIG],[
    AC_CONFIG_FILES([opal/mca/pmix/pmix4x/Makefile])

    OPAL_VAR_SCOPE_PUSH([PMIX_VERSION opal_pmix_pmix4x_save_CPPFLAGS opal_pmix_pmix2_save_CFLAGS opal_pmix_pmix4x_save_LDFLAGS opal_pmix_pmix4x_save_LIBS opal_pmix_pmix4x_basedir opal_pmix_pmix4x_args  pmix_pmix4x_status_filename])

    opal_pmix_pmix4x_basedir=opal/mca/pmix/pmix4x

    opal_pmix_pmix4x_save_CFLAGS=$CFLAGS
    opal_pmix_pmix4x_save_CPPFLAGS=$CPPFLAGS
    opal_pmix_pmix4x_save_LDFLAGS=$LDFLAGS
    opal_pmix_pmix4x_save_LIBS=$LIBS

    AC_ARG_ENABLE([pmix-timing],
                  [AC_HELP_STRING([--enable-pmix-timing],
                                  [Enable PMIx timing measurements (default: disabled)])])
    AC_ARG_WITH([pmix-platform],
                [AC_HELP_STRING([--with-pmix-platform],
                                [Platform file to use when building the internal PMIx support])])

    AC_MSG_CHECKING([if PMIx timing is enabled])
    if test "$enable_pmix_timing" = "yes"; then
        AC_MSG_RESULT([yes])
        opal_pmix_pmix4x_timing_flag=--enable-pmix-timing
    else
        AC_MSG_RESULT([no (disabled)])
        opal_pmix_pmix4x_timing_flag=--disable-pmix-timing
    fi

    opal_pmix_pmix4x_args="$opal_pmix_pmix4x_timing_flag --without-tests-examples --with-pmix-symbol-rename=OPAL_MCA_PMIX4X_ --disable-pmix-binaries --disable-pmix-backward-compatibility --disable-visibility --enable-embedded-mode --with-libevent-header=\\\"opal/util/event.h\\\" --with-hwloc-header=\\\"opal/mca/hwloc/hwloc-internal.h\\\""
    AS_IF([test "$enable_debug" = "yes"],
          [opal_pmix_pmix4x_args="--enable-debug $opal_pmix_pmix4x_args"
           CFLAGS="$OPAL_CFLAGS_BEFORE_PICKY $OPAL_VISIBILITY_CFLAGS -g"],
          [opal_pmix_pmix4x_args="--disable-debug $opal_pmix_pmix4x_args"
           CFLAGS="$OPAL_CFLAGS_BEFORE_PICKY $OPAL_VISIBILITY_CFLAGS"])
    AS_IF([test "$with_devel_headers" = "yes"],
          [opal_pmix_pmix4x_args="--with-devel-headers  $opal_pmix_pmix4x_args"])
    if test ! -z $with_pmix_platform && test "$with_pix_platform" != "yes"; then
        opal_pmix_pmix4x_args="$opal_pmix_pmix4x_args --with-platform=$with_pmix_platform"
    fi
    CPPFLAGS="-I$OPAL_TOP_SRCDIR -I$OPAL_TOP_BUILDDIR -I$OPAL_TOP_SRCDIR/opal/include -I$OPAL_TOP_BUILDDIR/opal/include $CPPFLAGS"

    OPAL_CONFIG_SUBDIR([$opal_pmix_pmix4x_basedir/openpmix],
                       [$opal_pmix_pmix4x_args $opal_subdir_args 'CFLAGS=$CFLAGS' 'CPPFLAGS=$CPPFLAGS'],
                       [opal_pmix_pmix4x_happy=1], [opal_pmix_pmix4x_happy=0])

    CFLAGS=$opal_pmix_pmix4x_save_CFLAGS
    CPPFLAGS=$opal_pmix_pmix4x_save_CPPFLAGS
    LDFLAGS=$opal_pmix_pmix4x_save_LDFLAGS
    LIBS=$opal_pmix_pmix4x_save_LIBS

    # See if we are using the internal version. NOTE: we still did all the
    # above configury so that all the proper GNU Autotools
    # infrastructure is setup properly (e.g., w.r.t. SUBDIRS=pmix in
    # this directory's Makefile.am, we still need the Autotools "make
    # distclean" infrastructure to work properly).
    AC_MSG_CHECKING([if v4.x component is to be used])
    AS_IF([test "$opal_external_pmix_happy" = "yes"],
          [AC_MSG_RESULT([no - disqualifying this component])
           opal_pmix_pmix4x_happy=0],
          [AC_MSG_RESULT([yes - using the internal v4.x library])
           AS_IF([test "$opal_pmix_pmix4x_happy" = "0"],
                 [AC_MSG_WARN([INTERNAL PMIX FAILED TO CONFIGURE])
                  AC_MSG_ERROR([CANNOT CONTINUE])])
           # Build flags for our Makefile.am
           opal_pmix_pmix4x_LDFLAGS=
           opal_pmix_pmix4x_LIBS="$OPAL_TOP_BUILDDIR/$opal_pmix_pmix4x_basedir/openpmix/src/libpmix.la"
           opal_pmix_pmix4x_CPPFLAGS="-I$OPAL_TOP_BUILDDIR/$opal_pmix_pmix4x_basedir/openpmix/include -I$OPAL_TOP_BUILDDIR/$opal_pmix_pmix4x_basedir/openpmix -I$OPAL_TOP_SRCDIR/$opal_pmix_pmix4x_basedir/openpmix/include -I$OPAL_TOP_SRCDIR/$opal_pmix_pmix4x_basedir/openpmix"
           opal_pmix_pmix4x_DEPENDENCIES="$OPAL_TOP_BUILDDIR/$opal_pmix_pmix4x_basedir/openpmix/src/libpmix.la"])

   AC_SUBST([opal_pmix_pmix4x_LIBS])
   AC_SUBST([opal_pmix_pmix4x_CPPFLAGS])
   AC_SUBST([opal_pmix_pmix4x_LDFLAGS])
   AC_SUBST([opal_pmix_pmix4x_DEPENDENCIES])

   # Finally, add some flags to the wrapper compiler so that our
   # headers can be found.
   pmix_pmix4x_status_filename="$OPAL_TOP_BUILDDIR/$opal_pmix_pmix4x_basedir/openpmix/config.status"
   pmix_pmix4x_WRAPPER_EXTRA_CPPFLAGS=`egrep PMIX_EMBEDDED_CPPFLAGS $pmix_pmix4x_status_filename | cut -d\" -f4`
   pmix_pmix4x_WRAPPER_EXTRA_LDFLAGS=`egrep PMIX_EMBEDDED_LDFLAGS $pmix_pmix4x_status_filename | cut -d\" -f4`
   pmix_pmix4x_WRAPPER_EXTRA_LIBS=`egrep PMIX_EMBEDDED_LIBS $pmix_pmix4x_status_filename | cut -d\" -f4`

   AC_MSG_CHECKING([PMIx extra wrapper CPPFLAGS])
   AC_MSG_RESULT([$pmix_pmix4x_WRAPPER_EXTRA_CPPFLAGS])
   AC_MSG_CHECKING([PMIx extra wrapper LDFLAGS])
   AC_MSG_RESULT([$pmix_pmix4x_WRAPPER_EXTRA_LDFLAGS])
   AC_MSG_CHECKING([PMIx extra wrapper LIBS])
   AC_MSG_RESULT([$pmix_pmix4x_WRAPPER_EXTRA_LIBS])

    AS_IF([test $opal_pmix_pmix4x_happy -eq 1],
          [$1],
          [$2])

    OPAL_VAR_SCOPE_POP
])dnl
