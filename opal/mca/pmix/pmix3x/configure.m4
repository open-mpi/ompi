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
# Copyright (c) 2010-2020 Cisco Systems, Inc.  All rights reserved
# Copyright (c) 2013-2019 Intel, Inc.  All rights reserved.
# Copyright (c) 2015-2016 Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# Copyright (c) 2020      IBM Corporation.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_pmix_pmix3x_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_opal_pmix_pmix3x_CONFIG],[
    AC_CONFIG_FILES([opal/mca/pmix/pmix3x/Makefile])

    OPAL_VAR_SCOPE_PUSH([PMIX_VERSION opal_pmix_pmix3x_save_CPPFLAGS opal_pmix_pmix2_save_CFLAGS opal_pmix_pmix3x_save_LDFLAGS opal_pmix_pmix3x_save_LIBS opal_pmix_pmix3x_basedir opal_pmix_pmix3x_args opal_pmix_pmix3x_happy  pmix_pmix3x_status_filename])

    opal_pmix_pmix3x_basedir=opal/mca/pmix/pmix3x

    opal_pmix_pmix3x_save_CFLAGS=$CFLAGS
    opal_pmix_pmix3x_save_CPPFLAGS=$CPPFLAGS
    opal_pmix_pmix3x_save_LDFLAGS=$LDFLAGS
    opal_pmix_pmix3x_save_LIBS=$LIBS

    AC_ARG_ENABLE([pmix-timing],
                  [AC_HELP_STRING([--enable-pmix-timing],
                                  [Enable PMIx timing measurements (default: disabled)])])
    AC_MSG_CHECKING([if PMIx timing is enabled])
    if test "$enable_pmix_timing" = "yes"; then
        AC_MSG_RESULT([yes])
        opal_pmix_pmix3x_timing_flag=--enable-pmix-timing
    else
        AC_MSG_RESULT([no (disabled)])
        opal_pmix_pmix3x_timing_flag=--disable-pmix-timing
    fi

    opal_pmix_pmix3x_args="$opal_pmix_pmix3x_timing_flag --without-tests-examples --disable-pmix-binaries --disable-pmix-backward-compatibility --disable-visibility --enable-embedded-libevent --with-libevent-header=\\\"opal/mca/event/$opal_event_base_include\\\" --enable-embedded-hwloc --with-hwloc-header=\\\"$opal_hwloc_base_include\\\""
    opal_pmix_pmix3x_args="--with-pmix-symbol-rename=OPAL_MCA_PMIX3X_ --enable-embedded-mode $opal_pmix_pmix3x_args"
    AS_IF([test "$enable_debug" = "yes"],
          [opal_pmix_pmix3x_args="--enable-debug $opal_pmix_pmix3x_args"
           CFLAGS="$OPAL_CFLAGS_BEFORE_PICKY $OPAL_VISIBILITY_CFLAGS -g"],
          [opal_pmix_pmix3x_args="--disable-debug $opal_pmix_pmix3x_args"
           CFLAGS="$OPAL_CFLAGS_BEFORE_PICKY $OPAL_VISIBILITY_CFLAGS"])
    AS_IF([test "$with_devel_headers" = "yes"],
          [opal_pmix_pmix3x_args="--with-devel-headers  $opal_pmix_pmix3x_args"])
    CPPFLAGS="-I$OPAL_TOP_SRCDIR -I$OPAL_TOP_BUILDDIR -I$OPAL_TOP_SRCDIR/opal/include -I$OPAL_TOP_BUILDDIR/opal/include $CPPFLAGS"

    # OpenPMIx Man pages are not needed when embedded. Avoids pandoc check
    opal_pmix_pmix3x_args="--disable-man-pages  $opal_pmix_pmix3x_args"

    OPAL_CONFIG_SUBDIR([$opal_pmix_pmix3x_basedir/pmix],
                       [$opal_pmix_pmix3x_args $opal_subdir_args 'CFLAGS=$CFLAGS' 'CPPFLAGS=$CPPFLAGS'],
                       [opal_pmix_pmix3x_happy=1], [opal_pmix_pmix3x_happy=0])

    CFLAGS=$opal_pmix_pmix3x_save_CFLAGS
    CPPFLAGS=$opal_pmix_pmix3x_save_CPPFLAGS
    LDFLAGS=$opal_pmix_pmix3x_save_LDFLAGS
    LIBS=$opal_pmix_pmix3x_save_LIBS

    # See if we are using the internal version. NOTE: we still did all the
    # above configury so that all the proper GNU Autotools
    # infrastructure is setup properly (e.g., w.r.t. SUBDIRS=pmix in
    # this directory's Makefile.am, we still need the Autotools "make
    # distclean" infrastructure to work properly).
    AC_MSG_CHECKING([if v3.x component is to be used])
    AS_IF([test "$opal_external_pmix_happy" = "yes"],
          [AC_MSG_RESULT([no - disqualifying this component])
           opal_pmix_pmix3x_happy=0],
          [AC_MSG_RESULT([yes - using the internal v3.x library])
           AS_IF([test "$opal_pmix_pmix3x_happy" = "0"],
                 [AC_MSG_WARN([INTERNAL PMIX FAILED TO CONFIGURE])
                  AC_MSG_ERROR([CANNOT CONTINUE])])
           # Build flags for our Makefile.am
           opal_pmix_pmix3x_LDFLAGS=
           opal_pmix_pmix3x_LIBS="$OPAL_TOP_BUILDDIR/$opal_pmix_pmix3x_basedir/pmix/src/libpmix.la"
           opal_pmix_pmix3x_CPPFLAGS="-I$OPAL_TOP_BUILDDIR/$opal_pmix_pmix3x_basedir/pmix/include -I$OPAL_TOP_BUILDDIR/$opal_pmix_pmix3x_basedir/pmix -I$OPAL_TOP_SRCDIR/$opal_pmix_pmix3x_basedir/pmix/include -I$OPAL_TOP_SRCDIR/$opal_pmix_pmix3x_basedir/pmix"
           opal_pmix_pmix3x_DEPENDENCIES="$OPAL_TOP_BUILDDIR/$opal_pmix_pmix3x_basedir/pmix/src/libpmix.la"])

   AC_SUBST([opal_pmix_pmix3x_LIBS])
   AC_SUBST([opal_pmix_pmix3x_CPPFLAGS])
   AC_SUBST([opal_pmix_pmix3x_LDFLAGS])
   AC_SUBST([opal_pmix_pmix3x_DEPENDENCIES])

   # Finally, add some flags to the wrapper compiler so that our
   # headers can be found.  Do not grab them from config.status,
   # because the value is located in an area that is part of an awk
   # script, and sometimes autoconf decides to break up super-long
   # lines into multiple lines (awk has line continuation syntax).
   # Instead, grab it from the generated Makefile.
   pmix_pmix3x_makefile_filename="$OPAL_TOP_BUILDDIR/$opal_pmix_pmix3x_basedir/pmix/Makefile"
   pmix_pmix3x_WRAPPER_EXTRA_CPPFLAGS=`egrep PMIX_EMBEDDED_CPPFLAGS $pmix_pmix3x_makefile_filename | cut -d= -f2-`
   pmix_pmix3x_WRAPPER_EXTRA_LDFLAGS=`egrep PMIX_EMBEDDED_LDFLAGS $pmix_pmix3x_makefile_filename | cut -d= -f2-`
   pmix_pmix3x_WRAPPER_EXTRA_LIBS=`egrep PMIX_EMBEDDED_LIBS $pmix_pmix3x_makefile_filename | cut -d= -f2-`

   AC_MSG_CHECKING([PMIx extra wrapper CPPFLAGS])
   AC_MSG_RESULT([$pmix_pmix3x_WRAPPER_EXTRA_CPPFLAGS])
   AC_MSG_CHECKING([PMIx extra wrapper LDFLAGS])
   AC_MSG_RESULT([$pmix_pmix3x_WRAPPER_EXTRA_LDFLAGS])
   AC_MSG_CHECKING([PMIx extra wrapper LIBS])
   AC_MSG_RESULT([$pmix_pmix3x_WRAPPER_EXTRA_LIBS])

    AS_IF([test $opal_pmix_pmix3x_happy -eq 1],
          [$1],
          [$2])

    OPAL_VAR_SCOPE_POP
])dnl
