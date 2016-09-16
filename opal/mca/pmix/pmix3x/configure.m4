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
# Copyright (c) 2010-2016 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2013-2016 Intel, Inc. All rights reserved.
# Copyright (c) 2015-2016 Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
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

    OPAL_VAR_SCOPE_PUSH([PMIX_VERSION opal_pmix_pmix3x_save_CPPFLAGS opal_pmix_pmix3x_save_LDFLAGS opal_pmix_pmix3x_save_LIBS opal_pmix_pmix3x_basedir opal_pmix_pmix3x_save_cflags opal_pmix_pmix3x_sm_flag])

    opal_pmix_pmix3x_basedir=opal/mca/pmix/pmix3x

    opal_pmix_pmix3x_save_CFLAGS=$CFLAGS
    opal_pmix_pmix3x_save_CPPFLAGS=$CPPFLAGS
    opal_pmix_pmix3x_save_LDFLAGS=$LDFLAGS
    opal_pmix_pmix3x_save_LIBS=$LIBS

    AC_ARG_ENABLE([pmix3-dstore],
                  [AC_HELP_STRING([--enable-pmix3-dstore],
                                  [Enable PMIx shared memory data store (default: disabled)])])
    AC_MSG_CHECKING([if PMIx3 shared memory data store is enabled])
    if test "$enable_pmix3_dstore" = "yes"; then
        AC_MSG_RESULT([yes])
        opal_pmix_pmix3x_sm_flag=--enable-dstore
    else
        AC_MSG_RESULT([no (disabled)])
        opal_pmix_pmix3x_sm_flag=--disable-dstore
    fi

    opal_pmix_pmix3x_args="$opal_pmix_pmix3x_sm_flag --without-tests-examples --disable-visibility --enable-embedded-libevent --with-libevent-header=\\\"opal/mca/event/$opal_event_base_include\\\" --enable-embedded-hwloc --with-hwloc-header=\\\"$opal_hwloc_base_include\\\""
    AS_IF([test "$enable_debug" = "yes"],
          [opal_pmix_pmix3x_args="--enable-debug $opal_pmix_pmix3x_args"
           CFLAGS="$OPAL_CFLAGS_BEFORE_PICKY $OPAL_VISIBILITY_CFLAGS -g"],
          [opal_pmix_pmix3x_args="--disable-debug $opal_pmix_pmix3x_args"
           CFLAGS="$OPAL_CFLAGS_BEFORE_PICKY $OPAL_VISIBILITY_CFLAGS"])
    AS_IF([test "$with_devel_headers" = "yes"], [],
          [opal_pmix_pmix3x_args="--enable-embedded-mode $opal_pmix_pmix3x_args"])
    CPPFLAGS="-I$OPAL_TOP_SRCDIR -I$OPAL_TOP_BUILDDIR -I$OPAL_TOP_SRCDIR/opal/include -I$OPAL_TOP_BUILDDIR/opal/include $CPPFLAGS"

    OPAL_CONFIG_SUBDIR([$opal_pmix_pmix3x_basedir/pmix],
                       [$opal_pmix_pmix3x_args $opal_subdir_args 'CFLAGS=$CFLAGS' 'CPPFLAGS=$CPPFLAGS'],
                       [opal_pmix_pmix3x_happy=1], [opal_pmix_pmix3x_happy=0])

    CFLAGS=$opal_pmix_pmix3x_save_CFLAGS
    CPPFLAGS=$opal_pmix_pmix3x_save_CPPFLAGS
    LDFLAGS=$opal_pmix_pmix3x_save_LDFLAGS
    LIBS=$opal_pmix_pmix3x_save_LIBS

    # If we are not building the internal pmix, then check to see
    # if we are linking to an external v3.x library. If not, then
    # do not use this component.  NOTE: we still did all the
    # above configury so that all the proper GNU Autotools
    # infrastructure is setup properly (e.g., w.r.t. SUBDIRS=pmix in
    # this directory's Makefile.am, we still need the Autotools "make
    # distclean" infrastructure to work properly).
    AC_MSG_CHECKING([if v3.x component is to be used])
    AS_IF([test "$opal_external_pmix_happy" = "yes"],
          [AS_IF([test "$opal_external_pmix_version" = "3X"],
                 [AC_MSG_RESULT([yes - using an external v3.x library])
                  # Build flags for our Makefile.am
                  opal_pmix_pmix3x_CPPFLAGS=$opal_external_pmix_CPPFLAGS
                  opal_pmix_pmix3x_LDFLAGS=$opal_external_pmix_LDFLAGS
                  opal_pmix_pmix3x_LIBS=$opal_external_pmix_LIBS
                  opal_pmix_pmix3x_DEPENDENCIES=
                  # setup wrapper flags
                  pmix_pmix3x_WRAPPER_EXTRA_LDFLAGS=$opal_external_pmix_LDFLAGS
                  pmix_pmix3x_WRAPPER_EXTRA_LIBS=$opal_external_pmix_LIBS],
                 [AC_MSG_RESULT([no - disqualifying this component])
                  opal_pmix_pmix3x_happy=0])],
          [AC_MSG_RESULT([yes - using the internal v3.x library])
           # Build flags for our Makefile.am
           opal_pmix_pmix3x_LDFLAGS=
           opal_pmix_pmix3x_LIBS='$(OPAL_TOP_BUILDDIR)/'"$opal_pmix_pmix3x_basedir"'/pmix/src/libpmix.la'
           opal_pmix_pmix3x_CPPFLAGS='-I$(OPAL_TOP_BUILDDIR)/opal/mca/pmix/pmix3x/pmix/include -I$(OPAL_TOP_BUILDDIR)/opal/mca/pmix/pmix3x/pmix -I$(OPAL_TOP_SRCDIR)/opal/mca/pmix/pmix3x/pmix/include -I$(OPAL_TOP_SRCDIR)/opal/mca/pmix/pmix3x/pmix'
           opal_pmix_pmix3x_DEPENDENCIES='$(OPAL_TOP_BUILDDIR)/opal/mca/pmix/pmix3x/pmix/src/libpmix.la'])

   AC_SUBST([opal_pmix_pmix3x_LIBS])
   AC_SUBST([opal_pmix_pmix3x_CPPFLAGS])
   AC_SUBST([opal_pmix_pmix3x_LDFLAGS])
   AC_SUBST([opal_pmix_pmix3x_DEPENDENCIES])

    AS_IF([test $opal_pmix_pmix3x_happy -eq 1],
          [$1],
          [$2])

    OPAL_VAR_SCOPE_POP
])dnl
