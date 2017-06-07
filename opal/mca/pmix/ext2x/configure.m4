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
# Copyright (c) 2013-2017 Intel, Inc.  All rights reserved.
# Copyright (c) 2015-2016 Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_pmix_ext2x_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_opal_pmix_ext2x_CONFIG],[
    AC_CONFIG_FILES([opal/mca/pmix/ext2x/Makefile])

    OPAL_VAR_SCOPE_PUSH([PMIX_VERSION opal_pmix_ext2x_save_CPPFLAGS opal_pmix_pmix2_save_CFLAGS opal_pmix_ext2x_save_LDFLAGS opal_pmix_ext2x_save_LIBS opal_pmix_ext2x_basedir opal_pmix_ext2x_args opal_pmix_ext2x_happy opal_pmix_ext2x_sm_flag pmix_ext2x_status_filename])

    opal_pmix_ext2x_basedir=opal/mca/pmix/ext2x

    opal_pmix_ext2x_save_CFLAGS=$CFLAGS
    opal_pmix_ext2x_save_CPPFLAGS=$CPPFLAGS
    opal_pmix_ext2x_save_LDFLAGS=$LDFLAGS
    opal_pmix_ext2x_save_LIBS=$LIBS

    AC_ARG_ENABLE([pmix-dstore],
                  [AC_HELP_STRING([--enable-pmix-dstore],
                                  [Enable PMIx shared memory data store (default: enabled)])])
    AC_MSG_CHECKING([if PMIx shared memory data store is enabled])
    if test "$enable_pmix_dstore" != "no"; then
        AC_MSG_RESULT([yes])
        opal_pmix_ext2x_sm_flag=--enable-dstore
    else
        AC_MSG_RESULT([no (disabled)])
        opal_pmix_ext2x_sm_flag=--disable-dstore
    fi

    AC_ARG_ENABLE([pmix-timing],
                  [AC_HELP_STRING([--enable-pmix-timing],
                                  [Enable PMIx timing measurements (default: disabled)])])
    AC_MSG_CHECKING([if PMIx timing is enabled])
    if test "$enable_pmix_timing" == "yes"; then
        AC_MSG_RESULT([yes])
        opal_pmix_ext2x_timing_flag=--enable-pmix-timing
    else
        AC_MSG_RESULT([no (disabled)])
        opal_pmix_ext2x_timing_flag=--disable-pmix-timing
    fi

    opal_pmix_ext2x_args="--with-pmix-symbol-rename=OPAL_MCA_PMIX2X_ $opal_pmix_ext2x_sm_flag $opal_pmix_ext2x_timing_flag --without-tests-examples --disable-pmix-backward-compatibility --disable-visibility --enable-embedded-libevent --with-libevent-header=\\\"opal/mca/event/$opal_event_base_include\\\" --enable-embedded-mode"
    AS_IF([test "$enable_debug" = "yes"],
          [opal_pmix_ext2x_args="--enable-debug $opal_pmix_ext2x_args"
           CFLAGS="$OPAL_CFLAGS_BEFORE_PICKY $OPAL_VISIBILITY_CFLAGS -g"],
          [opal_pmix_ext2x_args="--disable-debug $opal_pmix_ext2x_args"
           CFLAGS="$OPAL_CFLAGS_BEFORE_PICKY $OPAL_VISIBILITY_CFLAGS"])
    AS_IF([test "$with_devel_headers" = "yes"],
          [opal_pmix_ext2x_args="--with-devel-headers $opal_pmix_ext2x_args"],
          [opal_pmix_ext2x_args=$opal_pmix_ext2x_args])
    CPPFLAGS="-I$OPAL_TOP_SRCDIR -I$OPAL_TOP_BUILDDIR -I$OPAL_TOP_SRCDIR/opal/include -I$OPAL_TOP_BUILDDIR/opal/include $CPPFLAGS"

    OPAL_CONFIG_SUBDIR([$opal_pmix_ext2x_basedir/pmix],
                       [$opal_pmix_ext2x_args $opal_subdir_args 'CFLAGS=$CFLAGS' 'CPPFLAGS=$CPPFLAGS'],
                       [opal_pmix_ext2x_happy=1], [opal_pmix_ext2x_happy=0])

    CFLAGS=$opal_pmix_ext2x_save_CFLAGS
    CPPFLAGS=$opal_pmix_ext2x_save_CPPFLAGS
    LDFLAGS=$opal_pmix_ext2x_save_LDFLAGS
    LIBS=$opal_pmix_ext2x_save_LIBS

    # if we are linking to an external v2.x library. If not, then
    # do not use this component.
    AC_MSG_CHECKING([if external v2.x component is to be used])
    AS_IF([test "$opal_external_pmix_happy" = "yes"],
          [AS_IF([test "$opal_external_pmix_version" = "2x"],
                 [AC_MSG_RESULT([yes - using an external v2.x library])
                  opal_pmix_ext2x_happy=1
                  # Build flags for our Makefile.am
                  opal_pmix_ext2x_CPPFLAGS=$opal_external_pmix_CPPFLAGS
                  opal_pmix_ext2x_LDFLAGS=$opal_external_pmix_LDFLAGS
                  opal_pmix_ext2x_LIBS=$opal_external_pmix_LIBS
                  # setup wrapper flags
                  pmix_ext2x_WRAPPER_EXTRA_LDFLAGS=$opal_external_pmix_LDFLAGS
                  pmix_ext2x_WRAPPER_EXTRA_LIBS=$opal_external_pmix_LIBS],
                 [AC_MSG_RESULT([no - disqualifying this component])
                  opal_pmix_ext2x_happy=0])],
           [AC_MSG_RESULT([no])
            opal_pmix_ext2x_happy=0])

   AC_SUBST([opal_pmix_ext2x_LIBS])
   AC_SUBST([opal_pmix_ext2x_CPPFLAGS])
   AC_SUBST([opal_pmix_ext2x_LDFLAGS])
   AC_SUBST([opal_pmix_ext2x_DEPENDENCIES])

   AC_MSG_CHECKING([PMIx extra wrapper CPPFLAGS])
   AC_MSG_RESULT([$pmix_ext2x_WRAPPER_EXTRA_CPPFLAGS])
   AC_MSG_CHECKING([PMIx extra wrapper LDFLAGS])
   AC_MSG_RESULT([$pmix_ext2x_WRAPPER_EXTRA_LDFLAGS])
   AC_MSG_CHECKING([PMIx extra wrapper LIBS])
   AC_MSG_RESULT([$pmix_ext2x_WRAPPER_EXTRA_LIBS])

    AS_IF([test $opal_pmix_ext2x_happy -eq 1],
          [$1],
          [$2])

    OPAL_VAR_SCOPE_POP
])dnl
