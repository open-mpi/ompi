dnl -*- autoconf -*-
dnl
dnl Copyright (c) 2009-2018 Cisco Systems, Inc.  All rights reserved
dnl Copyright (c) 2013      Los Alamos National Security, LLC.  All rights reserved.
dnl Copyright (c) 2015-2018 Research Organization for Information Science
dnl                         and Technology (RIST). All rights reserved.
dnl Copyright (c) 2020-2021 Amazon.com, Inc. or its affiliates.  All Rights
dnl                         reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

dnl Check for / configure libevent package.  Prefer finding an
dnl external libevent, build our internal one if required.  If we can
dnl not find an external libevent and the internal one fails to
dnl configure, abort.
dnl
dnl This macro will change the environment in the following way:
dnl
dnl   * opal_libevent_header [legacy] - will be set if building
dnl         internally, to the header file that should be included for
dnl         embedded builds.  This is used by PRRTE, but should not
dnl         be used by new code.
dnl   * opal_libevent_mode - either external or internal.  If internal,
dnl         --with-libevent should be ignored by other packages
dnl   * opal_libevent_CPPFLAGS - the C Preprocessor flags necessary to
dnl         run the preprocessor on a file which relies on Libevent
dnl         headers.  This will be folded into the global CPPFLAGS,
dnl         so most people should never need this.
dnl   * opal_libevent_LDFLAGS - the linker flags necessary to run the
dnl         linker on a file which relies on Libevent libraries.  This
dnl         will be folded into the global CPPFLAGS, so most people
dnl         should never need this.
dnl   * opal_libevent_LIBS - the libraries necessary to link source which
dnl         uses Libevent.  Cannot be added to LIBS yet, because then
dnl         other execution tests later in configure (there are sadly
dnl         some) would fail if the path in LDFLAGS was not added to
dnl         LD_LIBRARY_PATH.
dnl   * opal_libevent_WRAPPER_LDFLAGS - the linker flags necessary to
dnl         add to the wrapper compilers in order to link an opal
dnl         application when opal is built as a static library.
dnl   * opal_libevent_WRAPPER_LIBS - the linker flags necessary to
dnl         add to the wrapper compilers in order to link an opal
dnl         application when opal is built as a static library.
dnl   * CPPFLAGS, LDFLAGS - Updated opal_libevent_CPPFLAGS and
dnl         opal_libevent_LDFLAGS.
AC_DEFUN([OPAL_CONFIG_LIBEVENT], [
    OPAL_VAR_SCOPE_PUSH([internal_libevent_happy external_libevent_happy pkg_config_core pkg_config_pthreads pkg_config_core_ldflags pkg_config_pthreads_ldflags pkg_config_core_libs pkg_config_pthreads_libs pkg_config_happy])

    opal_show_subtitle "Configuring Libevent"

    OPAL_3RDPARTY_WITH([libevent], [libevent], [package_libevent])

    opal_libevent_header=""

    # unless internal specifically requested by the user, try to find
    # an external that works.
    external_libevent_happy=0
    AS_IF([test "$opal_libevent_mode" != "internal"],
          [_OPAL_CONFIG_LIBEVENT_EXTERNAL(
              [external_libevent_happy=1
               opal_libevent_mode="external"],
              [external_libevent_happy=0
               AS_IF([test "$opal_libevent_mode" = "external"],
                     [AC_MSG_ERROR([External libevent requested but not found.])])])])

    internal_libevent_happy=0
    m4_ifdef([package_libevent],
        [AS_IF([test "$external_libevent_happy" = "0"],
             [_OPAL_CONFIG_LIBEVENT_INTERNAL([internal_libevent_happy=1
                                              opal_libevent_mode="internal"])])])

    AS_IF([test "$external_libevent_happy" = "0" -a "$internal_libevent_happy" = "0"],
          [AC_MSG_ERROR([Could not find viable libevent build.])])

    AS_IF([test "$opal_libevent_mode" = "internal"],
          [pkg_config_core="${OMPI_TOP_BUILDDIR}/3rd-party/libevent_directory/libevent_core.pc"
           pkg_config_pthreads="${OMPI_TOP_BUILDDIR}/3rd-party/libevent_directory/libevent_pthreads.pc"
           PKG_CONFIG_PATH="${OMPI_TOP_BUILDDIR}/3rd-party/libevent_directory:${PKG_CONFIG_PATH}"],
          [test -n "$with_libevent"],
          [pkg_config_core="${with_libevent}/lib/pkgconfig/libevent_core.pc"
           pkg_config_pthreads="${with_libevent}/lib/pkgconfig/libevent_pthreads.pc"
           PKG_CONFIG_PATH="${with_libevent}/lib/pkgconfig:${PKG_CONFIG_PATH}"],
          [pkg_config_core="libevent_core"
           pkg_config_pthreads="libevent_pthreads"])

    pkg_config_happy=1
    OPAL_GET_LDFLAGS_FROM_PC([$pkg_config_core], [pkg_config_core_ldflags], [pkg_config_happy=0])
    OPAL_GET_LDFLAGS_FROM_PC([$pkg_config_pthreads], [pkg_config_pthreads_ldflags], [pkg_config_happy=0])
    OPAL_GET_LIBS_FROM_PC([$pkg_config_core], [pkg_config_core_libs], [pkg_config_happy=0])
    OPAL_GET_LIBS_FROM_PC([$pkg_config_pthreads], [pkg_config_pthreads_libs], [pkg_config_happy=0])

    AS_IF([test $pkg_config_happy -ne 0],
          [# Strip -levent from pkg_config_pthreads_libs, since we
           # only want to link against libevent_core.  We'll pick up
           # the core library from pkg_config_core_libs.
           pkg_config_pthreads_libs=`echo $pkg_config_pthreads_libs | sed "s/\\-levent\b//g"`
           opal_libevent_WRAPPER_LDFLAGS="$pkg_config_core_ldflags"
           OPAL_FLAGS_APPEND_UNIQ([opal_libevent_WRAPPER_LDFLAGS], [$pkg_config_pthreads_ldflags])
           opal_libevent_WRAPPER_LIBS="$pkg_config_pthreads_libs"
           OPAL_FLAGS_APPEND_MOVE([opal_libevent_WRAPPER_LIBS], [$pkg_config_core_libs])],
          [# guess that what we have from compiling OMPI is good enough
           AS_IF([test -z "$opal_libevent_WRAPPER_LDFLAGS"],
                 [opal_libevent_WRAPPER_LDFLAGS="$opal_libevent_LDFLAGS"])
           AS_IF([test -z "$opal_libevent_WRAPPER_LIBS"],
                 [opal_libevent_WRAPPER_LIBS="$opal_libevent_LIBS"])])

    OPAL_WRAPPER_FLAGS_ADD([LDFLAGS], [$opal_libevent_WRAPPER_LDFLAGS])
    OPAL_WRAPPER_FLAGS_ADD([LIBS], [$opal_libevent_WRAPPER_LIBS])

    # this will work even if there is no libevent package included,
    # because libevent_tarball and libevent_directory will evaluate to
    # an empty string.  These are relative to the 3rd-party/
    # directory.
    OPAL_3RDPARTY_EXTRA_DIST="$OPAL_3RDPARTY_EXTRA_DIST libevent_tarball"
    OPAL_3RDPARTY_DISTCLEAN_DIRS="$OPAL_3RDPARTY_DISTCLEAN_DIRS libevent_directory"

    AC_SUBST(opal_libevent_CPPFLAGS)
    AC_SUBST(opal_libevent_LIBS)
    AC_SUBST(opal_libevent_LDFLAGS)

    OPAL_SUMMARY_ADD([[Miscellaneous]],[[libevent]],[libevent], [$opal_libevent_mode])

    OPAL_VAR_SCOPE_POP
])

dnl _OPAL_CONFIG_LIBEVENT_EXTERNAL(action-if-happy, action-if-not-happy)
dnl
dnl only safe to call from OPAL_CONFIG_LIBEVENT, assumes variables
dnl from there are set.
AC_DEFUN([_OPAL_CONFIG_LIBEVENT_EXTERNAL], [
    OPAL_VAR_SCOPE_PUSH([opal_libevent_CPPFLAGS_save opal_libevent_LDFLAGS_save opal_libevent_LIBS_save opal_libevent_external_support])

    opal_libevent_CPPFLAGS_save=$CPPFLAGS
    opal_libevent_LDFLAGS_save=$LDFLAGS
    opal_libevent_LIBS_save=$LIBS

    AS_IF([test ! -z "$with_libevent_libdir"],
          [OPAL_CHECK_WITHDIR([libevent-libdir], [$with_libevent_libdir],
                              [libevent.*])])

    OPAL_CHECK_PACKAGE([opal_libevent],
                       [event2/event.h],
                       [event_core],
                       [event_config_new],
                       [-levent_pthreads],
                       [$with_libevent],
                       [$with_libevent_libdir],
                       [opal_libevent_external_support=yes],
                       [opal_libevent_external_support=no])

    # need these set for the tests below.  If things fail, will undo at the end.
    CPPFLAGS="$opal_libevent_CPPFLAGS_save $opal_libevent_CPPFLAGS"
    LDFLAGS="$opal_libevent_LDFLAGS_save $opal_libevent_LDFLAGS"
    LIBS="$opal_libevent_LIBS_save $opal_libevent_LIBS"

    # Ensure that this libevent has the symbol
    # "evthread_set_lock_callbacks", which will only exist if
    # libevent was configured with thread support.
    AS_IF([test "$opal_libevent_external_support" = "yes"],
          [AC_CHECK_LIB([event_core], [evthread_set_lock_callbacks],
                        [],
                        [AC_MSG_WARN([External libevent does not have thread support])
                         AC_MSG_WARN([Open MPI requires libevent to be compiled with])
                         AC_MSG_WARN([thread support enabled])
                         opal_libevent_external_support=no])])

    AS_IF([test "$opal_libevent_external_support" = "yes"],
          [AC_CHECK_LIB([event_pthreads], [evthread_use_pthreads],
                        [],
                        [AC_MSG_WARN([External libevent does not have thread support])
                         AC_MSG_WARN([Open MPI requires libevent to be compiled with])
                         AC_MSG_WARN([thread support enabled])
                         opal_libevent_external_support=no])])

    # Open MPI used to fall back to the internal libevent if the
    # installed version was older than the internal version.  This
    # isn't what we want, because we really want to prefer external
    # versions.  Pin the "oldest supported" external version to
    # 2.0.21, which we know works from testing on RHEL7.
    AS_IF([test "$opal_libevent_external_support" = "yes"],
          [AC_MSG_CHECKING([if external libevent version is 2.0.21 or greater])
              AC_COMPILE_IFELSE(
                  [AC_LANG_PROGRAM([[#include <event2/event.h>]],
                                 [[
#if defined(_EVENT_NUMERIC_VERSION) && _EVENT_NUMERIC_VERSION < 0x02001500
#error "libevent API version is less than 0x02001500"
#elif defined(EVENT__NUMERIC_VERSION) && EVENT__NUMERIC_VERSION < 0x02001500
#error "libevent API version is less than 0x02001500"
#endif
                                 ]])],
                  [AC_MSG_RESULT([yes])],
                  [AC_MSG_RESULT([no])
                   AC_MSG_WARN([external libevent version is too old (2.0.21 or later required)])
                   opal_libevent_external_support=no])])

    LDFLAGS="$opal_libevent_LDFLAGS_save"
    LIBS="$opal_libevent_LIBS_save"

    AS_IF([test "$opal_libevent_external_support" = "yes"],
          [$1],
          [CPPFLAGS="$opal_libevent_CPPFLAGS_save"
           $2])

    OPAL_VAR_SCOPE_POP
])

dnl _OPAL_CONFIG_LIBEVENT_INTERNAL(action-if-happy, action-if-not-happy)
dnl
dnl Configure the packaged libevent.  Only needs to be run if the
dnl external libevent is not going to be used.  Assumes that if
dnl this function is called, that success means the internal package
dnl will be used.
AC_DEFUN([_OPAL_CONFIG_LIBEVENT_INTERNAL], [
    OPAL_VAR_SCOPE_PUSH(subconfig_happy subconfig_prefix internal_libevent_location)

    AS_IF([test ! -z $prefix], [subconfig_prefix="--prefix=$prefix"])

    # Note: To update the version of libevent shipped, update the
    # constant in autogen.pl.
    OPAL_EXPAND_TARBALL([3rd-party/libevent_tarball], [3rd-party/libevent_directory], [configure])
    # We disable the GCC warnings because 1) we're not developers of
    # Libevent, so we will never actually fix said warnnings and 2)
    # some of the warning options cause failures with compilers that
    # fake being GCC (I'm looking at you, PGI).
    OPAL_SUBDIR_ENV_CLEAN([opal_libevent_configure])
    PAC_CONFIG_SUBDIR_ARGS([3rd-party/libevent_directory],
       [--disable-dns --disable-http --disable-rpc --disable-openssl --enable-thread-support --disable-evport --disable-gcc-warnings --disable-libevent-regress],
       [], [subconfig_happy=1], [subconfig_happy=0])
    OPAL_SUBDIR_ENV_RESTORE([opal_libevent_configure])

    AS_IF([test "$subconfig_happy" = "1"],
        [internal_libevent_location="3rd-party/libevent_directory"
         # note: because we only ship/commit a tarball (and not the source
         # directory), the source is always expanded in the builddir, so we
         # only need to add a -I to the builddir.
         opal_libevent_CPPFLAGS="-I$OMPI_TOP_BUILDDIR/$internal_libevent_location -I$OMPI_TOP_BUILDDIR/$internal_libevent_location/include"
         CPPFLAGS="$CPPFLAGS $opal_libevent_CPPFLAGS"
         # No need to update LDFLAGS, because they will install into
         # our tree and in the mean time are referenced by their .la
         # files.
         opal_libevent_LIBS="$OMPI_TOP_BUILDDIR/$internal_libevent_location/libevent_core.la $OMPI_TOP_BUILDDIR/$internal_libevent_location/libevent_pthreads.la"
	 opal_libevent_WRAPPER_LIBS="-levent_core -levent_pthreads"

         opal_libevent_header="$OMPI_TOP_BUILDDIR/$internal_libevent_location/event.h"

         # no need to add to DIST_SUBDIRS, because we only ship the
         # tarball.  This is relative to the 3rd-party/ directory.
         OPAL_3RDPARTY_SUBDIRS="$OPAL_3RDPARTY_SUBDIRS libevent_directory"

         # The tarball as configured can not be used for compile
         # tests, because libevent uses a Makefile rule rather than
         # Autoconf to generate their config file (sigh).  Force
         # generation of that file now, so that other 3rd party
         # packages can run compile tests.
         AC_MSG_NOTICE([Generating Libevent's event-config.h])
         (cd $OMPI_TOP_BUILDDIR/$internal_libevent_location/ ; ${MAKE-make} include/event2/event-config.h)
         AS_IF([test $? -ne 0], [AC_MSG_ERROR([Could not generate event-config.h.])])

         $1], [$2])

    OPAL_VAR_SCOPE_POP
])
