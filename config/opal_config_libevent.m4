dnl -*- autoconf -*-
dnl
dnl Copyright (c) 2009-2018 Cisco Systems, Inc.  All rights reserved
dnl Copyright (c) 2013      Los Alamos National Security, LLC.  All rights reserved.
dnl Copyright (c) 2015-2018 Research Organization for Information Science
dnl                         and Technology (RIST). All rights reserved.
dnl Copyright (c) 2020-2022 Amazon.com, Inc. or its affiliates.  All Rights reserved.
dnl Copyright (c) 2022      IBM Corporation.  All rights reserved.
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
dnl   * opal_libevent_mode - either external or internal.  If internal,
dnl         --with-libevent should be ignored by other packages
dnl   * CPPFLAGS, LDFLAGS, LIBS - Updated to build against libevent.
dnl         Note that the value may be updated right before
dnl         config.status.
dnl
dnl OPAL_WRAPPER_FLAGS_ADD will be called to add the correct LDFLAGS,
dnl STATIC_LDFLAGS, LIBS, and STATIC_LIBS for libevent.  Note that we
dnl intentionally do not use pkg-config modules for Libevent because
dnl we want to avoid libevent in preference for libevent_core.
dnl
dnl The following environment variables will only be set if
dnl opal_libevent_mode is "internal":
dnl
dnl   * opal_libevent_BUILD_CPPFLAGS - the C Preprocessor flags
dnl         necessary to run the preprocessor on a file which relies
dnl         on Libevent headers.  This will be folded into the global
dnl         CPPFLAGS (see note above).
dnl   * opal_libevent_BUILD_LIBS - the libraries necessary to link
dnl         source which uses Libevent.  Cannot be added to LIBS yet,
dnl         because then other execution tests later in configure
dnl         (there are sadly some) would fail if the path in LDFLAGS
dnl         was not added to LD_LIBRARY_PATH.
dnl   * opal_libevent_WRAPPER_LIBS - the linker flags necessary to
dnl         add to the wrapper compilers in order to link an opal
dnl         application when opal is built as a static library.
AC_DEFUN([OPAL_CONFIG_LIBEVENT], [
    OPAL_VAR_SCOPE_PUSH([internal_libevent_happy external_libevent_happy opal_libevent_STATIC_LDFLAGS opal_libevent_LIBS opal_libevent_STATIC_LIBS])

    opal_show_subtitle "Configuring Libevent"

    OPAL_3RDPARTY_WITH([libevent], [libevent], [package_libevent])

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

    dnl this will work even if there is no libevent package included,
    dnl because libevent_tarball and libevent_directory will evaluate to
    dnl an empty string.  These are relative to the 3rd-party/
    dnl directory.
    OPAL_APPEND([OPAL_3RDPARTY_EXTRA_DIST], [libevent_tarball])
    OPAL_APPEND([OPAL_3RDPARTY_DISTCLEAN_DIRS], [libevent_directory])

    OPAL_WRAPPER_FLAGS_ADD([LDFLAGS], [${opal_libevent_LDFLAGS}])
    OPAL_WRAPPER_FLAGS_ADD([STATIC_LDFLAGS], [${opal_libevent_STATIC_LDFLAGS}])
    OPAL_WRAPPER_FLAGS_ADD([LIBS], [${opal_libevent_LIBS}])
    OPAL_WRAPPER_FLAGS_ADD([STATIC_LIBS], [${opal_libevent_STATIC_LIBS}])
    OPAL_WRAPPER_FLAGS_ADD([PC_MODULES], [${opal_libevent_PC_MODULES}])

    AC_CONFIG_COMMANDS_PRE([OPAL_CONFIG_LIBEVENT_INTERNAL_LIBS_HANDLER])

    OPAL_SUMMARY_ADD([Miscellaneous], [libevent], [], [$opal_libevent_mode])

    OPAL_VAR_SCOPE_POP
])


dnl _OPAL_CONFIG_LIBEVENT_EXTERNAL(action-if-happy, action-if-not-happy)
dnl
dnl only safe to call from OPAL_CONFIG_LIBEVENT, assumes variables
dnl from there are set.
AC_DEFUN([_OPAL_CONFIG_LIBEVENT_EXTERNAL], [
    OPAL_VAR_SCOPE_PUSH([opal_event_min_version opal_event_min_num_version opal_libevent_CPPFLAGS_save opal_libevent_LDFLAGS_save opal_libevent_LIBS_save opal_libevent_external_support])

    dnl Look at libevent_core, not libevent_pthread, because
    dnl trying to avoid picking up libevent.so.  The wrappers and
    dnl ompi.pc will include the -levent_pthreads, so we're
    dnl still good from a linking perspective.
    m4_define([libevent_pkgconfig_module], [libevent_core])
    OAC_CHECK_PACKAGE([libevent],
                      [opal_libevent],
                      [event2/event.h],
                      [event_core],
                      [event_config_new],
                      [opal_libevent_external_support=yes],
                      [opal_libevent_external_support=no])
    dnl Manually add libevent_pthreads.
    OPAL_APPEND([opal_libevent_LIBS], [-levent_pthreads])

    # need these set for the tests below.
    opal_libevent_CPPFLAGS_save=$CPPFLAGS
    OPAL_FLAGS_APPEND_UNIQ([CPPFLAGS], [$opal_libevent_CPPFLAGS])

    # verify libevent is configured with thread support
    AS_IF([test "$opal_libevent_external_support" = "yes"],
          [AC_CACHE_CHECK([if libevent threads enabled],
              [opal_libevent_cv_threads_enabled],
              [# Check for general threading support
               AC_COMPILE_IFELSE([AC_LANG_PROGRAM([
#include <event.h>
#include <event2/thread.h>
               ], [[
#if !(EVTHREAD_LOCK_API_VERSION >= 1)
#  error "No threads!"
#endif
               ]])],
              [opal_libevent_cv_threads_enabled="yes"],
              [opal_libevent_cv_threads_enabled="no"])])
           AS_IF([test "${opal_libevent_cv_threads_enabled}" = "no"],
                 [AC_MSG_WARN([Open MPI requires libevent to be compiled with thread support enabled])
                  opal_libevent_external_support="no"])])

    AS_IF([test "$opal_libevent_external_support" = "yes"],
          [AC_CACHE_CHECK([for libevent pthreads support],
              [opal_libevent_cv_pthread_support],
              [AC_COMPILE_IFELSE([AC_LANG_PROGRAM([
#include <event.h>
#include <event2/thread.h>
               ], [[
#if !defined(EVTHREAD_USE_PTHREADS_IMPLEMENTED) || !EVTHREAD_USE_PTHREADS_IMPLEMENTED
#  error "No pthreads!"
#endif
               ]])],
              [opal_libevent_cv_pthread_support="yes"],
              [opal_libevent_cv_pthread_support="no"])])
           AS_IF([test "${opal_libevent_cv_pthread_support}" = "no"],
                 [AC_MSG_WARN([PMIX requires libevent to be compiled with pthread support enabled])
                  opal_libevent_external_support="no"])])

    # Open MPI used to fall back to the internal libevent if the
    # installed version was older than the internal version.  This
    # isn't what we want, because we really want to prefer external
    # versions.  Pin the "oldest supported" external version to
    # 2.0.21, which we know works from testing on RHEL7.
    opal_event_min_num_version=OMPI_EVENT_NUMERIC_MIN_VERSION
    opal_event_min_version=OMPI_EVENT_MIN_VERSION
    AS_IF([test "$opal_libevent_external_support" = "yes"],
          [AC_CACHE_CHECK([if external libevent version is OMPI_EVENT_MIN_VERSION or greater],
              [opal_libevent_cv_version_check],
              [AC_COMPILE_IFELSE(
                  [AC_LANG_PROGRAM([[#include <event2/event.h>]],
                                   [[
#if defined(_EVENT_NUMERIC_VERSION) && _EVENT_NUMERIC_VERSION < $opal_event_min_num_version
#error "libevent API version is less than $opal_event_min_version"
#elif defined(EVENT__NUMERIC_VERSION) && EVENT__NUMERIC_VERSION < $opal_event_min_num_version
#error "libevent API version is less than $opal_event_min_version"
#endif
                                    ]])],
                  [opal_libevent_cv_version_check="yes"],
                  [opal_libevent_cv_version_check="no"])])
          AS_IF([test "${opal_libevent_cv_version_check}" = "no"],
                [AC_MSG_WARN([external libevent version is too old (OMPI_EVENT_MIN_VERSION or later required)])
                 opal_libevent_external_support=no])])

    CPPFLAGS="$opal_libevent_CPPFLAGS_save"

    AS_IF([test "$opal_libevent_external_support" = "yes"],
          [dnl Do not add libevent flags until late, because
           dnl it will screw up other tests (like the pthread tests)
           opal_libevent_BUILD_LIBS="${opal_libevent_LIBS}"

           $1],
          [$2])

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

    AS_IF([test ${subconfig_happy} -eq 1],
        [internal_libevent_location="3rd-party/libevent_directory"

         # The tarball as configured can not be used for compile
         # tests, because libevent uses a Makefile rule rather than
         # Autoconf to generate their config file (sigh).  Force
         # generation of that file now, so that other 3rd party
         # packages can run compile tests.
         AC_MSG_NOTICE([Generating Libevent's event-config.h])
         (cd $OMPI_TOP_BUILDDIR/$internal_libevent_location/ ; ${MAKE-make} include/event2/event-config.h)
         AS_IF([test $? -ne 0], [AC_MSG_ERROR([Could not generate event-config.h.])])

         # because we don't use pkg-config with libevent, have to
         # guess at many of these fields (and likely get static
         # versions wrong).
         opal_libevent_LDFLAGS=
         opal_libevent_STATIC_LDFLAGS=
         opal_libevent_LIBS="-levent_core -levent_pthreads"
         opal_libevent_STATIC_LIBS=

         AC_MSG_CHECKING([for internal libevent LIBS])
         AC_MSG_RESULT([${opal_libevent_LIBS}])

         # note: because we only ship/commit a tarball (and not the source
         # directory), the source is always expanded in the builddir, so we
         # only need to add a -I to the builddir.
         opal_libevent_CPPFLAGS="-I$OMPI_TOP_BUILDDIR/$internal_libevent_location -I$OMPI_TOP_BUILDDIR/$internal_libevent_location/include"
         opal_libevent_BUILD_CPPFLAGS="${opal_libevent_CPPFLAGS}"

         # No need to update LDFLAGS, because they will install into
         # our tree and in the mean time are referenced by their .la
         # files.
         opal_libevent_BUILD_LIBS="$OMPI_TOP_BUILDDIR/$internal_libevent_location/libevent_core.la $OMPI_TOP_BUILDDIR/$internal_libevent_location/libevent_pthreads.la"
         opal_libevent_WRAPPER_LIBS="${opal_libevent_LIBS}"

         # no need to add to DIST_SUBDIRS, because we only ship the
         # tarball.  This is relative to the 3rd-party/ directory.
         OPAL_3RDPARTY_SUBDIRS="$OPAL_3RDPARTY_SUBDIRS libevent_directory"

         $1], [$2])

    OPAL_VAR_SCOPE_POP
])


dnl We need to delay adding .la files to LIBS until the very end of
dnl configure, to avoid pulling it into other configure tests.
AC_DEFUN([OPAL_CONFIG_LIBEVENT_INTERNAL_LIBS_HANDLER], [
    OPAL_FLAGS_APPEND_UNIQ([CPPFLAGS], [${opal_libevent_CPPFLAGS}])
    OPAL_FLAGS_APPEND_UNIQ([LDFLAGS], [${opal_libevent_LDFLAGS}])
    OPAL_FLAGS_APPEND_MOVE([LIBS], [${opal_libevent_BUILD_LIBS}])
])
