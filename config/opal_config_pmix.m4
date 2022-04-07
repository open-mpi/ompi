dnl -*- autoconf -*-
dnl
dnl Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
dnl                         University Research and Technology
dnl                         Corporation.  All rights reserved.
dnl Copyright (c) 2004-2005 The University of Tennessee and The University
dnl                         of Tennessee Research Foundation.  All rights
dnl                         reserved.
dnl Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
dnl                         University of Stuttgart.  All rights reserved.
dnl Copyright (c) 2004-2005 The Regents of the University of California.
dnl                         All rights reserved.
dnl Copyright (c) 2009-2019 Cisco Systems, Inc.  All rights reserved.
dnl Copyright (c) 2011-2014 Los Alamos National Security, LLC. All rights
dnl                         reserved.
dnl Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
dnl Copyright (c) 2014-2021 Research Organization for Information Science
dnl                         and Technology (RIST).  All rights reserved.
dnl Copyright (c) 2016-2021 IBM Corporation.  All rights reserved.
dnl Copyright (c) 2020      Triad National Security, LLC. All rights
dnl                         reserved.
dnl Copyright (c) 2020-2022 Amazon.com, Inc. or its affiliates.  All Rights reserved.
dnl Copyright (c) 2021      Nanook Consulting.  All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

dnl Check for / configure PMIx package.  Prefer finding an
dnl external PMIx, build our internal one if required.  If we can
dnl not find an external PMIx and the internal one fails to
dnl configure, abort.
dnl
dnl This macro will change the environment in the following way:
dnl
dnl   * opal_pmix_mode - either external or internal.  If internal,
dnl         --with-pmix should be ignored by other packages
dnl   * CPPFLAGS, LDFLAGS, LIBS - Updated to build against PMIx.
dnl         Note that the values may be updated right before
dnl         config.status, to avoid interfering with other tests.
dnl
dnl OPAL_WRAPPER_FLAGS_ADD will be called to add the correct LDFLAGS,
dnl STATIC_LDFLAGS, LIBS, and STATIC_LIBS for PMIx.
dnl
dnl The following environment variables will only be set if
dnl opal_pmix_mode is "internal":
dnl
dnl   * opal_pmix_BUILD_CPPFLAGS - the C Preprocessor flags necessary to
dnl         run the preprocessor on a file which relies on PMIx
dnl         headers.  This will be folded into the global
dnl         CPPFLAGS (see note above).
dnl   * opal_pmix_BUILD_LIBS - the libraries necessary to link source which
dnl         uses PMIx.  Cannot be added to LIBS yet, because then
dnl         other execution tests later in configure (there are sadly
dnl         some) would fail if the path in LDFLAGS was not added to
dnl         LD_LIBRARY_PATH.
AC_DEFUN([OPAL_CONFIG_PMIX], [
    OPAL_VAR_SCOPE_PUSH([external_pmix_happy internal_pmix_happy internal_pmix_args internal_pmix_wrapper_libs internal_pmix_CPPFLAGS opal_pmix_STATIC_LDFLAGS opal_pmix_LIBS opal_pmix_STATIC_LIBS])

    opal_show_subtitle "Configuring PMIx"

    OPAL_3RDPARTY_WITH([pmix], [pmix], [package_pmix])

    internal_pmix_happy=0
    m4_ifdef([package_pmix],
        [OMPI_PMIX_ADD_ARGS
         AS_IF([test "$opal_pmix_mode" = "unspecified" -o "$opal_pmix_mode" = "internal"],
               [# Run PMIx's configure script unless the user
                # explicitly asked us to use an external PMIX, so that
                # "make dist" includes PMIx in the dist tarball.  This
                # does mean that "make dist" will not work if Open MPI
                # was configured to use an external PMIx library, but
                # we decided this was a reasonable tradeoff for not
                # having to deal with PMIx (or PRRTE) potentially
                # failing to configure in a situation where it isn't
                # desired.

                internal_pmix_args="--without-tests-examples --enable-pmix-binaries --disable-pmix-backward-compatibility --disable-visibility"
                internal_pmix_wrapper_libs=
                internal_pmix_CPPFLAGS=

                AS_IF([test "$opal_libevent_mode" = "internal"],
                      [OPAL_APPEND([internal_pmix_args], [--with-libevent --disable-libevent-lib-checks])
                       OPAL_APPEND([internal_pmix_args], [--with-libevent-extra-libs=\"$opal_libevent_BUILD_LIBS\"])
                       OPAL_APPEND([internal_pmix_wrapper_libs], [$opal_libevent_WRAPPER_LIBS])
                       OPAL_APPEND([internal_pmix_CPPFLAGS], [$opal_libevent_BUILD_CPPFLAGS])])

                AS_IF([test "$opal_hwloc_mode" = "internal"],
                      [OPAL_APPEND([internal_pmix_args], [--disable-hwloc-lib-checks])
                       OPAL_APPEND([internal_pmix_args], [--with-hwloc-extra-libs=\"$opal_hwloc_BUILD_LIBS\"])
                       OPAL_APPEND([internal_pmix_wrapper_libs], [$opal_hwloc_WRAPPER_BUILD_LIBS])
                       OPAL_APPEND([internal_pmix_CPPFLAGS], [$opal_hwloc_BUILD_CPPFLAGS])])

                if test "$WANT_DEBUG" = "1"; then
                     OPAL_APPEND([internal_pmix_args], [--enable-debug])
                fi

                # Pass all our compiler/linker flags to PMIx, so that it
                # picks up how to build an internal HWLOC and libevent, plus
                # picks up any user-specified compiler flags from the master
                # configure run.
                OPAL_SUBDIR_ENV_CLEAN([opal_pmix_configure])
                AS_IF([test -n "$internal_pmix_CPPFLAGS"],
                      [OPAL_SUBDIR_ENV_APPEND([CPPFLAGS], [$internal_pmix_CPPFLAGS])])
                AS_IF([test -n "$internal_pmix_wrapper_libs"],
                      [OPAL_APPEND([internal_pmix_args], [--with-wrapper-libs=\"$internal_pmix_wrapper_libs\"])])

                PAC_CONFIG_SUBDIR_ARGS([3rd-party/openpmix], [$internal_pmix_args],
                                       [[--with-libevent=internal], [--with-hwloc=internal],
                                        [--with-libevent=external], [--with-hwloc=external],
                                        [--with-pmix=[[^ 	]]*], [--with-platform=[[^ 	]]*]],
                                       [internal_pmix_happy=1])
                OPAL_SUBDIR_ENV_RESTORE([opal_pmix_configure])
                OPAL_3RDPARTY_DIST_SUBDIRS="$OPAL_3RDPARTY_DIST_SUBDIRS openpmix"])

         # if we have a pmix package and configure did not complete
         # successfullly (or wasn't started), then disable make dist.
         AS_IF([test $internal_pmix_happy != 1],
               [OPAL_MAKEDIST_DISABLE="$OPAL_MAKEDIST_DISABLE PMIX"])])

    # unless internal specifically requested by the user, try to find
    # an external that works.
    external_pmix_happy=0
    AS_IF([test "$opal_pmix_mode" != "internal"],
          [_OPAL_CONFIG_PMIX_EXTERNAL(
              [external_pmix_happy=1
               opal_pmix_mode="external"],
              [AS_IF([test "$opal_pmix_mode" = "external"],
                     [AC_MSG_ERROR([External PMIx requested but not found.])])])])

    # external did not work out and customer did not specify external,
    # so try the internal version.
    AS_IF([test "$external_pmix_happy" = "0" -a "$internal_pmix_happy" = "1"],
          [opal_pmix_mode="internal"
           OPAL_USING_INTERNAL_PMIX=1
           _OPAL_CONFIG_PMIX_INTERNAL_POST()],
          [OPAL_USING_INTERNAL_PMIX=0])

    AS_IF([test "$external_pmix_happy" = "0" -a "$internal_pmix_happy" = "0"],
          [AC_MSG_ERROR([Could not find viable pmix build.])])

    OPAL_WRAPPER_FLAGS_ADD([LDFLAGS], [${opal_pmix_LDFLAGS}])
    OPAL_WRAPPER_FLAGS_ADD([STATIC_LDFLAGS], [${opal_pmix_STATIC_LDFLAGS}])
    OPAL_WRAPPER_FLAGS_ADD([LIBS], [${opal_pmix_LIBS}])
    OPAL_WRAPPER_FLAGS_ADD([STATIC_LIBS], [${opal_pmix_STATIC_LIBS}])
    OPAL_WRAPPER_FLAGS_ADD([PC_MODULES], [${opal_pmix_PC_MODULES}])

    AC_CONFIG_COMMANDS_PRE([OPAL_CONFIG_PMIX_INTERNAL_LIBS_HANDLER])

    AC_DEFINE_UNQUOTED([OPAL_USING_INTERNAL_PMIX],
                       [$OPAL_USING_INTERNAL_PMIX],
                       [Whether or not we are using the internal PMIx])

    OPAL_SUMMARY_ADD([Miscellaneous], [pmix], [], [$opal_pmix_mode])

    OPAL_VAR_SCOPE_POP
])

dnl _OPAL_CONFIG_PMIX_EXTERNAL(action-if-happy, action-if-not-happy)
dnl
dnl only safe to call from OPAL_CONFIG_PMIX, assumes variables from
dnl there are set.
AC_DEFUN([_OPAL_CONFIG_PMIX_EXTERNAL], [
    OPAL_VAR_SCOPE_PUSH([opal_pmix_CPPFLAGS_save opal_pmix_LDFLAGS_save opal_pmix_LIBS_save opal_pmix_external_support])

    opal_pmix_external_support="yes"

    AS_IF([test "$opal_libevent_mode" = "internal" -o "$opal_hwloc_mode" = "internal"],
          [opal_pmix_external_support="no"
           AS_IF([test "$opal_pmix_mode" = "external"],
                 [AC_MSG_ERROR([Building against an external PMIx with an internal Libevent or HWLOC is unsupported.  Cannot continue.])])],

          [dnl Need to explicitly enable wrapper compiler to get the dependent libraries
           dnl when pkg-config is not available.
           pmix_USE_WRAPPER_COMPILER=1
           OAC_CHECK_PACKAGE([pmix],
                             [opal_pmix],
                             [pmix.h],
                             [pmix],
                             [PMIx_Init],
                             [],
                             [opal_pmix_external_support=no])

           opal_pmix_CPPFLAGS_save=$CPPFLAGS
           opal_pmix_LDFLAGS_save=$LDFLAGS
           opal_pmix_LIBS_save=$LIBS

           CPPFLAGS="$opal_pmix_CPPFLAGS_save $opal_pmix_CPPFLAGS"
           LDFLAGS="$opal_pmix_LDFLAGS_save $opal_pmix_LDFLAGS"
           LIBS="$opal_pmix_LIBS_save $opal_pmix_LIBS"

           AS_IF([test "$opal_pmix_external_support" = "yes"],
                 [AC_MSG_CHECKING([if external PMIx version is 3.1.5 or greater])
                    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <pmix_version.h>
                          ]], [[
#if PMIX_NUMERIC_VERSION < 0x00030105
#error "pmix API version is less than 3.1.5"
#endif
                          ]])],
                          [AC_MSG_RESULT([yes])],
                          [AC_MSG_RESULT([no])
                           opal_pmix_external_support=no])])

           CPPFLAGS="$opal_pmix_CPPFLAGS_save"
           LDFLAGS="$opal_pmix_LDFLAGS_save"
           LIBS="$opal_pmix_LIBS_save"

           AS_IF([test "$opal_pmix_external_support" = "yes"],
                 [dnl Do not add libevent libs to LIBS until late, because
                  dnl it will screw up other tests (like the pthread tests)
                  opal_pmix_BUILD_LIBS="${opal_pmix_LIBS}"

                  $1],
                 [$2])])

    OPAL_VAR_SCOPE_POP
])

dnl _OPAL_CONFIG_PMIX_INTERNAL_POST()
dnl
dnl Expectation is that this is called only if external fails, the
dnl caller configured libpmix configure, and the configure script
dnl succeeded.
AC_DEFUN([_OPAL_CONFIG_PMIX_INTERNAL_POST], [
    dnl pkg-config might not always be available, and we always need
    dnl to pull in the relevant flags.  We parse the compiler wrapper
    dnl config file all the time rather than use pkg-config when
    dnl available, so that we have a smaller testing surface.
    datafile="${OMPI_TOP_BUILDDIR}/3rd-party/openpmix/src/tools/wrapper/pmixcc-wrapper-data.txt"
    AS_IF([test ! -f "${datafile}"],
          [AC_MSG_ERROR([Cannot find PMIx wrapper compiler data.  Aborting])])

    pmix_internal_happy=1

    dnl Don't pull LDFLAGS, because we don't have a good way to avoid
    dnl a -L to our install directory, which can cause some weirdness
    dnl if there's an old OMPI install there.  And it makes filtering
    dnl redundant flags easier.
    opal_pmix_LDFLAGS=

    AC_CACHE_CHECK([for internal pmix static LDFLAGS],
                   [opal_internal_pmix_cv_STATIC_LDFLAGS],
                   [_OPAL_CONFIG_PMIX_GET_WRAPPER_FIELD([${datafile}], [linker_flags_static],
                                                        [opal_internal_pmix_cv_STATIC_LDFLAGS],
                                                        [], [pmix_internal_happy=0])])
    opal_pmix_STATIC_LDFLAGS="${opal_internal_pmix_cv_STATIC_LDFLAGS}"

    AC_CACHE_CHECK([for internal pmix LIBS],
                   [opal_internal_pmix_cv_LIBS],
                   [_OPAL_CONFIG_PMIX_GET_WRAPPER_FIELD([${datafile}], [libs],
                                                        [opal_internal_pmix_cv_LIBS],
                                                        [], [pmix_internal_happy=0])])
    opal_pmix_LIBS="${opal_internal_pmix_cv_LIBS}"

    AC_CACHE_CHECK([for internal pmix static LIBS],
                   [opal_internal_pmix_cv_STATIC_LIBS],
                   [_OPAL_CONFIG_PMIX_GET_WRAPPER_FIELD([${datafile}], [libs_static],
                                                        [opal_internal_pmix_cv_STATIC_LIBS],
                                                        [], [pmix_internal_happy=0])])
    opal_pmix_STATIC_LIBS="${opal_internal_pmix_cv_STATIC_LIBS}"

    AC_MSG_CHECKING([for internal pmix pkg-config module])
    opal_pmix_PC_MODULES="pmix"
    AC_MSG_RESULT([${opal_pmix_PC_MODULES}])

    AS_IF([test ${pmix_internal_happy} -eq 0],
          [AC_MSG_ERROR([Failed to retrieve PMIx wrapper data.  Aborting.])])

    # Overwrite the wrapper data results for CPPFLAGS, because it's
    # the install dir location, not the build location.
    opal_pmix_CPPFLAGS="-I$OMPI_TOP_BUILDDIR/3rd-party/openpmix/include -I$OMPI_TOP_SRCDIR/3rd-party/openpmix/include -I$OMPI_TOP_BUILDDIR/3rd-party/openpmix/ -I$OMPI_TOP_SRCDIR/3rd-party/openpmix/"
    opal_pmix_BUILD_CPPFLAGS="${opal_pmix_CPPFLAGS}"

    opal_pmix_BUILD_LIBS="$OMPI_TOP_BUILDDIR/3rd-party/openpmix/src/libpmix.la"

    OPAL_3RDPARTY_SUBDIRS="$OPAL_3RDPARTY_SUBDIRS openpmix"
])


AC_DEFUN([_OPAL_CONFIG_PMIX_GET_WRAPPER_FIELD], [
    OAC_LOG_COMMAND([wrapper_field_results=`sed -ne 's/$2=\(.*\)/\1/p' $1 2>&1`],
                    [AS_VAR_COPY([$3], [wrapper_field_results])
                     $4],
                    [$5])
    OAC_LOG_MSG([wrapper field output: $2: ${wrapper_field_results}], [1])
])


dnl We need to delay adding .la files to LIBS until the very end of
dnl configure, to avoid pulling it into other configure tests.
AC_DEFUN([OPAL_CONFIG_PMIX_INTERNAL_LIBS_HANDLER], [
    OPAL_FLAGS_APPEND_UNIQ([CPPFLAGS], [${opal_pmix_CPPFLAGS}])
    OPAL_FLAGS_APPEND_UNIQ([LDFLAGS], [${opal_pmix_LDFLAGS}])
    OPAL_FLAGS_APPEND_MOVE([LIBS], [${opal_pmix_BUILD_LIBS}])
])
