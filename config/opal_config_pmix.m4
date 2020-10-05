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
dnl Copyright (c) 2014-2019 Intel, Inc.  All rights reserved.
dnl Copyright (c) 2014-2018 Research Organization for Information Science
dnl                         and Technology (RIST).  All rights reserved.
dnl Copyright (c) 2016      IBM Corporation.  All rights reserved.
dnl Copyright (c) 2020      Triad National Security, LLC. All rights
dnl                         reserved.
dnl Copyright (c) 2020      Amazon.com, Inc. or its affiliates.  All Rights
dnl                         reserved.
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
dnl   * opal_pmix_header [legacy] - will be set if building
dnl         internally, to the header file that should be included for
dnl         embedded builds.  This is used by PRRTE, but should not
dnl         be used by new code.
dnl   * opal_pmix_mode - either external or internal.  If internal,
dnl         --with-pmix should be ignored by other packages
dnl   * opal_pmix_CPPFLAGS - the C Preprocessor flags necessary to
dnl         run the preprocessor on a file which relies on PMIx
dnl         headers.  This will be folded into the global CPPFLAGS,
dnl         so most people should never need this.
dnl   * opal_pmix_LDFLAGS - the linker flags necessary to run the
dnl         linker on a file which relies on PMIx libraries.  This
dnl         will be folded into the global CPPFLAGS, so most people
dnl         should never need this.
dnl   * opal_pmix_LIBS - the libraries necessary to link source which
dnl         uses PMIx.  Cannot be added to LIBS yet, because then
dnl         other execution tests later in configure (there are sadly
dnl         some) would fail if the path in LDFLAGS was not added to
dnl         LD_LIBRARY_PATH.
dnl   * CPPFLAGS, LDFLAGS - Updated opal_pmix_CPPFLAGS and
dnl         opal_pmix_LDFLAGS.
AC_DEFUN([OPAL_CONFIG_PMIX], [
    OPAL_VAR_SCOPE_PUSH([external_pmix_happy internal_pmix_happy internal_pmix_args internal_pmix_libs])

    opal_show_subtitle "Configuring PMIx"

    OPAL_3RDPARTY_WITH([pmix], [pmix], [package_pmix])

    opal_pmix_header=""

    internal_pmix_happy=0
    m4_ifdef([package_pmix],
        [# always configure the internal pmix, so that
         # make dist always works.
	 internal_pmix_args="--without-tests-examples --disable-pmix-binaries --disable-pmix-backward-compatibility --disable-visibility"
         internal_pmix_libs=

         AS_IF([test "$opal_libevent_mode" = "internal"],
               [internal_pmix_args="$internal_pmix_args --with-libevent=cobuild"
                internal_pmix_libs="$internal_pmix_libs $opal_libevent_LIBS"])

         AS_IF([test "$opal_hwloc_mode" = "internal"],
               [internal_pmix_args="$internal_pmix_args --with-hwloc=cobuild"
                internal_pmix_libs="$internal_pmix_libs $opal_hwloc_LIBS"])

         AS_IF([test ! -z "$internal_pmix_libs"],
               [internal_pmix_args="$internal_pmix_args --with-prte-extra-lib=\"$internal_pmix_libs\""])

         # Pass all our compiler/linker flags to PMIx, so that it
         # picks up how to build an internal HWLOC and libevent, plus
         # picks up any user-specified compiler flags from the master
         # configure run.
         export CFLAGS CPPFLAGS LDFLAGS
         PAC_CONFIG_SUBDIR_ARGS([3rd-party/openpmix], [$internal_pmix_args],
                           [[--with-libevent=internal], [--with-hwloc=internal],
                            [--with-libevent=external], [--with-hwloc=external],
                            [--with-pmix=[[^ 	]]*], [--with-platform=[[^ 	]]*]],
                           [internal_pmix_happy=1])
         OPAL_3RDPARTY_DIST_SUBDIRS="$OPAL_3RDPARTY_DIST_SUBDIRS openpmix"])

    # unless internal specifically requested by the user, try to find
    # an external that works.
    external_pmix_happy=0
    AS_IF([test "opal_pmix_mode" != "internal"],
          [_OPAL_CONFIG_PMIX_EXTERNAL(
              [external_pmix_happy=1
               opal_pmix_mode="external"],
              [AS_IF([test "$opal_pmix_mode" = "external"],
                     [AC_MSG_ERROR([External PMIx requested but not found.])])])])

    # external did not work out and customer did not specify external,
    # so try the internal version.
    AS_IF([test "$external_pmix_happy" = "0" -a "$internal_pmix_happy" = "1"],
          [opal_pmix_mode="internal"
           _OPAL_CONFIG_PMIX_INTERNAL_POST()])

    AS_IF([test "$external_pmix_happy" = "0" -a "$internal_pmix_happy" = "0"],
          [AC_MSG_ERROR([Could not find viable pmix build.])])

    AC_SUBST(opal_pmix_CPPFLAGS)
    AC_SUBST(opal_pmix_LDFLAGS)
    AC_SUBST(opal_pmix_LIBS)

    OPAL_SUMMARY_ADD([[Miscellaneous]], [[pmix]], [pmix], [$opal_pmix_mode])

    OPAL_VAR_SCOPE_POP
])

dnl _OPAL_CONFIG_PMIX_EXTERNAL(action-if-happy, action-if-not-happy)
dnl
dnl only safe to call from OPAL_CONFIG_PMIX, assumes variables from
dnl there are set.
AC_DEFUN([_OPAL_CONFIG_PMIX_EXTERNAL], [
    OPAL_VAR_SCOPE_PUSH([opal_pmix_CPPFLAGS_save opal_pmix_LDFLAGS_save opal_pmix_LIBS_save opal_pmix_external_support])

    opal_pmix_CPPFLAGS_save=$CPPFLAGS
    opal_pmix_LDFLAGS_save=$LDFLAGS
    opal_pmix_LIBS_save=$LIBS

    opal_pmix_external_support="yes"

    AS_IF([test "$opal_libevent_mode" = "internal" -o "$opal_hwloc_mode" = "internal"],
          [opal_pmix_external_support="no"
           AS_IF([test "$opal_pmix_mode" = "external"],
                 [AC_MSG_ERROR([Building against an external PMIx with an internal Libevent or HWLOC is unsupported.  Cannot continue.])])],
          [AS_IF([test ! -z "$with_pmix_libdir"],
                 [OPAL_CHECK_WITHDIR([pmix-libdir], [$with_pmix_libdir],
                                     [libpmix.*])])

           OPAL_CHECK_PACKAGE([opal_pmix],
                              [pmix.h],
                              [pmix],
                              [PMIx_Init],
                              [],
                              [$with_pmix],
                              [$with_pmix_libdir],
                              [],
                              [opal_pmix_external_support=no])

           # need these set for the tests below.  If things fail, will undo at the end.
           CPPFLAGS="$opal_pmix_CPPFLAGS_save $opal_pmix_CPPFLAGS"
           LDFLAGS="$opal_pmix_LDFLAGS_save $opal_pmix_LDFLAGS"
           LIBS="$opal_pmix_LIBS_save $opal_pmix_LIBS"

           AS_IF([test "$opal_pmix_external_support" = "yes"],
                 [AC_MSG_CHECKING([if external PMIx version is 3.0.0 or greater])
                    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <pmix_version.h>]],
                          [[
#if PMIX_VERSION_MAJOR < 3L
#error "pmix API version is less than 3.0.0"
#endif
                          ]])],
                          [AC_MSG_RESULT([yes])],
                          [AC_MSG_RESULT([no])
                           opal_pmix_external_support=no])])

           LDFLAGS="$opal_pmix_LDFLAGS_save"
           LIBS="$opal_pmix_LIBS_save"

           AS_IF([test "$opal_pmix_external_support" = "yes"],
                 [$1],
                 [CPPFLAGS="$opal_pmix_CPPFLAGS_save"
                  $2])])

    OPAL_VAR_SCOPE_POP
])

dnl _OPAL_CONFIG_PMIX_INTERNAL_POST()
dnl
dnl Expectation is that this is called only if external fails, the
dnl caller configured libpmix configure, and the configure script
dnl succeeded.
AC_DEFUN([_OPAL_CONFIG_PMIX_INTERNAL_POST], [
    opal_pmix_CPPFLAGS="-I$OMPI_TOP_BUILDDIR/3rd-party/openpmix/include -I$OMPI_TOP_SRCDIR/3rd-party/openpmix/include"
    opal_pmix_LDFLAGS=""
    opal_pmix_LIBS="$OMPI_TOP_BUILDDIR/3rd-party/openpmix/src/libpmix.la"

    CPPFLAGS="$CPPFLAGS $opal_pmix_CPPFLAGS"

    opal_pmix_header="$OMPI_TOP_SRCDIR/opal/mca/pmix/pmix-3rdparty.h"

    OPAL_3RDPARTY_SUBDIRS="$OPAL_3RDPARTY_SUBDIRS openpmix"
])
