dnl -*- autoconf -*-
dnl
dnl Copyright (c) 2009-2017 Cisco Systems, Inc.  All rights reserved
dnl Copyright (c) 2014-2018 Research Organization for Information Science
dnl                         and Technology (RIST). All rights reserved.
dnl Copyright (c) 2020-2021 Amazon.com, Inc. or its affiliates.  All Rights
dnl                         reserved.
dnl Copyright (c) 2020      Intel, Inc.  All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

dnl Check for / configure hwloc package.  Prefer finding an
dnl external hwloc, build our internal one if required.  If we can
dnl not find an external hwloc and the internal one fails to
dnl configure, abort.
dnl
dnl This macro will change the environment in the following way:
dnl
dnl   * opal_hwloc_header [legacy] - will be set if building
dnl         internally, to the header file that should be included for
dnl         embedded builds.  This is used by PRRTE, but should not
dnl         be used by new code.
dnl   * opal_hwloc_mode - either external or internal.  If internal,
dnl         --with-hwloc should be ignored by other packages
dnl   * opal_hwloc_CPPFLAGS - the C Preprocessor flags necessary to
dnl         run the preprocessor on a file which relies on Hwloc
dnl         headers.  This will be folded into the global CPPFLAGS,
dnl         so most people should never need this.
dnl   * opal_hwloc_LDFLAGS - the linker flags necessary to run the
dnl         linker on a file which relies on Hwloc libraries.  This
dnl         will be folded into the global CPPFLAGS, so most people
dnl         should never need this.
dnl   * opal_hwloc_LIBS - the libraries necessary to link source which
dnl         uses Hwloc.  Cannot be added to LIBS yet, because then
dnl         other execution tests later in configure (there are sadly
dnl         some) would fail if the path in LDFLAGS was not added to
dnl         LD_LIBRARY_PATH.
dnl   * opal_hwloc_WRAPPER_LDFLAGS - the linker flags necessary to
dnl         add to the wrapper compilers in order to link an opal
dnl         application when opal is built as a static library.
dnl   * opal_hwloc_WRAPPER_LIBS - the linker flags necessary to
dnl         add to the wrapper compilers in order to link an opal
dnl         application when opal is built as a static library.
dnl   * CPPFLAGS, LDFLAGS - Updated opal_hwloc_CPPFLAGS and
dnl         opal_hwloc_LDFLAGS.
AC_DEFUN([OPAL_CONFIG_HWLOC], [
    OPAL_VAR_SCOPE_PUSH([external_hwloc_happy internal_hwloc_happy pkg_config_file pkg_config_happy pkg_config_ldflags pkg_config_libs])

    opal_show_subtitle "Configuring hwloc"

    OPAL_3RDPARTY_WITH([hwloc], [hwloc], [package_hwloc])

    opal_hwloc_header=""

    # unless internal specifically requested by the user, try to find
    # an external that works.
    external_hwloc_happy=0
    AS_IF([test "$opal_hwloc_mode" != "internal"],
          [_OPAL_CONFIG_HWLOC_EXTERNAL(
              [external_hwloc_happy=1
               opal_hwloc_mode="external"],
              [external_hwloc_happy=0
               AS_IF([test "$opal_hwloc_mode" = "external"],
                     [AC_MSG_ERROR([External hwloc requested but not found.])])])])

    internal_hwloc_happy=0
    m4_ifdef([package_hwloc],
        [AS_IF([test "$external_hwloc_happy" = "0"],
             [_OPAL_CONFIG_HWLOC_INTERNAL([internal_hwloc_happy=1
                                              opal_hwloc_mode="internal"])])])

    AS_IF([test "$external_hwloc_happy" = "0" -a "$internal_hwloc_happy" = "0"],
          [AC_MSG_ERROR([Could not find viable hwloc build.])])

    AS_IF([test "$opal_hwloc_mode" = "internal"],
          [pkg_config_file="${OMPI_TOP_BUILDDIR}/3rd-party/hwloc_directory/hwloc.pc"
           PKG_CONFIG_PATH="${OMPI_TOP_BUILDDIR}/3rd-party/hwloc_directory:${PKG_CONFIG_PATH}"],
          [test -n "$with_hwloc"],
          [pkg_config_file="${with_hwloc}/lib/pkgconfig/hwloc.pc"
           PKG_CONFIG_PATH="${with_hwloc}/lib/pkgconfig:${PKG_CONFIG_PATH}"],
          [pkg_config_file="hwloc"])

    pkg_config_happy=1
    OPAL_GET_LDFLAGS_FROM_PC([$pkg_config_file], [pkg_config_ldflags], [pkg_config_happy=0])
    OPAL_GET_LIBS_FROM_PC([$pkg_config_file], [pkg_config_libs], [pkg_config_happy=0])

    AS_IF([test $pkg_config_happy -ne 0],
          [opal_hwloc_WRAPPER_LDFLAGS="$pkg_config_ldflags"
           opal_hwloc_WRAPPER_LIBS="$pkg_config_libs"],
          [# guess that what we have from compiling OMPI is good enough
           AS_IF([test -z "$opal_hwloc_WRAPPER_LDFLAGS"],
                 [opal_hwloc_WRAPPER_LDFLAGS="$opal_hwloc_LDFLAGS"])
           AS_IF([test -z "$opal_hwloc_WRAPPER_LIBS"],
                 [opal_hwloc_WRAPPER_LIBS="$opal_hwloc_LIBS"])])

    OPAL_WRAPPER_FLAGS_ADD([LDFLAGS], [$opal_hwloc_WRAPPER_LDFLAGS])
    OPAL_WRAPPER_FLAGS_ADD([LIBS], [$opal_hwloc_WRAPPER_LIBS])

    # this will work even if there is no hwloc package included,
    # because hwloc_tarball and hwloc_directory will evaluate to an
    # empty string.  These are relative to the 3rd-party/ directory.
    OPAL_3RDPARTY_EXTRA_DIST="$OPAL_3RDPARTY_EXTRA_DIST hwloc_tarball"
    OPAL_3RDPARTY_DISTCLEAN_DIRS="$OPAL_3RDPARTY_DISTCLEAN_DIRS hwloc_directory"

    AC_SUBST(opal_hwloc_CPPFLAGS)
    AC_SUBST(opal_hwloc_LIBS)
    AC_SUBST(opal_hwloc_LDFLAGS)

    OPAL_SUMMARY_ADD([[Miscellaneous]], [[hwloc]], [hwloc], [$opal_hwloc_mode])

    OPAL_VAR_SCOPE_POP
])

dnl _OPAL_CONFIG_HWLOC_EXTERNAL(action-if-happy, action-if-not-happy)
dnl
dnl only safe to call from OPAL_CONFIG_HWLOC, assumes variables from
dnl there are set.
AC_DEFUN([_OPAL_CONFIG_HWLOC_EXTERNAL], [
    OPAL_VAR_SCOPE_PUSH([opal_hwloc_CPPFLAGS_save opal_hwloc_LDFLAGS_save opal_hwloc_LIBS_save opal_hwloc_external_support])

    opal_hwloc_CPPFLAGS_save=$CPPFLAGS
    opal_hwloc_LDFLAGS_save=$LDFLAGS
    opal_hwloc_LIBS_save=$LIBS

    AS_IF([test ! -z "$with_hwloc_libdir"],
          [OPAL_CHECK_WITHDIR([hwloc-libdir], [$with_hwloc_libdir],
                              [libhwloc.*])])

    OPAL_CHECK_PACKAGE([opal_hwloc],
                       [hwloc.h],
                       [hwloc],
                       [hwloc_topology_init],
                       [],
                       [$with_hwloc],
                       [$with_hwloc_libdir],
                       [opal_hwloc_external_support=yes],
                       [opal_hwloc_external_support=no])

    # need these set for the tests below.  If things fail, will undo at the end.
    CPPFLAGS="$opal_hwloc_CPPFLAGS_save $opal_hwloc_CPPFLAGS"
    LDFLAGS="$opal_hwloc_LDFLAGS_save $opal_hwloc_LDFLAGS"
    LIBS="$opal_hwloc_LIBS_save $opal_hwloc_LIBS"

    AS_IF([test "$opal_hwloc_external_support" = "yes"],
          [AC_MSG_CHECKING([if external hwloc version is 1.11.0 or greater])
           AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <hwloc.h>]],
                               [[
#if HWLOC_API_VERSION < 0x00010500
#error "hwloc API version is less than 0x00011100"
#endif
                               ]])],
                   [AC_MSG_RESULT([yes])],
                   [AC_MSG_RESULT([no])
                    AC_MSG_WARN([external hwloc version is too old (1.11.0 or later required)])
                    opal_hwloc_external_support="no"])])

    AS_IF([test "$opal_hwloc_external_support" = "yes"],
          [AC_CHECK_DECLS([HWLOC_OBJ_OSDEV_COPROC], [], [], [#include <hwloc.h>])
           AC_CHECK_FUNCS([hwloc_topology_dup])])

    LDFLAGS="$opal_hwloc_LDFLAGS_save"
    LIBS="$opal_hwloc_LIBS_save"

    AS_IF([test "$opal_hwloc_external_support" = "yes"],
          [$1],
          [CPPFLAGS="$opal_hwloc_CPPFLAGS_save"
           $2])

    OPAL_VAR_SCOPE_POP
])

dnl _OPAL_CONFIG_HWLOC_INTERNAL(action-if-happy, action-if-not-happy)
dnl
dnl Configure the packaged hwloc.  Only needs to be run if the
dnl external hwloc is not going to be used.  Assumes that if
dnl this function is called, that success means the internal package
dnl will be used.
AC_DEFUN([_OPAL_CONFIG_HWLOC_INTERNAL], [
    OPAL_VAR_SCOPE_PUSH([subconfig_happy internal_hwloc_location extra_configure_args found_enable_plugins hwloc_config_arg])

    extra_configure_args=

    # look for a --{enable/disable}-plugins option in the top level
    # configure arguments, so that we can add --enable-plugins if
    # appropriate.
    found_enable_plugins=0
    eval "set x $ac_configure_args"
    shift
    for hwloc_config_arg
    do
	case $hwloc_config_arg in
        --enable-plugins|--enable-plugins=*|--disable-plugins)
            found_enable_plugins=1
	    ;;
	esac
    done

    # while the plugins in hwloc are not explicitly using Open MPI's dlopen
    # interface, it seems rude to enable plugins in hwloc if the builder asked
    # us not to use plugins in Open MPI.  So only enable plugins in hwloc if there's
    # a chance we're going to do so.  We enable plugins by default so that libhwloc
    # does not end up with a dependency on libcuda, which would mean everything else
    # would end up with a dependency on libcuda (and similar).
    AS_IF([test $found_enable_plugins -eq 0 -a "$enable_dlopen" != "no"],
          [extra_configure_args="--enable-plugins"])

    # Note: To update the version of hwloc shipped, update the
    # constant in autogen.pl.
    OPAL_EXPAND_TARBALL([3rd-party/hwloc_tarball], [3rd-party/hwloc_directory], [configure])
    OPAL_SUBDIR_ENV_CLEAN([opal_hwloc_configure])
    PAC_CONFIG_SUBDIR_ARGS([3rd-party/hwloc_directory], [$extra_configure_args], [[--enable-debug]],
        [subconfig_happy=1], [subconfig_happy=0])
    OPAL_SUBDIR_ENV_RESTORE([opal_hwloc_configure])

    AS_IF([test "$subconfig_happy" = "1"],
        [internal_hwloc_location="3rd-party/hwloc_directory"
         # note: because we only ship/commit a tarball (and not the source
         # directory), the source is always expanded in the builddir, so we
         # only need to add a -I to the builddir.
         opal_hwloc_CPPFLAGS="-I$OMPI_TOP_BUILDDIR/$internal_hwloc_location/include -I$OMPI_TOP_SRCDIR/$internal_hwloc_location/include"
         CPPFLAGS="$CPPFLAGS $opal_hwloc_CPPFLAGS"
         # No need to update LDFLAGS, because they will install into
         # our tree and in the mean time are referenced by their .la
         # files.
         opal_hwloc_LIBS="$OMPI_TOP_BUILDDIR/$internal_hwloc_location/hwloc/libhwloc.la"
         opal_hwloc_WRAPPER_LIBS="-lhwloc"

         opal_hwloc_header="$OMPI_TOP_BUILDDIR/$internal_hwloc_location/include/hwloc.h"

         # no need to add to DIST_SUBDIRS, because we only ship the
         # tarball.  This is relative to the 3rd-party/ directory.
         OPAL_3RDPARTY_SUBDIRS="$OPAL_3RDPARTY_SUBDIRS hwloc_directory"

         $1], [$2])

    OPAL_VAR_SCOPE_POP
])
