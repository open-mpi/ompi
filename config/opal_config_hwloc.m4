dnl -*- autoconf -*-
dnl
dnl Copyright (c) 2009-2017 Cisco Systems, Inc.  All rights reserved
dnl Copyright (c) 2014-2018 Research Organization for Information Science
dnl                         and Technology (RIST). All rights reserved.
dnl Copyright (c) 2020-2022 Amazon.com, Inc. or its affiliates.  All Rights reserved.
dnl Copyright (c) 2020      Intel, Inc.  All rights reserved.
dnl Copyright (c) 2022      IBM Corporation.  All rights reserved.
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
dnl   * opal_hwloc_mode - either external or internal.  If internal,
dnl         --with-hwloc should be ignored by other packages
dnl   * CPPFLAGS, LDFLAGS, LIBS - Updated to build against hwloc.
dnl         Note that the values may be updated right before
dnl         config.status.
dnl
dnl OPAL_WRAPPER_FLAGS_ADD will be called to add the correct LDFLAGS,
dnl STATIC_LDFLAGS, LIBS, and STATIC_LIBS for hwloc.
dnl
dnl The following environment variables will only be set if
dnl opal_hwloc_mode is "internal":
dnl
dnl   * opal_hwloc_BUILD_CPPFLAGS - the C Preprocessor flags
dnl         necessary to run the preprocessor on a file which relies
dnl         on hwloc headers.  This will be folded into the global
dnl         CPPFLAGS (see note above).
dnl   * opal_hwloc_BUILD_LIBS - the libraries necessary to link
dnl         source which uses hwloc.  Cannot be added to LIBS yet,
dnl         because then other execution tests later in configure
dnl         (there are sadly some) would fail if the path in LDFLAGS
dnl         was not added to LD_LIBRARY_PATH.
dnl   * opal_hwloc_WRAPPER_LIBS - the linker flags necessary to
dnl         add to the wrapper compilers in order to link an opal
dnl         application when opal is built as a static library.
AC_DEFUN([OPAL_CONFIG_HWLOC], [
    OPAL_VAR_SCOPE_PUSH([external_hwloc_happy internal_hwloc_happy  opal_hwloc_STATIC_LDFLAGS opal_hwloc_LIBS opal_hwloc_STATIC_LIBS])

    opal_show_subtitle "Configuring hwloc"

    OPAL_3RDPARTY_WITH([hwloc], [hwloc], [package_hwloc])

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

    dnl this will work even if there is no hwloc package included,
    dnl because hwloc_tarball and hwloc_directory will evaluate to an
    dnl empty string.  These are relative to the 3rd-party/ directory.
    OPAL_3RDPARTY_EXTRA_DIST="$OPAL_3RDPARTY_EXTRA_DIST hwloc_tarball"
    OPAL_3RDPARTY_DISTCLEAN_DIRS="$OPAL_3RDPARTY_DISTCLEAN_DIRS hwloc_directory"

    OPAL_WRAPPER_FLAGS_ADD([LDFLAGS], [${opal_hwloc_LDFLAGS}])
    OPAL_WRAPPER_FLAGS_ADD([STATIC_LDFLAGS], [${opal_hwloc_STATIC_LDFLAGS}])
    OPAL_WRAPPER_FLAGS_ADD([LIBS], [${opal_hwloc_LIBS}])
    OPAL_WRAPPER_FLAGS_ADD([STATIC_LIBS], [${opal_hwloc_STATIC_LIBS}])
    OPAL_WRAPPER_FLAGS_ADD([PC_MODULES], [${opal_hwloc_PC_MODULES}])

    AC_CONFIG_COMMANDS_PRE([OPAL_CONFIG_HWLOC_INTERNAL_LIBS_HANDLER])

    OPAL_SUMMARY_ADD([Miscellaneous], [hwloc], [], [$opal_hwloc_mode])

    OPAL_VAR_SCOPE_POP
])


dnl _OPAL_CONFIG_HWLOC_EXTERNAL(action-if-happy, action-if-not-happy)
dnl
dnl only safe to call from OPAL_CONFIG_HWLOC, assumes variables from
dnl there are set.
AC_DEFUN([_OPAL_CONFIG_HWLOC_EXTERNAL], [
    OPAL_VAR_SCOPE_PUSH([opal_hwloc_min_num_version opal_hwloc_min_version opal_hwlox_max_num_version opal_hwloc_CPPFLAGS_save opal_hwloc_LDFLAGS_save opal_hwloc_LIBS_save opal_hwloc_external_support])

    OAC_CHECK_PACKAGE([hwloc],
                      [opal_hwloc],
                      [hwloc.h],
                      [hwloc],
                      [hwloc_topology_init],
                      [opal_hwloc_external_support=yes],
                      [opal_hwloc_external_support=no])

    # need these set for the tests below.
    opal_hwloc_CPPFLAGS_save=$CPPFLAGS
    opal_hwloc_LDFLAGS_save=$LDFLAGS
    opal_hwloc_LIBS_save=$LIBS

    OPAL_FLAGS_APPEND_UNIQ([CPPFLAGS], [$opal_hwloc_CPPFLAGS])
    OPAL_FLAGS_APPEND_UNIQ([LDFLAGS], [$opal_hwloc_LDFLAGS])
    OPAL_FLAGS_APPEND_UNIQ([LIBS], [$opal_hwloc_LIBS])

    opal_hwloc_min_num_version=OMPI_HWLOC_NUMERIC_MIN_VERSION
    opal_hwloc_min_version=OMPI_HWLOC_NUMERIC_MIN_VERSION
    AS_IF([test "$opal_hwloc_external_support" = "yes"],
          [AC_MSG_CHECKING([if external hwloc version is version OMPI_HWLOC_MIN_VERSION or greater])
           AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <hwloc.h>
                              ]], [[
#if HWLOC_API_VERSION < $opal_hwloc_min_num_version
#error "hwloc API version is less than $opal_hwloc_min_version"
#endif
                               ]])],
                   [AC_MSG_RESULT([yes])],
                   [AC_MSG_RESULT([no])
                    AC_MSG_WARN([external hwloc version is too old (OMPI_HWLOC_MIN_VERSION or later required)])
                    opal_hwloc_external_support="no"])])

    # Ensure that we are not using Hwloc >= v3.x.  Open MPI does not
    # (yet) support Hwloc >= v3.x (which will potentially have ABI and
    # API breakage compared to <= v2.x), and using it would lead to
    # complicated failure cases.  Hence, we just abort outright if we
    # find an external Hwloc >= v3.x.
    AS_IF([test "$opal_hwloc_external_support" = "yes"],
          [AC_MSG_CHECKING([if external hwloc version is less than version 3.0.0])
	   opal_hwloc_max_num_version=0x00030000
           AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <hwloc.h>
                              ]], [[
#if HWLOC_API_VERSION >= $opal_hwloc_max_num_version
#error "hwloc API version is >= $opal_hwloc_max_num_version"
#endif
                               ]])],
                   [AC_MSG_RESULT([yes])],
                   [AC_MSG_RESULT([no])
                    AC_MSG_WARN([External hwloc version is too new (less than v3.0.0 is required)])
		    dnl Yes, the URL below will be wrong for master
		    dnl builds.  But this is "good enough" -- we're
		    dnl more concerned about getting the URL correct
		    dnl for end-user builds of official release Open
		    dnl MPI distribution tarballs.
		    AC_MSG_WARN([See https://docs.open-mpi.org/en/v$OMPI_MAJOR_VERSION.$OMPI_MINOR_VERSION.x/installing-open-mpi/required-support-libraries.html for more details])
		    AC_MSG_ERROR([Cannot continue])])])

    AS_IF([test "$opal_hwloc_external_support" = "yes"],
          [AC_CHECK_DECLS([HWLOC_OBJ_OSDEV_COPROC], [], [], [#include <hwloc.h>
])
           AC_CHECK_FUNCS([hwloc_topology_dup])])

    CPPFLAGS="$opal_hwloc_CPPFLAGS_save"
    LDFLAGS="$opal_hwloc_LDFLAGS_save"
    LIBS="$opal_hwloc_LIBS_save"

    AS_IF([test "$opal_hwloc_external_support" = "yes"],
          [dnl Do not add hwloc libs to LIBS until late, because
           dnl it will screw up other tests (like the pthread tests)
           opal_hwloc_BUILD_LIBS="${opal_hwloc_LIBS}"

           $1],
          [$2])

    OPAL_VAR_SCOPE_POP
])

dnl _OPAL_CONFIG_HWLOC_INTERNAL(action-if-happy, action-if-not-happy)
dnl
dnl Configure the packaged hwloc.  Only needs to be run if the
dnl external hwloc is not going to be used.  Assumes that if
dnl this function is called, that success means the internal package
dnl will be used.
AC_DEFUN([_OPAL_CONFIG_HWLOC_INTERNAL], [
    OPAL_VAR_SCOPE_PUSH([subconfig_happy internal_hwloc_location extra_configure_args found_enable_plugins hwloc_config_arg pkg_config_file pkg_config_happy])

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

    AS_IF([test ${subconfig_happy} -eq 1],
        [internal_hwloc_location="3rd-party/hwloc_directory"

         dnl We do not consider it an error if pkg-config doesn't work / exist / etc.
         pkg_config_file="${OMPI_TOP_BUILDDIR}/3rd-party/hwloc_directory/hwloc.pc"
         pkg_config_happy=0

         OAC_CHECK_PACKAGE_PARSE_PKGCONFIG([hwloc_internal], [opal_hwloc], [${pkg_config_file}], [pkg_config_happy=1])

         dnl Don't pull LDFLAGS, because we don't have a good way to avoid
         dnl a -L to our install directory, which can cause some weirdness
         dnl if there's an old OMPI install there.  And it makes filtering
         dnl redundant flags easier.
         opal_hwloc_LDFLAGS=

         dnl with no pkg-config data, guess.  assume that -L${libdir} is already added to LDFLAGS
         AS_IF([test $pkg_config_happy -eq 0],
               [opal_hwloc_STATIC_LDFLAGS=
                opal_hwloc_LIBS="-lhwloc"
                opal_hwloc_STATIC_LIBS=
                opal_hwloc_PC_MODULES=])

         # note: because we only ship/commit a tarball (and not the
         # source directory), the source is always expanded in the
         # builddir, so we only need to add a -I to the builddir.
         # Overwrite the OAC_CHECK_PACKAGE_PARSE PKGCONFIG results,
         # because it's the install dir location, not the build
         # location.
         opal_hwloc_CPPFLAGS="-I$OMPI_TOP_BUILDDIR/$internal_hwloc_location/include -I$OMPI_TOP_SRCDIR/$internal_hwloc_location/include"
         opal_hwloc_BUILD_CPPFLAGS="${opal_hwloc_CPPFLAGS}"

         # No need to update LDFLAGS, because they will install into
         # our tree and in the mean time are referenced by their .la
         # files.
         opal_hwloc_BUILD_LIBS="$OMPI_TOP_BUILDDIR/$internal_hwloc_location/hwloc/libhwloc.la"
         opal_hwloc_WRAPPER_LIBS="${opal_hwloc_LIBS}"

         # no need to add to DIST_SUBDIRS, because we only ship the
         # tarball.  This is relative to the 3rd-party/ directory.
         OPAL_3RDPARTY_SUBDIRS="$OPAL_3RDPARTY_SUBDIRS hwloc_directory"

         $1], [$2])

    OPAL_VAR_SCOPE_POP
])


dnl We need to delay adding .la files to LIBS until the very end of
dnl configure, to avoid pulling it into other configure tests.
AC_DEFUN([OPAL_CONFIG_HWLOC_INTERNAL_LIBS_HANDLER], [
    OPAL_FLAGS_APPEND_UNIQ([CPPFLAGS], [${opal_hwloc_CPPFLAGS}])
    OPAL_FLAGS_APPEND_UNIQ([LDFLAGS], [$opal_hwloc_LDFLAGS])
    OPAL_FLAGS_APPEND_MOVE([LIBS], [${opal_hwloc_BUILD_LIBS}])
])
