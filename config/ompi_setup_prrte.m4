dnl -*- autoconf -*-
dnl
dnl Copyright (c) 2004-2009 The Trustees of Indiana University and Indiana
dnl                         University Research and Technology
dnl                         Corporation.  All rights reserved.
dnl Copyright (c) 2004-2021 The University of Tennessee and The University
dnl                         of Tennessee Research Foundation.  All rights
dnl                         reserved.
dnl Copyright (c) 2004-2007 High Performance Computing Center Stuttgart,
dnl                         University of Stuttgart.  All rights reserved.
dnl Copyright (c) 2004-2005 The Regents of the University of California.
dnl                         All rights reserved.
dnl Copyright (c) 2006-2009 Cisco Systems, Inc.  All rights reserved.
dnl Copyright (c) 2006-2008 Sun Microsystems, Inc.  All rights reserved.
dnl Copyright (c) 2006-2007 Los Alamos National Security, LLC.  All rights
dnl                         reserved.
dnl Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
dnl Copyright (c) 2019-2020 Intel, Inc.  All rights reserved.
dnl Copyright (c) 2020-2021 Amazon.com, Inc. or its affiliates.
dnl                         All Rights reserved.
dnl Copyright (c) 2021      Nanook Consulting.  All rights reserved.
dnl Copyright (c) 2021      IBM Corporation.  All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

dnl Check for / configure PRRTE package.  Prefer finding an external
dnl PRRTE, build our internal one if required.  If we can not find an
dnl external PRRTE and the internal one fails to configure, abort.
dnl
dnl This macro wil change the environment in the following way:
dnl
dnl * PRTE_PATH will be AC_SUBST'ed to the full path (minus the EXE
dnl   extension) of the prte binary.
dnl
dnl A Makefile conditional OMPI_WANT_PRRTE will be defined based on the
dnl results of the build.
AC_DEFUN([OMPI_SETUP_PRRTE],[
    OPAL_VAR_SCOPE_PUSH([prrte_setup_internal_happy prrte_setup_external_happy prrte_setup_success_var])

    opal_show_subtitle "Configuring PRRTE"

    OPAL_3RDPARTY_WITH([prrte], [prrte], [package_prrte], [1])

    prrte_setup_internal_happy=0
    m4_ifdef([package_prrte],
        [OMPI_PRRTE_ADD_ARGS
         AS_IF([test "$opal_prrte_mode" = "unspecified" -o "$opal_prrte_mode" = "internal"],
               [# Run PRRTE's configure script unless the user
		# explicitly asked us to use an external PMIX, so that
		# "make dist" includes PRRTE in the dist tarball.  This
		# does mean that "make dist" will not work if Open MPI
		# was configured to use an external PRRTE library, but
		# we decided this was a reasonable tradeoff for not
		# having to deal with PRRTE (or PMIx) potentially
		# failing to configure in a situation where it isn't
		# desired.
                _OMPI_SETUP_PRRTE_INTERNAL([prrte_setup_internal_happy=1],
                                           [prrte_setup_internal_happy=0])])

         # if we have a pmix package and configure did not complete
         # successfullly (or wasn't started), then disable make dist.
         AS_IF([test $prrte_setup_internal_happy != 1],
               [OPAL_MAKEDIST_DISABLE="$OPAL_MAKEDIST_DISABLE PRRTE"])])

    # unless internal specifically requested by the user, try to find
    # an external that works.
    prrte_setup_external_happy=0
    AS_IF([test "$opal_prrte_mode" != "internal" -o "$opal_prrte_mode" != "disabled"],
          [_OMPI_SETUP_PRRTE_EXTERNAL(
              [prrte_setup_external_happy=1
               opal_prrte_mode="external"],
              [AS_IF([test "$opal_prrte_mode" = "external"],
                     [AC_MSG_ERROR([External PRRTE requested but not found.])])])])

    # external did not work out and customer did not specify external,
    # so try the internal version.
    AS_IF([test "$prrte_setup_external_happy" = "0" -a "$prrte_setup_internal_happy" = "1"],
          [opal_prrte_mode="internal"
           OMPI_USING_INTERNAL_PRRTE=1
           _OMPI_SETUP_PRRTE_INTERNAL_POST()],
          [OMPI_USING_INTERNAL_PRRTE=0])

    AS_IF([test "$opal_prrte_mode" != "disabled"],
          [AS_IF([test "$prrte_setup_external_happy" = "0" -a "$prrte_setup_internal_happy" = "0"],
                 [AC_MSG_ERROR([Could not find viable prrte build.])])
           OMPI_HAVE_PRRTE=1],
          [OMPI_HAVE_PRRTE=0])

    AC_SUBST([PRTE_PATH])

    AM_CONDITIONAL([OMPI_WANT_PRRTE],
           [test "$prrte_setup_internal_happy" = "1" -o "$prrte_setup_external_happy" = "1"])

    AC_DEFINE_UNQUOTED([OMPI_HAVE_PRRTE],
                       [$OMPI_HAVE_PRRTE],
                       [Whether or not PRRTE is available])

    AS_IF([test "$opal_prrte_mode" = "external"],
          [AC_DEFINE_UNQUOTED([OMPI_PRTERUN_PATH],
                      ["$PRTE_PATH"],
                      [Path to prterun])])

    AC_DEFINE_UNQUOTED([OMPI_USING_INTERNAL_PRRTE],
                       [$OMPI_USING_INTERNAL_PRRTE],
                       [Whether or not we are using the internal PRRTE])

    OPAL_SUMMARY_ADD([[Miscellaneous]], [[prrte]], [prrte], [$opal_prrte_mode])

    OPAL_VAR_SCOPE_POP
])


dnl _OMPI_SETUP_PRRTE_INTERNAL([action-if-success], [action-if-not-success])
dnl
dnl Attempt to configure the built-in PRRTE.
AC_DEFUN([_OMPI_SETUP_PRRTE_INTERNAL], [
    OPAL_VAR_SCOPE_PUSH([internal_prrte_args internal_prrte_extra_libs internal_prrte_happy deprecated_prefix_by_default print_prrte_warning internal_prrte_CPPFLAGS])

    # This is really a PRTE option that should not be in Open MPI, but
    # there is not a great way to support the orterun/mpirun checks
    # without this argument.
    AC_ARG_ENABLE([prte-prefix-by-default],
        [AS_HELP_STRING([--enable-prte-prefix-by-default],
            [Make "mpirun ..." behave exactly the same as "mpirun --prefix \$prefix", where \$prefix is the value given to --prefix in configure (default:enabled)])])

    AC_ARG_ENABLE([orterun-prefix-by-default],
        [AS_HELP_STRING([--enable-orterun-prefix-by-default],
            [*DEPRECATED* Please use --enable-prte-prefix-by-default in the future.])],
        [print_prrte_warning="yes"
         deprecated_prefix_by_default=$orterun_prefix_by_default])

    AC_ARG_ENABLE([mpirun-prefix-by-default],
        [AS_HELP_STRING([--enable-mpirun-prefix-by-default],
            [*DEPRECATED* Please use --enable-prte-prefix-by-default in the future.])],
        [print_prrte_warning="yes"
         deprecated_prefix_by_default=$mpirun_prefix_by_default])

    AS_IF([test "$print_prrte_warning" = "yes"], [
        AC_MSG_WARN([Open MPI no longer uses the ORTE environment - it has been])
        AC_MSG_WARN([replaced by PRRTE. Accordingly, the "--enable-orterun-prefix-by-default"])
        AC_MSG_WARN([and "--enable-mpirun-prefix-by-default" options have been replaced])
        AC_MSG_WARN([by "--enable-prte-prefix-by-default". We will do the translation for])
        AC_MSG_WARN([you now, but these older options are deprecated and will be removed])
        AC_MSG_WARN([in a later release, so please update your build scripts.])])

    AS_IF([test -n "$prte_prefix_by_default" -a -n "$deprecated_prefix_by_default"],
          [AC_MSG_ERROR([--enable-prte-prefix-by-default cannot be used with --enable-mpirun-prefix-by-default or --enable-orterun-prefix-by-default.  Please only specify --enable-prte-prefix-by-default.])])

    internal_prrte_CPPFLAGS=
    internal_prrte_args="--with-proxy-version-string=$OPAL_VERSION --with-proxy-package-name=\"Open MPI\" --with-proxy-bugreport=\"https://www.open-mpi.org/community/help/\""

    # Set --enable-prte-prefix-by-default to the deprecated options,
    # if they were specified.  Otherwise, set it to enabled if the
    # user did not specify an option.  PRTE defaults to not enabling
    # prefix-by-default, but open mpi wants that behavior.
    AS_IF([test -n "$deprecated_prefix_by_default"],
              [internal_prrte_args="$internal_prrte_args --enable-prte-prefix-by-default=$deprecated_prefix_by_default"],
          [test -z "$enable_prte_prefix_by_default"],
              [internal_prrte_args="$internal_prrte_args --enable-prte-prefix-by-default"])

    AS_IF([test "$opal_libevent_mode" = "internal"],
          [internal_prrte_args="$internal_prrte_args --with-libevent --disable-libevent-lib-checks"
           internal_prrte_args="$internal_prrte_args --with-libevent-extra-libs=\"$opal_libevent_LIBS\""
           internal_prrte_CPPFLAGS="$internal_prrte_CPPFLAGS $opal_libevent_CPPFLAGS"])

    AS_IF([test "$opal_hwloc_mode" = "internal"],
          [internal_prrte_args="$internal_prrte_args --disable-hwloc-lib-checks"
           internal_prrte_args="$internal_prrte_args --with-hwloc-extra-libs=\"$opal_hwloc_LIBS\""
           internal_prrte_CPPFLAGS="$internal_prrte_CPPFLAGS $opal_hwloc_CPPFLAGS"])

    AS_IF([test "$opal_pmix_mode" = "internal"],
          [internal_prrte_args="$internal_prrte_args --disable-pmix-lib-checks"
           internal_prrte_args="$internal_prrte_args --with-pmix-extra-libs=\"$opal_pmix_LIBS\""
           internal_prrte_CPPFLAGS="$internal_prrte_CPPFLAGS $opal_pmix_CPPFLAGS"])

    AC_MSG_CHECKING([if PMIx version is 4.0.0 or greater])
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <pmix_version.h>]],
            [[
#if PMIX_VERSION_MAJOR < 4L
#error "pmix API version is less than 4.0.0"
#endif
             ]])],
            [AC_MSG_RESULT([yes])],
            [AC_MSG_RESULT([no])
             AC_MSG_WARN([OMPI's internal runtime environment "PRRTE" does not support])
             AC_MSG_WARN([PMIx versions less than v4.x as they lack adequate tool])
             AC_MSG_WARN([support. You can, if desired, build OMPI against an earlier])
             AC_MSG_WARN([version of PMIx for strictly direct-launch purposes - e.g., using])
             AC_MSG_WARN([Slurm's srun to launch the job - by configuring with the])
             AC_MSG_WARN([--without-prrte option.])
             AC_MSG_ERROR([Cannot continue])])

    AS_IF([test "$with_ft" != "no"],
          [internal_prrte_args="--enable-prte-ft $internal_prrte_args"],
          [])

    if test "$WANT_DEBUG" = "1"; then
        internal_prrte_args="$internal_prrte_args --enable-debug"
    fi

    # Pass all our compiler/linker flags to PRRTE, so that it
    # picks up how to build an internal HWLOC, libevent, and PMIx, plus
    # picks up any user-specified compiler flags from the master
    # configure run.
    OPAL_SUBDIR_ENV_CLEAN([opal_prrte_configure])
    AS_IF([test -n "$internal_prrte_CPPFLAGS"],
          [OPAL_SUBDIR_ENV_APPEND([CPPFLAGS], [$internal_prrte_CPPFLAGS])])
    PAC_CONFIG_SUBDIR_ARGS([3rd-party/prrte], [$internal_prrte_args],
            [[--with-libevent=internal], [--with-hwloc=internal],
             [--with-libevent=external], [--with-hwloc=external],
             [--with-pmix=internal], [--with-pmix=external],
             [--with-platform=[[^ 	]]*]],
            [internal_prrte_happy="yes"], [internal_prrte_happy="no"])
    OPAL_SUBDIR_ENV_RESTORE([opal_prrte_configure])
    OPAL_3RDPARTY_DIST_SUBDIRS="$OPAL_3RDPARTY_DIST_SUBDIRS prrte"

    AS_IF([test "$internal_prrte_happy" = "no" -a "$enable_internal_rte" != "no"],
          [AC_MSG_ERROR([PRRTE configuration failed.  Cannot continue.])])

    AS_IF([test "$internal_prrte_happy" = "yes"],
          [$1], [$2])

    OPAL_VAR_SCOPE_POP
])


dnl _OMPI_SETUP_CONFIG_PRRTE_INTERNAL_POST()
dnl
dnl Expectation is that this is called only if external fails, the
dnl caller configured libprrte configure, and the configure script
dnl succeeded.
AC_DEFUN([_OMPI_SETUP_PRRTE_INTERNAL_POST], [
    OPAL_3RDPARTY_SUBDIRS="$OPAL_3RDPARTY_SUBDIRS prrte"

    PRTE_PATH="prte"
])


dnl _OMPI_SETUP_PRRTE_EXTERNAL([action if success], [action if not success])
dnl
dnl Try to find an external prrte with sufficient version.  Since we
dnl don't link against prrte, only output environment variable is
dnl PRTE_PATH.
AC_DEFUN([_OMPI_SETUP_PRRTE_EXTERNAL], [
    OPAL_VAR_SCOPE_PUSH([setup_prrte_external_happy opal_prrte_CPPFLAGS_save])

    opal_prrte_CPPFLAGS_save=$CPPFLAGS

    _OPAL_CHECK_PACKAGE_HEADER([opal_prrte], [prte.h], [$with_prrte],
			       [setup_prrte_external_happy=yes],
			       [setup_prrte_external_happy=no])

    CPPFLAGS="$opal_prrte_CPPFLAGS_save $opal_prrte_CPPFLAGS"

    AC_MSG_CHECKING([if external PRRTE version is 2.0.0 or greater])
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <prte_version.h>]],
                 [[
#if PRTE_NUMERIC_VERSION < 0x00020000
#error "prrte API version is less than 2.0.0"
#endif
                 ]])],
                 [AC_MSG_RESULT([yes])
                  setup_prrte_external_happy=yes],
                 [AC_MSG_RESULT([no])
                  setup_prrte_external_happy=no])

    CPPFLAGS="$opal_prrte_CPPFLAGS_save"

    AS_IF([test "$setup_prrte_external_happy" = "yes"],
          [AS_IF([test -n "$with_prrte"],
                 [PRTE_PATH="${with_prrte}/bin/prte"
                  AS_IF([test ! -r ${PRTE_PATH}],
                        [AC_MSG_ERROR([Could not find prte binary at $PRTE_PATH])],
                        [PRTE_PATH="${with_prrte}/bin"])],
		 [PRTE_PATH=""
                  OPAL_WHICH([prte], [PRTE_PATH])
                  AS_IF([tets -z "$PRTE_PATH"],
                        [AC_MSG_WARN([Could not find prte in PATH])
                         setup_prrte_external_happy=no],
                        [PRTE_PATH="`echo $PRTE_PATH | sed -e 's/\/prte//'`"])])])
    AS_IF([test "$setup_prrte_external_happy" = "yes"],
          [$1], [$2])

    OPAL_VAR_SCOPE_POP
])


