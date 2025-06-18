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
dnl Copyright (c) 2020-2022 Amazon.com, Inc. or its affiliates.  All Rights reserved.
dnl Copyright (c) 2021      Nanook Consulting.  All rights reserved.
dnl Copyright (c) 2021-2022 IBM Corporation.  All rights reserved.
dnl Copyright (c) 2023-2024 Jeffrey M. Squyres.  All rights reserved.
dnl Copyright (c) 2025      Advanced Micro Devices, Inc. All rights reserved.
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
dnl This macro will change the environment in the following way:
dnl
dnl A Makefile conditional OMPI_WANT_PRRTE will be defined based on the
dnl results of the build.
AC_DEFUN([OMPI_SETUP_PRRTE],[
    AC_REQUIRE([AC_PROG_LN_S])

OPAL_VAR_SCOPE_PUSH([prrte_setup_internal_happy target_rst_dir])

    opal_show_subtitle "Configuring PRRTE"

    # We *must* have setup Sphinx before invoking this macro (i.e., it
    # is a programming error -- not a run-time error -- if Sphinx was
    # not previously setup).
    OAC_ASSERT_BEFORE([OAC_SETUP_SPHINX], [OMPI_SETUP_PRRTE])

    # These are sym links to folders with PRRTE's RST files that we'll
    # slurp into mpirun.1.rst.  We'll remove these links (or even
    # accidental full copies) now and replace them with new links to
    # the PRRTE that we find, below.
    target_rst_dir="$OMPI_TOP_BUILDDIR/docs"
    rm -rf "$target_rst_dir/prrte-rst-content"
    rm -rf "$target_rst_dir/schizo-ompi-rst-content"

    AC_ARG_WITH([prrte],
       [AS_HELP_STRING([--with-prrte],
           [Enable/disable building with PRRTE. Supports 'yes', 'internal',  and 'no', defaulting to 'yes'])])
    
    AC_ARG_WITH([rte],
       [AS_HELP_STRING([--with-rte],
           [Enable/disable building with RTE. Supports 'yes' and 'no', defaulting to 'yes'])])
    
    # Checking if --with-prrte and --with-rte values differ. 
    AS_IF([test -n "$with_prrte" -a -n "$with_rte" -a "$with_prrte" != "$with_rte"],
         AC_MSG_ERROR(['--with-prrte' and '--with-rte' are both defined but have differing values. Please specify one or the other.]))

    # Setting --with-prrte to --with-rte's value to avoid duplicate code
    AS_IF([test -z "$with_prrte"],
          [with_prrte="$with_rte"])

    # We only want to accept 'yes' or 'no' as args for --with-prrte. 
    # If user does not specify, it defaults to 'yes'.
    # We no longer support external prrte builds.
    prrte_setup_internal_happy=0
    AS_IF([test "$with_prrte" != "yes" -a "$with_prrte" != "no" -a "$with_prrte" != "" -a "$with_prrte" != "internal"],
         AC_MSG_ERROR(['--with-prrte' option defaults to 'yes' and supports 'yes', 'internal',  or 'no'. Internal is equivalent to yes. External PRRTE builds are no longer supported.]))
    
    # Determines if user wants to build with PRRTE. 
    # Defaults to building with PRRTE if unspecified.
    AS_CASE([$with_prrte],
            ["yes"],      [prrte_setup_internal_happy=1
                           opal_prrte_mode="internal"],
            ["internal"], [prrte_setup_internal_happy=1
                           opal_prrte_mode="internal"],
            ["no"],       [prrte_setup_internal_happy=0
                           opal_prrte_mode="disabled"],
            [""],         [prrte_setup_internal_happy=1
                           opal_prrte_mode="internal"])

    m4_ifdef([package_prrte],
        [OMPI_PRRTE_ADD_ARGS
         AS_IF([test "$opal_prrte_mode" = "internal"],
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
    # successfully (or wasn't started), then disable make dist.
    AS_IF([test $prrte_setup_internal_happy != 1],
          [OPAL_MAKEDIST_DISABLE="$OPAL_MAKEDIST_DISABLE PRRTE"])])
    
    AS_IF([test "$opal_prrte_mode" = "disabled"],
          [OMPI_WANT_PRRTE=0
           OMPI_HAVE_PRRTE=0
           OMPI_USING_INTERNAL_PRRTE=0
           OMPI_HAVE_PRRTE_RST=0],
          [AS_IF([test "$prrte_setup_internal_happy" = "1"],
                 [OMPI_WANT_PRRTE=1
                  OMPI_HAVE_PRRTE=1
                  OMPI_USING_INTERNAL_PRRTE=1
                  _OMPI_SETUP_PRRTE_INTERNAL_POST()],
                 [OMPI_HAVE_PRRTE=0
                  OMPI_USING_INTERNAL_PRRTE=0])])

    AM_CONDITIONAL([OMPI_WANT_PRRTE],
           [test "$prrte_setup_internal_happy" = "1"])

    AC_DEFINE_UNQUOTED([OMPI_HAVE_PRRTE],
                       [$OMPI_HAVE_PRRTE],
                       [Whether or not PRRTE is available])

    AC_DEFINE_UNQUOTED([OMPI_USING_INTERNAL_PRRTE],
                       [$OMPI_USING_INTERNAL_PRRTE],
                       [Whether or not we are using the internal PRRTE])

    AC_SUBST(OMPI_PRRTE_RST_CONTENT_DIR)
    AC_SUBST(OMPI_SCHIZO_OMPI_RST_CONTENT_DIR)
    AM_CONDITIONAL(OMPI_HAVE_PRRTE_RST, [test $OMPI_HAVE_PRRTE_RST -eq 1])

    OPAL_SUMMARY_ADD([Miscellaneous], [PRRTE], [], [$opal_prrte_mode])

    OPAL_VAR_SCOPE_POP
])

dnl _OMPI_SETUP_PRRTE_INTERNAL([action-if-success], [action-if-not-success])
dnl
dnl Attempt to configure the built-in PRRTE.
AC_DEFUN([_OMPI_SETUP_PRRTE_INTERNAL], [
    OPAL_VAR_SCOPE_PUSH([internal_prrte_args internal_prrte_happy deprecated_prefix_by_default print_prrte_warning internal_prrte_CPPFLAGS opal_prrte_CPPFLAGS_save])

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
    # PRRTE sets -Werror on devel builds so avoid buid breaks caused by 3rd-party codes
    internal_prrte_args="$internal_prrte_args --disable-devel-check"

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
           internal_prrte_args="$internal_prrte_args --with-libevent-extra-libs=\"$opal_libevent_BUILD_LIBS\""
           internal_prrte_CPPFLAGS="$internal_prrte_CPPFLAGS $opal_libevent_BUILD_CPPFLAGS"])

    AS_IF([test "$opal_hwloc_mode" = "internal"],
          [internal_prrte_args="$internal_prrte_args --disable-hwloc-lib-checks"
           internal_prrte_args="$internal_prrte_args --with-hwloc-extra-libs=\"$opal_hwloc_BUILD_LIBS\""
           internal_prrte_CPPFLAGS="$internal_prrte_CPPFLAGS $opal_hwloc_BUILD_CPPFLAGS"])

    AS_IF([test "$opal_pmix_mode" = "internal"],
          [internal_prrte_args="$internal_prrte_args --disable-pmix-lib-checks"
           internal_prrte_args="$internal_prrte_args --with-pmix-extra-libs=\"$opal_pmix_BUILD_LIBS\""
           internal_prrte_CPPFLAGS="$internal_prrte_CPPFLAGS $opal_pmix_BUILD_CPPFLAGS"])

    opal_prrte_CPPFLAGS_save="${CPPFLAGS}"
    OPAL_FLAGS_APPEND_UNIQ([CPPFLAGS], [${opal_pmix_CPPFLAGS}])

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

    CPPFLAGS="${opal_prrte_CPPFLAGS_save}"

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

    OMPI_HAVE_PRRTE_RST=0
    AS_IF([test "$internal_prrte_happy" = "yes"],
          [AC_MSG_CHECKING([for internal PRRTE RST files])
           AS_IF([test -n "$SPHINX_BUILD"],
                 [OMPI_HAVE_PRRTE_RST=1
                  OMPI_PRRTE_RST_CONTENT_DIR="$OMPI_TOP_SRCDIR/3rd-party/prrte/src/docs/prrte-rst-content"
                  OMPI_SCHIZO_OMPI_RST_CONTENT_DIR="$OMPI_TOP_SRCDIR/3rd-party/prrte/src/mca/schizo/ompi"
                  AC_MSG_RESULT([found])],
		 [AC_MSG_RESULT([not found])])
           $1],
	  [$2])

    OPAL_VAR_SCOPE_POP
])


dnl _OMPI_SETUP_CONFIG_PRRTE_INTERNAL_POST()
dnl
dnl Expectation is that this is called only if external fails, the
dnl caller configured libprrte configure, and the configure script
dnl succeeded.
AC_DEFUN([_OMPI_SETUP_PRRTE_INTERNAL_POST], [
    OPAL_3RDPARTY_SUBDIRS="$OPAL_3RDPARTY_SUBDIRS prrte"
])
