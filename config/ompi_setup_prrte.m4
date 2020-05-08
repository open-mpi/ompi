# -*- shell-script -*-
#
# Copyright (c) 2004-2009 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2007 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2006-2009 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2006-2008 Sun Microsystems, Inc.  All rights reserved.
# Copyright (c) 2006-2007 Los Alamos National Security, LLC.  All rights
#                         reserved.
# Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
# Copyright (c) 2019-2020 Intel, Inc.  All rights reserved.
# Copyright (c) 2020      Amazon.com, Inc. or its affiliates.
#                         All Rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# Check for / configure PRRTE package.  Unlike the other 3rd party
# packages, prrte is either internal or not used (since prrte is a
# pmix server, we don't need to link against it or such in the
# external case).  So we don't have to do some of the complex steps we
# do for pmix or hwloc in terms of external hunting.
#
# This macro will not change the environment.
#
# A Makefile conditional OMPI_WANT_PRRTE will be defined based on the
# results of the build.  Unless --disable-internal-rte is specified,
# the top-level configure will abort if the PRRTE configure fails.
AC_DEFUN([OMPI_SETUP_PRRTE],[
    OPAL_VAR_SCOPE_PUSH([internal_prrte_build])

    internal_prrte_build=0
    m4_ifdef([package_prrte],
             [OMPI_SETUP_PRRTE_INTERNAL([internal_prrte_build=1], [internal_prrte_build=0])])

    AM_CONDITIONAL([OMPI_WANT_PRRTE], [test "$internal_prrte_build" = "1"])

    OPAL_VAR_SCOPE_POP
])

AC_DEFUN([OMPI_SETUP_PRRTE_INTERNAL], [
    OPAL_VAR_SCOPE_PUSH([internal_prrte_args internal_prrte_extra_libs internal_prrte_happy deprecated_prefix_by_default print_prrte_warning])

    opal_show_subtitle "Configuring PRRTE"

    AC_ARG_ENABLE([internal-rte],
                  [AC_HELP_STRING([--enable-internal-rte],
                                  [Enable internal runtime support and provide mpiexec/mpirun (default: enabled)])])

    # This is really a PRTE option that should not be in Open MPI, but
    # there is not a great way to support the orterun/mpirun checks
    # without this argument.
    AC_ARG_ENABLE([prte-prefix-by-default],
        [AC_HELP_STRING([--enable-prte-prefix-by-default],
            [Make "mpirun ..." behave exactly the same as "mpirun --prefix \$prefix", where \$prefix is the value given to --prefix in configure (default:enabled)])])

    AC_ARG_ENABLE([orterun-prefix-by-default],
        [AC_HELP_STRING([--enable-orterun-prefix-by-default],
            [*DEPRECATED* Please use --enable-prte-prefix-by-default in the future.])],
        [print_prrte_warning="yes"
         deprecated_prefix_by_default=$orterun_prefix_by_default])

    AC_ARG_ENABLE([mpirun-prefix-by-default],
        [AC_HELP_STRING([--enable-mpirun-prefix-by-default],
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

    internal_prrte_happy="yes"
    AS_IF([test "$enable_internal_rte" = "no"],
          [internal_prrte_happy="no"])

    internal_prrte_args="--with-proxy-version-string=$OPAL_VERSION --with-proxy-package-name=\"Open MPI\" --with-proxy-bugreport=\"https://www.open-mpi.org/community/help/\""
    internal_prrte_libs=

    # Set --enable-prte-prefix-by-default to the deprecated options,
    # if they were specified.  Otherwise, set it to enabled if the
    # user did not specify an option.  PRTE defaults to not enabling
    # prefix-by-default, but open mpi wants that behavior.
    AS_IF([test -n "$deprecated_prefix_by_default"],
              [internal_prrte_args="internal_prrte_args --enable-prte-prefix-by-default=$deprecated_prefix_by_default"],
          [test -z "$enable_prte_prefix_by_default"],
              [internal_prrte_args="$internal_prrte_args --enable-prte-prefix-by-default"])

    AS_IF([test "$opal_libevent_mode" = "internal"],
          [internal_prrte_args="$internal_prrte_args --with-libevent-header=$opal_libevent_header"
           internal_prrte_libs="$internal_prrte_libs $opal_libevent_LIBS"])

    AS_IF([test "$opal_hwloc_mode" = "internal"],
          [internal_prrte_args="$internal_prrte_args --with-hwloc-header=$opal_hwloc_header"
           internal_prrte_libs="$internal_prrte_libs $opal_hwloc_LIBS"])

    AS_IF([test "$opal_pmix_mode" = "internal"],
          [internal_prrte_args="$internal_prrte_args --with-pmix-header=$opal_pmix_header"
           internal_prrte_libs="$internal_prrte_libs $opal_pmix_LIBS"])

    AS_IF([test "$internal_prrte_happy" = "yes"],
          [AC_MSG_CHECKING([if PMIx version is 4.0.0 or greater])
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
                        AC_MSG_WARN([version of PMIx for strictly direct-launch purposes - e.g., using)])
                        AC_MSG_WARN([Slurm's srun to launch the job - by configuring with the])
                        AC_MSG_WARN([--disable-internal-rte option.])
                        AC_MSG_ERROR([Cannot continue])])])

dnl    AS_IF([test ! -z $with_prrte_platform && test "$with_prrte_platform" != "yes"],
dnl        [internal_prrte_args="$internal_prrte_args --with-platform=$with_prrte_platform"])

    # add the extra libs
    internal_prrte_args="$internal_prrte_args --with-prte-extra-lib=\"$internal_prrte_libs\" --with-prte-extra-ltlib=\"$internal_prrte_libs\""

    # Pass all our compiler/linker flags to PRRTE, so that it
    # picks up how to build an internal HWLOC, libevent, and PMIx, plus
    # picks up any user-specified compiler flags from the master
    # configure run.
    export CFLAGS CPPFLAGS LDFLAGS
    PAC_CONFIG_SUBDIR_ARGS([3rd-party/prrte], [$internal_prrte_args],
            [[--with-libevent=internal], [--with-hwloc=internal],
             [--with-libevent=external], [--with-hwloc=external],
             [--with-pmix=internal], [--with-pmix=external],
             [--with-platform=.*]],
            [], [internal_prrte_happy="no"])
    OPAL_3RDPARTY_DIST_SUBDIRS="$OPAL_3RDPARTY_DIST_SUBDIRS prrte"

    AS_IF([test "$internal_prrte_happy" = "no" -a "$enable_internal_rte" != "no"],
          [AC_MSG_ERROR([PRRTE configuration failed.  Cannot continue.])])

    AS_IF([test "$internal_prrte_happy" = "yes"],
          [OPAL_3RDPARTY_SUBDIRS="$OPAL_3RDPARTY_SUBDIRS prrte"
           $1],
          [$2])

    OPAL_SUMMARY_ADD([[Miscellaneous]], [[PRRTE]], [prrte], [$internal_prrte_happy])

    OPAL_VAR_SCOPE_POP
])
