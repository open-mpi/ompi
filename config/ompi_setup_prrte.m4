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

AC_DEFUN([OMPI_SETUP_PRRTE],[
    OPAL_VAR_SCOPE_PUSH([opal_prrte_save_CPPFLAGS opal_prrte_save_CFLAGS opal_prrte_save_LDFLAGS opal_prrte_save_LIBS opal_prrte_args opal_prrte_save_enable_dlopen opal_prrte_save_enable_mca_dso opal_prrte_save_enable_mca_static opal_prrte_extra_libs opal_prrte_extra_ltlibs opal_prrte_extra_ldflags])

    opal_prrte_save_CFLAGS=$CFLAGS
    opal_prrte_save_CPPFLAGS=$CPPFLAGS
    opal_prrte_save_LDFLAGS=$LDFLAGS
    opal_prrte_save_LIBS=$LIBS
    opal_prrte_save_enable_dlopen=enable_dlopen
    opal_prrte_save_enable_mca_dso=enable_mca_dso
    opal_prrte_save_enable_mca_static=enable_mca_static

    AC_ARG_ENABLE([internal-rte],
                  [AC_HELP_STRING([--enable-internal-rte],
                                  [Enable internal runtime support and provide mpiexec/mpirun (default: enabled)])])

    AC_ARG_WITH([prrte-platform],
                [AC_HELP_STRING([--with-prrte-platform],
                                [Platform file to use when building the internal PRRTE runtime support])])

    AC_ARG_ENABLE([prte-prefix-by-default],
        [AC_HELP_STRING([--enable-prte-prefix-by-default],
            [Make "mpirun ..." behave exactly the same as "mpirun --prefix \$prefix" (where \$prefix is the value given to --prefix in configure) (default:enabled)])])

    AC_MSG_CHECKING([if RTE support is enabled])
    AS_IF([test "$enable_internal_rte" != "no"],
       [AC_MSG_RESULT([yes])
        ompi_want_prrte=yes
        opal_prrte_extra_libs=
        opal_prrte_extra_ltlibs=

        AS_IF([test "$opal_libevent_mode" = "internal"],
           [opal_prrte_extra_libs="$opal_prrte_extra_libs $opal_libevent_LIBS"
            opal_prrte_extra_ltlibs="$opal_prrte_extra_ltlibs $opal_libevent_LIBS"

            AS_IF([test ! -z "$opal_libevent_header"]
               [opal_prrte_libevent_arg="--with-libevent-header=$opal_libevent_header"])],
           [opal_prrte_libevent_arg="--with-libevent=$with_libevent"
            AS_IF([test ! -z "$with_libevent_libdir"],
               [opal_prrte_libevent_arg="$opal_prrte_libevent_arg --with-libevent-libdir=$with_libevent_libdir"])])

        AS_IF([test "$opal_hwloc_mode" = "internal"],
           [opal_prrte_extra_libs="$opal_prrte_extra_libs $opal_hwloc_LIBS"
            opal_prrte_extra_ltlibs="$opal_prrte_extra_ltlibs $opal_hwloc_LIBS"

            AS_IF([test ! -z "$opal_hwloc_header"],
               [opal_prrte_hwloc_arg="--with-hwloc-header=$opal_hwloc_header"])],
           [opal_prrte_hwloc_arg="--with-hwloc=$with_hwloc"
            AS_IF([test ! -z "$with_hwloc_libdir"],
               [opal_prrte_hwloc_arg="$opal_prrte_hwloc_arg --with-hwloc-libdir=$with_hwloc_libdir"])])

        AS_IF([test "$opal_pmix_mode" = "internal"],
           [opal_prrte_extra_libs="$opal_prrte_extra_libs $opal_pmix_LIBS"
            opal_prrte_extra_ltlibs="$opal_prrte_extra_ltlibs $opal_pmix_LIBS"

            AS_IF([test ! -z "$opal_pmix_header"],
               [opal_prrte_pmix_arg="--with-pmix-header=$opal_pmix_header"])],
           [OPAL_VAR_SCOPE_PUSH([opal_prrte_CPPFLAGS_save])
            opal_prrte_CPPFLAGS_save=$CPPFLAGS

            AC_MSG_CHECKING([if external PMIx version is 3.0.0 or greater])
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
                        AC_MSG_ERROR([Cannot continue])])

            CPPFLAGS=$opal_prrte_CPPFLAGS_save

            OPAL_VAR_SCOPE_POP

            opal_prrte_pmix_arg="--with-pmix=$with_pmix"
            AS_IF([test ! -z "$with_pmix_libdir"],
               [opal_prrte_pmix_arg="$opal_prrte_pmix_arg --with-pmix-libdir=$with_pmix_libdir"])])

        if test -z $enable_prte_prefix_by_default || test "$enable_prte_prefix_by_default" = "yes" ||
           test "$enable_orterun_prefix_given" = "yes"; then
           opal_prrte_prefix_arg="--enable-prte-prefix-by-default"
        else
            opal_prrte_prefix_arg=
        fi

        opal_prrte_args="--prefix=$prefix --with-proxy-version-string=$OPAL_VERSION --with-proxy-package-name=\"Open MPI\" --with-proxy-bugreport=\"https://www.open-mpi.org/community/help/\" $opal_prrte_prefix_arg $opal_prrte_libevent_arg $opal_prrte_hwloc_arg $opal_prrte_pmix_arg"
        AS_IF([test "$enable_debug" = "yes"],
              [opal_prrte_args="--enable-debug $opal_prrte_args"
               CFLAGS="$OPAL_CFLAGS_BEFORE_PICKY $OPAL_VISIBILITY_CFLAGS -g"],
              [opal_prrte_args="--disable-debug $opal_prrte_args"
               CFLAGS="$OPAL_CFLAGS_BEFORE_PICKY $OPAL_VISIBILITY_CFLAGS"])
        AS_IF([test "$with_devel_headers" = "yes"],
              [opal_prrte_args="--with-devel-headers  $opal_prrte_args"])
        if test ! -z $with_prrte_platform && test "$with_prrte_platform" != "yes"; then
            opal_prrte_args="$opal_prrte_args --with-platform=$with_prrte_platform"
        fi
        # add the extra libs
        opal_prrte_args="$opal_prrte_args --with-prte-extra-lib=\"$opal_prrte_extra_libs\" --with-prte-extra-ltlib=\"$opal_prrte_extra_ltlibs\""

        AC_MSG_CHECKING([final prrte configure args])
        AC_MSG_RESULT([$opal_prrte_args])

        CPPFLAGS="-I$OPAL_TOP_SRCDIR -I$OPAL_TOP_BUILDDIR -I$OPAL_TOP_SRCDIR/opal/include -I$OPAL_TOP_BUILDDIR/opal/include $CPPFLAGS"
        OPAL_CONFIG_SUBDIR([prrte],
                           [$opal_prrte_args $opal_subdir_args 'CFLAGS=$CFLAGS' 'CPPFLAGS=$CPPFLAGS'],
                           [opal_prrte_happy=1], [opal_prrte_happy=0])

        OPAL_SUMMARY_ADD([[Miscellaneous]],[[PRRTE]],[prrte],[yes])],
       [OPAL_SUMMARY_ADD([[Miscellaneous]],[[PRRTE]],[prrte],[no (disabled)])
        AC_MSG_RESULT([no (disabled)])
        ompi_want_prrte=no])

    CFLAGS=$opal_prrte_save_CFLAGS
    CPPFLAGS=$opal_prrte_save_CPPFLAGS
    LDFLAGS=$opal_prrte_save_LDFLAGS
    LIBS=$opal_prrte_save_LIBS
    enable_dlopen=$opal_prrte_save_enable_dlopen
    enable_mca_dso=$opal_prrte_save_enable_mca_dso
    enable_mca_static=$opal_prrte_save_enable_mca_static

    OPAL_VAR_SCOPE_POP

])
