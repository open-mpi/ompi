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
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AC_DEFUN([OMPI_SETUP_PRRTE],[
    OPAL_VAR_SCOPE_PUSH([opal_prrte_save_CPPFLAGS opal_prrte_save_CFLAGS opal_prrte_save_LDFLAGS opal_prrte_save_LIBS opal_prrte_args opal_prrte_save_enable_dlopen opal_prrte_save_enable_mca_dso opal_prrte_save_enable_mca_static])

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
    AC_MSG_CHECKING([if RTE support is enabled])
    if test "$enable_internal_rte" != "no"; then
        AC_MSG_RESULT([yes])
        if test -z $with_libevent || test "$with_libevent" = "internal" || test "$with_libevent" = "yes"; then
            opal_prrte_libevent_arg="--with-libevent-header=$OMPI_TOP_SRCDIR/opal/mca/event/event.h"
        elif test "$with_libevent" = "external"; then
            opal_prrte_libevent_arg=""
        else
            opal_prrte_libevent_arg="--with-libevent=$with_libevent"
        fi

        if test -z $with_hwloc || test "$with_hwloc" = "internal" || test "$with_hwloc" = "yes"; then
               opal_prrte_hwloc_arg="--with-hwloc-header=$OMPI_TOP_SRCDIR/opal/mca/hwloc/hwloc-internal.h"
        elif test "$with_hwloc" = "external"; then
            opal_prrte_hwloc_arg=""
        else
            opal_prrte_hwloc_arg="--with-hwloc=$with_hwloc"
        fi

        if test -z $with_pmix || test "$with_pmix" = "internal" || test "$with_pmix" = "yes"; then
            opal_prrte_pmix_arg="--with-pmix-header=$OMPI_TOP_SRCDIR/opal/mca/pmix/pmix-internal.h"
        elif test "$with_pmix" = "external"; then
            opal_prrte_pmix_arg=""
        else
            opal_prrte_pmix_arg="--with-pmix=$with_pmix"
        fi

        opal_prrte_args="--prefix=$prefix --disable-dlopen $opal_prrte_libevent_arg $opal_prrte_hwloc_arg $opal_prrte_pmix_arg"
        AS_IF([test "$enable_debug" = "yes"],
              [opal_prrte_args="--enable-debug $opal_prrte_args"
               CFLAGS="$OPAL_CFLAGS_BEFORE_PICKY $OPAL_VISIBILITY_CFLAGS -g"],
              [opal_prrte_args="--disable-debug $opal_prrte_args"
               CFLAGS="$OPAL_CFLAGS_BEFORE_PICKY $OPAL_VISIBILITY_CFLAGS"])
        AS_IF([test "$with_devel_headers" = "yes"],
              [opal_prrte_args="--with-devel-headers  $opal_prrte_args"])
        # add the extra libs
        opal_prrte_args="$opal_prrte_args --with-prrte-extra-lib=$OMPI_TOP_BUILDDIR/opal/libopen-pal.la --with-prrte-extra-ltlib=$OMPI_TOP_BUILDDIR/opal/libopen-pal.la"

        AC_MSG_CHECKING([final prrte configure args])
        AC_MSG_RESULT([$opal_prrte_args])

        CPPFLAGS="-I$OPAL_TOP_SRCDIR -I$OPAL_TOP_BUILDDIR -I$OPAL_TOP_SRCDIR/opal/include -I$OPAL_TOP_BUILDDIR/opal/include $CPPFLAGS"
        OPAL_CONFIG_SUBDIR([prrte],
                           [$opal_prrte_args $opal_subdir_args 'CFLAGS=$CFLAGS' 'CPPFLAGS=$CPPFLAGS'],
                           [opal_prrte_happy=1], [opal_prrte_happy=0])

        OPAL_SUMMARY_ADD([[Miscellaneous]],[[PRRTE]],[prrte],[yes])

    else
        OPAL_SUMMARY_ADD([[Miscellaneous]],[[PRRTE]],[prrte],[no (disabled)])
        AC_MSG_RESULT([no (disabled)])
    fi

    CFLAGS=$opal_prrte_save_CFLAGS
    CPPFLAGS=$opal_prrte_save_CPPFLAGS
    LDFLAGS=$opal_prrte_save_LDFLAGS
    LIBS=$opal_prrte_save_LIBS
    enable_dlopen=$opal_prrte_save_enable_dlopen
    enable_mca_dso=$opal_prrte_save_enable_mca_dso
    enable_mca_static=$opal_prrte_save_enable_mca_static

    OPAL_VAR_SCOPE_POP

])
