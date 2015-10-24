# -*- shell-script -*-
#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2011-2013 Los Alamos National Security, LLC.
#                         All rights reserved.
# Copyright (c) 2010-2015 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2013-2015 Intel, Inc. All rights reserved.
# Copyright (c) 2015      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_pmix_pmix1xx_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_opal_pmix_pmix1xx_CONFIG],[
    AC_CONFIG_FILES([opal/mca/pmix/pmix1xx/Makefile])

    OPAL_VAR_SCOPE_PUSH([PMIX_VERSION opal_pmix_pmix1xx_save_CPPFLAGS opal_pmix_pmix1xx_save_LDFLAGS opal_pmix_pmix1xx_save_LIBS opal_pmix_pmix1xx_basedir opal_pmix_pmix1xx_save_cflags])

    PMIX_VERSION=
    opal_pmix_pmix1xx_basedir=opal/mca/pmix/pmix1xx

    opal_pmix_pmix1xx_save_CFLAGS=$CFLAGS
    opal_pmix_pmix1xx_save_CPPFLAGS=$CPPFLAGS
    opal_pmix_pmix1xx_save_LDFLAGS=$LDFLAGS
    opal_pmix_pmix1xx_save_LIBS=$LIBS

    opal_pmix_pmix1xx_args="--enable-embedded-mode --with-pmix-symbol-prefix=opal_pmix_pmix1xx_ --with-libevent-header=\\\"opal/mca/event/$opal_event_base_include\\\" --with-hwloc-header=\\\"$opal_hwloc_base_include\\\""
    if test "$enable_debug" = "yes"; then
        opal_pmix_pmix1xx_args="--enable-debug $opal_pmix_pmix1xx_args"
        CFLAGS="$OPAL_CFLAGS_BEFORE_PICKY $OPAL_VISIBILITY_CFLAGS -g"
    else
        CFLAGS="$OPAL_CFLAGS_BEFORE_PICKY $OPAL_VISIBILITY_CFLAGS"
    fi
    CPPFLAGS="-I$OPAL_TOP_SRCDIR -I$OPAL_TOP_BUILDDIR -I$OPAL_TOP_SRCDIR/opal/include -I$OPAL_TOP_BUILDDIR/opal/include $CPPFLAGS"

    OPAL_CONFIG_SUBDIR([$opal_pmix_pmix1xx_basedir/pmix],
        [$opal_pmix_pmix1xx_args $opal_subdir_args 'CFLAGS=$CFLAGS' 'CPPFLAGS=$CPPFLAGS'],
        [opal_pmix_pmix1xx_happy=1], [opal_pmix_pmix1xx_happy=0])

    if test $opal_pmix_pmix1xx_happy -eq 1; then
        PMIX_VERSION="internal v`$srcdir/$opal_pmix_pmix1xx_basedir/pmix/config/pmix_get_version.sh $srcdir/$opal_pmix_pmix1xx_basedir/pmix/VERSION`"
        # Build flags for our Makefile.am
        opal_pmix_pmix1xx_LIBS='$(OPAL_TOP_BUILDDIR)/'"$opal_pmix_pmix1xx_basedir"'/pmix/libpmix.la'
        opal_pmix_pmix1xx_CPPFLAGS='-I$(OPAL_TOP_BUILDDIR)/opal/mca/pmix/pmix1xx/pmix/include/pmix -I$(OPAL_TOP_BUILDDIR)/opal/mca/pmix/pmix1xx/pmix/include -I$(OPAL_TOP_BUILDDIR)/opal/mca/pmix/pmix1xx/pmix -I$(OPAL_TOP_SRCDIR)/opal/mca/pmix/pmix1xx/pmix'
        AC_SUBST([opal_pmix_pmix1xx_LIBS])
        AC_SUBST([opal_pmix_pmix1xx_CPPFLAGS])
    fi

    AC_DEFINE_UNQUOTED([PMIX_PMIX1XX_PMIX_VERSION],
                       ["$PMIX_VERSION"],
                       [Version of PMIx])

    # Finally, add a flag to support static builds
    pmix_pmix1xx_WRAPPER_EXTRA_LIBS=-lpmix

    CFLAGS=$opal_pmix_pmix1xx_save_CFLAGS
    CPPFLAGS=$opal_pmix_pmix1xx_save_CPPFLAGS
    LDFLAGS=$opal_pmix_pmix1xx_save_LDFLAGS
    LIBS=$opal_pmix_pmix1xx_save_LIBS

    AS_IF([test $opal_pmix_pmix1xx_happy -eq 1],
          [$1],
          [$2])

    OPAL_VAR_SCOPE_POP
])dnl
