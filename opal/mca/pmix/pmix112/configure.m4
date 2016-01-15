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
# Copyright (c) 2010-2016 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2013-2015 Intel, Inc. All rights reserved.
# Copyright (c) 2015      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_pmix_pmix112_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_opal_pmix_pmix112_CONFIG],[
    AC_CONFIG_FILES([opal/mca/pmix/pmix112/Makefile])

    OPAL_VAR_SCOPE_PUSH([PMIX_VERSION opal_pmix_pmix112_save_CPPFLAGS opal_pmix_pmix112_save_LDFLAGS opal_pmix_pmix112_save_LIBS opal_pmix_pmix112_basedir opal_pmix_pmix112_save_cflags])

    AS_IF([test "$opal_external_pmix_happy" = "yes"],
          [AC_MSG_WARN([using an external pmix; disqualifiying this component])
           opal_pmix_pmix112_happy=0],
          [PMIX_VERSION=
           opal_pmix_pmix112_basedir=opal/mca/pmix/pmix112

           opal_pmix_pmix112_save_CFLAGS=$CFLAGS
           opal_pmix_pmix112_save_CPPFLAGS=$CPPFLAGS
           opal_pmix_pmix112_save_LDFLAGS=$LDFLAGS
           opal_pmix_pmix112_save_LIBS=$LIBS

           opal_pmix_pmix112_args="--enable-embedded-mode --with-pmix-symbol-prefix=opal_pmix_pmix112_ --with-libevent-header=\\\"opal/mca/event/$opal_event_base_include\\\" --with-hwloc-header=\\\"$opal_hwloc_base_include\\\""
           AS_IF([test "$enable_debug" = "yes"],
                 [opal_pmix_pmix112_args="--enable-debug $opal_pmix_pmix112_args"
                  CFLAGS="$OPAL_CFLAGS_BEFORE_PICKY $OPAL_VISIBILITY_CFLAGS -g"],
                 [opal_pmix_pmix112_args="--disable-debug $opal_pmix_pmix112_args"
                  CFLAGS="$OPAL_CFLAGS_BEFORE_PICKY $OPAL_VISIBILITY_CFLAGS"])
           CPPFLAGS="-I$OPAL_TOP_SRCDIR -I$OPAL_TOP_BUILDDIR -I$OPAL_TOP_SRCDIR/opal/include -I$OPAL_TOP_BUILDDIR/opal/include $CPPFLAGS"

           OPAL_CONFIG_SUBDIR([$opal_pmix_pmix112_basedir/pmix],
               [$opal_pmix_pmix112_args $opal_subdir_args 'CFLAGS=$CFLAGS' 'CPPFLAGS=$CPPFLAGS'],
               [opal_pmix_pmix112_happy=1], [opal_pmix_pmix112_happy=0])

           AS_IF([test $opal_pmix_pmix112_happy -eq 1],
                 [PMIX_VERSION="internal v`$srcdir/$opal_pmix_pmix112_basedir/pmix/config/pmix_get_version.sh $srcdir/$opal_pmix_pmix112_basedir/pmix/VERSION`"
                  # Build flags for our Makefile.am
                  opal_pmix_pmix112_LIBS='$(OPAL_TOP_BUILDDIR)/'"$opal_pmix_pmix112_basedir"'/pmix/libpmix.la'
                  opal_pmix_pmix112_CPPFLAGS='-I$(OPAL_TOP_BUILDDIR)/opal/mca/pmix/pmix112/pmix/include/pmix -I$(OPAL_TOP_BUILDDIR)/opal/mca/pmix/pmix112/pmix/include -I$(OPAL_TOP_BUILDDIR)/opal/mca/pmix/pmix112/pmix -I$(OPAL_TOP_SRCDIR)/opal/mca/pmix/pmix112/pmix'
                  AC_SUBST([opal_pmix_pmix112_LIBS])
                  AC_SUBST([opal_pmix_pmix112_CPPFLAGS])])

           CFLAGS=$opal_pmix_pmix112_save_CFLAGS
           CPPFLAGS=$opal_pmix_pmix112_save_CPPFLAGS
           LDFLAGS=$opal_pmix_pmix112_save_LDFLAGS
           LIBS=$opal_pmix_pmix112_save_LIBS
          ])

    AS_IF([test $opal_pmix_pmix112_happy -eq 1],
          [$1],
          [$2])

    OPAL_VAR_SCOPE_POP
])dnl
