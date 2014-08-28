# -*- Mode: Shell ; indent-tabs-mode:nil -*-
#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2006 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2006      QLogic Corp. All rights reserved.
# Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2011-2014 Los Alamos National Security, LLC.
#                         All rights reserved.
# Copyright (c) 2014      Intel, Inc. All rights reserved
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# OPAL_CHECK_UGNI(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
# check if GNI support can be found.  sets prefix_{CPPFLAGS,
# LDFLAGS, LIBS} as needed and runs action-if-found if there is
# support, otherwise executes action-if-not-found
#
# NOTES
# on Cray XE6 systems, the GNI development header (gni_pub.h) is in a
# completely different place than the ugni library (libugni).
#
# EXAMPLE CONFIGURE USAGE:
# --with-ugni=/base/path/to/libugni --with-ugni-includedir=/path/to/gni_pub.h
#
# --with-ugni=/opt/cray/ugni/default --with-ugni-includedir=/opt/cray/gni-headers/default/include

AC_DEFUN([OPAL_CHECK_UGNI], [
    AC_ARG_WITH([ugni], [AC_HELP_STRING([--with-ugni],
        [Build support for Cray GNI. Set PKG_CONFIG_PATH env. variable to specify alternate path.])])

    opal_check_ugni_happy="no"

    AS_IF([test "$with_ugni" = "no"],
          [opal_check_ugni_happy="no"],
          [PKG_CHECK_MODULES([CRAY_UGNI], [cray-ugni],
                      [$1_LDFLAGS="$CRAY_UGNI_LIBS"
                       $1_CPPFLAGS="$CRAY_UGNI_CFLAGS"
                       opal_check_ugni_happy="yes"],
                      [AC_MSG_RESULT([no])
                       opal_check_ugni_happy="no"])])

    opal_check_ugni_$1_save_CPPFLAGS="$CPPFLAGS"
    opal_check_ugni_$1_save_LDFLAGS="$LDFLAGS"
    opal_check_ugni_$1_save_LIBS="$LIBS"

    CPPFLAGS="$CPPFLAGS $$1_CPPFLAGS"
    LDFLAGS="$LDFLAGS $$1_LDFLAGS"
#    echo "+++++++++++++++++++++++CPPFLAGS",$CPPFLAGS
#    echo "+++++++++++++++++++++++LDFLAGSS",$LDFLAGS
#    echo "+++++++++++++++++++++++1_CPPFLAGS",$$1_CPPFLAGS
#    echo "+++++++++++++++++++++++1_LDFLAGSS",$$1_LDFLAGS

#   sanity checks

    AS_IF([test "$opal_check_ugni_happy" = "yes"],
          [AC_CHECK_HEADER([gni_pub.h],[],AC_MSG_ERROR(['gni_pub.h not found.']))
           AC_CHECK_FUNCS([GNI_GetJobResInfo])])

#    AS_IF([test "$opal_check_ugni_happy" = "yes"],
#           [AC_CHECK_FUNCS([GNI_GetJobResInfo])])

    CPPFLAGS="$opal_check_ugni_$1_save_CPPFLAGS"
    LDFLAGS="$opal_check_ugni_$1_save_LDFLAGS"
    LIBS="$opal_check_ugni_$1_save_LIBS"

    AS_IF([test "$opal_check_ugni_happy" = "yes" -a "$enable_progress_threads" = "yes"],
          [AC_MSG_WARN([GNI driver does not currently support progress threads.  Disabling.])
           opal_check_ugni_happy="no"])

    AS_IF([test "$opal_check_ugni_happy" = "yes"],
          [$2],
          [AS_IF([test ! -z "$with_ugni" -a "$with_ugni" != "no"],
                 [AC_MSG_ERROR([GNI support requested but not found.  Cannot continue.])])
           $3])

])
