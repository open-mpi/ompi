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
    AC_ARG_WITH([ugni], [AC_HELP_STRING([--with-ugni(=DIR)],
        [Build GNI (Cray Gemini) support, optionally adding DIR/include, DIR/lib, and DIR/lib64 to the search path for headers and libraries])])

    dnl does the path exist?
    OPAL_CHECK_WITHDIR([ugni], [$with_ugni], [.])

    AC_ARG_WITH([ugni-libdir], [AC_HELP_STRING([--with-ugni-libdir=DIR],
         [Search for GNI (Cray Gemini) libraries in DIR])])
    OPAL_CHECK_WITHDIR([ugni-libdir], [$with_ugni_libdir], [libugni.*])

    AC_ARG_WITH([ugni-includedir], 
         [AC_HELP_STRING([--with-ugni-includedir=DIR],
         [Search for GNI (Cray Gemini) headers in DIR])])
    OPAL_CHECK_WITHDIR([ugni-includedir], [$with_ugni_includedir], [gni_pub.h])

    AS_IF([test "$with_ugni_includedir" != "" -a "$with_ugni_includedir" != "yes" -a "$with_ugni_includedir" != "no"],
          [$1_CPPFLAGS="$$1_CPPFLAGS -I$with_ugni_includedir"])

    opal_check_ugni_$1_save_CPPFLAGS="$CPPFLAGS"
    opal_check_ugni_$1_save_LDFLAGS="$LDFLAGS"
    opal_check_ugni_$1_save_LIBS="$LIBS"

    AS_IF([test "$with_ugni" != "no"], [
        AS_IF([test ! -z "$with_ugni" -a "$with_ugni" != "yes"], [
            opal_check_ugni_dir="$with_ugni"])
        AS_IF([test ! -z "$with_ugni_libdir" -a "$with_ugni_libdir" != "yes"], [
            opal_check_ugni_libdir="$with_ugni_libdir"])

        OPAL_CHECK_PACKAGE([$1],
            [ugni.h],
            [ugni],
            [GNI_CdmCreate],
            [],
            [$opal_check_ugni_dir],
            [$opal_check_ugni_libdir],
            [opal_check_ugni_happy="yes"],
            [opal_check_ugni_happy="no"])],
          [opal_check_ugni_happy="no"])

    LIBS="$LIBS $$1_LIBS"
    LDFLAGS="$LDFLAGS $$1_LDFLAGS"

    AS_IF([test "$opal_check_ugni_happy" = "yes"],
          [AC_CHECK_FUNCS([GNI_GetJobResInfo])])

    CPPFLAGS="$opal_check_ugni_$1_save_CPPFLAGS"
    LDFLAGS="$opal_check_ugni_$1_save_LDFLAGS"
    LIBS="$opal_check_ugni_$1_save_LIBS"

    dnl XXX not sure if this is true, but will assume so...
    AS_IF([test "$opal_check_ugni_happy" = "yes" -a "$enable_progress_threads" = "yes"],
          [AC_MSG_WARN([GNI driver does not currently support progress threads.  Disabling.])
           opal_check_ugni_happy="no"])

    AS_IF([test "$opal_check_ugni_happy" = "yes"],
          [$2],
          [AS_IF([test ! -z "$with_ugni" -a "$with_ugni" != "no"],
                 [AC_MSG_ERROR([GNI support requested but not found.  Cannot continue.])])
           $3])
])
