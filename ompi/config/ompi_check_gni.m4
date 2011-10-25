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
# Copyright (c) 2004-2006 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2006      QLogic Corp. All rights reserved.
# Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2011      Los Alamos National Security, LLC.
#                         All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# OMPI_CHECK_GNI(prefix, [action-if-found], [action-if-not-found])
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
# --with-gni=/base/path/to/libugni --with-gni-includedir=/path/to/gni_pub.h
#
# --with-gni=/opt/cray/ugni/default --with-gni-includedir=/opt/cray/gni-headers/default/include

AC_DEFUN([OMPI_CHECK_GNI], [
    AC_ARG_WITH([gni], [
        AC_HELP_STRING([--with-gni(=DIR)],
        [Build GNI (Cray Gemini) support, optionally adding DIR/include, DIR/lib, and DIR/lib64 to the search path for headers and libraries])])

    dnl does the path exist?
    OMPI_CHECK_WITHDIR([gni], [$with_gni], [.])

    AC_ARG_WITH([gni-libdir], [
        AC_HELP_STRING([--with-gni-libdir=DIR], [
            Search for GNI (Cray Gemini) libraries in DIR])])
    OMPI_CHECK_WITHDIR([gni-libdir], [$with_gni_libdir], [libugni.*])

    AC_ARG_WITH([gni-includedir], [
        AC_HELP_STRING([--with-gni-includedir=DIR], [
            Search for GNI (Cray Gemini) headers in DIR])])
    OMPI_CHECK_WITHDIR([gni-includedir], [$with_gni_includedir], [gni_pub.h])

    AS_IF([test "$with_gni_includedir" != "" -a "$with_gni_includedir" != "yes" -a "$with_gni_includedir" != "no"],
          [$1_CPPFLAGS="$$1_CPPFLAGS -I$with_gni_includedir"])

    ompi_check_gni_$1_save_CPPFLAGS="$CPPFLAGS"
    ompi_check_gni_$1_save_LDFLAGS="$LDFLAGS"
    ompi_check_gni_$1_save_LIBS="$LIBS"

    AS_IF([test "$with_gni" != "no"], [
        AS_IF([test ! -z "$with_gni" -a "$with_gni" != "yes"], [
            ompi_check_gni_dir="$with_gni"])
        AS_IF([test ! -z "$with_gni_libdir" -a "$with_gni_libdir" != "yes"], [
            ompi_check_gni_libdir="$with_gni_libdir"])

        OMPI_CHECK_PACKAGE([$1],
            [ugni.h],
            [ugni],
            [GNI_CdmCreate],
            [],
            [$ompi_check_gni_dir],
            [$ompi_check_gni_libdir],
            [ompi_check_gni_happy="yes"],
            [ompi_check_gni_happy="no"])],
          [ompi_check_gni_happy="no"])

    CPPFLAGS="$ompi_check_gni_$1_save_CPPFLAGS"
    LDFLAGS="$ompi_check_gni_$1_save_LDFLAGS"
    LIBS="$ompi_check_gni_$1_save_LIBS"

    dnl XXX not sure if this is true, but will assume so...
    AS_IF([test "$ompi_check_gni_happy" = "yes" -a "$enable_progress_threads" = "yes"],
          [AC_MSG_WARN([GNI driver does not currently support progress threads.  Disabling.])
           ompi_check_gni_happy="no"])

    AS_IF([test "$ompi_check_gni_happy" = "yes"],
          [$2],
          [AS_IF([test ! -z "$with_gni" -a "$with_gni" != "no"],
                 [AC_MSG_ERROR([GNI support requested but not found.  Cannot continue.])])
           $3])
])

