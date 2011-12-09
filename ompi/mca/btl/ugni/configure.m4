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

# OMPI_CHECK_UGNI(prefix, [action-if-found], [action-if-not-found])
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

AC_DEFUN([MCA_ompi_btl_ugni_CONFIG],[
    AC_CONFIG_FILES([ompi/mca/btl/ugni/Makefile])

    OMPI_CHECK_UGNI([btl_ugni],
                     [btl_ugni_happy="yes"],
                     [btl_ugni_happy="no"])

    AS_IF([test "$btl_ugni_happy" = "yes"],
          [btl_ugni_WRAPPER_EXTRA_LDFLAGS="$btl_ugni_LDFLAGS"
           btl_ugni_WRAPPER_EXTRA_LIBS="$btl_ugni_LIBS"
           $1],
          [$2])

    # substitute in the things needed to build ugni
    AC_SUBST([btl_ugni_CPPFLAGS])
    AC_SUBST([btl_ugni_LDFLAGS])
    AC_SUBST([btl_ugni_LIBS])
])dnl
