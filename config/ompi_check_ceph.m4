dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
dnl                         University Research and Technology
dnl                         Corporation.  All rights reserved.
dnl Copyright (c) 2004-2005 The University of Tennessee and The University
dnl                         of Tennessee Research Foundation.  All rights
dnl                         reserved.
dnl Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
dnl                         University of Stuttgart.  All rights reserved.
dnl Copyright (c) 2004-2006 The Regents of the University of California.
dnl                         All rights reserved.
dnl Copyright (c) 2009-2017 Cisco Systems, Inc.  All rights reserved
dnl Copyright (c) 2008-2018 University of Houston. All rights reserved.
dnl Copyright (c) 2015-2022 Research Organization for Information Science
dnl                         and Technology (RIST).  All rights reserved.
dnl Copyright (c) 2020      Triad National Security, LLC. All rights
dnl                         reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

# OMPI_CHECK_CEPH(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
# check if CEPH support can be found.  sets prefix_{CPPFLAGS,
# LDFLAGS, LIBS} as needed and runs action-if-found if there is
# support, otherwise executes action-if-not-found
AC_DEFUN([OMPI_CHECK_CEPH],[
    OPAL_VAR_SCOPE_PUSH([ompi_check_ceph_happy CPPFLAGS_save])

    # Get some configuration information
    AC_ARG_WITH([ceph],
        [AS_HELP_STRING([--with-ceph(=DIR)],
             [Build Lustre support, optionally adding DIR/include, DIR/lib, and DIR/lib64 to the search path for headers and libraries])])

     OAC_CHECK_PACKAGE([ceph],
                       [$1],
                       [cephfs/libcephfs.h],
                       [cephfs],
                       [ceph_open_layout],
                       [ompi_check_ceph_happy="yes"],
                       [ompi_check_ceph_happy="no"])
     AS_IF([test $ompi_check_ceph_happy = "no"],
           [CPPFLAGS_save=$CPPFLAGS
            CPPFLAGS="-D_FILE_OFFSET_BITS=64 $CPPFLAGS"
            OAC_CHECK_PACKAGE_INVALIDATE_GENERIC_CACHE(ceph, $1, cephfs/libcephfs.h, ceph_open_layout)
            OAC_CHECK_PACKAGE([ceph],
                              [$1],
                              [cephfs/libcephfs.h],
                              [cephfs],
                              [ceph_open_layout],
                              [$1_CPPFLAGS="-D_FILE_OFFSET_BITS=64"
                               ompi_check_ceph_happy="yes"],
                              [ompi_check_ceph_happy="no"])
            CPPFLAGS=$CPPFLAGS_save])

    AC_SUBST([$1_CPPFLAGS])
    OPAL_SUMMARY_ADD([OMPIO File Systems], [Ceph], [], [${$1_SUMMARY}])

    AS_IF([test "$ompi_check_ceph_happy" = "yes"],
          [$2],
          [AS_IF([test -n "$with_ceph" && test "$with_ceph" != "no"],
                 [AC_MSG_ERROR([Ceph support requested but not found.  Aborting])])
           $3])

    OPAL_VAR_SCOPE_POP
])
