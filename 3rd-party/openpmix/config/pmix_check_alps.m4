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
dnl Copyright (c) 2004-2005 The Regents of the University of California.
dnl                         All rights reserved.
dnl Copyright (c) 2009-2020 Cisco Systems, Inc.  All rights reserved
dnl Copyright (c) 2015      Research Organization for Information Science
dnl                         and Technology (RIST). All rights reserved.
dnl Copyright (c) 2016      Los Alamos National Security, LLC. All rights
dnl                         reserved.
dnl Copyright (c) 2019      Intel, Inc.  All rights reserved.
dnl Copyright (c) 2020      Triad National Security, LLC. All rights
dnl                         reserved.
dnl Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
dnl Copyright (c) 2022      Amazon.com, Inc. or its affiliates.
dnl                         All Rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

# PMIX_CHECK_ALPS_CLE4([action-if-found], [action-if-not-found])
# --------------------------------------------------------
AC_DEFUN([PMIX_CHECK_ALPS_CLE4],[

#
#   if we've gotten here, its because we are building on a CLE 4 system
#
    pmix_check_alps_cle4_libdir_happy="no"
    pmix_check_alps_cle4_dir_happy="no"

    AC_MSG_CHECKING([Checking for ALPS components on a CLE 4 system with alps  $with_alps])

    AC_ARG_WITH([alps-libdir],
                [AS_HELP_STRING([--with-alps-libdir=DIR],
                [Location of alps libraries (alpslli, alpsutil) (default: /usr/lib/alps (/opt/cray/xe-sysroot/default/user on eslogin nodes))])])

#
#   check to see if PMIx is being built on a CLE 4 eslogin node
#
    AS_IF([test -f /etc/opt/cray/release/ESLrelease],
          [default_alps_dir="/opt/cray/xe-sysroot/default/usr"],
          [default_alps_dir="/usr"])

     AS_IF([test -z "$with_alps_libdir"],
           [AS_IF([test "$with_alps" != "yes" && test "$with_alps" != "auto"],
                  [AS_IF([test -d "$with_alps_libdir/lib64"],
                         [pmix_check_alps_libdir="$with_alps_libdir/lib64"],
                         [pmix_check_alps_libdir="$with_alps_libdir/lib"])],
                  [ pmix_check_alps_libdir="$default_alps_dir/lib/alps"])
           ],[])

     AS_IF([test "$with_alps" = "yes" || test "$with_alps" = "auto"],
           [pmix_check_alps_dir=$default_alps_dir],
           [pmix_check_alps_dir=$with_alps])

    AC_MSG_CHECKING([if $pmix_check_alps_libdir/libalps.a is present])
    AS_IF([test -f "$pmix_check_alps_libdir/libalps.a"],
          [pmix_check_alps_libdir_cle4_happy="yes"],
          [pmix_check_alps_libdir_cle4_happy="no",
           AC_MSG_RESULT([no])])

    AC_MSG_CHECKING([if $pmix_check_alps_dir/include/alps/apInfo.h is present])
    AS_IF([test -f "$pmix_check_alps_dir/include/alps/apInfo.h"],
          [pmix_check_alps_dir_cle4_happy="yes"],
          [pmix_check_alps_dir_cle4_happy="no"
           AC_MSG_RESULT([no])])

    AS_IF([test "$pmix_check_alps_libdir_cle4_happy" = "yes" && test "$pmix_check_alps_dir_cle4_happy" = "yes"],
          [CRAY_ALPSLLI_CFLAGS="-I$pmix_check_alps_dir/include"
           CRAY_ALPSLLI_LIBS="-L$pmix_check_alps_libdir -lalpslli -lalpsutil"
           CRAY_ALPSLLI_STATIC_LIBS="-L$pmix_check_alps_libdir -lalpslli -lalpsutil"
           $1],
          [$2])
])




# PMIX_CHECK_ALPS(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
AC_DEFUN([PMIX_CHECK_ALPS],[
    if test -z "$pmix_check_cray_alps_happy"; then

        AC_ARG_WITH([alps],
                    [AS_HELP_STRING([--with-alps(=DIR|yes|no)],
                    [Build with ALPS scheduler component, optionally adding DIR/include, DIR/lib, and DIR/lib64 to the search path for headers and libraries (default: auto)])],[],with_alps=auto)

        if test -f /etc/opt/cray/release/clerelease; then
            cle_level=`awk -F. '{print [$]1}' /etc/opt/cray/release/clerelease`
        else
            cle_level="unknown"
        fi

        AC_MSG_CHECKING([for ALPS support cle level $cle_level])
        AS_IF([test "$cle_level" = "4" && test "$with_alps" != "no"],
              [PMIX_CHECK_ALPS_CLE4([pmix_check_cray_alps_happy="yes"],
                                    [pmix_check_cray_alps_happy="no"])],
              [AS_IF([test "$with_alps" = "no"],
                     [AC_MSG_RESULT([no])
                      $3],
                     [AS_IF([test "$with_alps" = "auto" || test "$with_alps" = "yes"],
                            [PKG_CHECK_MODULES_STATIC([CRAY_ALPSLLI], [cray-alpslli],
                                                      [pmix_check_cray_alps_happy="yes"],
                                                      [pmix_check_cray_alps_happy="no"]
                                                      [AS_IF([test "$with_alps" = "yes"],
                                                             [AC_MSG_WARN([ALPS support requested but pkg-config failed.])
                                                              AC_MSG_WARN([Need to explicitly indicate ALPS directory])
                                                              AC_MSG_WARN([on the configure line using --with-alps option.])
                                                              AC_MSG_ERROR([Aborting])],[])]
                                                      )
                              PKG_CHECK_MODULES_STATIC([CRAY_ALPSUTIL], [cray-alpsutil],
                                                [pmix_check_cray_alps_happy="yes"],
                                                [pmix_check_cray_alps_happy="no"]
                                                [AS_IF([test "$with_alps" = "yes"],
                                                       [AC_MSG_WARN([ALPS support requested but pkg-config failed.])
                                                        AC_MSG_WARN([Need to explicitly indicate ALPS directory])
                                                        AC_MSG_WARN([on the configure line using --with-alps option.])
                                                        AC_MSG_ERROR([Aborting])],[])]
                                                       )

                               PKG_CHECK_MODULES_STATIC([CRAY_ALPS], [cray-alps],
                                               [pmix_check_cray_alps_happy="yes"],
                                               [pmix_check_cray_alps_happy="no"]
                                               [AS_IF([test "$with_alps" = "yes"],
                                                      [AC_MSG_WARN([ALPS support requested but pkg-config failed.])
                                                       AC_MSG_WARN([Need to explicitly indicate ALPS directory])
                                                       AC_MSG_WARN([on the configure line using --with-alps option.])
                                                       AC_MSG_ERROR([Aborting])],[])]
                                                       )
                               PKG_CHECK_MODULES_STATIC([CRAY_WLM_DETECT], [cray-wlm_detect],
                                               [pmix_check_cray_alps_happy="yes"
                                                AC_DEFINE_UNQUOTED([CRAY_WLM_DETECT],[1],
                                                                   [defined to 1 if cray wlm available, 0 otherwise])
                                               ],
                                               [pmix_check_cray_alps_happy="no"]
                                               [AS_IF([test "$with_alps" = "yes"],
                                                      [AC_MSG_WARN([ALPS support requested but pkg-config failed.])
                                                       AC_MSG_WARN([Need to explicitly indicate ALPS directory])
                                                       AC_MSG_WARN([on the configure line using --with-alps option.])
                                                       AC_MSG_ERROR([Aborting])],[])]
                                                       )
                            ],
                            [AC_MSG_WARN([See ./configure --help for how to control Open MPI])
                             AC_MSG_WARN([configuration for ALPS on CLE 5 and higher systems])
                             AC_MSG_ERROR([Aborting])])
                    ])
               ])

        AC_MSG_RESULT([pmix_check_cray_alps_happy = $pmix_check_cray_alps_happy])

        AS_IF([test "$pmix_check_cray_alps_happy" = "yes"],
             [pmix_have_cray_alps=1],
             [pmix_have_cray_alps=0])

        AC_DEFINE_UNQUOTED([PMIX_HAVE_CRAY_ALPS],
                           [$pmix_have_cray_alps],
                           [defined to 1 if cray alps env, 0 otherwise])

        AS_IF([test "$pmix_check_cray_alps_happy" = "yes" && test "$enable_static" = "yes"],
              [CRAY_ALPSLLI_LIBS = $CRAY_ALPSLLI_STATIC_LIBS
               CRAY_ALPSUTIL_LIBS = $CRAY_ALPSUTIL_STATIC_LIBS],
              [])

    fi

    AS_IF([test "$pmix_check_cray_alps_happy" = "yes"],
          [$1_LDFLAGS="[$]$1_LDFLAGS $CRAY_ALPSLLI_LIBS $CRAY_ALPSUTIL_LIBS"
           $1_CPPFLAGS="[$]$1_CPPFLAGS $CRAY_ALPSLLI_CFLAGS $CRAY_ALPSUTIL_CFLAGS $CRAY_ALPS_CFLAGS $CRAY_WLM_DETECT_CFLAGS"
           $1_LIBS="[$]$1_LIBS $CRAY_ALPSLLI_LIBS $CRAY_ALPSUTIL_LIBS $CRAY_WLM_DETECT_LIBS"
           $1_WRAPPER_EXTRA_LDFLAGS="$CRAY_ALPSLLI_LIBS $CRAY_ALPSUTIL_LIBS $CRAY_WLM_DETECT_LIBS"
           $1_WRAPPER_EXTRA_LIBS="$CRAY_ALPSLLI_LIBS $CRAY_ALPSUTIL_LIBS $CRAY_WLM_DETECT_LIBS"
	   $2],
	  [$3])
])
