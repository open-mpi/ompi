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
dnl Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
dnl Copyright (c) 2015      Research Organization for Information Science
dnl                         and Technology (RIST). All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

# ORTE_CHECK_ALPS_CLE4([action-if-found], [action-if-not-found])
# --------------------------------------------------------
AC_DEFUN([ORTE_CHECK_ALPS_CLE4],[

#
#   if we've gotten here, its because we are building on a CLE 4 system
#
    orte_check_alps_cle4_libdir_happy="no"
    orte_check_alps_cle4_dir_happy="no"

    AC_MSG_CHECKING([Checking for ALPS components on a CLE 4 system with alps  $with_alps])

    AC_ARG_WITH([alps-libdir],
                [AC_HELP_STRING([--with-alps-libdir=DIR],
                [Location of alps libraries (alpslli, alpsutil) (default: /usr/lib/alps (/opt/cray/xe-sysroot/default/user on eslogin nodes))])])

#
#   check to see if Open MPI is being built on a CLE 4 eslogin node
#
    AS_IF([test -f /etc/opt/cray/release/ESLrelease],
          [default_alps_dir="/opt/cray/xe-sysroot/default/usr"],
          [default_alps_dir="/usr"])

     AS_IF([test -z "$with_alps_libdir"],
           [AS_IF([test "$with_alps" != "yes" && test "$with_alps" != "auto"],
                  [AS_IF([test -d "$with_alps_libdir/lib64"],
                         [orte_check_alps_libdir="$with_alps_libdir/lib64"],
                         [orte_check_alps_libdir="$with_alps_libdir/lib"])],
                  [ orte_check_alps_libdir="$default_alps_dir/lib/alps"])
           ],[])

     AS_IF([test "$with_alps" = "yes" || test "$with_alps" = "auto"],
           [orte_check_alps_dir=$default_alps_dir],
           [orte_check_alps_dir=$with_alps])

    AC_MSG_CHECKING([if $orte_check_alps_libdir/libalps.a is present])
    AS_IF([test -f "$orte_check_alps_libdir/libalps.a"],
          [orte_check_alps_libdir_cle4_happy="yes"],
          [orte_check_alps_libdir_cle4_happy="no",
           AC_MSG_RESULT([no])])

    AC_MSG_CHECKING([if $orte_check_alps_dir/include/alps/apInfo.h is present])
    AS_IF([test -f "$orte_check_alps_dir/include/alps/apInfo.h"],
          [orte_check_alps_dir_cle4_happy="yes"],
          [orte_check_alps_dir_cle4_happy="no"
           AC_MSG_RESULT([no])])

    AS_IF([test "$orte_check_alps_libdir_cle4_happy" = "yes" && test "$orte_check_alps_dir_cle4_happy" = "yes"],
          [CRAY_ALPSLLI_CFLAGS="-I$orte_check_alps_dir/include"
           CRAY_ALPSLLI_LIBS="-L$orte_check_alps_libdir -lalpslli -lalpsutil"
           CRAY_ALPSLLI_STATIC_LIBS="-L$orte_check_alps_libdir -lalpslli -lalpsutil"
           $1],
          [$2])
])




# ORTE_CHECK_ALPS(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
AC_DEFUN([ORTE_CHECK_ALPS],[
    if test -z "$orte_check_alps_happy"; then

        AC_ARG_WITH([alps],
                    [AC_HELP_STRING([--with-alps(=DIR|yes|no)],
                    [Build with ALPS scheduler component, optionally adding DIR/include, DIR/lib, and DIR/lib64 to the search path for headers and libraries (default: auto)])],[],with_alps=auto)

        if test -f /etc/opt/cray/release/clerelease; then
            cle_level=`awk -F. '{print [$]1}' /etc/opt/cray/release/clerelease`
        else
            cle_level="unknown"
        fi

        AC_MSG_CHECKING([for ALPS support cle level $cle_level])
        AS_IF([test "$cle_level" = "4" && test "$with_alps" != "no"],
              [ORTE_CHECK_ALPS_CLE4([orte_check_cray_alps_happy="yes"],
                                    [orte_check_cray_alps_happy="no"])],
              [AS_IF([test "$with_alps" = "no"],
                     [AC_MSG_RESULT([no])
                      $3],
                     [AS_IF([test "$with_alps" = "auto" || test "$with_alps" = "yes"],
                            [PKG_CHECK_MODULES_STATIC([CRAY_ALPSLLI], [cray-alpslli],
                                                      [orte_check_cray_alps_happy="yes"],
                                                      [orte_check_cray_alps_happy="no"]
                                                      [AS_IF([test "$with_alps" = "yes"],
                                                             [AC_MSG_WARN([ALPS support requested but pkg-config failed.])
                                                              AC_MSG_WARN([Need to explicitly indicate ALPS directory])
                                                              AC_MSG_WARN([on the configure line using --with-alps option.])
                                                              AC_MSG_ERROR([Aborting])],[])]
                                                      )
                              PKG_CHECK_MODULES_STATIC([CRAY_ALPSUTIL], [cray-alpsutil],
                                                [orte_check_cray_alps_happy="yes"],
                                                [orte_check_cray_alps_happy="no"]
                                                [AS_IF([test "$with_alps" = "yes"],
                                                       [AC_MSG_WARN([ALPS support requested but pkg-config failed.])
                                                        AC_MSG_WARN([Need to explicitly indicate ALPS directory])
                                                        AC_MSG_WARN([on the configure line using --with-alps option.])
                                                        AC_MSG_ERROR([Aborting])],[])]
                                                       )

                               PKG_CHECK_MODULES_STATIC([CRAY_ALPS], [cray-alps],
                                               [orte_check_cray_alps_happy="yes"],
                                               [orte_check_cray_alps_happy="no"]
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

        AC_MSG_RESULT([orte_check_cray_alps_happy = $orte_check_cray_alps_happy])

        AS_IF([test "$orte_check_cray_alps_happy" = "yes" && test "$enable_static" = "yes"],
              [CRAY_ALPSLLI_LIBS = $CRAY_ALPSLLI_STATIC_LIBS
               CRAY_ALPSUTIL_LIBS = $CRAY_ALPSUTIL_STATIC_LIBS],
              [])

        AS_IF([test "$orte_check_cray_alps_happy" = "yes"],
              [$1_LDFLAGS="$CRAY_ALPSLLI_LIBS $CRAY_ALPSUTIL_LIBS"
               $1_CPPFLAGS="$CRAY_ALPSLLI_CFLAGS $CRAY_ALPSUTIL_CFLAGS $CRAY_ALPS_CFLAGS"
               $1_LIBS="$CRAY_ALPSLLI_LIBS $CRAY_ALPSUTIL_LIBS"
               $1_WRAPPER_EXTRA_LDFLAGS="$CRAY_ALPSLLI_LIBS $CRAY_ALPSUTIL_LIBS"
               $1_WRAPPER_EXTRA_LIBS="$CRAY_ALPSLLI_LIBS $CRAY_ALPSUTIL_LIBS"],
              [])

    fi

    AS_IF([test "$orte_check_cray_alps_happy" = "yes"],
          [$2], [$3])
])
