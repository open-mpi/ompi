dnl -*- shell-script ; indent-tabs-mode:nil -*-
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
dnl Copyright (c) 2009-2011 Cisco Systems, Inc.  All rights reserved.
dnl Copyright (c) 2011-2014 Los Alamos National Security, LLC. All rights
dnl                         reserved.
dnl Copyright (c) 2014      Intel, Inc. All rights reserved.
dnl Copyright (c) 2014-2015 Research Organization for Information Science
dnl                         and Technology (RIST). All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

AC_DEFUN([OPAL_CHECK_CRAY_PMI_EXPLICIT],[

    opal_cray_pmi_lib_good=0
    opal_cray_pmi_include_good=0

    AC_MSG_CHECKING([Checking for Cray PMI with explicit path $with_cray_pmi])
    AS_IF([test -d "$with_cray_pmi/lib64"],
          [opal_cray_pmi_lib_good=1],
          [opal_cray_pmi_lib_good=0
          AC_MSG_WARN([No libpmi in path specified by $with_cray_pmi])])

    AS_IF([test -f "$with_cray_pmi/include/pmi.h"],
          [opal_cray_pmi_include_good=1],
          [opal_cray_pmi_include_good=0
          AC_MSG_WARN([No pmi.h in path specified by $with_cray_pmi])])

    AS_IF([test "$opal_cray_pmi_lib_good" -eq 1 && test "$opal_cray_pmi_include_good" -eq 1],
           [CRAY_PMI_LDFLAGS="-L$with_cray_pmi/lib64 -L/usr/lib/alps"
            CRAY_PMI_LIBS="-L$with_cray_pmi/lib64 -lpmi"
            CRAY_PMI_CFLAGS="-I $with_cray_pmi/include"
            $1],
            [$2])

#
#   this logic assumes knowledge about all the dependencies of the Cray PMI library,
#   something that Cray doesn't generally document
#
    AS_IF([test "$enable_static" == "yes"],
          [AS_IF([test -d /usr/lib/alps],
                 [AC_MSG_RESULT([Detected presense of /usr/lib/alps])
                  CRAY_PMI_LDFLAGS="$CRAY_PMI_LDFLAGS -L/usr/lib/alps -lalpslli -lalpsutil"
                  CRAY_PMI_LIBS="$CRAY_PMI_LIBS -L/usr/lib/alps -lalpslli -lalpsutil"],
                 [AS_IF([test -d /opt/cray/xe-sysroot/default/usr/lib/alps],
                        [AC_MSG_RESULT([Detected presense of /opt/cray/xe-sysroot/default/usr/lib/alps])
                         CRAY_PMI_LDFLAGS="$CRAY_PMI_LDFLAGS -L/opt/cray/xe-sysroot/default/usr/lib/alps -lalpslli -lalpsutil"
                         CRAY_PMI_LIBS="$CRAY_PMI_LIBS -L/opt/cray/xe-sysroot/default/usr/lib/alps -lalpslli -lalpsutil"],
                        [AC_MSG_ERROR([Requested enabling static linking but unable to local libalpslli and libalpsutil])])
                  ])
           ])
])

#
# special check for cray pmi, uses macro(s) from pkg.m4
#
# OPAL_CHECK_CRAY_PMI(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
AC_DEFUN([OPAL_CHECK_CRAY_PMI],[
    AC_ARG_WITH([cray_pmi],
                [AC_HELP_STRING([--with-cray-pmi(=DIR)],
                [Build Cray PMI support, optionally adding DIR to the search path (default: auto)])],
                                                                                [], with_cray_pmi=auto)
   AC_MSG_CHECKING([for Cray PMI support])
   AS_IF([test "$with_cray_pmi" = "no"],
         [AC_MSG_RESULT([no])
          $3],
         [AS_IF([test "$with_cray_pmi" = "auto" || test "$with_cray_pmi" = "yes"],
                 [PKG_CHECK_MODULES_STATIC([CRAY_PMI], [cray-pmi],
                                    [opal_check_cray_pmi_happy="yes"],
                                    [opal_check_cray_pmi_happy="no"]
                                    [AS_IF([test "$with_cray_pmi" = "yes"],
                                           [AC_MSG_WARN([Cray PMI support requested but pkg-config failed.])
                                            AC_MSG_WARN([Need to explicitly indicate cray pmi directory])
                                            AC_MSG_WARN([on the configure line using --with-cray-pmi option.])
                                            AC_MSG_ERROR([Aborting])],[])]
                                     )],
                 [OPAL_CHECK_CRAY_PMI_EXPLICIT([opal_check_cray_pmi_happy="yes"],
                                               [opal_check_cray_pmi_happy="no"])
                  AC_MSG_WARN([opal_chack_cray_pmi_happy = $opal_check_cray_pmi_happy])])
         ])

    AS_IF([test "$opal_check_cray_pmi_happy" = "yes" && test "$enable_static" = "yes"],
          [CRAY_PMI_LIBS = $CRAY_PMI_STATIC_LIBS],[])

    AS_IF([test "$opal_check_cray_pmi_happy" = "yes"],
          [$1_LDFLAGS="$CRAY_PMI_LIBS"
           $1_CPPFLAGS="$CRAY_PMI_CFLAGS"
           $1_LIBS="$CRAY_PMI_LIBS"
           $2], [$3])
])



