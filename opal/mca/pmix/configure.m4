dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2014      Intel, Inc.  All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

AC_DEFUN([MCA_opal_pmix_CONFIG],[

    # configure all the components first
    MCA_CONFIGURE_FRAMEWORK($1, $2, 1)

    # Get the CPPFLAGS for the PMI headers
    AC_MSG_CHECKING([for PMI headers])
    OPAL_CHECK_PMI([pmix], [opal_pmix_happy=1], [opal_pmix_happy=0])
    OPAL_CHECK_CRAY_PMI([pmix], [opal_pmix_cray_happy=1], [opal_pmix_cray_happy=0])
    AS_IF([test $opal_pmix_happy = 1 -o $opal_pmix_cray_happy = 1],
          [AC_MSG_RESULT([yes])],
          [AC_MSG_RESULT([no])])

    # add the required make directives - we only care about the CPPFLAGS
    AC_MSG_CHECKING([for PMIX CPPFLAGS])
    AC_SUBST([pmix_CPPFLAGS])
    AC_MSG_RESULT([$pmix_CPPFLAGS])
])dnl
