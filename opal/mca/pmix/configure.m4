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
    
    # check for PMI headers
    AC_MSG_CHECKING([for PMI headers])
    OPAL_CHECK_PMI([pmix], [opal_pmix_happy=1], [opal_pmix_happy=0])
    AS_IF([test $opal_pmix_happy = 1],
          [AC_MSG_RESULT([yes])],
          [AC_MSG_RESULT([no])])

    # tell us whether or not to build
    AC_DEFINE_UNQUOTED([HAVE_PMI_HEADERS], [$opal_pmix_happy], [Whether we found the PMI headers or not])

    # add the required make directives - we only care about the CPPFLAGS
    AC_MSG_CHECKING([for PMIX CPPFLAGS])
    AC_SUBST([pmix_CPPFLAGS])
    AC_MSG_RESULT([$pmix_CPPFLAGS])
])dnl
