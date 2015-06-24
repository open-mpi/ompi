dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2014      Intel, Inc. All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

# MCA_rtc_freq_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_orte_rtc_freq_CONFIG], [
    AC_CONFIG_FILES([orte/mca/rtc/freq/Makefile])

    # do not build if not on linux
    AC_MSG_CHECKING([for freq control support])
    AS_IF([test "$opal_found_linux" = "yes"],
          [AC_MSG_RESULT([yes])
           $1],
          [AC_MSG_RESULT([only supported on Linux systems])
           $2])
])dnl
