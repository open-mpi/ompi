dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2014      Intel, Inc. All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

# MCA_sensor_coretemp_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_orte_sensor_coretemp_CONFIG], [
    AC_CONFIG_FILES([orte/mca/sensor/coretemp/Makefile])

    AC_ARG_WITH([coretemp],
                [AC_HELP_STRING([--with-coretemp],
                                [Build coretemp support (default: no)])],
	                        [], with_coretemp=no)

    # do not build if support not requested
    AS_IF([test "$with_coretemp" != "no"],
          [AS_IF([test "$opal_found_linux" = "yes"],
                 [$1],
                 [AC_MSG_WARN([Core temperature sensing was requested but is only supported on Linux systems])
                  AC_MSG_ERROR([Cannot continue])
                  $2])
          ],
          [$2])
])dnl
