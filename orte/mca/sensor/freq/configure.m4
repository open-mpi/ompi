dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2014      Intel, Inc. All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

# MCA_sensor_freq_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_orte_sensor_freq_CONFIG], [
    AC_CONFIG_FILES([orte/mca/sensor/freq/Makefile])

    AC_ARG_WITH([freq],
                [AC_HELP_STRING([--with-freq],
                                [Build freq support (default: no)])],
	                        [], with_freq=no)

    # do not build if support not requested
    AS_IF([test "$with_freq" != "no"],
          [AS_IF([test "$opal_found_linux" = "yes"],
                 [AS_IF([test -r "/sys/devices/system/cpu/cpu0/cpufreq/"],
                        [sensor_freq_happy=yes],
                        [AC_MSG_WARN([Core frequency sensing was requested but the required directory])
                         AC_MSG_WARN([was not found])
                         sensor_freq_happy=no])],
                 [AC_MSG_WARN([Core frequency sensing was requested but is only supported on Linux systems])
                  sensor_freq_happy=no])
           AS_IF([test "$sensor_freq_happy" = "yes"],
                 [$1],
                 [AC_MSG_ERROR([Cannot continue])
                  $2])
          ],
          [$2])
])dnl
