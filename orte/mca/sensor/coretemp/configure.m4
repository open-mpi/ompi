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
        [case "${host}" in
            i?86-*linux*|x86_64*linux*|ia64-*linux*|powerpc-*linux*|powerpc64-*linux*|sparc*-*linux*)
            AS_IF([test -r "/sys/bus/platform/devices/coretemp.0"],
                  [sensor_coretemp_happy=yes],
                  [AC_MSG_WARN([Core temperature sensing was requested but the required directory])
                   AC_MSG_WARN([was not found. This usually indicates that the \"coretemp\"])
                   AC_MSG_WARN([kernel module is not installed. Please install the module])
                   AC_MSG_WARN([and try again, or remove the core temperature sensing request.])
                   sensor_coretemp_happy=no])
            ;;
            *)
            AC_MSG_WARN([Core temperature sensing was requested but is only supported on Linux systems])
            sensor_coretemp_happy=no
            ;;
         esac
        AS_IF([test "$sensor_coretemp_happy" = "yes"],
            [$1],
            [AC_MSG_ERROR([Cannot continue])
             $2])
        ],
        [$2])
])dnl
