dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2014      Intel, Inc. All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

# MCA_sensor_sigar_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_orte_sensor_sigar_CONFIG], [
    AC_CONFIG_FILES([orte/mca/sensor/sigar/Makefile])

    AC_ARG_WITH([sigar],
                [AC_HELP_STRING([--with-sigar],
                                [Build sigar support (default: no)])],
	                        [], with_sigar=no)

    # do not build if support not requested
    AS_IF([test "$with_sigar" != "no"],
        [case "${host}" in
            i?86-*linux*|x86_64*linux*|ia64-*linux*|powerpc-*linux*|powerpc64-*linux*|sparc*-*linux*)
            AS_IF([test -r "/proc/cpuinfo"],
                [sensor_linux_happy="yes"],
                [sensor_linux_happy="no"])
            ;;
            *)
            sensor_linux_happy="no"
            ;;
         esac

         AS_IF([test "$sensor_linux_happy" = "yes"], 
               [libname="sigar"], [libname="sigar-universal-macosx"])

         AS_IF([test ! -z "$with_sigar" -a "$with_sigar" != "yes"],
               [orte_check_sigar_dir="$with_sigar"])

         OMPI_CHECK_PACKAGE([sensor_sigar],
                            [sigar.h],
                            [$libname],
                            [sigar_proc_cpu_get],
                            [],
                            [$orte_check_sigar_dir],
                            [],
                            [$1],
                            [AC_MSG_WARN([SIGAR SENSOR SUPPORT REQUESTED])
                             AC_MSG_WARN([BUT REQUIRED LIBRARY OR HEADER NOT FOUND])
                             AC_MSG_ERROR([CANNOT CONTINUE])
                             $2])],
          [$2])

    AC_DEFINE_UNQUOTED(ORTE_SIGAR_LINUX, [test "$sensor_linux_happy" = "yes"],
                       [Which name to use for the sigar library on this OS])
    AC_SUBST(sensor_sigar_CPPFLAGS)
    AC_SUBST(sensor_sigar_LDFLAGS)
    AC_SUBST(sensor_sigar_LIBS)
])dnl
