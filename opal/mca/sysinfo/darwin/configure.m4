# -*- shell-script -*-
#
# Copyright (c) 2009-2010 Cisco Systems, Inc. All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#
AC_DEFUN([MCA_opal_sysinfo_darwin_PRIORITY], [50])

# MCA_sysinfo_darwin_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_opal_sysinfo_darwin_CONFIG],[
    AC_CONFIG_FILES([opal/mca/sysinfo/darwin/Makefile])

    OMPI_VAR_SCOPE_PUSH([sysinfo_darwin_happy])
    # check to see if we have <mach/mach_host.h>
    # as this is a Darwin-specific thing
    AC_CHECK_HEADER([mach/mach_host.h], [sysinfo_darwin_happy=yes], [sysinfo_darwin_happy=no])

    AS_IF([test "$sysinfo_darwin_happy" = "yes"], [$1], [$2])
    OMPI_VAR_SCOPE_POP
])dnl

