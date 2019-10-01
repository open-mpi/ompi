# -*- shell-script -*-
#
# Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2016      Intel, Inc.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AC_DEFUN([MCA_pmix_pif_solaris_ipv6_COMPILE_MODE], [
    AC_MSG_CHECKING([for MCA component $1:$2 compile mode])
    $3="static"
    AC_MSG_RESULT([$$3])
])

# MCA_pif_config_CONFIG(action-if-can-compile,
#                        [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_pmix_pif_solaris_ipv6_CONFIG], [
    AC_CONFIG_FILES([src/mca/pif/solaris_ipv6/Makefile])

    AC_REQUIRE([PMIX_CHECK_OS_FLAVORS])

    # check to see if we are on a solaris machine
    AS_IF([test "$pmix_found_sun" = "yes"], [$1], [$2])
])dnl

#
# ifdef __sun__
#
