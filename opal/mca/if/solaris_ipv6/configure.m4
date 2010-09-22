# -*- shell-script -*-
#
# Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved. 
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

AC_DEFUN([MCA_opal_if_solaris_ipv6_COMPILE_MODE], [
    AC_MSG_CHECKING([for MCA component $2:$3 compile mode])
    $4="static"
    AC_MSG_RESULT([$$4])
])

# MCA_if_config_CONFIG(action-if-can-compile, 
#                        [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_opal_if_solaris_ipv6_CONFIG], [
    AC_CONFIG_FILES([opal/mca/if/solaris_ipv6/Makefile])

    AC_REQUIRE([OPAL_CHECK_OS_FLAVORS])

    # check to see if we are on a solaris machine
    AS_IF([test "$opal_found_sun" = "yes"], [$1], [$2])
])dnl

#
# ifdef __sun__
#

