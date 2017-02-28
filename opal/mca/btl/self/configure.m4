# -*- shell-script -*-
#
# Copyright (c) 2016      Los Alamos National Security, LLC. All rights
#                         reserved.
#
# Additional copyrights may follow
#
# $HEADER$
#

AC_DEFUN([MCA_opal_btl_self_COMPILE_MODE], [
    AC_MSG_CHECKING([for MCA component btl:self compile mode])
    $4=static
    AC_MSG_RESULT([static])
])

AC_DEFUN([MCA_opal_btl_self_CONFIG], [
    AC_CONFIG_FILES([opal/mca/btl/self/Makefile])
])
