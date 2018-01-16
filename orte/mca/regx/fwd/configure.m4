# -*- shell-script -*-
#
# Copyright (c) 2018      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_patcher_overwrite_CONFIG(action-if-can-compile,
#                        [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_orte_regx_fwd_CONFIG],[
    AC_CONFIG_FILES([orte/mca/regx/fwd/Makefile])
    [$1]
])

#
# Force this component to compile in static-only mode
#
AC_DEFUN([MCA_orte_regx_fwd_COMPILE_MODE],[
    AC_MSG_CHECKING([for MCA component $2:$3 compile mode])
    $4="static"
    AC_MSG_RESULT([$$4])
])
