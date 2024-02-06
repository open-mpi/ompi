# -*- shell-script -*-
#
# Copyright (c) 2024      NVIDIA Corporation.
#
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AC_DEFUN([MCA_opal_accelerator_null_PRIORITY], [0])

#
# Force this component to compile in static-only mode
#
AC_DEFUN([MCA_opal_accelerator_null_COMPILE_MODE], [
    AC_MSG_CHECKING([for MCA component $2:$3 compile mode])
    $4="static"
    AC_MSG_RESULT([$$4])
])

# MCA_accelerator_null_CONFIG([action-if-can-compile],
#                             [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_opal_accelerator_null_CONFIG],[
    AC_CONFIG_FILES([opal/mca/accelerator/null/Makefile])
])
