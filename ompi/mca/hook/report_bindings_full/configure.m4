#
# Copyright (c) 2019      IBM Corporation.  All rights reserved.
#
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# Make this a static component
AC_DEFUN([MCA_ompi_hook_report_bindings_full_COMPILE_MODE], [
    AC_MSG_CHECKING([for MCA component $2:$3 compile mode])
    $4="static"
    AC_MSG_RESULT([$$4])
])

# MCA_hook_report_bindings_full_CONFIG([action-if-can-compile],
#                      [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_ompi_hook_report_bindings_full_CONFIG],[
    AC_CONFIG_FILES([ompi/mca/hook/report_bindings_full/Makefile])

    $1
])
