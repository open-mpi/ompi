#
# SPDX-FileCopyrightText:  Copyright Hewlett Packard Enterprise Development LP
# SPDX-License-Identifier:  MIT
#
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# Make this a static component
AC_DEFUN([MCA_ompi_hook_hwpc_cxi_COMPILE_MODE], [
    AC_MSG_CHECKING([for MCA component $2:$3 compile mode])
    $4="static"
    AC_MSG_RESULT([$$4])
])

# MCA_hook_hwpc_cxi_CONFIG([action-if-can-compile],
#                          [action-if-cant-compile])
# ----------------------------------------------------
AC_DEFUN([MCA_ompi_hook_hwpc_cxi_CONFIG],[
    AC_CONFIG_FILES([ompi/mca/hook/hwpc_cxi/Makefile])

    AS_IF([test "$project_hwpc_cxi_amc" = "true"],
          [$1],
          [$2])
])
