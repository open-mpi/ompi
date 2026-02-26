# -*- shell-script -*-
#
# Copyright (c) 2026      Advanced Micro Devices, Inc. All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_opal_btl_smcuda_CONFIG([action-if-can-compile],
#                            [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_opal_btl_smcuda_CONFIG],[
    AC_CONFIG_FILES([opal/mca/btl/smcuda/Makefile])

    OPAL_CHECK_ALL_GPUS([btl_smcuda],
	        [btl_smcuda_happy="yes"],
	        [btl_smcuda_happy="no"])

    AS_IF([test "$btl_smcuda_happy" = "yes"],
	  [$1],
	  [$2])
])dnl
