# -*- shell-script -*-
#
# Copyright (c) 2026      Advanced Micro Devices, Inc. All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_opal_smsc_accelerator_CONFIG([action-if-can-compile],
#                                  [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_opal_smsc_accelerator_CONFIG],[
    AC_CONFIG_FILES([opal/mca/smsc/accelerator/Makefile])

    OPAL_CHECK_ALL_GPUS([smsc_accelerator],
	        [smsc_accelerator_happy="yes"],
	        [smsc_accelerator_happy="no"])

    AS_IF([test "$smsc_accelerator_happy" = "yes"],
	  [$1],
	  [$2])
])dnl
