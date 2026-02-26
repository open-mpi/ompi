# -*- shell-script -*-
#
# Copyright (c) 2026      Advanced Micro Devices, Inc. All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_ompi_coll_accelerator_CONFIG([action-if-can-compile],
#                                  [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_ompi_coll_accelerator_CONFIG],[
    AC_CONFIG_FILES([ompi/mca/coll/accelerator/Makefile])

    OPAL_CHECK_ALL_GPUS([coll_accelerator],
	        [coll_accelerator_happy="yes"],
	        [coll_accelerator_happy="no"])

    AS_IF([test "$coll_accelerator_happy" = "yes"],
	  [$1],
	  [$2])
])dnl
