# -*- shell-script -*-
#
# Copyright (c) 2026      Advanced Micro Devices, Inc. All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_opal_rcache_gpusm_CONFIG([action-if-can-compile],
#                              [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_opal_rcache_gpusm_CONFIG],[
    AC_CONFIG_FILES([opal/mca/rcache/gpusm/Makefile])

    OPAL_CHECK_ALL_GPUS([rcache_gpusm],
	        [rcache_gpusm_happy="yes"],
	        [rcache_gpusm_happy="no"])

    AS_IF([test "$rcache_gpusm_happy" = "yes"],
	  [$1],
	  [$2])
])dnl
