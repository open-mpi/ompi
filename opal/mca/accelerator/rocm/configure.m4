# -*- shell-script -*-
#
# Copyright (c) 2022      Advanced Micro Devices, Inc. All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_opal_accelerator_rocm_CONFIG([action-if-can-compile],
#                                  [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_opal_accelerator_rocm_CONFIG],[
    AC_CONFIG_FILES([opal/mca/accelerator/rocm/Makefile])

    OPAL_CHECK_ROCM([opal_rocm],
	        [opal_rocm_happy="yes"],
	        [opal_rocm_happy="no"])
    OPAL_SUMMARY_ADD([Accelerators], [ROCm support], [], [$opal_rocm_happy])

    AS_IF([test "$opal_rocm_happy" = "yes"],
	  [$1],
	  [$2])
    AC_SUBST([opal_rocm_LDFLAGS])
    AC_SUBST([opal_rocm_LIBS])
    AC_SUBST([opal_rocm_CPPFLAGS])
])dnl
