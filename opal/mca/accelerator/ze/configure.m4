# -*- shell-script -*-
#
# Copyright (c) 2022      Advanced Micro Devices, Inc. All rights reserved.
# Copyright (c) 2023      Triad National Security, LLC. All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_opal_accelerator_ze_CONFIG([action-if-can-compile],
#                                  [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_opal_accelerator_ze_CONFIG],[
    AC_CONFIG_FILES([opal/mca/accelerator/ze/Makefile])

    OPAL_CHECK_ZE([opal_ze],
	        [opal_ze_happy="yes"],
	        [opal_ze_happy="no"])
    OPAL_SUMMARY_ADD([Accelerators], [Intel ZE support], [], [$opal_ze_happy])

    AS_IF([test "$opal_ze_happy" = "yes"],
	  [$1],
	  [$2])
    AC_SUBST([opal_ze_LDFLAGS])
    AC_SUBST([opal_ze_LIBS])
])dnl
