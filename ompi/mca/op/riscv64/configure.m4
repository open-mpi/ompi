# -*- shell-script -*-
#
# Copyright (c) 2019-2020 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2025      Software System Team, SANECHIPS.  All rights reserved.
#
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_ompi_op_riscv_CONFIG([action-if-can-compile],
#                        [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_ompi_op_riscv64_CONFIG],[
    AC_CONFIG_FILES([ompi/mca/op/riscv64/Makefile])
    case "${host}" in
        riscv64*)
            op_riscv64_check="yes";;
        *)
            op_riscv64_check="no";;
    esac
    AS_IF([test "$op_riscv64_check" = "yes"],
          [AC_LANG_PUSH([C])

           #
           # Check for RVV support
           #
           AC_CACHE_CHECK([for RVV support], op_cv_rvv_support, 
                [
                  AC_LINK_IFELSE(
                      [AC_LANG_PROGRAM([[
#if defined(__riscv) && defined(__riscv_v) && __riscv_xlen == 64
#include <riscv_vector.h>
#else
#error "Not a 64-bit RISC-V target"
#endif
                                       ]],
                                       [[
#if defined(__riscv) && defined(__riscv_v) && __riscv_xlen == 64
    size_t vl = __riscv_vsetvl_e32m1(4);
#endif
                                       ]])],
                      [op_cv_rvv_support=yes],
                      [op_cv_rvv_support=no])])
           AC_LANG_POP
])
    AM_CONDITIONAL([MCA_BUILD_ompi_op_has_rvv_support],
                   [test "$op_cv_rvv_support" = "yes"])

    AC_SUBST(MCA_BUILD_ompi_op_has_rvv_support)

    AS_IF([test "$op_cv_rvv_support" = "yes"],
          [AC_DEFINE([OMPI_MCA_OP_HAVE_RVV], [1],[RVV supported in the current build])],
          [AC_DEFINE([OMPI_MCA_OP_HAVE_RVV], [0],[RVV not supported in the current build])])

    # If we have at least support for Neon or SVE
    AS_IF([test "$op_cv_rvv_support" = "yes"],
          [$1],
          [$2])
])dnl
