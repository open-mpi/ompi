# -*- shell-script -*-
#
# Copyright (c) 2019-2020 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2024      NVIDIA Corporation.  All rights reserved.
#
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_ompi_op_arm_CONFIG([action-if-can-compile],
#		         [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_ompi_op_aarch64_CONFIG],[
    AC_CONFIG_FILES([ompi/mca/op/aarch64/Makefile])
    case "${host}" in
        aarch64*|arm64*)
            op_aarch64_check="yes";;
        *)
            op_aarch64_check="no";;
    esac
    AS_IF([test "$op_aarch64_check" = "yes"],
          [AC_LANG_PUSH([C])

           #
           # Check for NEON support
           #
           AC_CACHE_CHECK([for NEON support], op_cv_neon_support, 
                [
                  AC_LINK_IFELSE(
                      [AC_LANG_PROGRAM([[
#if defined(__aarch64__) && defined(__ARM_NEON)
#include <arm_neon.h>
#else
#error "No support for __aarch64__"
#endif
                                       ]],
                                       [[
#if defined(__aarch64__) && defined(__ARM_NEON)
    int32x4_t vA;
    vA = vmovq_n_s32(0)
#endif
                                       ]])],
                      [op_cv_neon_support=yes],
                      [op_cv_neon_support=no])])

           #
           # Check for NEON FP support
           #
           AC_CACHE_CHECK([for NEON FP support], op_cv_neon_fp_support,
                [AS_IF([test "$op_cv_neon_support" = "yes"],
                        [
                          AC_LINK_IFELSE(
                              [AC_LANG_PROGRAM([[
#if defined(__aarch64__) && defined(__ARM_NEON) && (defined(__ARM_NEON_FP) || defined(__ARM_FP))
#include <arm_neon.h>
#else
#error "No support for __aarch64__ or NEON FP"
#endif
                                             ]],
                                             [[
#if defined(__aarch64__) && defined(__ARM_NEON) && (defined(__ARM_NEON_FP) || defined(__ARM_FP))
    float32x4_t vA;
    vA = vmovq_n_f32(0)
#endif
                                             ]])],
                            [op_cv_neon_fp_support=yes],
                            [op_cv_neon_fp_support=no])])])

           #
           # Check for SVE support
           #
           AC_CACHE_CHECK([for SVE support], op_cv_sve_support,
                 [AS_IF([test "$op_cv_neon_support" = "yes"],
                        [
                          AC_LINK_IFELSE(
                              [AC_LANG_PROGRAM([[
#if defined(__aarch64__) && defined(__ARM_FEATURE_SVE)
#include <arm_sve.h>
#else
#error "No support for __aarch64__ or SVE"
#endif
                                             ]],
                                             [[
#if defined(__aarch64__) && defined(_ARM_FEATURE_SVE)
    svfloat32_t vA;
    vA = svdup_n_f32(0)
#endif
                                             ]])],
                      [op_cv_sve_support=yes],
                      [op_cv_sve_support=no])])])
          ])

    AM_CONDITIONAL([MCA_BUILD_ompi_op_has_neon_support],
                   [test "$op_cv_neon_support" = "yes"])
    AM_CONDITIONAL([MCA_BUILD_ompi_op_has_neon_fp_support],
                   [test "$op_cv_neon_fp_support" = "yes"])
    AM_CONDITIONAL([MCA_BUILD_ompi_op_has_sve_support],
                   [test "$op_cv_sve_support" = "yes"])
    AC_SUBST(MCA_BUILD_ompi_op_has_neon_support)
    AC_SUBST(MCA_BUILD_ompi_op_has_neon_fp_support)
    AC_SUBST(MCA_BUILD_ompi_op_has_sve_support)

    AS_IF([test "$op_cv_neon_support" = "yes"],
          [AC_DEFINE([OMPI_MCA_OP_HAVE_NEON], [1],[NEON supported in the current build])])
    AS_IF([test "$op_cv_neon_fp_support" = "yes"],
          [AC_DEFINE([OMPI_MCA_OP_HAVE_NEON_FP], [1],[NEON FP supported in the current build])])
    AS_IF([test "$op_cv_sve_support" = "yes"],
          [AC_DEFINE([OMPI_MCA_OP_HAVE_SVE], [1],[SVE supported in the current build])])

    # If we have at least support for Neon
    AS_IF([test "$op_cv_neon_support" = "yes"],
          [$1],
          [$2])
])dnl
