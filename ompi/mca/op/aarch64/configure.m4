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
#                        [action-if-cant-compile])
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
          AC_CACHE_CHECK([for SVE support], [op_cv_sve_support], [
              AC_MSG_RESULT([])      
              # initialize result variables
              op_cv_sve_support=no
              op_cv_sve_add_flags=no

              # first attempt: no extra flags
              AC_MSG_CHECKING([for SVE support (no additional flags)])
              AC_LINK_IFELSE(
                  [AC_LANG_SOURCE([[
#if defined(__aarch64__) && defined(__ARM_FEATURE_SVE)
#include <arm_sve.h>
#else
#error "No support for __aarch64__ or SVE"
#endif

int main(void) {
  svfloat32_t vA;
  vA = svdup_n_f32(0);
  return 0;
}
               ]])],
               [ op_cv_sve_support=yes
                 AC_MSG_RESULT([yes]) ],
               [ AC_MSG_RESULT([no ]) ]
             )

            # second attempt: use +sve attribute
            AS_IF([test "$op_cv_sve_support" = "no"],[
                AC_MSG_CHECKING([for SVE support (with +sve)])
                AC_LINK_IFELSE(
                    [AC_LANG_SOURCE([[
#if defined(__aarch64__) && defined(__linux__)
  #include <arm_sve.h>
#else
  #error "this feature is only supported on aarch64 + linux platforms"
#endif

__attribute__((__target__("+sve")))
int main(void) {
  svbool_t    pg = svptrue_b32();
  svuint32_t  a  = svdup_u32(0);
  svuint32_t  b  = svdup_u32(0);
  svuint32_t  c  = svadd_u32_m(pg, a, b);
  return (int)svaddv_u32(pg, c);
}
                 ]])],
                 [ op_cv_sve_support=yes
                   op_cv_sve_add_flags=yes
                   AC_MSG_RESULT([yes]) ],
                 [ AC_MSG_RESULT([no ]) ]
               )
             ])
           ])

           AC_LANG_POP
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
    AS_IF([test "$op_cv_sve_add_flags" = "yes"],
          [AC_DEFINE([OMPI_MCA_OP_SVE_EXTRA_FLAGS], [1],[SVE supported with additional compile attributes])],
          [AC_DEFINE([OMPI_MCA_OP_SVE_EXTRA_FLAGS], [0],[SVE not supported])])

    # If we have at least support for Neon or SVE
    AS_IF([test "$op_cv_neon_support" = "yes" || test "$op_cv_sve_support" = "yes" ],
          [$1],
          [$2])
])dnl
