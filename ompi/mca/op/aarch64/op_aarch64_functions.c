/*
 * Copyright (c) 2019      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2019      Arm Ltd.  All rights reserved.
 * Copyright (c) 2024      NVIDIA Corporation.  All rights reserved.
 * Copyright (c) 2024      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include "opal/util/output.h"

#include "ompi/op/op.h"
#include "ompi/mca/op/op.h"
#include "ompi/mca/op/base/base.h"
#include "ompi/mca/op/aarch64/op_aarch64.h"

#if defined(GENERATE_SVE_CODE)
#    include <arm_sve.h>
#define OMPI_OP_TYPE_PREPEND sv
#define OMPI_OP_OP_PREPEND sv
#define APPEND   _sve
#elif defined(GENERATE_NEON_CODE)
#    include <arm_neon.h>
#define OMPI_OP_TYPE_PREPEND
#define OMPI_OP_OP_PREPEND v
#define APPEND   _neon
#else
#error we should not reach this
#endif /* OMPI_MCA_OP_HAVE_SVE */

/*
 * Concatenate preprocessor tokens A and B without expanding macro definitions
 * (however, if invoked from a macro, macro arguments are expanded).
 */
#define OP_CONCAT_NX(A, B) A ## B

/*
 * Concatenate preprocessor tokens A and B after macro-expanding them.
 */
#define OP_CONCAT(A, B) OP_CONCAT_NX(A, B)

#if defined(GENERATE_SVE_CODE)
#    define svcnt(X)           \
        _Generic((X),          \
            int8_t: svcntb,    \
            uint8_t: svcntb,   \
            int16_t: svcnth,   \
            uint16_t: svcnth,  \
            int32_t: svcntw,   \
            uint32_t: svcntw,  \
            int64_t: svcntd,   \
            uint64_t: svcntd,  \
            float32_t: svcntw, \
            float64_t: svcntd)()
#else
#define DUMP2(out, in1, in2) \
        case 2: (out)[1] = current_func((in1)[1], (in2)[1]); \
        case 1: (out)[0] = current_func((in1)[0], (in2)[0]);
#define DUMP4(out, in1, in2) \
        case 4: (out)[3] = current_func((in1)[3], (in2)[3]); \
        case 3: (out)[2] = current_func((in1)[2], (in2)[2]); \
        DUMP2(out, in1, in2)
#define DUMP8(out, in1, in2) \
        case 8: (out)[7] = current_func((in1)[7], (in2)[7]); \
        case 7: (out)[6] = current_func((in1)[6], (in2)[6]); \
        case 6: (out)[5] = current_func((in1)[5], (in2)[5]); \
        case 5: (out)[4] = current_func((in1)[4], (in2)[4]); \
        DUMP4(out, in1, in2)
#define DUMP16(out, in1, in2) \
        case 16: (out)[15] = current_func((in1)[15], (in2)[15]); \
        case 15: (out)[14] = current_func((in1)[14], (in2)[14]); \
        case 14: (out)[13] = current_func((in1)[13], (in2)[13]); \
        case 13: (out)[12] = current_func((in1)[12], (in2)[12]); \
        case 12: (out)[11] = current_func((in1)[11], (in2)[11]); \
        case 11: (out)[10] = current_func((in1)[10], (in2)[10]); \
        case 10: (out)[ 9] = current_func((in1)[ 9], (in2)[ 9]); \
        case  9: (out)[ 8] = current_func((in1)[ 8], (in2)[ 8]); \
        DUMP8(out, in1, in2)

#    define neon_loop(how_much, out, in1, in2)   \
_Generic((*(out)), \
         int8_t:    __extension__({ switch ((how_much)) { DUMP16(out, in1, in2) }}), \
         uint8_t:   __extension__({ switch ((how_much)) { DUMP16(out, in1, in2) }}), \
         int16_t:   __extension__({ switch ((how_much)) { DUMP8(out, in1, in2) }}), \
         uint16_t:  __extension__({ switch ((how_much)) { DUMP8(out, in1, in2) }}), \
         int32_t:   __extension__({ switch ((how_much)) { DUMP4(out, in1, in2) }}), \
         uint32_t:  __extension__({ switch ((how_much)) { DUMP4(out, in1, in2) }}), \
         int64_t:   __extension__({ switch ((how_much)) { DUMP2(out, in1, in2) }}), \
         uint64_t:  __extension__({ switch ((how_much)) { DUMP2(out, in1, in2) }}), \
         float32_t: __extension__({ switch ((how_much)) { DUMP4(out, in1, in2) }}), \
         float64_t: __extension__({ switch ((how_much)) { DUMP2(out, in1, in2) }}))
#endif /* defined(GENERATE_SVE_CODE) */

/*
 * Since all the functions in this file are essentially identical, we
 * use a macro to substitute in names and types.  The core operation
 * in all functions that use this macro is the same.
 *
 * This macro is for (out op in).
 *
 */
#if defined(GENERATE_NEON_CODE)
#define OP_AARCH64_FUNC(name, type_name, type_size, type_cnt, type, op)                       \
    static void OP_CONCAT(ompi_op_aarch64_2buff_##name##_##type##type_size##_t,               \
                          APPEND)(const void *_in, void *_out, int *count,                    \
                                  struct ompi_datatype_t **dtype,                             \
                                  struct ompi_op_base_module_1_0_0_t *module)                 \
    {                                                                                         \
        int left_over = *count;                                                               \
        type##type_size##_t *in = (type##type_size##_t *) _in,                                \
                            *out = (type##type_size##_t *) _out;                              \
        OP_CONCAT(OMPI_OP_TYPE_PREPEND, type##type_size##x##type_cnt##_t) vsrc, vdst;         \
        for (; left_over >= type_cnt; left_over -= type_cnt) {                                \
            vsrc = vld1q##_##type_name##type_size(in);                                        \
            vdst = vld1q##_##type_name##type_size(out);                                       \
            in += type_cnt;                                                                   \
            vdst = OP_CONCAT(OMPI_OP_OP_PREPEND, op##q##_##type_name##type_size)(vdst, vsrc); \
            vst1q##_##type_name##type_size(out, vdst);                                        \
            out += type_cnt;                                                                  \
        }                                                                                     \
                                                                                              \
        if(left_over > 0) {                                                                   \
            neon_loop(left_over, out, out, in);                                               \
        }                                                                                     \
    }
#elif defined(GENERATE_SVE_CODE)
#define OP_AARCH64_FUNC(name, type_name, type_size, type_cnt, type, op)           \
    static void OP_CONCAT(ompi_op_aarch64_2buff_##name##_##type##type_size##_t, APPEND) \
                            (const void *_in, void *_out, int *count,             \
                             struct ompi_datatype_t **dtype,                      \
                             struct ompi_op_base_module_1_0_0_t *module)          \
    {                                                                             \
        const int types_per_step = svcnt(*((type##type_size##_t *) _in));         \
        const int cnt = *count;                                                   \
        type##type_size##_t *in = (type##type_size##_t *) _in,                    \
                            *out = (type##type_size##_t *) _out;                  \
        OP_CONCAT(OMPI_OP_TYPE_PREPEND, type##type_size##_t) vsrc, vdst;          \
        for (int idx=0; idx < cnt; idx += types_per_step) {                    \
            svbool_t pred = svwhilelt_b##type_size(idx, cnt);                     \
            vsrc = svld1(pred, &in[idx]);                                         \
            vdst = svld1(pred, &out[idx]);                                        \
            vdst = OP_CONCAT(OMPI_OP_OP_PREPEND, op##_x)(pred, vdst, vsrc);       \
            OP_CONCAT(OMPI_OP_OP_PREPEND, st1)(pred, &out[idx], vdst);            \
        }                                                                         \
    }
#endif

/*************************************************************************
 * Max
 *************************************************************************/
#undef current_func
#define current_func(a, b) ((a) > (b) ? (a) : (b))
    OP_AARCH64_FUNC(max, s,  8, 16,   int, max)
    OP_AARCH64_FUNC(max, u,  8, 16,  uint, max)
    OP_AARCH64_FUNC(max, s, 16,  8,   int, max)
    OP_AARCH64_FUNC(max, u, 16,  8,  uint, max)
    OP_AARCH64_FUNC(max, s, 32,  4,   int, max)
    OP_AARCH64_FUNC(max, u, 32,  4,  uint, max)
#if defined(GENERATE_SVE_CODE)
    OP_AARCH64_FUNC(max, s, 64,  2,   int, max)
    OP_AARCH64_FUNC(max, u, 64,  2,  uint, max)
#endif  /* defined(GENERATE_SVE_CODE) */

    OP_AARCH64_FUNC(max, f, 32,  4, float, max)
    OP_AARCH64_FUNC(max, f, 64,  2, float, max)

/*************************************************************************
 * Min
 *************************************************************************/
#undef current_func
#define current_func(a, b) ((a) < (b) ? (a) : (b))
    OP_AARCH64_FUNC(min, s,  8, 16,   int, min)
    OP_AARCH64_FUNC(min, u,  8, 16,  uint, min)
    OP_AARCH64_FUNC(min, s, 16,  8,   int, min)
    OP_AARCH64_FUNC(min, u, 16,  8,  uint, min)
    OP_AARCH64_FUNC(min, s, 32,  4,   int, min)
    OP_AARCH64_FUNC(min, u, 32,  4,  uint, min)
#if defined(GENERATE_SVE_CODE)
    OP_AARCH64_FUNC(min, s, 64,  2,   int, min)
    OP_AARCH64_FUNC(min, u, 64,  2,  uint, min)
#endif  /* defined(GENERATE_SVE_CODE) */

    OP_AARCH64_FUNC(min, f, 32,  4, float, min)
    OP_AARCH64_FUNC(min, f, 64,  2, float, min)
    /*************************************************************************
     * Sum
     ************************************************************************/
#undef current_func
#define current_func(a, b) ((a) + (b))
    OP_AARCH64_FUNC(sum, s,  8, 16,   int, add)
    OP_AARCH64_FUNC(sum, u,  8, 16,  uint, add)
    OP_AARCH64_FUNC(sum, s, 16,  8,   int, add)
    OP_AARCH64_FUNC(sum, u, 16,  8,  uint, add)
    OP_AARCH64_FUNC(sum, s, 32,  4,   int, add)
    OP_AARCH64_FUNC(sum, u, 32,  4,  uint, add)
    OP_AARCH64_FUNC(sum, s, 64,  2,   int, add)
    OP_AARCH64_FUNC(sum, u, 64,  2,  uint, add)

    OP_AARCH64_FUNC(sum, f, 32,  4, float, add)
    OP_AARCH64_FUNC(sum, f, 64,  2, float, add)

/*************************************************************************
 * Product
 *************************************************************************/
#undef current_func
#define current_func(a, b) ((a) * (b))
    OP_AARCH64_FUNC(prod, s,  8, 16,   int, mul)
    OP_AARCH64_FUNC(prod, u,  8, 16,  uint, mul)
    OP_AARCH64_FUNC(prod, s, 16,  8,   int, mul)
    OP_AARCH64_FUNC(prod, u, 16,  8,  uint, mul)
    OP_AARCH64_FUNC(prod, s, 32,  4,   int, mul)
    OP_AARCH64_FUNC(prod, u, 32,  4,  uint, mul)
#if defined(GENERATE_SVE_CODE)
    OP_AARCH64_FUNC(prod, s, 64,  2,   int, mul)
    OP_AARCH64_FUNC(prod, u, 64,  2,  uint, mul)
#endif  /* defined(GENERATE_SVE_CODE) */

    OP_AARCH64_FUNC(prod, f, 32,  4, float, mul)
    OP_AARCH64_FUNC(prod, f, 64,  2, float, mul)

/*************************************************************************
 * Bitwise AND
 *************************************************************************/
#undef current_func
#define current_func(a, b) ((a) & (b))
    OP_AARCH64_FUNC(band, s,  8, 16,  int, and)
    OP_AARCH64_FUNC(band, u,  8, 16, uint, and)
    OP_AARCH64_FUNC(band, s, 16,  8,  int, and)
    OP_AARCH64_FUNC(band, u, 16,  8, uint, and)
    OP_AARCH64_FUNC(band, s, 32,  4,  int, and)
    OP_AARCH64_FUNC(band, u, 32,  4, uint, and)
    OP_AARCH64_FUNC(band, s, 64,  2,  int, and)
    OP_AARCH64_FUNC(band, u, 64,  2, uint, and)

 /*************************************************************************
 * Bitwise OR
 *************************************************************************/
#undef current_func
#define current_func(a, b) ((a) | (b))
    OP_AARCH64_FUNC(bor, s,  8, 16,  int, orr)
    OP_AARCH64_FUNC(bor, u,  8, 16, uint, orr)
    OP_AARCH64_FUNC(bor, s, 16,  8,  int, orr)
    OP_AARCH64_FUNC(bor, u, 16,  8, uint, orr)
    OP_AARCH64_FUNC(bor, s, 32,  4,  int, orr)
    OP_AARCH64_FUNC(bor, u, 32,  4, uint, orr)
    OP_AARCH64_FUNC(bor, s, 64,  2,  int, orr)
    OP_AARCH64_FUNC(bor, u, 64,  2, uint, orr)

/*************************************************************************
 * Bitwise XOR
 *************************************************************************/
#undef current_func
#define current_func(a, b) ((a) ^ (b))
    OP_AARCH64_FUNC(bxor, s,  8, 16,  int, eor)
    OP_AARCH64_FUNC(bxor, u,  8, 16, uint, eor)
    OP_AARCH64_FUNC(bxor, s, 16,  8,  int, eor)
    OP_AARCH64_FUNC(bxor, u, 16,  8, uint, eor)
    OP_AARCH64_FUNC(bxor, s, 32,  4,  int, eor)
    OP_AARCH64_FUNC(bxor, u, 32,  4, uint, eor)
    OP_AARCH64_FUNC(bxor, s, 64,  2,  int, eor)
    OP_AARCH64_FUNC(bxor, u, 64,  2, uint, eor)

    /*
     *  This is a three buffer (2 input and 1 output) version of the reduction
     *  routines, needed for some optimizations.
     */
#if defined(GENERATE_NEON_CODE)
#define OP_AARCH64_FUNC_3BUFF(name, type_name, type_size, type_cnt, type, op)                 \
static void OP_CONCAT(ompi_op_aarch64_3buff_##name##_##type##type_size##_t, APPEND)       \
                             (const void *_in1, const void *_in2, void *_out, int *count, \
                              struct ompi_datatype_t **dtype,                             \
                              struct ompi_op_base_module_1_0_0_t *module)                 \
{                                                                                         \
    int left_over = *count;                                                               \
    type##type_size##_t *in1 = (type##type_size##_t *) _in1,                              \
                        *in2 = (type##type_size##_t *) _in2,                              \
                        *out = (type##type_size##_t *) _out;                              \
    OP_CONCAT(OMPI_OP_TYPE_PREPEND, type##type_size##x##type_cnt##_t) vsrc, vdst;         \
    for (; left_over >= type_cnt; left_over -= type_cnt) {                                \
        vsrc = vld1q##_##type_name##type_size(in1);                                       \
        vdst = vld1q##_##type_name##type_size(in2);                                       \
        in1 += type_cnt;                                                                  \
        in2 += type_cnt;                                                                  \
        vdst = OP_CONCAT(OMPI_OP_OP_PREPEND, op##q##_##type_name##type_size)(vdst, vsrc); \
        vst1q##_##type_name##type_size(out, vdst);                                        \
        out += type_cnt;                                                                  \
    }                                                                                     \
    if (left_over > 0) {                                                                  \
        neon_loop(left_over, out, in1, in2);                                              \
    }                                                                                     \
}
#elif defined(GENERATE_SVE_CODE)
#define OP_AARCH64_FUNC_3BUFF(name, type_name, type_size, type_cnt, type, op)             \
static void OP_CONCAT(ompi_op_aarch64_3buff_##name##_##type##type_size##_t, APPEND)       \
                             (const void *_in1, const void *_in2, void *_out, int *count, \
                              struct ompi_datatype_t **dtype,                             \
                              struct ompi_op_base_module_1_0_0_t *module)                 \
{                                                                                         \
    const int types_per_step = svcnt(*((type##type_size##_t *) _in1));                    \
    type##type_size##_t *in1 = (type##type_size##_t *) _in1,                              \
                        *in2 = (type##type_size##_t *) _in2,                              \
                        *out = (type##type_size##_t *) _out;                              \
    const int cnt = *count;                                                               \
    OP_CONCAT(OMPI_OP_TYPE_PREPEND, type##type_size##_t) vsrc, vdst;                      \
    for (int idx=0; idx < cnt; idx += types_per_step) {                                   \
        svbool_t pred = svwhilelt_b##type_size(idx, cnt);                                 \
        vsrc = svld1(pred, &in1[idx]);                                                    \
        vdst = svld1(pred, &in2[idx]);                                                    \
        vdst = OP_CONCAT(OMPI_OP_OP_PREPEND, op##_x)(pred, vdst, vsrc);                   \
        OP_CONCAT(OMPI_OP_OP_PREPEND, st1)(pred, &out[idx], vdst);                        \
    }                                                                                     \
}
#endif  /* defined(GENERATE_SVE_CODE) */

/*************************************************************************
 * Max
 *************************************************************************/
#undef current_func
#define current_func(a, b) ((a) > (b) ? (a) : (b))
    OP_AARCH64_FUNC_3BUFF(max, s,  8, 16,   int, max)
    OP_AARCH64_FUNC_3BUFF(max, u,  8, 16,  uint, max)
    OP_AARCH64_FUNC_3BUFF(max, s, 16,  8,   int, max)
    OP_AARCH64_FUNC_3BUFF(max, u, 16,  8,  uint, max)
    OP_AARCH64_FUNC_3BUFF(max, s, 32,  4,   int, max)
    OP_AARCH64_FUNC_3BUFF(max, u, 32,  4,  uint, max)
#if defined(GENERATE_SVE_CODE)
    OP_AARCH64_FUNC_3BUFF(max, s, 64,  2,   int, max)
    OP_AARCH64_FUNC_3BUFF(max, u, 64,  2,  uint, max)
#endif  /* defined(GENERATE_SVE_CODE) */

    OP_AARCH64_FUNC_3BUFF(max, f, 32,  4, float, max)
    OP_AARCH64_FUNC_3BUFF(max, f, 64,  2, float, max)

    /*************************************************************************
     * Min
     *************************************************************************/
#undef current_func
#define current_func(a, b) ((a) < (b) ? (a) : (b))
    OP_AARCH64_FUNC_3BUFF(min, s,  8, 16,   int, min)
    OP_AARCH64_FUNC_3BUFF(min, u,  8, 16,  uint, min)
    OP_AARCH64_FUNC_3BUFF(min, s, 16,  8,   int, min)
    OP_AARCH64_FUNC_3BUFF(min, u, 16,  8,  uint, min)
    OP_AARCH64_FUNC_3BUFF(min, s, 32,  4,   int, min)
    OP_AARCH64_FUNC_3BUFF(min, u, 32,  4,  uint, min)
#if defined(GENERATE_SVE_CODE)
    OP_AARCH64_FUNC_3BUFF(min, s, 64,  2,   int, min)
    OP_AARCH64_FUNC_3BUFF(min, u, 64,  2,  uint, min)
#endif  /* defined(GENERATE_SVE_CODE) */

    OP_AARCH64_FUNC_3BUFF(min, f, 32,  4, float, min)
    OP_AARCH64_FUNC_3BUFF(min, f, 64,  2, float, min)

 /*************************************************************************
 * Sum
 ************************************************************************/
#undef current_func
#define current_func(a, b) ((a) + (b))
    OP_AARCH64_FUNC_3BUFF(sum, s,  8, 16,  int, add)
    OP_AARCH64_FUNC_3BUFF(sum, u,  8, 16, uint, add)
    OP_AARCH64_FUNC_3BUFF(sum, s, 16,  8,  int, add)
    OP_AARCH64_FUNC_3BUFF(sum, u, 16,  8, uint, add)
    OP_AARCH64_FUNC_3BUFF(sum, s, 32,  4,  int, add)
    OP_AARCH64_FUNC_3BUFF(sum, u, 32,  4, uint, add)
    OP_AARCH64_FUNC_3BUFF(sum, s, 64,  2,  int, add)
    OP_AARCH64_FUNC_3BUFF(sum, u, 64,  2, uint, add)

    OP_AARCH64_FUNC_3BUFF(sum, f, 32,  4, float, add)
    OP_AARCH64_FUNC_3BUFF(sum, f, 64,  2, float, add)

/*************************************************************************
 * Product
 *************************************************************************/
#undef current_func
#define current_func(a, b) ((a) * (b))
    OP_AARCH64_FUNC_3BUFF(prod, s,  8, 16,   int, mul)
    OP_AARCH64_FUNC_3BUFF(prod, u,  8, 16,  uint, mul)
    OP_AARCH64_FUNC_3BUFF(prod, s, 16,  8,   int, mul)
    OP_AARCH64_FUNC_3BUFF(prod, u, 16,  8,  uint, mul)
    OP_AARCH64_FUNC_3BUFF(prod, s, 32,  4,   int, mul)
    OP_AARCH64_FUNC_3BUFF(prod, u, 32,  4,  uint, mul)
#if defined(GENERATE_SVE_CODE)
    OP_AARCH64_FUNC_3BUFF(prod, s, 64,  2,   int, mul)
    OP_AARCH64_FUNC_3BUFF(prod, u, 64,  2,  uint, mul)
#endif  /* defined(GENERATE_SVE_CODE) */

    OP_AARCH64_FUNC_3BUFF(prod, f, 32,  4, float, mul)
    OP_AARCH64_FUNC_3BUFF(prod, f, 64,  2, float, mul)

/*************************************************************************
 * Bitwise AND
 *************************************************************************/
#undef current_func
#define current_func(a, b) ((a) & (b))
    OP_AARCH64_FUNC_3BUFF(band, s,  8, 16,  int, and)
    OP_AARCH64_FUNC_3BUFF(band, u,  8, 16, uint, and)
    OP_AARCH64_FUNC_3BUFF(band, s, 16,  8,  int, and)
    OP_AARCH64_FUNC_3BUFF(band, u, 16,  8, uint, and)
    OP_AARCH64_FUNC_3BUFF(band, s, 32,  4,  int, and)
    OP_AARCH64_FUNC_3BUFF(band, u, 32,  4, uint, and)
    OP_AARCH64_FUNC_3BUFF(band, s, 64,  2,  int, and)
    OP_AARCH64_FUNC_3BUFF(band, u, 64,  2, uint, and)

 /*************************************************************************
 * Bitwise OR
 *************************************************************************/
#undef current_func
#define current_func(a, b) ((a) | (b))
    OP_AARCH64_FUNC_3BUFF(bor, s,  8, 16,  int, orr)
    OP_AARCH64_FUNC_3BUFF(bor, u,  8, 16, uint, orr)
    OP_AARCH64_FUNC_3BUFF(bor, s, 16,  8,  int, orr)
    OP_AARCH64_FUNC_3BUFF(bor, u, 16,  8, uint, orr)
    OP_AARCH64_FUNC_3BUFF(bor, s, 32,  4,  int, orr)
    OP_AARCH64_FUNC_3BUFF(bor, u, 32,  4, uint, orr)
    OP_AARCH64_FUNC_3BUFF(bor, s, 64,  2,  int, orr)
    OP_AARCH64_FUNC_3BUFF(bor, u, 64,  2, uint, orr)

/*************************************************************************
 * Bitwise XOR
 *************************************************************************/
#undef current_func
#define current_func(a, b) ((a) ^ (b))
    OP_AARCH64_FUNC_3BUFF(bxor, s,  8, 16,  int, eor)
    OP_AARCH64_FUNC_3BUFF(bxor, u,  8, 16, uint, eor)
    OP_AARCH64_FUNC_3BUFF(bxor, s, 16,  8,  int, eor)
    OP_AARCH64_FUNC_3BUFF(bxor, u, 16,  8, uint, eor)
    OP_AARCH64_FUNC_3BUFF(bxor, s, 32,  4,  int, eor)
    OP_AARCH64_FUNC_3BUFF(bxor, u, 32,  4, uint, eor)
    OP_AARCH64_FUNC_3BUFF(bxor, s, 64,  2,  int, eor)
    OP_AARCH64_FUNC_3BUFF(bxor, u, 64,  2, uint, eor)

    /** C integer ***********************************************************/
#define C_INTEGER_BASE(name, ftype)                                         \
    [OMPI_OP_BASE_TYPE_INT8_T]   = OP_CONCAT(ompi_op_aarch64_##ftype##_##name##_int8_t, APPEND), \
    [OMPI_OP_BASE_TYPE_UINT8_T]  = OP_CONCAT(ompi_op_aarch64_##ftype##_##name##_uint8_t, APPEND), \
    [OMPI_OP_BASE_TYPE_INT16_T]  = OP_CONCAT(ompi_op_aarch64_##ftype##_##name##_int16_t, APPEND), \
    [OMPI_OP_BASE_TYPE_UINT16_T] = OP_CONCAT(ompi_op_aarch64_##ftype##_##name##_uint16_t, APPEND), \
    [OMPI_OP_BASE_TYPE_INT32_T]  = OP_CONCAT(ompi_op_aarch64_##ftype##_##name##_int32_t, APPEND), \
    [OMPI_OP_BASE_TYPE_UINT32_T] = OP_CONCAT(ompi_op_aarch64_##ftype##_##name##_uint32_t, APPEND)
#define C_INTEGER_EX(name, ftype)                                         \
    [OMPI_OP_BASE_TYPE_INT64_T]  = OP_CONCAT(ompi_op_aarch64_##ftype##_##name##_int64_t, APPEND), \
    [OMPI_OP_BASE_TYPE_UINT64_T] = OP_CONCAT(ompi_op_aarch64_##ftype##_##name##_uint64_t, APPEND)

    /** Floating point, including all the Fortran reals *********************/
#define FLOAT(name, ftype)  OP_CONCAT(ompi_op_aarch64_##ftype##_##name##_float32_t, APPEND)
#define DOUBLE(name, ftype) OP_CONCAT(ompi_op_aarch64_##ftype##_##name##_float64_t, APPEND)

#define FLOATING_POINT(name, ftype)                                    \
    [OMPI_OP_BASE_TYPE_FLOAT] = FLOAT(name, ftype),                    \
    [OMPI_OP_BASE_TYPE_DOUBLE] = DOUBLE(name, ftype)

/*
 * MPI_OP_NULL
 * All types
 */
#define FLAGS_NO_FLOAT \
        (OMPI_OP_FLAGS_INTRINSIC | OMPI_OP_FLAGS_ASSOC | OMPI_OP_FLAGS_COMMUTE)
#define FLAGS \
        (OMPI_OP_FLAGS_INTRINSIC | OMPI_OP_FLAGS_ASSOC | \
         OMPI_OP_FLAGS_FLOAT_ASSOC | OMPI_OP_FLAGS_COMMUTE)

    ompi_op_base_handler_fn_t OP_CONCAT(ompi_op_aarch64_functions, APPEND) [OMPI_OP_BASE_FORTRAN_OP_MAX][OMPI_OP_BASE_TYPE_MAX] =
{
    /* Corresponds to MPI_OP_NULL */
    [OMPI_OP_BASE_FORTRAN_NULL] = {
        /* Leaving this empty puts in NULL for all entries */
        NULL,
    },
    /* Corresponds to MPI_MAX */
    [OMPI_OP_BASE_FORTRAN_MAX] = {
        C_INTEGER_BASE(max, 2buff),
#if defined(GENERATE_SVE_CODE)
        C_INTEGER_EX(max, 2buff),
#endif /* defined(GENERATE_SVE_CODE) */
        FLOATING_POINT(max, 2buff),
    },
    /* Corresponds to MPI_MIN */
    [OMPI_OP_BASE_FORTRAN_MIN] = {
        C_INTEGER_BASE(min, 2buff),
#if defined(GENERATE_SVE_CODE)
        C_INTEGER_EX(min, 2buff),
#endif /* defined(GENERATE_SVE_CODE) */
        FLOATING_POINT(min, 2buff),
    },
    /* Corresponds to MPI_SUM */
    [OMPI_OP_BASE_FORTRAN_SUM] = {
        C_INTEGER_BASE(sum, 2buff),
        C_INTEGER_EX(sum, 2buff),
        FLOATING_POINT(sum, 2buff),
    },
    /* Corresponds to MPI_PROD */
    [OMPI_OP_BASE_FORTRAN_PROD] = {
        C_INTEGER_BASE(prod, 2buff),
#if defined(GENERATE_SVE_CODE)
        C_INTEGER_EX(prod, 2buff),
#endif /* defined(GENERATE_SVE_CODE) */
        FLOATING_POINT(prod, 2buff),
    },
    /* Corresponds to MPI_LAND */
    [OMPI_OP_BASE_FORTRAN_LAND] = {
        NULL,
    },
    /* Corresponds to MPI_BAND */
    [OMPI_OP_BASE_FORTRAN_BAND] = {
        C_INTEGER_BASE(band, 2buff),
        C_INTEGER_EX(band, 2buff),
    },
    /* Corresponds to MPI_LOR */
    [OMPI_OP_BASE_FORTRAN_LOR] = {
        NULL,
    },
    /* Corresponds to MPI_BOR */
    [OMPI_OP_BASE_FORTRAN_BOR] = {
        C_INTEGER_BASE(bor, 2buff),
        C_INTEGER_EX(bor, 2buff),
    },
    /* Corresponds to MPI_LXOR */
    [OMPI_OP_BASE_FORTRAN_LXOR] = {
        NULL,
    },
    /* Corresponds to MPI_BXOR */
    [OMPI_OP_BASE_FORTRAN_BXOR] = {
        C_INTEGER_BASE(bxor, 2buff),
        C_INTEGER_EX(bxor, 2buff),
    },
    /* Corresponds to MPI_REPLACE */
    [OMPI_OP_BASE_FORTRAN_REPLACE] = {
        /* (MPI_ACCUMULATE is handled differently than the other
           reductions, so just zero out its function
           implementations here to ensure that users don't invoke
           MPI_REPLACE with any reduction operations other than
           ACCUMULATE) */
        NULL,
    },

};

    ompi_op_base_3buff_handler_fn_t OP_CONCAT(ompi_op_aarch64_3buff_functions, APPEND)[OMPI_OP_BASE_FORTRAN_OP_MAX][OMPI_OP_BASE_TYPE_MAX] =
{
    /* Corresponds to MPI_OP_NULL */
    [OMPI_OP_BASE_FORTRAN_NULL] = {
        /* Leaving this empty puts in NULL for all entries */
        NULL,
    },
    /* Corresponds to MPI_MAX */
    [OMPI_OP_BASE_FORTRAN_MAX] = {
        C_INTEGER_BASE(max, 3buff),
#if defined(GENERATE_SVE_CODE)
        C_INTEGER_EX(max, 3buff),
#endif  /* defined(GENERATE_SVE_CODE) */
        FLOATING_POINT(max, 3buff),
    },
    /* Corresponds to MPI_MIN */
    [OMPI_OP_BASE_FORTRAN_MIN] = {
        C_INTEGER_BASE(min, 3buff),
#if defined(GENERATE_SVE_CODE)
        C_INTEGER_EX(min, 3buff),
#endif  /* defined(GENERATE_SVE_CODE) */
        FLOATING_POINT(min, 3buff),
    },
    /* Corresponds to MPI_SUM */
    [OMPI_OP_BASE_FORTRAN_SUM] = {
        C_INTEGER_BASE(sum, 3buff),
        C_INTEGER_EX(sum, 3buff),
        FLOATING_POINT(sum, 3buff),
    },
    /* Corresponds to MPI_PROD */
    [OMPI_OP_BASE_FORTRAN_PROD] = {
        C_INTEGER_BASE(prod, 3buff),
#if defined(GENERATE_SVE_CODE)
        C_INTEGER_EX(prod, 3buff),
#endif /* defined(GENERATE_SVE_CODE) */
        FLOATING_POINT(prod, 3buff),
    },
    /* Corresponds to MPI_LAND */
    [OMPI_OP_BASE_FORTRAN_LAND] ={
        NULL,
    },
    /* Corresponds to MPI_BAND */
    [OMPI_OP_BASE_FORTRAN_BAND] = {
        C_INTEGER_BASE(band, 3buff),
        C_INTEGER_EX(band, 3buff),
    },
    /* Corresponds to MPI_LOR */
    [OMPI_OP_BASE_FORTRAN_LOR] = {
        NULL,
    },
    /* Corresponds to MPI_BOR */
    [OMPI_OP_BASE_FORTRAN_BOR] = {
        C_INTEGER_BASE(bor, 3buff),
        C_INTEGER_EX(bor, 3buff),
    },
    /* Corresponds to MPI_LXOR */
    [OMPI_OP_BASE_FORTRAN_LXOR] = {
        NULL,
    },
    /* Corresponds to MPI_BXOR */
    [OMPI_OP_BASE_FORTRAN_BXOR] = {
        C_INTEGER_BASE(bxor, 3buff),
        C_INTEGER_EX(bxor, 3buff),
    },
    /* Corresponds to MPI_REPLACE */
    [OMPI_OP_BASE_FORTRAN_REPLACE] = {
        /* MPI_ACCUMULATE is handled differently than the other
           reductions, so just zero out its function
           implementations here to ensure that users don't invoke
           MPI_REPLACE with any reduction operations other than
           ACCUMULATE */
        NULL,
    },
};
