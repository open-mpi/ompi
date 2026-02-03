/*
 * Copyright (c) 2019      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2025      Software System Team, SANECHIPS.  All rights reserved.
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
#include "ompi/mca/op/riscv64/op_riscv64.h"

#if GENERATE_RVV_CODE
#    include <riscv_vector.h> 
#    define APPEND _rvv
#    define LMUL m4
#endif

/*
 * Concatenate preprocessor tokens A and B without expanding macro definitions
 * (however, if invoked from a macro, macro arguments are expanded).
 */
#define OP_CONCAT_NX(A, B) A ## B

/*
 * Concatenate preprocessor tokens A and B after macro-expanding them.
 */
#define OP_CONCAT(A, B) OP_CONCAT_NX(A, B)

/*
 * Since all the functions in this file are essentially identical, we
 * use a macro to substitute in names and types.  The core operation
 * in all functions that use this macro is the same.
 *
 * This macro is for (out op in).
 *
 */

typedef float float32_t;
typedef double float64_t;

#define RVV_FUNC(A)  OP_CONCAT(__riscv_, OP_CONCAT(A, LMUL))
#define RVV_TYPE(A)  OP_CONCAT(OP_CONCAT(A, LMUL), _t)

#if GENERATE_RVV_CODE
#define OP_RISCV64_FUNC(name, type_abbr, type_size, type, op)                                           \
    static void OP_CONCAT(ompi_op_riscv64_2buff_##name##_##type##type_size##_t, APPEND)                 \
        (const void *_in, void *_out, int *count,                                                       \
        struct ompi_datatype_t **dtype,                                                                 \
        struct ompi_op_base_module_1_0_0_t *module)                                                     \
    {                                                                                                   \
        size_t vl;                                                                                      \
        const int cnt = *count;                                                                         \
        type##type_size##_t *in = (type##type_size##_t *) _in,                                          \
                            *out = (type##type_size##_t *) _out;                                        \
        for (size_t i = 0; i < cnt; i += vl) {                                                          \
            vl = RVV_FUNC(vsetvl_e##type_size)(cnt - i);                                                \
            RVV_TYPE(v##type##type_size) vsrc = RVV_FUNC(vle##type_size##_v_##type_abbr)(&in[i], vl);   \
            RVV_TYPE(v##type##type_size) vdst = RVV_FUNC(vle##type_size##_v_##type_abbr)(&out[i], vl);  \
            vdst = RVV_FUNC(op##_vv_##type_abbr)(vdst, vsrc, vl);                                       \
            RVV_FUNC(vse##type_size##_v_##type_abbr)(&out[i], vdst, vl);                                \
        }                                                                                               \
    }
#endif

/*************************************************************************
 * Max
 *************************************************************************/
#undef current_func
#define current_func(a, b) ((a) > (b) ? (a) : (b))
    OP_RISCV64_FUNC(max, i8, 8, int, vmax)
    OP_RISCV64_FUNC(max, u8, 8, uint, vmaxu)
    OP_RISCV64_FUNC(max, i16, 16, int, vmax)
    OP_RISCV64_FUNC(max, u16, 16, uint, vmaxu)
    OP_RISCV64_FUNC(max, i32, 32, int, vmax)
    OP_RISCV64_FUNC(max, u32, 32, uint, vmaxu)
    OP_RISCV64_FUNC(max, i64, 64, int, vmax)
    OP_RISCV64_FUNC(max, u64, 64, uint, vmaxu)
    OP_RISCV64_FUNC(max, f32, 32, float, vfmax)
    OP_RISCV64_FUNC(max, f64, 64, float, vfmax)

/*************************************************************************
 * Min
 *************************************************************************/
#undef current_func
#define current_func(a, b) ((a) < (b) ? (a) : (b))
    OP_RISCV64_FUNC(min, i8, 8, int, vmin)
    OP_RISCV64_FUNC(min, u8, 8, uint, vminu)
    OP_RISCV64_FUNC(min, i16, 16, int, vmin)
    OP_RISCV64_FUNC(min, u16, 16, uint, vminu)
    OP_RISCV64_FUNC(min, i32, 32, int, vmin)
    OP_RISCV64_FUNC(min, u32, 32, uint, vminu)
    OP_RISCV64_FUNC(min, i64, 64, int, vmin)
    OP_RISCV64_FUNC(min, u64, 64, uint, vminu)
    OP_RISCV64_FUNC(min, f32, 32, float, vfmin)
    OP_RISCV64_FUNC(min, f64, 64, float, vfmin)

/*************************************************************************
* Sum
************************************************************************/
#undef current_func
#define current_func(a, b) ((a) + (b))
    OP_RISCV64_FUNC(sum, i8, 8, int, vadd)
    OP_RISCV64_FUNC(sum, u8, 8, uint, vadd)
    OP_RISCV64_FUNC(sum, i16, 16, int, vadd)
    OP_RISCV64_FUNC(sum, u16, 16, uint, vadd)
    OP_RISCV64_FUNC(sum, i32, 32, int, vadd)
    OP_RISCV64_FUNC(sum, u32, 32, uint, vadd)
    OP_RISCV64_FUNC(sum, i64, 64, int, vadd)
    OP_RISCV64_FUNC(sum, u64, 64, uint, vadd)
    OP_RISCV64_FUNC(sum, f32, 32, float, vfadd)
    OP_RISCV64_FUNC(sum, f64, 64, float, vfadd)

/*************************************************************************
 * Product
 *************************************************************************/
#undef current_func
#define current_func(a, b) ((a) * (b))
    OP_RISCV64_FUNC(prod, i8, 8, int, vmul)
    OP_RISCV64_FUNC(prod, u8, 8, uint, vmul)
    OP_RISCV64_FUNC(prod, i16, 16, int, vmul)
    OP_RISCV64_FUNC(prod, u16, 16, uint, vmul)
    OP_RISCV64_FUNC(prod, i32, 32, int, vmul)
    OP_RISCV64_FUNC(prod, u32, 32, uint, vmul)
    OP_RISCV64_FUNC(prod, i64, 64, int, vmul)
    OP_RISCV64_FUNC(prod, u64, 64, uint, vmul)
    OP_RISCV64_FUNC(prod, f32, 32, float, vfmul)
    OP_RISCV64_FUNC(prod, f64, 64, float, vfmul)

/*************************************************************************
 * Bitwise AND
 *************************************************************************/
#undef current_func
#define current_func(a, b) ((a) & (b))
    OP_RISCV64_FUNC(band, i8, 8, int, vand)
    OP_RISCV64_FUNC(band, u8, 8, uint, vand)
    OP_RISCV64_FUNC(band, i16, 16, int, vand)
    OP_RISCV64_FUNC(band, u16, 16, uint, vand)
    OP_RISCV64_FUNC(band, i32, 32, int, vand)
    OP_RISCV64_FUNC(band, u32, 32, uint, vand)
    OP_RISCV64_FUNC(band, i64, 64, int, vand)
    OP_RISCV64_FUNC(band, u64, 64, uint, vand)

 /*************************************************************************
 * Bitwise OR
 *************************************************************************/
#undef current_func
#define current_func(a, b) ((a) | (b))
    OP_RISCV64_FUNC(bor, i8, 8, int, vor)
    OP_RISCV64_FUNC(bor, u8, 8, uint, vor)
    OP_RISCV64_FUNC(bor, i16, 16, int, vor)
    OP_RISCV64_FUNC(bor, u16, 16, uint, vor)
    OP_RISCV64_FUNC(bor, i32, 32, int, vor)
    OP_RISCV64_FUNC(bor, u32, 32, uint, vor)
    OP_RISCV64_FUNC(bor, i64, 64, int, vor)
    OP_RISCV64_FUNC(bor, u64, 64, uint, vor)

/*************************************************************************
 * Bitwise XOR
 *************************************************************************/
#undef current_func
#define current_func(a, b) ((a) ^ (b))
    OP_RISCV64_FUNC(bxor, i8, 8, int, vxor)
    OP_RISCV64_FUNC(bxor, u8, 8, uint, vxor)
    OP_RISCV64_FUNC(bxor, i16, 16, int, vxor)
    OP_RISCV64_FUNC(bxor, u16, 16, uint, vxor)
    OP_RISCV64_FUNC(bxor, i32, 32, int, vxor)
    OP_RISCV64_FUNC(bxor, u32, 32, uint, vxor)
    OP_RISCV64_FUNC(bxor, i64, 64, int, vxor)
    OP_RISCV64_FUNC(bxor, u64, 64, uint, vxor)

/*
*  This is a three buffer (2 input and 1 output) version of the reduction
*  routines.
*/
#if GENERATE_RVV_CODE
#define OP_RISCV64_FUNC_3BUFF(name, type_abbr, type_size, type, op)                                     \
    static void OP_CONCAT(ompi_op_riscv64_3buff_##name##_##type##type_size##_t, APPEND)                 \
        (const void *_in1, const void *_in2, void *_out, int *count,                                    \
        struct ompi_datatype_t **dtype,                                                                 \
        struct ompi_op_base_module_1_0_0_t *module)                                                     \
    {                                                                                                   \
        size_t vl;                                                                                      \
        type##type_size##_t *in1 = (type##type_size##_t *) _in1,                                        \
                            *in2 = (type##type_size##_t *) _in2,                                        \
                            *out = (type##type_size##_t *) _out;                                        \
        for (size_t i = 0, cnt = *count; i < cnt; i += vl) {                                            \
            vl = RVV_FUNC(vsetvl_e##type_size)(cnt - i);                                                \
            RVV_TYPE(v##type##type_size) vsrc1 = RVV_FUNC(vle##type_size##_v_##type_abbr)(&in1[i], vl); \
            RVV_TYPE(v##type##type_size) vsrc2 = RVV_FUNC(vle##type_size##_v_##type_abbr)(&in2[i], vl); \
            RVV_TYPE(v##type##type_size) vdst = RVV_FUNC(op##_vv_##type_abbr)(vsrc1, vsrc2, vl);        \
            RVV_FUNC(vse##type_size##_v_##type_abbr)(&out[i], vdst, vl);                                \
        }                                                                                               \
    }
#endif

/*************************************************************************
 * Max
 *************************************************************************/
#undef current_func
#define current_func(a, b) ((a) > (b) ? (a) : (b))
    OP_RISCV64_FUNC_3BUFF(max, i8, 8, int, vmax)
    OP_RISCV64_FUNC_3BUFF(max, u8, 8, uint, vmaxu)
    OP_RISCV64_FUNC_3BUFF(max, i16, 16, int, vmax)
    OP_RISCV64_FUNC_3BUFF(max, u16, 16, uint, vmaxu)
    OP_RISCV64_FUNC_3BUFF(max, i32, 32, int, vmax)
    OP_RISCV64_FUNC_3BUFF(max, u32, 32, uint, vmaxu)
    OP_RISCV64_FUNC_3BUFF(max, i64, 64, int, vmax)
    OP_RISCV64_FUNC_3BUFF(max, u64, 64, uint, vmaxu)
    OP_RISCV64_FUNC_3BUFF(max, f32, 32, float, vfmax)
    OP_RISCV64_FUNC_3BUFF(max, f64, 64, float, vfmax)

/*************************************************************************
* Min
*************************************************************************/
#undef current_func
#define current_func(a, b) ((a) < (b) ? (a) : (b))
    OP_RISCV64_FUNC_3BUFF(min, i8, 8, int, vmin)
    OP_RISCV64_FUNC_3BUFF(min, u8, 8, uint, vminu)
    OP_RISCV64_FUNC_3BUFF(min, i16, 16, int, vmin)
    OP_RISCV64_FUNC_3BUFF(min, u16, 16, uint, vminu)
    OP_RISCV64_FUNC_3BUFF(min, i32, 32, int, vmin)
    OP_RISCV64_FUNC_3BUFF(min, u32, 32, uint, vminu)
    OP_RISCV64_FUNC_3BUFF(min, i64, 64, int, vmin)
    OP_RISCV64_FUNC_3BUFF(min, u64, 64, uint, vminu)
    OP_RISCV64_FUNC_3BUFF(min, f32, 32, float, vfmin)
    OP_RISCV64_FUNC_3BUFF(min, f64, 64, float, vfmin)

 /*************************************************************************
 * Sum
 ************************************************************************/
#undef current_func
#define current_func(a, b) ((a) + (b))
    OP_RISCV64_FUNC_3BUFF(sum, i8, 8, int, vadd)
    OP_RISCV64_FUNC_3BUFF(sum, u8, 8, uint, vadd)
    OP_RISCV64_FUNC_3BUFF(sum, i16, 16, int, vadd)
    OP_RISCV64_FUNC_3BUFF(sum, u16, 16, uint, vadd)
    OP_RISCV64_FUNC_3BUFF(sum, i32, 32, int, vadd)
    OP_RISCV64_FUNC_3BUFF(sum, u32, 32, uint, vadd)
    OP_RISCV64_FUNC_3BUFF(sum, i64, 64, int, vadd)
    OP_RISCV64_FUNC_3BUFF(sum, u64, 64, uint, vadd)
    OP_RISCV64_FUNC_3BUFF(sum, f32, 32, float, vfadd)
    OP_RISCV64_FUNC_3BUFF(sum, f64, 64, float, vfadd)

/*************************************************************************
 * Product
 *************************************************************************/
#undef current_func
#define current_func(a, b) ((a) * (b))
    OP_RISCV64_FUNC_3BUFF(prod, i8, 8, int, vmul)
    OP_RISCV64_FUNC_3BUFF(prod, u8, 8, uint, vmul)
    OP_RISCV64_FUNC_3BUFF(prod, i16, 16, int, vmul)
    OP_RISCV64_FUNC_3BUFF(prod, u16, 16, uint, vmul)
    OP_RISCV64_FUNC_3BUFF(prod, i32, 32, int, vmul)
    OP_RISCV64_FUNC_3BUFF(prod, u32, 32, uint, vmul)
    OP_RISCV64_FUNC_3BUFF(prod, i64, 64, int, vmul)
    OP_RISCV64_FUNC_3BUFF(prod, u64, 64, uint, vmul)
    OP_RISCV64_FUNC_3BUFF(prod, f32, 32, float, vfmul)
    OP_RISCV64_FUNC_3BUFF(prod, f64, 64, float, vfmul)

/*************************************************************************
 * Bitwise AND
 *************************************************************************/
#undef current_func
#define current_func(a, b) ((a) & (b))
    OP_RISCV64_FUNC_3BUFF(band, i8, 8, int, vand)
    OP_RISCV64_FUNC_3BUFF(band, u8, 8, uint, vand)
    OP_RISCV64_FUNC_3BUFF(band, i16, 16, int, vand)
    OP_RISCV64_FUNC_3BUFF(band, u16, 16, uint, vand)
    OP_RISCV64_FUNC_3BUFF(band, i32, 32, int, vand)
    OP_RISCV64_FUNC_3BUFF(band, u32, 32, uint, vand)
    OP_RISCV64_FUNC_3BUFF(band, i64, 64, int, vand)
    OP_RISCV64_FUNC_3BUFF(band, u64, 64, uint, vand)

 /*************************************************************************
 * Bitwise OR
 *************************************************************************/
#undef current_func
#define current_func(a, b) ((a) | (b))
    OP_RISCV64_FUNC_3BUFF(bor, i8, 8, int, vor)
    OP_RISCV64_FUNC_3BUFF(bor, u8, 8, uint, vor)
    OP_RISCV64_FUNC_3BUFF(bor, i16, 16, int, vor)
    OP_RISCV64_FUNC_3BUFF(bor, u16, 16, uint, vor)
    OP_RISCV64_FUNC_3BUFF(bor, i32, 32, int, vor)
    OP_RISCV64_FUNC_3BUFF(bor, u32, 32, uint, vor)
    OP_RISCV64_FUNC_3BUFF(bor, i64, 64, int, vor)
    OP_RISCV64_FUNC_3BUFF(bor, u64, 64, uint, vor)

/*************************************************************************
 * Bitwise XOR
 *************************************************************************/
#undef current_func
#define current_func(a, b) ((a) ^ (b))
    OP_RISCV64_FUNC_3BUFF(bxor, i8, 8, int, vxor)
    OP_RISCV64_FUNC_3BUFF(bxor, u8, 8, uint, vxor)
    OP_RISCV64_FUNC_3BUFF(bxor, i16, 16, int, vxor)
    OP_RISCV64_FUNC_3BUFF(bxor, u16, 16, uint, vxor)
    OP_RISCV64_FUNC_3BUFF(bxor, i32, 32, int, vxor)
    OP_RISCV64_FUNC_3BUFF(bxor, u32, 32, uint, vxor)
    OP_RISCV64_FUNC_3BUFF(bxor, i64, 64, int, vxor)
    OP_RISCV64_FUNC_3BUFF(bxor, u64, 64, uint, vxor)

    /** C integer ***********************************************************/
#define C_INTEGER_BASE(name, ftype)                                         \
    [OMPI_OP_BASE_TYPE_INT8_T]   = OP_CONCAT(ompi_op_riscv64_##ftype##_##name##_int8_t, APPEND), \
    [OMPI_OP_BASE_TYPE_UINT8_T]  = OP_CONCAT(ompi_op_riscv64_##ftype##_##name##_uint8_t, APPEND), \
    [OMPI_OP_BASE_TYPE_INT16_T]  = OP_CONCAT(ompi_op_riscv64_##ftype##_##name##_int16_t, APPEND), \
    [OMPI_OP_BASE_TYPE_UINT16_T] = OP_CONCAT(ompi_op_riscv64_##ftype##_##name##_uint16_t, APPEND), \
    [OMPI_OP_BASE_TYPE_INT32_T]  = OP_CONCAT(ompi_op_riscv64_##ftype##_##name##_int32_t, APPEND), \
    [OMPI_OP_BASE_TYPE_UINT32_T] = OP_CONCAT(ompi_op_riscv64_##ftype##_##name##_uint32_t, APPEND)
#define C_INTEGER_EX(name, ftype)                                         \
    [OMPI_OP_BASE_TYPE_INT64_T]  = OP_CONCAT(ompi_op_riscv64_##ftype##_##name##_int64_t, APPEND), \
    [OMPI_OP_BASE_TYPE_UINT64_T] = OP_CONCAT(ompi_op_riscv64_##ftype##_##name##_uint64_t, APPEND)

    /** Floating point, including all the Fortran reals *********************/
#define FLOAT(name, ftype)  OP_CONCAT(ompi_op_riscv64_##ftype##_##name##_float32_t, APPEND)
#define DOUBLE(name, ftype) OP_CONCAT(ompi_op_riscv64_##ftype##_##name##_float64_t, APPEND)

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

    ompi_op_base_handler_fn_t OP_CONCAT(ompi_op_riscv64_functions, APPEND) [OMPI_OP_BASE_FORTRAN_OP_MAX][OMPI_OP_BASE_TYPE_MAX] =
{
    /* Corresponds to MPI_OP_NULL */
    [OMPI_OP_BASE_FORTRAN_NULL] = {
        /* Leaving this empty puts in NULL for all entries */
        NULL,
    },
    /* Corresponds to MPI_MAX */
    [OMPI_OP_BASE_FORTRAN_MAX] = {
        C_INTEGER_BASE(max, 2buff),
        C_INTEGER_EX(max, 2buff),
        FLOATING_POINT(max, 2buff),
    },
    /* Corresponds to MPI_MIN */
    [OMPI_OP_BASE_FORTRAN_MIN] = {
        C_INTEGER_BASE(min, 2buff),
        C_INTEGER_EX(min, 2buff),
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
        C_INTEGER_EX(prod, 2buff),
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

    ompi_op_base_3buff_handler_fn_t OP_CONCAT(ompi_op_riscv64_3buff_functions, APPEND)[OMPI_OP_BASE_FORTRAN_OP_MAX][OMPI_OP_BASE_TYPE_MAX] =
{
    /* Corresponds to MPI_OP_NULL */
    [OMPI_OP_BASE_FORTRAN_NULL] = {
        /* Leaving this empty puts in NULL for all entries */
        NULL,
    },
    /* Corresponds to MPI_MAX */
    [OMPI_OP_BASE_FORTRAN_MAX] = {
        C_INTEGER_BASE(max, 3buff),
        C_INTEGER_EX(max, 3buff),
        FLOATING_POINT(max, 3buff),
    },
    /* Corresponds to MPI_MIN */
    [OMPI_OP_BASE_FORTRAN_MIN] = {
        C_INTEGER_BASE(min, 3buff),
        C_INTEGER_EX(min, 3buff),
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
        C_INTEGER_EX(prod, 3buff),
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
