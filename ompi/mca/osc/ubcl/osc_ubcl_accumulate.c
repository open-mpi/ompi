/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2025 Bull SAS.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/**
 * @file
 *
 * Bull eXtreme Interconnect OSC API implementation.
 *
 * Implementation of API defined in osc.h. To see parameters and return values
 * of these functions, refer to ompi/mca/osc/osc.h.
 */

#include "ompi/mca/osc/ubcl/osc_ubcl.h"
#include "opal/mca/common/ubcl/common_ubcl.h"
#include "ompi/mca/osc/ubcl/osc_ubcl_info.h"
#include "ompi/mca/osc/ubcl/osc_ubcl_utils.h"
#include "ompi/mca/osc/ubcl/osc_ubcl_sync.h"
#include "ompi/mca/osc/ubcl/osc_ubcl_request.h"
#include "ompi/mca/common/ubcl/common_ubcl.h"

int get_ubcl_int_type(size_t size, bool is_signed, ubcl_win_atomic_datatype_t *ubcl_type)
{
    int ret = OMPI_SUCCESS;

    switch (size) {
    case 1:
        *ubcl_type = is_signed ? UBCL_INT8 : UBCL_UINT8;
        break;
    case 2:
        *ubcl_type = is_signed ? UBCL_INT16 : UBCL_UINT16;
        break;
    case 4:
        *ubcl_type = is_signed ? UBCL_INT32 : UBCL_UINT32;
        break;
    case 8:
        *ubcl_type = is_signed ? UBCL_INT64 : UBCL_UINT64;
        break;
    default:
        ret = OMPI_ERR_NOT_SUPPORTED;
        break;
    }

    return ret;
}

int get_ubcl_fp_type(size_t size, ubcl_win_atomic_datatype_t *ubcl_type)
{
    int ret = OMPI_SUCCESS;

    switch (size) {
    case sizeof(float):
        *ubcl_type = UBCL_FLOAT;
        break;
    case sizeof(double):
        *ubcl_type = UBCL_DOUBLE;
        break;
    case sizeof(long double):
        *ubcl_type = UBCL_LONG_DOUBLE;
        break;
    default:
        ret = OMPI_ERR_NOT_SUPPORTED;
        break;
    }

    return ret;
}

static int get_c_integer_ubcl_type(struct ompi_datatype_t *origin_dt,
                                   ubcl_win_atomic_datatype_t *ubcl_type)
{
    int ret = OMPI_SUCCESS;
    size_t dt_size;

    if (OMPI_SUCCESS != ompi_datatype_type_size(origin_dt, &dt_size)) {
        return OMPI_ERR_BAD_PARAM;
    }

    if (MPI_CHAR == origin_dt
        /* Note: MPI_CHAR is not a valid type for predefined operations
         *       but MPI_SIGNED_CHAR and MPI_UNSIGNED_CHAR are.
         *       We suppost MPI_CHAR behaves as MPI_SIGNED_CHAR.
         * C.F.: MPI 4.1 section 6.9.2 (p.227)
         *       MPI 5.0 section 6.9.2 (p.225)
         */
        || MPI_INT == origin_dt || MPI_LONG == origin_dt || MPI_SHORT == origin_dt
#if OPAL_HAVE_LONG_LONG
        || MPI_LONG_LONG_INT == origin_dt || MPI_LONG_LONG == origin_dt
#endif
        || MPI_SIGNED_CHAR == origin_dt || MPI_INT8_T == origin_dt || MPI_INT16_T == origin_dt
        || MPI_INT32_T == origin_dt || MPI_INT64_T == origin_dt) {

        ret = get_ubcl_int_type(dt_size, true, ubcl_type);

    } else if (MPI_UNSIGNED_SHORT == origin_dt || MPI_UNSIGNED == origin_dt
               || MPI_UNSIGNED_LONG == origin_dt
#if OPAL_HAVE_LONG_LONG
               || MPI_UNSIGNED_LONG_LONG == origin_dt
#endif
               || MPI_UNSIGNED_CHAR == origin_dt || MPI_UINT8_T == origin_dt
               || MPI_UINT16_T == origin_dt || MPI_UINT32_T == origin_dt
               || MPI_UINT64_T == origin_dt) {

        ret = get_ubcl_int_type(dt_size, false, ubcl_type);

    } else {
        ret = OMPI_ERR_BAD_PARAM;
    }
    return ret;
}

static int get_fortran_integer_ubcl_type(struct ompi_datatype_t *origin_dt,
                                         ubcl_win_atomic_datatype_t *ubcl_type)
{
    int ret = OMPI_SUCCESS;
    size_t dt_size;

    if (OMPI_SUCCESS != ompi_datatype_type_size(origin_dt, &dt_size)) {
        return OMPI_ERR_BAD_PARAM;
    }

    if (MPI_INTEGER == origin_dt
#if OMPI_HAVE_FORTRAN_INTEGER1
        || MPI_INTEGER1 == origin_dt
#endif
#if OMPI_HAVE_FORTRAN_INTEGER2
        || MPI_INTEGER2 == origin_dt
#endif
#if OMPI_HAVE_FORTRAN_INTEGER4
        || MPI_INTEGER4 == origin_dt
#endif
#if OMPI_HAVE_FORTRAN_INTEGER8
        || MPI_INTEGER8 == origin_dt
#endif
#if OMPI_HAVE_FORTRAN_INTEGER16
        || MPI_INTEGER16 == origin_dt
#endif
    ) {
        ret = get_ubcl_int_type(dt_size, true, ubcl_type);
    } else {
        ret = OMPI_ERR_BAD_PARAM;
    }
    return ret;
}

static int get_fp_ubcl_type(struct ompi_datatype_t *origin_dt,
                            ubcl_win_atomic_datatype_t *ubcl_type)
{
    /* TODO: handle MPI_TYPE_CREATE_F90_REAL handles */
    int ret = OMPI_SUCCESS;
    size_t dt_size;

    if (OMPI_SUCCESS != ompi_datatype_type_size(origin_dt, &dt_size)) {
        return OMPI_ERR_BAD_PARAM;
    }

    if (MPI_FLOAT == origin_dt || MPI_DOUBLE == origin_dt || MPI_REAL == origin_dt
        || MPI_DOUBLE_PRECISION == origin_dt || MPI_LONG_DOUBLE == origin_dt
/*#if OMPI_HAVE_FORTRAN_REAL2
 *      || MPI_REAL2 == origin_dt
 *#endif */
#if OMPI_HAVE_FORTRAN_REAL4
        || MPI_REAL4 == origin_dt
#endif
#if OMPI_HAVE_FORTRAN_REAL8
        || MPI_REAL8 == origin_dt
#endif
#if OMPI_HAVE_FORTRAN_REAL16
        || MPI_REAL16 == origin_dt
#endif
    ) {
        ret = get_ubcl_fp_type(dt_size, ubcl_type);
    } else {
        ret = OMPI_ERR_BAD_PARAM;
    }
    return ret;
}

static int get_logical_ubcl_type(struct ompi_datatype_t *origin_dt,
                                 ubcl_win_atomic_datatype_t *ubcl_type)
{
    int ret = OMPI_SUCCESS;
    size_t dt_size;

    if (OMPI_SUCCESS != ompi_datatype_type_size(origin_dt, &dt_size)) {
        return OMPI_ERR_BAD_PARAM;
    }

    /* Some consideration are needed to take care of fortran logical
     * Yet not we dit
     */
    if (MPI_C_BOOL == origin_dt || MPI_CXX_BOOL == origin_dt) {
        ret = get_ubcl_int_type(dt_size, false, ubcl_type);
    } else if (MPI_LOGICAL == origin_dt
#if OMPI_HAVE_FORTRAN_LOGICAL1
               || MPI_LOGICAL1 == origin_dt
#endif
#if OMPI_HAVE_FORTRAN_LOGICAL2
               || MPI_LOGICAL2 == origin_dt
#endif
#if OMPI_HAVE_FORTRAN_LOGICAL4
               || MPI_LOGICAL4 == origin_dt
#endif
#if OMPI_HAVE_FORTRAN_LOGICAL8
               || MPI_LOGICAL8 == origin_dt
#endif
#if OMPI_HAVE_FORTRAN_LOGICAL16
               || MPI_LOGICAL16 == origin_dt
#endif
    ) {
        ret = OMPI_ERR_NOT_IMPLEMENTED;
    } else {
        ret = OMPI_ERR_BAD_PARAM;
    }
    return ret;
}

static int get_complex_ubcl_type(struct ompi_datatype_t *origin_dt,
                                 ubcl_win_atomic_datatype_t *ubcl_type)
{
    int ret = OMPI_SUCCESS;
    if (MPI_COMPLEX == origin_dt
#if HAVE_FLOAT__COMPLEX
        || MPI_C_COMPLEX == origin_dt || MPI_C_FLOAT_COMPLEX == origin_dt
#endif
#if HAVE_DOUBLE__COMPLEX
        || MPI_C_DOUBLE_COMPLEX == origin_dt
#endif
#if HAVE_LONG_DOUBLE__COMPLEX
        || MPI_C_LONG_DOUBLE_COMPLEX == origin_dt
#endif
        || MPI_CXX_FLOAT_COMPLEX == origin_dt || MPI_CXX_DOUBLE_COMPLEX == origin_dt
        || MPI_CXX_LONG_DOUBLE_COMPLEX == origin_dt || MPI_DOUBLE_COMPLEX == origin_dt
/*#if OMPI_HAVE_FORTRAN_REAL2
 *  || MPI_COMPLEX4 == origin_dt
 *#endif */
#if OMPI_HAVE_FORTRAN_REAL4
        || MPI_COMPLEX8 == origin_dt
#endif
#if OMPI_HAVE_FORTRAN_REAL8
        || MPI_COMPLEX16 == origin_dt
#endif
#if OMPI_HAVE_FORTRAN_REAL16
        || MPI_COMPLEX32 == origin_dt
#endif
    ) {
        ret = OMPI_ERR_NOT_IMPLEMENTED;
    } else {
        ret = OMPI_ERR_BAD_PARAM;
    }
    return ret;
}

static int get_byte_ubcl_type(struct ompi_datatype_t *origin_dt,
                              ubcl_win_atomic_datatype_t *ubcl_type)
{
    int ret = OMPI_SUCCESS;

    if (MPI_BYTE == origin_dt) {
        *ubcl_type = UBCL_UINT8;
    } else {
        ret = OMPI_ERR_BAD_PARAM;
    }

    return ret;
}

static int get_multi_language_ubcl_type(struct ompi_datatype_t *origin_dt,
                                        ubcl_win_atomic_datatype_t *ubcl_type)
{
    int ret = OMPI_SUCCESS;
    size_t dt_size;

    if (OMPI_SUCCESS != ompi_datatype_type_size(origin_dt, &dt_size)) {
        return OMPI_ERR_BAD_PARAM;
    }

    if (MPI_AINT == origin_dt || MPI_OFFSET == origin_dt || MPI_COUNT == origin_dt) {
        ret = get_ubcl_int_type(dt_size, true, ubcl_type);
    } else {
        ret = OMPI_ERR_BAD_PARAM;
    }

    return ret;
}

static int get_pair_ubcl_type(struct ompi_datatype_t *origin_dt,
                              ubcl_win_atomic_datatype_t *ubcl_type)
{
    int ret = OMPI_SUCCESS;
    size_t dt_size;

    if (OMPI_SUCCESS != ompi_datatype_type_size(origin_dt, &dt_size)) {
        return OMPI_ERR_BAD_PARAM;
    }

    if (MPI_FLOAT_INT == origin_dt) {
        *ubcl_type = UBCL_FLOAT;
    } else if (MPI_DOUBLE_INT == origin_dt) {
        *ubcl_type = UBCL_DOUBLE;
    } else if (MPI_LONG_DOUBLE_INT == origin_dt) {
        *ubcl_type = UBCL_LONG_DOUBLE;
    } else if (MPI_LONG_INT == origin_dt) {
        ret = get_ubcl_int_type(sizeof(long), true, ubcl_type);
    } else if (MPI_SHORT_INT == origin_dt) {
        ret = get_ubcl_int_type(sizeof(short), true, ubcl_type);
    } else if (MPI_2INT == origin_dt) {
        ret = get_ubcl_int_type(sizeof(int), true, ubcl_type);
    } else if (MPI_2REAL == origin_dt || MPI_2DOUBLE_PRECISION == origin_dt) {
        ret = get_ubcl_fp_type(dt_size, ubcl_type);
    } else if (MPI_2INTEGER == origin_dt) {
        ret = get_ubcl_int_type(dt_size, true, ubcl_type);
    } else {
        ret = OMPI_ERR_BAD_PARAM;
    }

    return ret;
}

static int build_ubcl_loc_op(struct ompi_datatype_t *origin_dt, struct ompi_op_t *op,
                             ubcl_win_op_t *ubcl_op)
{
    ubcl_win_atomic_operator_t ubcl_operator;
    ubcl_win_atomic_datatype_t data_type;
    ubcl_win_atomic_datatype_t loc_type;
    int ret = OMPI_SUCCESS;

    if (MPI_MAXLOC == op) {
        ubcl_operator = UBCL_MAXLOC;
    } else if (MPI_MINLOC == op) {
        ubcl_operator = UBCL_MINLOC;
    } else {
        return OMPI_ERR_BAD_PARAM;
    }

    ret = get_ubcl_int_type(sizeof(int), true, &loc_type);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }

    ret = get_pair_ubcl_type(origin_dt, &data_type);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }

    return ubcl_error_to_ompi(ubcl_win_build_loc_op(ubcl_op, data_type, ubcl_operator, loc_type));
}

#define GET_TYPE(fct, origin_dt, data_type)            \
    do {                                               \
        int _err = fct(origin_dt, data_type);          \
        if (OMPI_SUCCESS == _err) {                    \
            goto got_type;                             \
        } else if (OMPI_ERR_NOT_IMPLEMENTED == _err) { \
            goto not_implemented;                      \
        }                                              \
    } while (0)

static int build_ubcl_minmax_op(struct ompi_datatype_t *origin_dt, struct ompi_op_t *op,
                                ubcl_win_op_t *ubcl_op)
{
    ubcl_win_atomic_operator_t ubcl_operator;
    ubcl_win_atomic_datatype_t data_type = UBCL_TYPE_NONE;

    if (MPI_MAX == op) {
        ubcl_operator = UBCL_MAX;
    } else if (MPI_MIN == op) {
        ubcl_operator = UBCL_MAX;
    } else {
        return OMPI_ERR_BAD_PARAM;
    }

    /* This macro calls goto on got_type or not_implemented labels if the
     * datatype is one of the predifines one of this category.
     */
    GET_TYPE(get_c_integer_ubcl_type, origin_dt, &data_type);
    GET_TYPE(get_fortran_integer_ubcl_type, origin_dt, &data_type);
    GET_TYPE(get_fp_ubcl_type, origin_dt, &data_type);
    GET_TYPE(get_multi_language_ubcl_type, origin_dt, &data_type);

    return OMPI_ERR_BAD_PARAM;

got_type:
    return ubcl_error_to_ompi(ubcl_win_build_op(ubcl_op, data_type, ubcl_operator));

not_implemented:
    return OMPI_ERR_NOT_IMPLEMENTED;
}

static int build_ubcl_arithmetic_op(struct ompi_datatype_t *origin_dt, struct ompi_op_t *op,
                                    ubcl_win_op_t *ubcl_op)
{
    ubcl_win_atomic_operator_t ubcl_operator;
    ubcl_win_atomic_datatype_t data_type = UBCL_TYPE_NONE;

    if (MPI_SUM == op) {
        ubcl_operator = UBCL_SUM;
    } else if (MPI_PROD == op) {
        ubcl_operator = UBCL_PROD;
    } else {
        return OMPI_ERR_BAD_PARAM;
    }

    /* This macro calls goto on got_type or not_implemented labels if the
     * datatype is one of the predifines one of this category.
     */
    GET_TYPE(get_c_integer_ubcl_type, origin_dt, &data_type);
    GET_TYPE(get_fortran_integer_ubcl_type, origin_dt, &data_type);
    GET_TYPE(get_fp_ubcl_type, origin_dt, &data_type);
    GET_TYPE(get_complex_ubcl_type, origin_dt, &data_type);
    GET_TYPE(get_multi_language_ubcl_type, origin_dt, &data_type);

    return OMPI_ERR_BAD_PARAM;

got_type:
    return ubcl_error_to_ompi(ubcl_win_build_op(ubcl_op, data_type, ubcl_operator));

not_implemented:
    return OMPI_ERR_NOT_IMPLEMENTED;
}

static int build_ubcl_logical_op(struct ompi_datatype_t *origin_dt, struct ompi_op_t *op,
                                 ubcl_win_op_t *ubcl_op)
{
    ubcl_win_atomic_operator_t ubcl_operator;
    ubcl_win_atomic_datatype_t data_type = UBCL_TYPE_NONE;

    if (MPI_LAND == op) {
        ubcl_operator = UBCL_LAND;
    } else if (MPI_LOR == op) {
        ubcl_operator = UBCL_LOR;
    } else if (MPI_LXOR == op) {
        ubcl_operator = UBCL_LXOR;
    } else {
        return OMPI_ERR_BAD_PARAM;
    }

    /* This macro calls goto on got_type or not_implemented labels if the
     * datatype is one of the predifines one of this category.
     */
    GET_TYPE(get_c_integer_ubcl_type, origin_dt, &data_type);
    GET_TYPE(get_logical_ubcl_type, origin_dt, &data_type);

    return OMPI_ERR_BAD_PARAM;

got_type:
    return ubcl_error_to_ompi(ubcl_win_build_op(ubcl_op, data_type, ubcl_operator));

not_implemented:
    return OMPI_ERR_NOT_IMPLEMENTED;
}

static int build_ubcl_bitwise_op(struct ompi_datatype_t *origin_dt, struct ompi_op_t *op,
                                 ubcl_win_op_t *ubcl_op)
{
    ubcl_win_atomic_operator_t ubcl_operator;
    ubcl_win_atomic_datatype_t data_type = UBCL_TYPE_NONE;

    if (MPI_BAND == op) {
        ubcl_operator = UBCL_BAND;
    } else if (MPI_BOR == op) {
        ubcl_operator = UBCL_BOR;
    } else if (MPI_BXOR == op) {
        ubcl_operator = UBCL_BXOR;
    } else {
        return OMPI_ERR_BAD_PARAM;
    }

    /* This macro calls goto on got_type or not_implemented labels if the
     * datatype is one of the predifines one of this category.
     */
    GET_TYPE(get_c_integer_ubcl_type, origin_dt, &data_type);
    GET_TYPE(get_fortran_integer_ubcl_type, origin_dt, &data_type);
    GET_TYPE(get_byte_ubcl_type, origin_dt, &data_type);
    GET_TYPE(get_multi_language_ubcl_type, origin_dt, &data_type);

    return OMPI_ERR_BAD_PARAM;

got_type:
    return ubcl_error_to_ompi(ubcl_win_build_op(ubcl_op, data_type, ubcl_operator));

not_implemented:
    return OMPI_ERR_NOT_IMPLEMENTED;
}

static int build_ubcl_fake_op(struct ompi_datatype_t *origin_dt, struct ompi_op_t *op,
                              ubcl_win_op_t *ubcl_op)
{
    int ret = OMPI_SUCCESS;
    ubcl_win_atomic_operator_t ubcl_operator;
    ubcl_win_atomic_datatype_t data_type = UBCL_TYPE_NONE;
    ubcl_win_atomic_datatype_t loc_type = UBCL_TYPE_NONE;

    if (MPI_REPLACE == op) {
        ubcl_operator = UBCL_REPLACE;
    } else if (MPI_NO_OP == op) {
        ubcl_operator = UBCL_NO_OP;
    } else {
        return OMPI_ERR_BAD_PARAM;
    }

    /* This macro calls goto on got_type or not_implemented labels if the
     * datatype is one of the predifines one of this category.
     */
    GET_TYPE(get_c_integer_ubcl_type, origin_dt, &data_type);
    GET_TYPE(get_fortran_integer_ubcl_type, origin_dt, &data_type);
    GET_TYPE(get_fp_ubcl_type, origin_dt, &data_type);
    GET_TYPE(get_logical_ubcl_type, origin_dt, &data_type);
    GET_TYPE(get_complex_ubcl_type, origin_dt, &data_type);
    GET_TYPE(get_byte_ubcl_type, origin_dt, &data_type);
    GET_TYPE(get_multi_language_ubcl_type, origin_dt, &data_type);

    ret = get_pair_ubcl_type(origin_dt, &data_type);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }
    ret = get_ubcl_int_type(sizeof(int), true, &loc_type);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }


got_type:
    return ubcl_error_to_ompi(ubcl_win_build_loc_op(ubcl_op, data_type, ubcl_operator, loc_type));

not_implemented:
    return OMPI_ERR_NOT_IMPLEMENTED;
}

static int build_ubcl_op(struct ompi_datatype_t *origin_dt, struct ompi_op_t *op,
                         ubcl_win_op_t *ubcl_op)
{
    if (MPI_MAXLOC == op || MPI_MINLOC == op) {
        return build_ubcl_loc_op(origin_dt, op, ubcl_op);
    } else if (MPI_MAX == op || MPI_MIN == op) {
        return build_ubcl_minmax_op(origin_dt, op, ubcl_op);
    } else if (MPI_SUM == op || MPI_PROD == op) {
        return build_ubcl_arithmetic_op(origin_dt, op, ubcl_op);
    } else if (MPI_LAND == op || MPI_LOR == op || MPI_LXOR == op) {
        return build_ubcl_logical_op(origin_dt, op, ubcl_op);
    } else if (MPI_BAND == op || MPI_BOR == op || MPI_BXOR == op) {
        return build_ubcl_bitwise_op(origin_dt, op, ubcl_op);
    } else if (MPI_REPLACE == op || MPI_NO_OP == op) {
        return build_ubcl_fake_op(origin_dt, op, ubcl_op);
    } else {
        return OMPI_ERR_BAD_PARAM;
    }
}

int ompi_osc_ubcl_accumulate(const void *origin_addr, int origin_count,
                             struct ompi_datatype_t *origin_dt, int target, ptrdiff_t target_disp,
                             int target_count, struct ompi_datatype_t *target_dt,
                             struct ompi_op_t *op, struct ompi_win_t *win)
{
    return ompi_osc_ubcl_raccumulate(origin_addr, origin_count, origin_dt, target, target_disp,
                                     target_count, target_dt, op, win, NULL);
}

int ompi_osc_ubcl_raccumulate(const void *origin_addr, int origin_count,
                              struct ompi_datatype_t *origin_dt, int target, ptrdiff_t target_disp,
                              int target_count, struct ompi_datatype_t *target_dt,
                              struct ompi_op_t *op, struct ompi_win_t *win,
                              struct ompi_request_t **ompi_req)
{
    return ompi_osc_ubcl_rget_accumulate(origin_addr, origin_count, origin_dt, NULL, 0, NULL,
                                         target, target_disp, target_count, target_dt, op, win,
                                         ompi_req);
}

int ompi_osc_ubcl_get_accumulate(const void *origin_addr, int origin_count,
                                 struct ompi_datatype_t *origin_dt, void *result_addr,
                                 int result_count, struct ompi_datatype_t *result_dt,
                                 int target_rank, ptrdiff_t target_disp, int target_count,
                                 struct ompi_datatype_t *target_dt, struct ompi_op_t *op,
                                 struct ompi_win_t *win)
{
    return ompi_osc_ubcl_rget_accumulate(origin_addr, origin_count, origin_dt, result_addr,
                                         result_count, result_dt, target_rank, target_disp,
                                         target_count, target_dt, op, win, NULL);
}

static int compute_aligned_iovecs_count(struct iovec **iovecs, size_t *iovecs_count,
                                        int iovecs_to_align, size_t *aligned_iovec_count)
{
    size_t segment[iovecs_to_align];
    size_t consumed_size[iovecs_to_align];
    size_t aligned_count = 0;

    for (int i = 0; i < iovecs_to_align; i++) {
        segment[i] = 0;
        consumed_size[i] = 0;
    }

    /* Stop when we reach the end of one iovec */
    while (true) {
        size_t min_remaining_size = UINT64_MAX;

        /* Get the minimum remaining size */
        for (int i = 0; i < iovecs_to_align; i++) {
            if (segment[i] >= iovecs_count[i]) {
                goto end_compute_aligned_count;
            }

            size_t remaining_size = iovecs[i][segment[i]].iov_len - consumed_size[i];

            if (remaining_size < min_remaining_size) {
                min_remaining_size = remaining_size;
            }
        }

        /* Consume size */
        for (int i = 0; i < iovecs_to_align; i++) {
            consumed_size[i] += min_remaining_size;

            if (consumed_size[i] == iovecs[i][segment[i]].iov_len) {
                consumed_size[i] = 0;
                segment[i]++;
            }
        }

        aligned_count++;
    }
end_compute_aligned_count:

    /* The send buffer must fit in the target buffer and the target buffer must fit
     * in the fetch buffer so the send buffer must be the smallest and all its segments
     * must have been consumed
     */
    if (segment[0] < iovecs_count[0]) {
        return OMPI_ERROR;
    }

    *aligned_iovec_count = aligned_count;

    return OMPI_SUCCESS;
}

static void compute_aligned_iovecs(struct iovec **iovecs, size_t *iovecs_count, int iovecs_to_align,
                                   struct iovec **aligned_iovecs, size_t aligned_iovec_count)
{
    size_t segment[iovecs_to_align];
    size_t consumed_size[iovecs_to_align];

    /* Run through iovecs a second time to fill aligned_iovecs */
    for (int i = 0; i < iovecs_to_align; i++) {
        segment[i] = 0;
        consumed_size[i] = 0;
    }

    for (size_t seg = 0; seg < aligned_iovec_count; seg++) {
        size_t min_remaining_size = UINT64_MAX;

        /* Get the minimum remaining size */
        for (int i = 0; i < iovecs_to_align; i++) {
            size_t remaining_size = iovecs[i][segment[i]].iov_len - consumed_size[i];

            if (remaining_size < min_remaining_size) {
                min_remaining_size = remaining_size;
            }
        }

        /* Consume size */
        for (int i = 0; i < iovecs_to_align; i++) {
            aligned_iovecs[i][seg].iov_base = iovecs[i][segment[i]].iov_base + consumed_size[i];
            aligned_iovecs[i][seg].iov_len = min_remaining_size;

            consumed_size[i] += min_remaining_size;

            if (consumed_size[i] == iovecs[i][segment[i]].iov_len) {
                consumed_size[i] = 0;
                segment[i]++;
            }
        }
    }
}

/* This function takes an array of iovec arrays with an arbitrary fragmentation,
 * and allocates a new array of iovec arrays describing the same memory areas but
 * potentially splitted in smaller segments.
 *
 * All the returned iovec have the same count of fragments and the i-th element
 * have the same length on each of it.
 *
 * If input iovec arrays have different total length, they must be provided in
 * total length increasing order.
 * In this case, iovecs are truncated according to the smallest one.
 * An error is raised if the smallest one is not the first one.
 */
static int align_iovecs(struct iovec **iovecs, size_t *iovecs_count, int iovecs_to_align,
                        struct iovec **aligned_iovecs, size_t *aligned_iovec_count)
{
    size_t aligned_count = 0;
    int ret;

    ret = compute_aligned_iovecs_count(iovecs, iovecs_count, iovecs_to_align, &aligned_count);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }

    /* Allocate aligned_iovecs */
    for (int i = 0; i < iovecs_to_align; i++) {
        aligned_iovecs[i] = (struct iovec *) malloc(aligned_count * sizeof(struct iovec));
    }

    compute_aligned_iovecs(iovecs, iovecs_count, iovecs_to_align, aligned_iovecs, aligned_count);

    *aligned_iovec_count = aligned_count;

    return OMPI_SUCCESS;
}

static struct ompi_datatype_t *segmented_rget_get_base_datatype(struct ompi_datatype_t *origin_dt,
                                                                struct ompi_datatype_t *target_dt,
                                                                struct ompi_datatype_t *result_dt,
                                                                struct ompi_op_t *op)
{
    struct ompi_datatype_t *base_datatype;

    /* Get predefined datatype used to build target_dt */
    base_datatype = ompi_datatype_get_single_predefined_type_from_args(target_dt);
    if (NULL == base_datatype) {
        /* Null means more than one, not allowed */
        return NULL;
    }

    /* Ensure origin_dt and result_dt are made in the same wood as target_dt */
    if (MPI_NO_OP != op
        && base_datatype != ompi_datatype_get_single_predefined_type_from_args(origin_dt)) {
        return NULL;
    }
    if (NULL != result_dt
        && base_datatype != ompi_datatype_get_single_predefined_type_from_args(result_dt)) {
        return NULL;
    }

    return base_datatype;
}

static int segmented_rget_build_aligned_iovecs(
    const void *origin_addr, int origin_count, struct ompi_datatype_t *origin_dt, void *result_addr,
    int result_count, struct ompi_datatype_t *result_dt, int target_rank, ptrdiff_t target_disp,
    int target_count, struct ompi_datatype_t *target_dt, struct ompi_op_t *op, ompi_proc_t *proc,
    mca_osc_ubcl_module_t *module, struct iovec *aligned_iovec[3], size_t *aligned_iovec_count)
{
    int ret;
    int64_t disp_unit;
    struct iovec *base_iovec[3] = {NULL, NULL, NULL};
    size_t base_iovec_count[3] = {0, 0, 0};
    disp_unit = osc_ubcl_get_disp_unit(module, target_rank);

    if (MPI_NO_OP != op) {
        /* Build origin iovec based on origin addr/count/datatype */
        ret = osc_ubcl_build_ddt_iov(origin_addr, proc, origin_count, origin_dt, &base_iovec[0],
                                     &base_iovec_count[0]);
        if (OMPI_SUCCESS != ret) {
            goto error;
        }
    }

    /* Build target iovec with relative offsets in the target window */
    ret = osc_ubcl_build_ddt_iov((void *) (target_disp * disp_unit), proc, target_count, target_dt,
                                 &base_iovec[1], &base_iovec_count[1]);
    if (OMPI_SUCCESS != ret) {
        goto error;
    }

    if (NULL != result_dt) {
        /* Build result iovec based on result addr/count/datatype */
        ret = osc_ubcl_build_ddt_iov(result_addr, proc, result_count, result_dt, &base_iovec[2],
                                     &base_iovec_count[2]);
        if (OMPI_SUCCESS != ret) {
            goto error;
        }
        if (MPI_NO_OP == op) {
            /* No origin iovec to align */
            ret = align_iovecs(&base_iovec[1], &base_iovec_count[1], 2, &aligned_iovec[1],
                               aligned_iovec_count);
        } else {
            ret = align_iovecs(base_iovec, base_iovec_count, 3, aligned_iovec, aligned_iovec_count);
        }

        /* TODO: compute additionnal no_op segments if target buffer is larger than origin buffer */
    } else {
        if (MPI_NO_OP == op) {
            ret = OMPI_ERR_BAD_PARAM;
            goto error;
        }
        /* No result iovec to align */
        ret = align_iovecs(base_iovec, base_iovec_count, 2, aligned_iovec, aligned_iovec_count);
    }

    if (OMPI_SUCCESS != ret) {
        goto error;
    }

    ret = OMPI_SUCCESS;

error:
    free(base_iovec[0]);
    free(base_iovec[1]);
    free(base_iovec[2]);
    return ret;
}

static int segmented_rget_accumulate(const void *origin_addr, int origin_count,
                                     struct ompi_datatype_t *origin_dt, void *result_addr,
                                     int result_count, struct ompi_datatype_t *result_dt,
                                     int target_rank, ptrdiff_t target_disp, int target_count,
                                     struct ompi_datatype_t *target_dt, struct ompi_op_t *op,
                                     struct ompi_win_t *win, struct ompi_request_t **ompi_req)
{
    int ret;
    mca_osc_ubcl_module_t *module;
    ompi_proc_t *proc;
    mca_common_ubcl_endpoint_t *endpoint;
    struct iovec *aligned_iovec[3] = {NULL, NULL, NULL};
    size_t aligned_iovec_count;
    struct ompi_datatype_t *base_datatype;
    ubcl_win_op_t ubcl_op;
    mca_osc_ubcl_request_t *req;
    ubcl_completion_callback_fct cb;
    void *cb_data;
    size_t base_dt_size;

    module = (mca_osc_ubcl_module_t *) win->w_osc_module;

    /* Get base datatype to build operation */
    base_datatype = segmented_rget_get_base_datatype(origin_dt, target_dt, result_dt, op);
    if (NULL == base_datatype) {
        return OMPI_ERR_BAD_PARAM;
    }
    ret = ompi_datatype_type_size(base_datatype, &base_dt_size);
    if (OMPI_SUCCESS != ret) {
        goto error;
    }
    ret = build_ubcl_op(base_datatype, op, &ubcl_op);
    if (OMPI_SUCCESS != ret) {
        goto error;
    }

    /* Get proc */
    proc = ompi_group_peer_lookup(win->w_group, target_rank);
    if (OPAL_UNLIKELY(NULL == proc)) {
        return OMPI_ERR_BAD_PARAM;
    }

    endpoint = (mca_common_ubcl_endpoint_t *) proc->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PML];

    /* Compute accumulate segmentation into contiguous parts */
    ret = segmented_rget_build_aligned_iovecs(origin_addr, origin_count, origin_dt, result_addr,
                                              result_count, result_dt, target_rank, target_disp,
                                              target_count, target_dt, op, proc, module,
                                              aligned_iovec, &aligned_iovec_count);
    if (OMPI_SUCCESS != ret) {
        goto error;
    }

    /* Build the request if needed */
    if (NULL == ompi_req) {
        req = NULL;
        cb = NULL;
        cb_data = NULL;
    } else {
        req = (mca_osc_ubcl_request_t *) opal_free_list_get(&mca_osc_ubcl_component.req_free_list);
        if (OPAL_UNLIKELY(NULL == req)) {
            ret = OMPI_ERR_OUT_OF_RESOURCE;
            goto error;
        }

        MCA_OSC_UBCL_REQUEST_INIT(req, target_rank, (struct ompi_datatype_t *) NULL,
                                  (struct ompi_datatype_t *) NULL, win, true);

        *ompi_req = &req->ompi_req;
        cb = ubcl_request_complete_cb;
        cb_data = req;
        req->segment_count = aligned_iovec_count;
    }

    for (size_t i = 0; i < aligned_iovec_count; i++) {
        void *sbuf;
        void *fetch_buf;
        ptrdiff_t offset;
        size_t count;
        ubcl_error_t err;

        /* Check if there is data to send */
        if (MPI_NO_OP == op) {
            sbuf = NULL;
        } else {
            sbuf = aligned_iovec[0][i].iov_base;
            assert(aligned_iovec[0][i].iov_len == aligned_iovec[1][i].iov_len);
        }

        /* Target buffer offsetn in bytes, relative to the window base */
        offset = (ptrdiff_t) aligned_iovec[1][i].iov_base;

        /* Check if there is data to fetch */
        if (NULL != result_dt) {
            fetch_buf = aligned_iovec[2][i].iov_base;
            assert(aligned_iovec[1][i].iov_len == aligned_iovec[2][i].iov_len);
        } else {
            fetch_buf = NULL;
        }

        /* Count in terms of base datatypes in this segment */
        count = aligned_iovec[1][i].iov_len / base_dt_size;

        /* Submit contiguous operation to ubcl */
        err = ubcl_accumulate(sbuf, fetch_buf, count, endpoint->rank, offset, &ubcl_op, module->wid,
                              cb, cb_data);
        ret = ubcl_error_to_ompi(err);
        if (OMPI_SUCCESS != ret) {
            if (0 == i && NULL != req) {
                /* This is the first segment, we can have a clean fail */
                opal_free_list_return(&mca_osc_ubcl_component.req_free_list, &req->super);
            } else {
                /* Some segments have already been sent, we are in a really bad satuation */
                mca_osc_ubcl_error(ret,
                                   "Fail to send fragment %zu in an accumulate "
                                   "operation segmented in %zu parts. "
                                   "This error is not recoverable\n",
                                   i, aligned_iovec_count);
            }
            goto error;
        }
    }

    ret = OMPI_SUCCESS;

error:
    free(aligned_iovec[0]);
    free(aligned_iovec[1]);
    free(aligned_iovec[2]);
    return ret;
}

int ompi_osc_ubcl_rget_accumulate(const void *origin_addr, int origin_count,
                                  struct ompi_datatype_t *origin_dt, void *result_addr,
                                  int result_count, struct ompi_datatype_t *result_dt,
                                  int target_rank, ptrdiff_t target_disp, int target_count,
                                  struct ompi_datatype_t *target_dt, struct ompi_op_t *op,
                                  struct ompi_win_t *win, struct ompi_request_t **ompi_req)
{
    ubcl_error_t err;
    int ret;
    int64_t disp_unit;
    mca_common_ubcl_endpoint_t *endpoint;
    mca_osc_ubcl_module_t *module;
    ompi_proc_t *proc;
    ubcl_win_op_t ubcl_op;
    ptrdiff_t remote_offset;
    ubcl_completion_callback_fct cb;
    void *cb_data;
    struct ompi_datatype_t *dt;
    size_t count;
    ptrdiff_t origin_size;
    ptrdiff_t target_size;
    ptrdiff_t gap;

    module = (mca_osc_ubcl_module_t *) win->w_osc_module;
    disp_unit = osc_ubcl_get_disp_unit(module, target_rank);

    if (MPI_NO_OP != op) {
        origin_size = opal_datatype_span((const opal_datatype_t *) origin_dt, origin_count, &gap);
    } else {
        origin_size = 0;
    }
    target_size = opal_datatype_span((const opal_datatype_t *) target_dt, target_count, &gap);
    (void) gap;

    if (0 == target_size || (NULL == result_dt && 0 == origin_size)) {
        if (NULL != ompi_req) {
            *ompi_req = &ompi_request_empty;
        }
        return OMPI_SUCCESS;
    }

    if ((MPI_NO_OP != op && !ompi_datatype_is_predefined(origin_dt))
        || !ompi_datatype_is_predefined(target_dt)
        || (NULL != result_dt && !ompi_datatype_is_predefined(result_dt))) {
        /* Let's take the hard way */
        return segmented_rget_accumulate(origin_addr, origin_count, origin_dt, result_addr,
                                         result_count, result_dt, target_rank, target_disp,
                                         target_count, target_dt, op, win, ompi_req);
    }

    /* Get proc */
    proc = ompi_group_peer_lookup(win->w_group, target_rank);
    if (OPAL_UNLIKELY(NULL == proc)) {
        return OMPI_ERR_BAD_PARAM;
    }

    endpoint = (mca_common_ubcl_endpoint_t *) proc->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PML];
    if (OMPI_SUCCESS != ompi_osc_ubcl_check_access_epoch(target_rank, win)) {
        return OMPI_ERR_RMA_CONFLICT;
    }

    if (MPI_NO_OP == op) {
        dt = target_dt;
        count = target_count;
        origin_addr = NULL;
    } else {
        dt = origin_dt;
        count = origin_count;
    }

    ret = build_ubcl_op(dt, op, &ubcl_op);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }

    if (NULL == ompi_req) {
        cb = NULL;
        cb_data = NULL;
    } else {
        mca_osc_ubcl_request_t *req;
        req = (mca_osc_ubcl_request_t *) opal_free_list_get(&mca_osc_ubcl_component.req_free_list);
        if (OPAL_UNLIKELY(NULL == req)) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        MCA_OSC_UBCL_REQUEST_INIT(req, target_rank, (struct ompi_datatype_t *) NULL,
                                  (struct ompi_datatype_t *) NULL, win, true);

        *ompi_req = &req->ompi_req;
        cb = ubcl_request_complete_cb;
        cb_data = req;
    }

    remote_offset = target_disp * disp_unit;

    /* TODO: handle non contiguous datatypes as MPI seems to allow some of it */
    err = ubcl_accumulate((void *) origin_addr, result_addr, count, endpoint->rank,
                          remote_offset, &ubcl_op, module->wid, cb, cb_data);

    ret = ubcl_error_to_ompi(err);

    if (OMPI_SUCCESS != ret && NULL != cb_data) {
        opal_free_list_return(&mca_osc_ubcl_component.req_free_list, cb_data);
    }

    return ret;
}

int ompi_osc_ubcl_fetch_and_op(const void *origin_addr, void *result_addr,
                               struct ompi_datatype_t *dt, int target, ptrdiff_t target_disp,
                               struct ompi_op_t *op, struct ompi_win_t *win)
{
    if (! ompi_datatype_is_predefined(dt)) {
        return OMPI_ERR_BAD_PARAM;
    }
    return ompi_osc_ubcl_get_accumulate(origin_addr, 1, dt, result_addr, 1, dt, target, target_disp,
                                        1, dt, op, win);
}

int ompi_osc_ubcl_compare_and_swap(const void *origin_addr, const void *compare_addr,
                                   void *result_addr, struct ompi_datatype_t *dt, int target,
                                   ptrdiff_t target_disp, struct ompi_win_t *win)
{
    ubcl_win_atomic_datatype_t data_type = UBCL_TYPE_NONE;
    int64_t disp_unit;
    mca_osc_ubcl_module_t *module;
    ompi_proc_t *proc;
    mca_common_ubcl_endpoint_t *endpoint;
    ubcl_error_t err;

    module = (mca_osc_ubcl_module_t *) win->w_osc_module;
    disp_unit = osc_ubcl_get_disp_unit(module, target);

    GET_TYPE(get_c_integer_ubcl_type, dt, &data_type);
    GET_TYPE(get_fortran_integer_ubcl_type, dt, &data_type);
    GET_TYPE(get_logical_ubcl_type, dt, &data_type);
    GET_TYPE(get_byte_ubcl_type, dt, &data_type);
    GET_TYPE(get_multi_language_ubcl_type, dt, &data_type);

    return OMPI_ERR_BAD_PARAM;

got_type:
    /* Get proc */
    proc = ompi_group_peer_lookup(win->w_group, target);
    if (OPAL_UNLIKELY(NULL == proc)) {
        return OMPI_ERR_BAD_PARAM;
    }

    endpoint = (mca_common_ubcl_endpoint_t *) proc->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PML];
    if (OMPI_SUCCESS != ompi_osc_ubcl_check_access_epoch(target, win)) {
        return OMPI_ERR_RMA_CONFLICT;
    }

    err = ubcl_cas(origin_addr, compare_addr, result_addr, data_type, endpoint->rank,
                   target_disp * disp_unit, module->wid, NULL, NULL);

    return ubcl_error_to_ompi(err);

not_implemented:
    return OMPI_ERR_NOT_IMPLEMENTED;
}
