/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include "opal/dss/dss_internal.h"

static void opal_dss_arith_int(int *value, int *operand, opal_dss_arith_op_t operation);
static void opal_dss_arith_uint(uint *value, uint *operand, opal_dss_arith_op_t operation);
static void opal_dss_arith_size(size_t *value, size_t *operand, opal_dss_arith_op_t operation);
static void opal_dss_arith_pid(pid_t *value, pid_t *operand, opal_dss_arith_op_t operation);
static void opal_dss_arith_byte(uint8_t *value, uint8_t *operand, opal_dss_arith_op_t operation);
static void opal_dss_arith_int8(int8_t *value, int8_t *operand, opal_dss_arith_op_t operation);
static void opal_dss_arith_int16(int16_t *value, int16_t *operand, opal_dss_arith_op_t operation);
static void opal_dss_arith_uint16(uint16_t *value, uint16_t *operand, opal_dss_arith_op_t operation);
static void opal_dss_arith_int32(int32_t *value, int32_t *operand, opal_dss_arith_op_t operation);
static void opal_dss_arith_uint32(uint32_t *value, uint32_t *operand, opal_dss_arith_op_t operation);
static void opal_dss_arith_int64(int64_t *value, int64_t *operand, opal_dss_arith_op_t operation);
static void opal_dss_arith_uint64(uint64_t *value, uint64_t *operand, opal_dss_arith_op_t operation);

/* some weird ones - but somebody *might* want to do it, I suppose... */
static void opal_dss_arith_data_type(opal_data_type_t *value, opal_data_type_t *operand, opal_dss_arith_op_t operation);

int opal_dss_arith(opal_dss_value_t *value, opal_dss_value_t *operand, opal_dss_arith_op_t operation)
{
    /* check for error */
    if (NULL == value || NULL == operand) {
        return OPAL_ERR_BAD_PARAM;
    }
    if (operand->type != value->type) {
        return OPAL_ERR_TYPE_MISMATCH;
    }

    /* Lookup the arith function for this type and call it */

    switch(operand->type) {
        case OPAL_INT:
            opal_dss_arith_int((int*)value->data, (int*)operand->data, operation);
            break;

        case OPAL_UINT:
            opal_dss_arith_uint((uint*)value->data, (uint*)operand->data, operation);
            break;

        case OPAL_SIZE:
            opal_dss_arith_size((size_t*)value->data, (size_t*)operand->data, operation);
            break;

        case OPAL_PID:
            opal_dss_arith_pid((pid_t*)value->data, (pid_t*)operand->data, operation);
            break;

        case OPAL_BYTE:
        case OPAL_UINT8:
            opal_dss_arith_byte((uint8_t*)value->data, (uint8_t*)operand->data, operation);
            break;

        case OPAL_INT8:
            opal_dss_arith_int8((int8_t*)value->data, (int8_t*)operand->data, operation);
            break;

        case OPAL_INT16:
            opal_dss_arith_int16((int16_t*)value->data, (int16_t*)operand->data, operation);
            break;

        case OPAL_UINT16:
            opal_dss_arith_uint16((uint16_t*)value->data, (uint16_t*)operand->data, operation);
            break;

        case OPAL_INT32:
            opal_dss_arith_int32((int32_t*)value->data, (int32_t*)operand->data, operation);
            break;

        case OPAL_UINT32:
            opal_dss_arith_uint32((uint32_t*)value->data, (uint32_t*)operand->data, operation);
            break;

        case OPAL_INT64:
            opal_dss_arith_int64((int64_t*)value->data, (int64_t*)operand->data, operation);
            break;

        case OPAL_UINT64:
            opal_dss_arith_uint64((uint64_t*)value->data, (uint64_t*)operand->data, operation);
            break;

        default:
            return OPAL_ERR_OPERATION_UNSUPPORTED;
    }

    return OPAL_SUCCESS;
}

int opal_dss_increment(opal_dss_value_t *value)
{
    int one;
    unsigned int uone;
    size_t sone;
    pid_t pone;
    uint8_t u8one;
    int8_t i8one;
    uint16_t u16one;
    int16_t i16one;
    uint32_t u32one;
    int32_t i32one;
    uint64_t u64one;
    int64_t i64one;
    opal_data_type_t datatypeone;

    /* check for error */
    if (NULL == value) {
        return OPAL_ERR_BAD_PARAM;
    }
    /* Lookup the arith function for this type and call it */

    switch(value->type) {
        case OPAL_INT:
            one = 1;
            opal_dss_arith_int((int*)value->data, &one, OPAL_DSS_ADD);
            break;

        case OPAL_UINT:
            uone = 1;
            opal_dss_arith_uint((uint*)value->data, &uone, OPAL_DSS_ADD);
            break;

        case OPAL_SIZE:
            sone = 1;
            opal_dss_arith_size((size_t*)value->data, &sone, OPAL_DSS_ADD);
            break;

        case OPAL_PID:
            pone = 1;
            opal_dss_arith_pid((pid_t*)value->data, &pone, OPAL_DSS_ADD);
            break;

        case OPAL_BYTE:
        case OPAL_UINT8:
            u8one = 1;
            opal_dss_arith_byte((uint8_t*)value->data, &u8one, OPAL_DSS_ADD);
            break;

        case OPAL_INT8:
            i8one = 1;
            opal_dss_arith_int8((int8_t*)value->data, &i8one, OPAL_DSS_ADD);
            break;

        case OPAL_INT16:
            i16one = 1;
            opal_dss_arith_int16((int16_t*)value->data, &i16one, OPAL_DSS_ADD);
            break;

        case OPAL_UINT16:
            u16one = 1;
            opal_dss_arith_uint16((uint16_t*)value->data, &u16one, OPAL_DSS_ADD);
            break;

        case OPAL_INT32:
            i32one = 1;
            opal_dss_arith_int32((int32_t*)value->data, &i32one, OPAL_DSS_ADD);
            break;

        case OPAL_UINT32:
            u32one = 1;
            opal_dss_arith_uint32((uint32_t*)value->data, &u32one, OPAL_DSS_ADD);
            break;

        case OPAL_INT64:
            i64one = 1;
            opal_dss_arith_int64((int64_t*)value->data, &i64one, OPAL_DSS_ADD);
            break;

        case OPAL_UINT64:
            u64one = 1;
            opal_dss_arith_uint64((uint64_t*)value->data, &u64one, OPAL_DSS_ADD);
            break;

        case OPAL_DATA_TYPE:
            datatypeone = 1;
            opal_dss_arith_data_type((opal_data_type_t*)value->data, &datatypeone, OPAL_DSS_ADD);
            break;

        default:
            return OPAL_ERR_OPERATION_UNSUPPORTED;
    }

    return OPAL_SUCCESS;
}

int opal_dss_decrement(opal_dss_value_t *value)
{
    int one;
    unsigned int uone;
    size_t sone;
    pid_t pone;
    uint8_t u8one;
    int8_t i8one;
    uint16_t u16one;
    int16_t i16one;
    uint32_t u32one;
    int32_t i32one;
    uint64_t u64one;
    int64_t i64one;
    opal_data_type_t datatypeone;

    /* check for error */
    if (NULL == value) {
        return OPAL_ERR_BAD_PARAM;
    }
    /* Lookup the arith function for this type and call it */

    switch(value->type) {
        case OPAL_INT:
            one = 1;
            opal_dss_arith_int((int*)value->data, &one, OPAL_DSS_SUB);
            break;

        case OPAL_UINT:
            uone = 1;
            opal_dss_arith_uint((uint*)value->data, &uone, OPAL_DSS_SUB);
            break;

        case OPAL_SIZE:
            sone = 1;
            opal_dss_arith_size((size_t*)value->data, &sone, OPAL_DSS_SUB);
            break;

        case OPAL_PID:
            pone = 1;
            opal_dss_arith_pid((pid_t*)value->data, &pone, OPAL_DSS_SUB);
            break;

        case OPAL_BYTE:
        case OPAL_UINT8:
            u8one = 1;
            opal_dss_arith_byte((uint8_t*)value->data, &u8one, OPAL_DSS_SUB);
            break;

        case OPAL_INT8:
            i8one = 1;
            opal_dss_arith_int8((int8_t*)value->data, &i8one, OPAL_DSS_SUB);
            break;

        case OPAL_INT16:
            i16one = 1;
            opal_dss_arith_int16((int16_t*)value->data, &i16one, OPAL_DSS_SUB);
            break;

        case OPAL_UINT16:
            u16one = 1;
            opal_dss_arith_uint16((uint16_t*)value->data, &u16one, OPAL_DSS_SUB);
            break;

        case OPAL_INT32:
            i32one = 1;
            opal_dss_arith_int32((int32_t*)value->data, &i32one, OPAL_DSS_SUB);
            break;

        case OPAL_UINT32:
            u32one = 1;
            opal_dss_arith_uint32((uint32_t*)value->data, &u32one, OPAL_DSS_SUB);
            break;

        case OPAL_INT64:
            i64one = 1;
            opal_dss_arith_int64((int64_t*)value->data, &i64one, OPAL_DSS_SUB);
            break;

        case OPAL_UINT64:
            u64one = 1;
            opal_dss_arith_uint64((uint64_t*)value->data, &u64one, OPAL_DSS_SUB);
            break;

        case OPAL_DATA_TYPE:
            datatypeone = 1;
            opal_dss_arith_data_type((opal_data_type_t*)value->data, &datatypeone, OPAL_DSS_SUB);
            break;

        default:
            return OPAL_ERR_OPERATION_UNSUPPORTED;
    }

    return OPAL_SUCCESS;
}

/*
 * NUMERIC arith FUNCTIONS
 */
static void opal_dss_arith_int(int *value, int *operand, opal_dss_arith_op_t operation)
{
    switch(operation) {
        case OPAL_DSS_ADD:
            (*value) += *operand;
            break;

        case OPAL_DSS_SUB:
            (*value) -= *operand;
            break;

        case OPAL_DSS_MUL:
            (*value) *= *operand;
            break;

        case OPAL_DSS_DIV:
            if (0 == *operand) {
                return;
            }
            (*value) /= *operand;
            break;

        default:
            break;
    }
    return;
}

static void opal_dss_arith_uint(uint *value, uint *operand, opal_dss_arith_op_t operation)
{
    switch(operation) {
        case OPAL_DSS_ADD:
            (*value) += *operand;
            break;

        case OPAL_DSS_SUB:
            (*value) -= *operand;
            break;

        case OPAL_DSS_MUL:
            (*value) *= *operand;
            break;

        case OPAL_DSS_DIV:
            if (0 == *operand) {
                return;
            }
            (*value) /= *operand;
            break;

        default:
            break;
    }
    return;
}

static void opal_dss_arith_size(size_t *value, size_t *operand, opal_dss_arith_op_t operation)
{
    switch(operation) {
        case OPAL_DSS_ADD:
            (*value) += *operand;
            break;

        case OPAL_DSS_SUB:
            (*value) -= *operand;
            break;

        case OPAL_DSS_MUL:
            (*value) *= *operand;
            break;

        case OPAL_DSS_DIV:
            if (0 == *operand) {
                return;
            }
            (*value) /= *operand;
            break;

        default:
            break;
    }
    return;
}

static void opal_dss_arith_pid(pid_t *value, pid_t *operand, opal_dss_arith_op_t operation)
{
    switch(operation) {
        case OPAL_DSS_ADD:
            (*value) += *operand;
            break;

        case OPAL_DSS_SUB:
            (*value) -= *operand;
            break;

        case OPAL_DSS_MUL:
            (*value) *= *operand;
            break;

        case OPAL_DSS_DIV:
            if (0 == *operand) {
                return;
            }
            (*value) /= *operand;
            break;

        default:
            break;
    }
    return;
}

static void opal_dss_arith_byte(uint8_t *value, uint8_t *operand, opal_dss_arith_op_t operation)
{
    switch(operation) {
        case OPAL_DSS_ADD:
            (*value) += *operand;
            break;

        case OPAL_DSS_SUB:
            (*value) -= *operand;
            break;

        case OPAL_DSS_MUL:
            (*value) *= *operand;
            break;

        case OPAL_DSS_DIV:
            if (0 == *operand) {
                return;
            }
            (*value) /= *operand;
            break;

        default:
            break;
    }
    return;
}

static void opal_dss_arith_int8(int8_t *value, int8_t *operand, opal_dss_arith_op_t operation)
{
    switch(operation) {
        case OPAL_DSS_ADD:
            (*value) += *operand;
            break;

        case OPAL_DSS_SUB:
            (*value) -= *operand;
            break;

        case OPAL_DSS_MUL:
            (*value) *= *operand;
            break;

        case OPAL_DSS_DIV:
            if (0 == *operand) {
                return;
            }
            (*value) /= *operand;
            break;

        default:
            break;
    }
    return;
}

static void opal_dss_arith_int16(int16_t *value, int16_t *operand, opal_dss_arith_op_t operation)
{
    switch(operation) {
        case OPAL_DSS_ADD:
            (*value) += *operand;
            break;

        case OPAL_DSS_SUB:
            (*value) -= *operand;
            break;

        case OPAL_DSS_MUL:
            (*value) *= *operand;
            break;

        case OPAL_DSS_DIV:
            if (0 == *operand) {
                return;
            }
            (*value) /= *operand;
            break;

        default:
            break;
    }
    return;
}

static void opal_dss_arith_uint16(uint16_t *value, uint16_t *operand, opal_dss_arith_op_t operation)
{
    switch(operation) {
        case OPAL_DSS_ADD:
            (*value) += *operand;
            break;

        case OPAL_DSS_SUB:
            (*value) -= *operand;
            break;

        case OPAL_DSS_MUL:
            (*value) *= *operand;
            break;

        case OPAL_DSS_DIV:
            if (0 == *operand) {
                return;
            }
            (*value) /= *operand;
            break;

        default:
            break;
    }
    return;
}

static void opal_dss_arith_int32(int32_t *value, int32_t *operand, opal_dss_arith_op_t operation)
{
    switch(operation) {
        case OPAL_DSS_ADD:
            (*value) += *operand;
            break;

        case OPAL_DSS_SUB:
            (*value) -= *operand;
            break;

        case OPAL_DSS_MUL:
            (*value) *= *operand;
            break;

        case OPAL_DSS_DIV:
            if (0 == *operand) {
                return;
            }
            (*value) /= *operand;
            break;

        default:
            break;
    }
    return;
}

static void opal_dss_arith_uint32(uint32_t *value, uint32_t *operand, opal_dss_arith_op_t operation)
{
    switch(operation) {
        case OPAL_DSS_ADD:
            (*value) += *operand;
            break;

        case OPAL_DSS_SUB:
            (*value) -= *operand;
            break;

        case OPAL_DSS_MUL:
            (*value) *= *operand;
            break;

        case OPAL_DSS_DIV:
            if (0 == *operand) {
                return;
            }
            (*value) /= *operand;
            break;

        default:
            break;
    }
    return;
}

static void opal_dss_arith_int64(int64_t *value, int64_t *operand, opal_dss_arith_op_t operation)
{
    switch(operation) {
        case OPAL_DSS_ADD:
            (*value) += *operand;
            break;

        case OPAL_DSS_SUB:
            (*value) -= *operand;
            break;

        case OPAL_DSS_MUL:
            (*value) *= *operand;
            break;

        case OPAL_DSS_DIV:
            if (0 == *operand) {
                return;
            }
            (*value) /= *operand;
            break;

        default:
            break;
    }
    return;
}

static void opal_dss_arith_uint64(uint64_t *value, uint64_t *operand, opal_dss_arith_op_t operation)
{
    switch(operation) {
        case OPAL_DSS_ADD:
            (*value) += *operand;
            break;

        case OPAL_DSS_SUB:
            (*value) -= *operand;
            break;

        case OPAL_DSS_MUL:
            (*value) *= *operand;
            break;

        case OPAL_DSS_DIV:
            if (0 == *operand) {
                return;
            }
            (*value) /= *operand;
            break;

        default:
            break;
    }
    return;
}

static void opal_dss_arith_data_type(opal_data_type_t *value, opal_data_type_t *operand, opal_dss_arith_op_t operation)
{
    switch(operation) {
        case OPAL_DSS_ADD:
            (*value) += *operand;
            break;

        case OPAL_DSS_SUB:
            (*value) -= *operand;
            break;

        case OPAL_DSS_MUL:
            (*value) *= *operand;
            break;

        case OPAL_DSS_DIV:
            if (0 == *operand) {
                return;
            }
            (*value) /= *operand;
            break;

        default:
            break;
    }
    return;
}

