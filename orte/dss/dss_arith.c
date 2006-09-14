/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
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

#include "orte_config.h"

#include <sys/types.h>
#if HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif
#include "opal/util/output.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/odls/odls_types.h"

#include "orte/dss/dss_internal.h"

static void orte_dss_arith_int(int *value, int *operand, orte_dss_arith_op_t operation);
static void orte_dss_arith_uint(uint *value, uint *operand, orte_dss_arith_op_t operation);
static void orte_dss_arith_size(size_t *value, size_t *operand, orte_dss_arith_op_t operation);
static void orte_dss_arith_pid(pid_t *value, pid_t *operand, orte_dss_arith_op_t operation);
static void orte_dss_arith_byte(uint8_t *value, uint8_t *operand, orte_dss_arith_op_t operation);
static void orte_dss_arith_int8(int8_t *value, int8_t *operand, orte_dss_arith_op_t operation);
static void orte_dss_arith_int16(int16_t *value, int16_t *operand, orte_dss_arith_op_t operation);
static void orte_dss_arith_uint16(uint16_t *value, uint16_t *operand, orte_dss_arith_op_t operation);
static void orte_dss_arith_int32(int32_t *value, int32_t *operand, orte_dss_arith_op_t operation);
static void orte_dss_arith_uint32(uint32_t *value, uint32_t *operand, orte_dss_arith_op_t operation);
static void orte_dss_arith_int64(int64_t *value, int64_t *operand, orte_dss_arith_op_t operation);
static void orte_dss_arith_uint64(uint64_t *value, uint64_t *operand, orte_dss_arith_op_t operation);

static void orte_dss_arith_std_cntr(orte_std_cntr_t *value, orte_std_cntr_t *operand, orte_dss_arith_op_t operation);

/* some weird ones - but somebody *might* want to do it, I suppose... */
static void orte_dss_arith_data_type(orte_data_type_t *value, orte_data_type_t *operand, orte_dss_arith_op_t operation);
static void orte_dss_arith_daemon_cmd(orte_daemon_cmd_flag_t *value, orte_daemon_cmd_flag_t *operand, orte_dss_arith_op_t operation);

int orte_dss_arith(orte_data_value_t *value, orte_data_value_t *operand, orte_dss_arith_op_t operation)
{
    /* check for error */
    if (NULL == value || NULL == operand) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }
    if (operand->type != value->type) {
        ORTE_ERROR_LOG(ORTE_ERR_TYPE_MISMATCH);
        return ORTE_ERR_TYPE_MISMATCH;
    }

    /* Lookup the arith function for this type and call it */

    switch(operand->type) {
        case ORTE_INT:
            orte_dss_arith_int((int*)value->data, (int*)operand->data, operation);
            break;

        case ORTE_UINT:
            orte_dss_arith_uint((uint*)value->data, (uint*)operand->data, operation);
            break;

        case ORTE_SIZE:
            orte_dss_arith_size((size_t*)value->data, (size_t*)operand->data, operation);
            break;

        case ORTE_PID:
            orte_dss_arith_pid((pid_t*)value->data, (pid_t*)operand->data, operation);
            break;

        case ORTE_BYTE:
        case ORTE_UINT8:
            orte_dss_arith_byte((uint8_t*)value->data, (uint8_t*)operand->data, operation);
            break;

        case ORTE_INT8:
            orte_dss_arith_int8((int8_t*)value->data, (int8_t*)operand->data, operation);
            break;

        case ORTE_INT16:
            orte_dss_arith_int16((int16_t*)value->data, (int16_t*)operand->data, operation);
            break;

        case ORTE_UINT16:
            orte_dss_arith_uint16((uint16_t*)value->data, (uint16_t*)operand->data, operation);
            break;

        case ORTE_INT32:
            orte_dss_arith_int32((int32_t*)value->data, (int32_t*)operand->data, operation);
            break;

        case ORTE_UINT32:
            orte_dss_arith_uint32((uint32_t*)value->data, (uint32_t*)operand->data, operation);
            break;

        case ORTE_INT64:
            orte_dss_arith_int64((int64_t*)value->data, (int64_t*)operand->data, operation);
            break;

        case ORTE_UINT64:
            orte_dss_arith_uint64((uint64_t*)value->data, (uint64_t*)operand->data, operation);
            break;

        case ORTE_STD_CNTR:
            orte_dss_arith_std_cntr((orte_std_cntr_t*)value->data, (orte_std_cntr_t*)operand->data, operation);
            break;
            
        default:
            ORTE_ERROR_LOG(ORTE_ERR_OPERATION_UNSUPPORTED);
            return ORTE_ERR_OPERATION_UNSUPPORTED;
    }

    return ORTE_SUCCESS;
}

int orte_dss_increment(orte_data_value_t *value)
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
    orte_daemon_cmd_flag_t daemoncmdone;
    orte_data_type_t datatypeone;
    orte_std_cntr_t stdcntrone;

    /* check for error */
    if (NULL == value) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }
    /* Lookup the arith function for this type and call it */

    switch(value->type) {
        case ORTE_INT:
            one = 1;
            orte_dss_arith_int((int*)value->data, &one, ORTE_DSS_ADD);
            break;

        case ORTE_UINT:
            uone = 1;
            orte_dss_arith_uint((uint*)value->data, &uone, ORTE_DSS_ADD);
            break;

        case ORTE_SIZE:
            sone = 1;
            orte_dss_arith_size((size_t*)value->data, &sone, ORTE_DSS_ADD);
            break;

        case ORTE_PID:
            pone = 1;
            orte_dss_arith_pid((pid_t*)value->data, &pone, ORTE_DSS_ADD);
            break;

        case ORTE_BYTE:
        case ORTE_UINT8:
            u8one = 1;
            orte_dss_arith_byte((uint8_t*)value->data, &u8one, ORTE_DSS_ADD);
            break;

        case ORTE_INT8:
            i8one = 1;
            orte_dss_arith_int8((int8_t*)value->data, &i8one, ORTE_DSS_ADD);
            break;

        case ORTE_INT16:
            i16one = 1;
            orte_dss_arith_int16((int16_t*)value->data, &i16one, ORTE_DSS_ADD);
            break;

        case ORTE_UINT16:
            u16one = 1;
            orte_dss_arith_uint16((uint16_t*)value->data, &u16one, ORTE_DSS_ADD);
            break;

        case ORTE_INT32:
            i32one = 1;
            orte_dss_arith_int32((int32_t*)value->data, &i32one, ORTE_DSS_ADD);
            break;

        case ORTE_UINT32:
            u32one = 1;
            orte_dss_arith_uint32((uint32_t*)value->data, &u32one, ORTE_DSS_ADD);
            break;

        case ORTE_INT64:
            i64one = 1;
            orte_dss_arith_int64((int64_t*)value->data, &i64one, ORTE_DSS_ADD);
            break;

        case ORTE_UINT64:
            u64one = 1;
            orte_dss_arith_uint64((uint64_t*)value->data, &u64one, ORTE_DSS_ADD);
            break;

        case ORTE_DAEMON_CMD:
            daemoncmdone = 1;
            orte_dss_arith_daemon_cmd((orte_daemon_cmd_flag_t*)value->data, &daemoncmdone, ORTE_DSS_ADD);
            break;

        case ORTE_DATA_TYPE:
            datatypeone = 1;
            orte_dss_arith_data_type((orte_data_type_t*)value->data, &datatypeone, ORTE_DSS_ADD);
            break;

        case ORTE_STD_CNTR:
            stdcntrone = 1;
            orte_dss_arith_std_cntr((orte_std_cntr_t*)value->data, &stdcntrone, ORTE_DSS_ADD);
            break;
            
        default:
            ORTE_ERROR_LOG(ORTE_ERR_OPERATION_UNSUPPORTED);
            return ORTE_ERR_OPERATION_UNSUPPORTED;
    }

    return ORTE_SUCCESS;
}

int orte_dss_decrement(orte_data_value_t *value)
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
    orte_daemon_cmd_flag_t daemoncmdone;
    orte_data_type_t datatypeone;
    orte_std_cntr_t stdcntrone;

    /* check for error */
    if (NULL == value) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }
    /* Lookup the arith function for this type and call it */

    switch(value->type) {
        case ORTE_INT:
            one = 1;
            orte_dss_arith_int((int*)value->data, &one, ORTE_DSS_SUB);
            break;

        case ORTE_UINT:
            uone = 1;
            orte_dss_arith_uint((uint*)value->data, &uone, ORTE_DSS_SUB);
            break;

        case ORTE_SIZE:
            sone = 1;
            orte_dss_arith_size((size_t*)value->data, &sone, ORTE_DSS_SUB);
            break;

        case ORTE_PID:
            pone = 1;
            orte_dss_arith_pid((pid_t*)value->data, &pone, ORTE_DSS_SUB);
            break;

        case ORTE_BYTE:
        case ORTE_UINT8:
            u8one = 1;
            orte_dss_arith_byte((uint8_t*)value->data, &u8one, ORTE_DSS_SUB);
            break;

        case ORTE_INT8:
            i8one = 1;
            orte_dss_arith_int8((int8_t*)value->data, &i8one, ORTE_DSS_SUB);
            break;

        case ORTE_INT16:
            i16one = 1;
            orte_dss_arith_int16((int16_t*)value->data, &i16one, ORTE_DSS_SUB);
            break;

        case ORTE_UINT16:
            u16one = 1;
            orte_dss_arith_uint16((uint16_t*)value->data, &u16one, ORTE_DSS_SUB);
            break;

        case ORTE_INT32:
            i32one = 1;
            orte_dss_arith_int32((int32_t*)value->data, &i32one, ORTE_DSS_SUB);
            break;

        case ORTE_UINT32:
            u32one = 1;
            orte_dss_arith_uint32((uint32_t*)value->data, &u32one, ORTE_DSS_SUB);
            break;

        case ORTE_INT64:
            i64one = 1;
            orte_dss_arith_int64((int64_t*)value->data, &i64one, ORTE_DSS_SUB);
            break;

        case ORTE_UINT64:
            u64one = 1;
            orte_dss_arith_uint64((uint64_t*)value->data, &u64one, ORTE_DSS_SUB);
            break;

        case ORTE_DAEMON_CMD:
            daemoncmdone = 1;
            orte_dss_arith_daemon_cmd((orte_daemon_cmd_flag_t*)value->data, &daemoncmdone, ORTE_DSS_SUB);
            break;

        case ORTE_DATA_TYPE:
            datatypeone = 1;
            orte_dss_arith_data_type((orte_data_type_t*)value->data, &datatypeone, ORTE_DSS_SUB);
            break;

        case ORTE_STD_CNTR:
            stdcntrone = 1;
            orte_dss_arith_std_cntr((orte_std_cntr_t*)value->data, &stdcntrone, ORTE_DSS_SUB);
            break;
            
        default:
            ORTE_ERROR_LOG(ORTE_ERR_OPERATION_UNSUPPORTED);
            return ORTE_ERR_OPERATION_UNSUPPORTED;
    }

    return ORTE_SUCCESS;
}

/*
 * NUMERIC arith FUNCTIONS
 */
static void orte_dss_arith_int(int *value, int *operand, orte_dss_arith_op_t operation)
{
    switch(operation) {
        case ORTE_DSS_ADD:
            (*value) += *operand;
            break;

        case ORTE_DSS_SUB:
            (*value) -= *operand;
            break;

        case ORTE_DSS_MUL:
            (*value) *= *operand;
            break;

        case ORTE_DSS_DIV:
            if (0 == *operand) {
                ORTE_ERROR_LOG(ORTE_ERR_OPERATION_UNSUPPORTED);
                return;
            }
            (*value) /= *operand;
            break;

        default:
            ORTE_ERROR_LOG(ORTE_ERR_OPERATION_UNSUPPORTED);
            break;
    }
    return;
}

static void orte_dss_arith_uint(uint *value, uint *operand, orte_dss_arith_op_t operation)
{
    switch(operation) {
        case ORTE_DSS_ADD:
            (*value) += *operand;
            break;

        case ORTE_DSS_SUB:
            (*value) -= *operand;
            break;

        case ORTE_DSS_MUL:
            (*value) *= *operand;
            break;

        case ORTE_DSS_DIV:
            if (0 == *operand) {
                ORTE_ERROR_LOG(ORTE_ERR_OPERATION_UNSUPPORTED);
                return;
            }
            (*value) /= *operand;
            break;

        default:
            ORTE_ERROR_LOG(ORTE_ERR_OPERATION_UNSUPPORTED);
            break;
    }
    return;
}

static void orte_dss_arith_size(size_t *value, size_t *operand, orte_dss_arith_op_t operation)
{
    switch(operation) {
        case ORTE_DSS_ADD:
            (*value) += *operand;
            break;

        case ORTE_DSS_SUB:
            (*value) -= *operand;
            break;

        case ORTE_DSS_MUL:
            (*value) *= *operand;
            break;

        case ORTE_DSS_DIV:
            if (0 == *operand) {
                ORTE_ERROR_LOG(ORTE_ERR_OPERATION_UNSUPPORTED);
                return;
            }
            (*value) /= *operand;
            break;

        default:
            ORTE_ERROR_LOG(ORTE_ERR_OPERATION_UNSUPPORTED);
            break;
    }
    return;
}

static void orte_dss_arith_pid(pid_t *value, pid_t *operand, orte_dss_arith_op_t operation)
{
    switch(operation) {
        case ORTE_DSS_ADD:
            (*value) += *operand;
            break;

        case ORTE_DSS_SUB:
            (*value) -= *operand;
            break;

        case ORTE_DSS_MUL:
            (*value) *= *operand;
            break;

        case ORTE_DSS_DIV:
            if (0 == *operand) {
                ORTE_ERROR_LOG(ORTE_ERR_OPERATION_UNSUPPORTED);
                return;
            }
            (*value) /= *operand;
            break;

        default:
            ORTE_ERROR_LOG(ORTE_ERR_OPERATION_UNSUPPORTED);
            break;
    }
    return;
}

static void orte_dss_arith_byte(uint8_t *value, uint8_t *operand, orte_dss_arith_op_t operation)
{
    switch(operation) {
        case ORTE_DSS_ADD:
            (*value) += *operand;
            break;

        case ORTE_DSS_SUB:
            (*value) -= *operand;
            break;

        case ORTE_DSS_MUL:
            (*value) *= *operand;
            break;

        case ORTE_DSS_DIV:
            if (0 == *operand) {
                ORTE_ERROR_LOG(ORTE_ERR_OPERATION_UNSUPPORTED);
                return;
            }
            (*value) /= *operand;
            break;

        default:
            ORTE_ERROR_LOG(ORTE_ERR_OPERATION_UNSUPPORTED);
            break;
    }
    return;
}

static void orte_dss_arith_int8(int8_t *value, int8_t *operand, orte_dss_arith_op_t operation)
{
    switch(operation) {
        case ORTE_DSS_ADD:
            (*value) += *operand;
            break;

        case ORTE_DSS_SUB:
            (*value) -= *operand;
            break;

        case ORTE_DSS_MUL:
            (*value) *= *operand;
            break;

        case ORTE_DSS_DIV:
            if (0 == *operand) {
                ORTE_ERROR_LOG(ORTE_ERR_OPERATION_UNSUPPORTED);
                return;
            }
            (*value) /= *operand;
            break;

        default:
            ORTE_ERROR_LOG(ORTE_ERR_OPERATION_UNSUPPORTED);
            break;
    }
    return;
}

static void orte_dss_arith_int16(int16_t *value, int16_t *operand, orte_dss_arith_op_t operation)
{
    switch(operation) {
        case ORTE_DSS_ADD:
            (*value) += *operand;
            break;

        case ORTE_DSS_SUB:
            (*value) -= *operand;
            break;

        case ORTE_DSS_MUL:
            (*value) *= *operand;
            break;

        case ORTE_DSS_DIV:
            if (0 == *operand) {
                ORTE_ERROR_LOG(ORTE_ERR_OPERATION_UNSUPPORTED);
                return;
            }
            (*value) /= *operand;
            break;

        default:
            ORTE_ERROR_LOG(ORTE_ERR_OPERATION_UNSUPPORTED);
            break;
    }
    return;
}

static void orte_dss_arith_uint16(uint16_t *value, uint16_t *operand, orte_dss_arith_op_t operation)
{
    switch(operation) {
        case ORTE_DSS_ADD:
            (*value) += *operand;
            break;

        case ORTE_DSS_SUB:
            (*value) -= *operand;
            break;

        case ORTE_DSS_MUL:
            (*value) *= *operand;
            break;

        case ORTE_DSS_DIV:
            if (0 == *operand) {
                ORTE_ERROR_LOG(ORTE_ERR_OPERATION_UNSUPPORTED);
                return;
            }
            (*value) /= *operand;
            break;

        default:
            ORTE_ERROR_LOG(ORTE_ERR_OPERATION_UNSUPPORTED);
            break;
    }
    return;
}

static void orte_dss_arith_int32(int32_t *value, int32_t *operand, orte_dss_arith_op_t operation)
{
    switch(operation) {
        case ORTE_DSS_ADD:
            (*value) += *operand;
            break;

        case ORTE_DSS_SUB:
            (*value) -= *operand;
            break;

        case ORTE_DSS_MUL:
            (*value) *= *operand;
            break;

        case ORTE_DSS_DIV:
            if (0 == *operand) {
                ORTE_ERROR_LOG(ORTE_ERR_OPERATION_UNSUPPORTED);
                return;
            }
            (*value) /= *operand;
            break;

        default:
            ORTE_ERROR_LOG(ORTE_ERR_OPERATION_UNSUPPORTED);
            break;
    }
    return;
}

static void orte_dss_arith_uint32(uint32_t *value, uint32_t *operand, orte_dss_arith_op_t operation)
{
    switch(operation) {
        case ORTE_DSS_ADD:
            (*value) += *operand;
            break;

        case ORTE_DSS_SUB:
            (*value) -= *operand;
            break;

        case ORTE_DSS_MUL:
            (*value) *= *operand;
            break;

        case ORTE_DSS_DIV:
            if (0 == *operand) {
                ORTE_ERROR_LOG(ORTE_ERR_OPERATION_UNSUPPORTED);
                return;
            }
            (*value) /= *operand;
            break;

        default:
            ORTE_ERROR_LOG(ORTE_ERR_OPERATION_UNSUPPORTED);
            break;
    }
    return;
}

static void orte_dss_arith_int64(int64_t *value, int64_t *operand, orte_dss_arith_op_t operation)
{
    switch(operation) {
        case ORTE_DSS_ADD:
            (*value) += *operand;
            break;

        case ORTE_DSS_SUB:
            (*value) -= *operand;
            break;

        case ORTE_DSS_MUL:
            (*value) *= *operand;
            break;

        case ORTE_DSS_DIV:
            if (0 == *operand) {
                ORTE_ERROR_LOG(ORTE_ERR_OPERATION_UNSUPPORTED);
                return;
            }
            (*value) /= *operand;
            break;

        default:
            ORTE_ERROR_LOG(ORTE_ERR_OPERATION_UNSUPPORTED);
            break;
    }
    return;
}

static void orte_dss_arith_uint64(uint64_t *value, uint64_t *operand, orte_dss_arith_op_t operation)
{
    switch(operation) {
        case ORTE_DSS_ADD:
            (*value) += *operand;
            break;

        case ORTE_DSS_SUB:
            (*value) -= *operand;
            break;

        case ORTE_DSS_MUL:
            (*value) *= *operand;
            break;

        case ORTE_DSS_DIV:
            if (0 == *operand) {
                ORTE_ERROR_LOG(ORTE_ERR_OPERATION_UNSUPPORTED);
                return;
            }
            (*value) /= *operand;
            break;

        default:
            ORTE_ERROR_LOG(ORTE_ERR_OPERATION_UNSUPPORTED);
            break;
    }
    return;
}

static void orte_dss_arith_std_cntr(orte_std_cntr_t *value, orte_std_cntr_t *operand, orte_dss_arith_op_t operation)
{
    switch(operation) {
        case ORTE_DSS_ADD:
            (*value) += *operand;
            break;
            
        case ORTE_DSS_SUB:
            (*value) -= *operand;
            break;
            
        case ORTE_DSS_MUL:
            (*value) *= *operand;
            break;
            
        case ORTE_DSS_DIV:
            if (0 == *operand) {
                ORTE_ERROR_LOG(ORTE_ERR_OPERATION_UNSUPPORTED);
                return;
            }
            (*value) /= *operand;
            break;
            
        default:
            ORTE_ERROR_LOG(ORTE_ERR_OPERATION_UNSUPPORTED);
            break;
    }
    return;
}

static void orte_dss_arith_data_type(orte_data_type_t *value, orte_data_type_t *operand, orte_dss_arith_op_t operation)
{
    switch(operation) {
        case ORTE_DSS_ADD:
            (*value) += *operand;
            break;

        case ORTE_DSS_SUB:
            (*value) -= *operand;
            break;

        case ORTE_DSS_MUL:
            (*value) *= *operand;
            break;

        case ORTE_DSS_DIV:
            if (0 == *operand) {
                ORTE_ERROR_LOG(ORTE_ERR_OPERATION_UNSUPPORTED);
                return;
            }
            (*value) /= *operand;
            break;

        default:
            ORTE_ERROR_LOG(ORTE_ERR_OPERATION_UNSUPPORTED);
            break;
    }
    return;
}

static void orte_dss_arith_daemon_cmd(orte_daemon_cmd_flag_t *value, orte_daemon_cmd_flag_t *operand, orte_dss_arith_op_t operation)
{
    switch(operation) {
        case ORTE_DSS_ADD:
            (*value) += *operand;
            break;

        case ORTE_DSS_SUB:
            (*value) -= *operand;
            break;

        case ORTE_DSS_MUL:
            (*value) *= *operand;
            break;

        case ORTE_DSS_DIV:
            if (0 == *operand) {
                ORTE_ERROR_LOG(ORTE_ERR_OPERATION_UNSUPPORTED);
                return;
            }
            (*value) /= *operand;
            break;

        default:
            ORTE_ERROR_LOG(ORTE_ERR_OPERATION_UNSUPPORTED);
            break;
    }
    return;
}

