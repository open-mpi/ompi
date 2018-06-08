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
 * Copyright (c) 2012      Los Alamos National Security, Inc.  All rights reserved.
 * Copyright (c) 2014-2018 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2016      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <src/include/pmix_config.h>

#include <src/include/types.h>

#include "src/util/argv.h"
#include "src/util/error.h"
#include "src/util/output.h"
#include "src/include/pmix_globals.h"
#include "src/mca/bfrops/bfrops_types.h"
#include "src/mca/bfrops/base/base.h"


static pmix_status_t pmix_bfrops_base_unpack_buffer(pmix_pointer_array_t *regtypes,
                                                    pmix_buffer_t *buffer,
                                                    void *dst, int32_t *num_vals,
                                                    pmix_data_type_t type)
{
    pmix_status_t rc;
    pmix_data_type_t local_type;
    pmix_bfrop_type_info_t *info;

    pmix_output_verbose(20, pmix_bfrops_base_framework.framework_output,
                        "pmix_bfrops_base_unpack_buffer( %p, %p, %lu, %d )\n",
                        (void*)buffer, dst, (long unsigned int)*num_vals, (int)type);

    /** Unpack the declared data type */
    if (PMIX_BFROP_BUFFER_FULLY_DESC == buffer->type) {
        if (PMIX_SUCCESS != (rc = pmix_bfrop_get_data_type(buffer, &local_type))) {
            PMIX_ERROR_LOG(rc);
            return rc;
        }
        /* if the data types don't match, then return an error */
        if (type != local_type) {
            pmix_output(0, "PMIX bfrop:unpack: got type %d when expecting type %d", local_type, type);
            return PMIX_ERR_PACK_MISMATCH;
        }
    }

    /* Lookup the unpack function for this type and call it */
    if (NULL == (info = (pmix_bfrop_type_info_t*)pmix_pointer_array_get_item(regtypes, type))) {
        PMIX_ERROR_LOG(PMIX_ERR_UNPACK_FAILURE);
        return PMIX_ERR_UNPACK_FAILURE;
    }

    return info->odti_unpack_fn(buffer, dst, num_vals, type);
}

pmix_status_t pmix_bfrops_base_unpack(pmix_pointer_array_t *regtypes,
                                      pmix_buffer_t *buffer,
                                      void *dst, int32_t *num_vals,
                                      pmix_data_type_t type)
{
    pmix_status_t rc, ret;
    int32_t local_num, n=1;
    pmix_data_type_t local_type;

    /* check for error */
    if (NULL == buffer || NULL == dst || NULL == num_vals) {
        return PMIX_ERR_BAD_PARAM;
    }

    /* if user provides a zero for num_vals, then there is no storage allocated
     * so return an appropriate error
     */
     if (0 == *num_vals) {
        pmix_output_verbose(20, pmix_bfrops_base_framework.framework_output,
                            "pmix_bfrop_unpack: inadequate space ( %p, %p, %lu, %d )\n",
                            (void*)buffer, dst, (long unsigned int)*num_vals, (int)type);
        return PMIX_ERR_UNPACK_INADEQUATE_SPACE;
    }

    /** Unpack the declared number of values
     * REMINDER: it is possible that the buffer is corrupted and that
     * the BFROP will *think* there is a proper int32_t variable at the
     * beginning of the unpack region - but that the value is bogus (e.g., just
     * a byte field in a string array that so happens to have a value that
     * matches the int32_t data type flag). Therefore, this error check is
     * NOT completely safe. This is true for ALL unpack functions, not just
     * int32_t as used here.
     */
     if (PMIX_BFROP_BUFFER_FULLY_DESC == buffer->type) {
        if (PMIX_SUCCESS != (rc = pmix_bfrop_get_data_type(buffer, &local_type))) {
            *num_vals = 0;
            /* don't error log here as the user may be unpacking past
             * the end of the buffer, which isn't necessarily an error */
            return rc;
        }
        if (PMIX_INT32 != local_type) { /* if the length wasn't first, then error */
            *num_vals = 0;
            PMIX_ERROR_LOG(PMIX_ERR_UNPACK_FAILURE);
            return PMIX_ERR_UNPACK_FAILURE;
        }
    }

    n=1;
    if (PMIX_SUCCESS != (rc = pmix_bfrops_base_unpack_int32(buffer, &local_num, &n, PMIX_INT32))) {
        *num_vals = 0;
            /* don't error log here as the user may be unpacking past
             * the end of the buffer, which isn't necessarily an error */
        return rc;
    }
    pmix_output_verbose(20, pmix_bfrops_base_framework.framework_output,
                        "pmix_bfrop_unpack: found %d values for %d provided storage",
                        local_num, *num_vals);

    /** if the storage provided is inadequate, set things up
     * to unpack as much as we can and to return an error code
     * indicating that everything was not unpacked - the buffer
     * is left in a state where it can not be further unpacked.
     */
     if (local_num > *num_vals) {
        local_num = *num_vals;
        pmix_output_verbose(20, pmix_bfrops_base_framework.framework_output,
                            "pmix_bfrop_unpack: inadequate space ( %p, %p, %lu, %d )\n",
                            (void*)buffer, dst, (long unsigned int)*num_vals, (int)type);
        ret = PMIX_ERR_UNPACK_INADEQUATE_SPACE;
    } else {  /** enough or more than enough storage */
        *num_vals = local_num;  /** let the user know how many we actually unpacked */
        ret = PMIX_SUCCESS;
    }

    /** Unpack the value(s) */
    if (PMIX_SUCCESS != (rc = pmix_bfrops_base_unpack_buffer(regtypes, buffer, dst, &local_num, type))) {
        *num_vals = 0;
        ret = rc;
    }

    return ret;
}

static pmix_status_t unpack_gentype(pmix_buffer_t *buffer, void *dest,
                                    int32_t *num_vals, pmix_data_type_t type)
{
    switch(type) {
        case PMIX_INT8:
        case PMIX_UINT8:
        return pmix_bfrops_base_unpack_byte(buffer, dest, num_vals, type);
        break;

        case PMIX_INT16:
        case PMIX_UINT16:
        return pmix_bfrops_base_unpack_int16(buffer, dest, num_vals, type);
        break;

        case PMIX_INT32:
        case PMIX_UINT32:
        return pmix_bfrops_base_unpack_int32(buffer, dest, num_vals, type);
        break;

        case PMIX_INT64:
        case PMIX_UINT64:
        return pmix_bfrops_base_unpack_int64(buffer, dest, num_vals, type);
        break;

        default:
        return PMIX_ERR_UNKNOWN_DATA_TYPE;
    }
}

/* UNPACK GENERIC SYSTEM TYPES */

/*
 * BOOL
 */
 pmix_status_t pmix_bfrops_base_unpack_bool(pmix_buffer_t *buffer, void *dest,
                                            int32_t *num_vals, pmix_data_type_t type)
 {
    int32_t i;
    uint8_t *src;
    bool *dst;

    pmix_output_verbose(20, pmix_bfrops_base_framework.framework_output,
                        "pmix_bfrop_unpack_bool * %d\n", (int)*num_vals);

    /* check to see if there's enough data in buffer */
    if (pmix_bfrop_too_small(buffer, *num_vals)) {
        return PMIX_ERR_UNPACK_READ_PAST_END_OF_BUFFER;
    }

    /* unpack the data */
    src = (uint8_t*)buffer->unpack_ptr;
    dst = (bool*)dest;

    for (i=0; i < *num_vals; i++) {
        if (src[i]) {
            dst[i] = true;
        } else {
            dst[i] = false;
        }
    }

    /* update buffer pointer */
    buffer->unpack_ptr += *num_vals;

    return PMIX_SUCCESS;
}

/*
 * INT
 */
pmix_status_t pmix_bfrops_base_unpack_int(pmix_buffer_t *buffer, void *dest,
                                          int32_t *num_vals, pmix_data_type_t type)
{
    pmix_status_t ret;
    pmix_data_type_t remote_type;

    if (PMIX_SUCCESS != (ret = pmix_bfrop_get_data_type(buffer, &remote_type))) {
        return ret;
    }

    if (remote_type == BFROP_TYPE_INT) {
        /* fast path it if the sizes are the same */
        /* Turn around and unpack the real type */
        if (PMIX_SUCCESS != (ret = unpack_gentype(buffer, dest, num_vals, BFROP_TYPE_INT))) {
        }
    } else {
        /* slow path - types are different sizes */
        PMIX_BFROP_UNPACK_SIZE_MISMATCH(int, remote_type, ret);
    }

    return ret;
}

/*
 * SIZE_T
 */
pmix_status_t pmix_bfrops_base_unpack_sizet(pmix_buffer_t *buffer, void *dest,
                                            int32_t *num_vals, pmix_data_type_t type)
{
    pmix_status_t ret;
    pmix_data_type_t remote_type;

    if (PMIX_SUCCESS != (ret = pmix_bfrop_get_data_type(buffer, &remote_type))) {
        return ret;
    }

    if (remote_type == BFROP_TYPE_SIZE_T) {
        /* fast path it if the sizes are the same */
        /* Turn around and unpack the real type */
        if (PMIX_SUCCESS != (ret = unpack_gentype(buffer, dest, num_vals, BFROP_TYPE_SIZE_T))) {
        }
    } else {
        /* slow path - types are different sizes */
        PMIX_BFROP_UNPACK_SIZE_MISMATCH(size_t, remote_type, ret);
    }

    return ret;
}

/*
 * PID_T
 */
pmix_status_t pmix_bfrops_base_unpack_pid(pmix_buffer_t *buffer, void *dest,
                                          int32_t *num_vals, pmix_data_type_t type)
{
    pmix_status_t ret;
    pmix_data_type_t remote_type;

    if (PMIX_SUCCESS != (ret = pmix_bfrop_get_data_type(buffer, &remote_type))) {
        return ret;
    }

    if (remote_type == BFROP_TYPE_PID_T) {
        /* fast path it if the sizes are the same */
        /* Turn around and unpack the real type */
        if (PMIX_SUCCESS != (ret = unpack_gentype(buffer, dest, num_vals, BFROP_TYPE_PID_T))) {
        }
    } else {
        /* slow path - types are different sizes */
        PMIX_BFROP_UNPACK_SIZE_MISMATCH(pid_t, remote_type, ret);
    }

    return ret;
}


/* UNPACK FUNCTIONS FOR NON-GENERIC SYSTEM TYPES */

/*
 * BYTE, CHAR, INT8
 */
pmix_status_t pmix_bfrops_base_unpack_byte(pmix_buffer_t *buffer, void *dest,
                                           int32_t *num_vals, pmix_data_type_t type)
{
    pmix_output_verbose(20, pmix_bfrops_base_framework.framework_output,
                        "pmix_bfrop_unpack_byte * %d\n", (int)*num_vals);

    /* check to see if there's enough data in buffer */
    if (pmix_bfrop_too_small(buffer, *num_vals)) {
        return PMIX_ERR_UNPACK_READ_PAST_END_OF_BUFFER;
    }

    /* unpack the data */
    memcpy(dest, buffer->unpack_ptr, *num_vals);

    /* update buffer pointer */
    buffer->unpack_ptr += *num_vals;

    return PMIX_SUCCESS;
}

pmix_status_t pmix_bfrops_base_unpack_int16(pmix_buffer_t *buffer, void *dest,
                                            int32_t *num_vals, pmix_data_type_t type)
{
    int32_t i;
    uint16_t tmp, *desttmp = (uint16_t*) dest;

    pmix_output_verbose(20, pmix_bfrops_base_framework.framework_output,
                        "pmix_bfrop_unpack_int16 * %d\n", (int)*num_vals);

    /* check to see if there's enough data in buffer */
    if (pmix_bfrop_too_small(buffer, (*num_vals)*sizeof(tmp))) {
        return PMIX_ERR_UNPACK_READ_PAST_END_OF_BUFFER;
    }

    /* unpack the data */
    for (i = 0; i < (*num_vals); ++i) {
        memcpy( &(tmp), buffer->unpack_ptr, sizeof(tmp) );
        tmp = pmix_ntohs(tmp);
        memcpy(&desttmp[i], &tmp, sizeof(tmp));
        buffer->unpack_ptr += sizeof(tmp);
    }

    return PMIX_SUCCESS;
}

pmix_status_t pmix_bfrops_base_unpack_int32(pmix_buffer_t *buffer, void *dest,
                                            int32_t *num_vals, pmix_data_type_t type)
{
    int32_t i;
    uint32_t tmp, *desttmp = (uint32_t*) dest;

    pmix_output_verbose(20, pmix_bfrops_base_framework.framework_output,
                        "pmix_bfrop_unpack_int32 * %d\n", (int)*num_vals);

    /* check to see if there's enough data in buffer */
    if (pmix_bfrop_too_small(buffer, (*num_vals)*sizeof(tmp))) {
        return PMIX_ERR_UNPACK_READ_PAST_END_OF_BUFFER;
    }

    /* unpack the data */
    for (i = 0; i < (*num_vals); ++i) {
        memcpy( &(tmp), buffer->unpack_ptr, sizeof(tmp) );
        tmp = ntohl(tmp);
        memcpy(&desttmp[i], &tmp, sizeof(tmp));
        buffer->unpack_ptr += sizeof(tmp);
    }

    return PMIX_SUCCESS;
}

pmix_status_t pmix_bfrops_base_unpack_datatype(pmix_buffer_t *buffer, void *dest,
                                               int32_t *num_vals, pmix_data_type_t type)
{
    return pmix_bfrops_base_unpack_int16(buffer, dest, num_vals, type);
}

pmix_status_t pmix_bfrops_base_unpack_int64(pmix_buffer_t *buffer, void *dest,
                                            int32_t *num_vals, pmix_data_type_t type)
{
    int32_t i;
    uint64_t tmp, *desttmp = (uint64_t*) dest;

    pmix_output_verbose(20, pmix_bfrops_base_framework.framework_output,
                        "pmix_bfrop_unpack_int64 * %d\n", (int)*num_vals);

    /* check to see if there's enough data in buffer */
    if (pmix_bfrop_too_small(buffer, (*num_vals)*sizeof(tmp))) {
        return PMIX_ERR_UNPACK_READ_PAST_END_OF_BUFFER;
    }

    /* unpack the data */
    for (i = 0; i < (*num_vals); ++i) {
        memcpy( &(tmp), buffer->unpack_ptr, sizeof(tmp) );
        tmp = pmix_ntoh64(tmp);
        memcpy(&desttmp[i], &tmp, sizeof(tmp));
        buffer->unpack_ptr += sizeof(tmp);
    }

    return PMIX_SUCCESS;
}

pmix_status_t pmix_bfrops_base_unpack_string(pmix_buffer_t *buffer, void *dest,
                                             int32_t *num_vals, pmix_data_type_t type)
{
    pmix_status_t ret;
    int32_t i, len, n=1;
    char **sdest = (char**) dest;

    for (i = 0; i < (*num_vals); ++i) {
        if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_int32(buffer, &len, &n, PMIX_INT32))) {
            return ret;
        }
        if (0 ==  len) {   /* zero-length string - unpack the NULL */
            sdest[i] = NULL;
        } else {
            sdest[i] = (char*)malloc(len);  // NULL terminator is included
            if (NULL == sdest[i]) {
                return PMIX_ERR_OUT_OF_RESOURCE;
            }
            if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_byte(buffer, sdest[i], &len, PMIX_BYTE))) {
                return ret;
            }
        }
    }

    return PMIX_SUCCESS;
}

pmix_status_t pmix_bfrops_base_unpack_float(pmix_buffer_t *buffer, void *dest,
                                            int32_t *num_vals, pmix_data_type_t type)
{
    int32_t i, n;
    float *desttmp = (float*) dest, tmp;
    pmix_status_t ret;
    char *convert;

    pmix_output_verbose(20, pmix_bfrops_base_framework.framework_output,
                        "pmix_bfrop_unpack_float * %d\n", (int)*num_vals);

    /* check to see if there's enough data in buffer */
    if (pmix_bfrop_too_small(buffer, (*num_vals)*sizeof(float))) {
        return PMIX_ERR_UNPACK_READ_PAST_END_OF_BUFFER;
    }

    /* unpack the data */
    for (i = 0; i < (*num_vals); ++i) {
        n=1;
        convert = NULL;
        if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_string(buffer, &convert, &n, PMIX_STRING))) {
            return ret;
        }
        if (NULL != convert) {
            tmp = strtof(convert, NULL);
            memcpy(&desttmp[i], &tmp, sizeof(tmp));
            free(convert);
        }
    }
    return PMIX_SUCCESS;
}

pmix_status_t pmix_bfrops_base_unpack_double(pmix_buffer_t *buffer, void *dest,
                                             int32_t *num_vals, pmix_data_type_t type)
{
    int32_t i, n;
    double *desttmp = (double*) dest, tmp;
    pmix_status_t ret;
    char *convert;

    pmix_output_verbose(20, pmix_bfrops_base_framework.framework_output,
                        "pmix_bfrop_unpack_double * %d\n", (int)*num_vals);

    /* check to see if there's enough data in buffer */
    if (pmix_bfrop_too_small(buffer, (*num_vals)*sizeof(double))) {
        return PMIX_ERR_UNPACK_READ_PAST_END_OF_BUFFER;
    }

    /* unpack the data */
    for (i = 0; i < (*num_vals); ++i) {
        n=1;
        convert = NULL;
        if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_string(buffer, &convert, &n, PMIX_STRING))) {
            return ret;
        }
        if (NULL != convert) {
            tmp = strtod(convert, NULL);
            memcpy(&desttmp[i], &tmp, sizeof(tmp));
            free(convert);
        }
    }
    return PMIX_SUCCESS;
}

pmix_status_t pmix_bfrops_base_unpack_timeval(pmix_buffer_t *buffer, void *dest,
                                              int32_t *num_vals, pmix_data_type_t type)
{
    int32_t i, n;
    int64_t tmp[2];
    struct timeval *desttmp = (struct timeval *) dest, tt;
    pmix_status_t ret;

    pmix_output_verbose(20, pmix_bfrops_base_framework.framework_output,
                        "pmix_bfrop_unpack_timeval * %d\n", (int)*num_vals);

    /* check to see if there's enough data in buffer */
    if (pmix_bfrop_too_small(buffer, (*num_vals)*sizeof(struct timeval))) {
        return PMIX_ERR_UNPACK_READ_PAST_END_OF_BUFFER;
    }

    /* unpack the data */
    for (i = 0; i < (*num_vals); ++i) {
        n=2;
        if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_int64(buffer, tmp, &n, PMIX_INT64))) {
            return ret;
        }
        tt.tv_sec = tmp[0];
        tt.tv_usec = tmp[1];
        memcpy(&desttmp[i], &tt, sizeof(tt));
    }
    return PMIX_SUCCESS;
}

pmix_status_t pmix_bfrops_base_unpack_time(pmix_buffer_t *buffer, void *dest,
                                           int32_t *num_vals, pmix_data_type_t type)
{
    int32_t i, n;
    time_t *desttmp = (time_t *) dest, tmp;
    pmix_status_t ret;
    uint64_t ui64;

    /* time_t is a system-dependent size, so cast it
     * to uint64_t as a generic safe size
     */

     pmix_output_verbose(20, pmix_bfrops_base_framework.framework_output,
                         "pmix_bfrop_unpack_time * %d\n", (int)*num_vals);

    /* check to see if there's enough data in buffer */
     if (pmix_bfrop_too_small(buffer, (*num_vals)*(sizeof(uint64_t)))) {
        return PMIX_ERR_UNPACK_READ_PAST_END_OF_BUFFER;
    }

    /* unpack the data */
    for (i = 0; i < (*num_vals); ++i) {
        n=1;
        if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_int64(buffer, &ui64, &n, PMIX_UINT64))) {
            return ret;
        }
        tmp = (time_t)ui64;
        memcpy(&desttmp[i], &tmp, sizeof(tmp));
    }
    return PMIX_SUCCESS;
}


pmix_status_t pmix_bfrops_base_unpack_status(pmix_buffer_t *buffer, void *dest,
                                             int32_t *num_vals, pmix_data_type_t type)
{
     pmix_output_verbose(20, pmix_bfrops_base_framework.framework_output,
                         "pmix_bfrop_unpack_status * %d\n", (int)*num_vals);

    /* check to see if there's enough data in buffer */
    if (pmix_bfrop_too_small(buffer, (*num_vals)*(sizeof(pmix_status_t)))) {
        return PMIX_ERR_UNPACK_READ_PAST_END_OF_BUFFER;
    }

    /* unpack the data */
    return pmix_bfrops_base_unpack_int32(buffer, dest, num_vals, PMIX_INT32);
}


/* UNPACK FUNCTIONS FOR GENERIC PMIX TYPES */

/*
 * PMIX_VALUE
 */
pmix_status_t pmix_bfrops_base_unpack_val(pmix_buffer_t *buffer,
                                          pmix_value_t *val)
{
    int m;
    pmix_status_t ret;

    m = 1;
    switch (val->type) {
        case PMIX_UNDEF:
            break;
        case PMIX_BOOL:
            if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_bool(buffer, &val->data.flag, &m, PMIX_BOOL))) {
                return ret;
            }
            break;
        case PMIX_BYTE:
            if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_byte(buffer, &val->data.byte, &m, PMIX_BYTE))) {
                return ret;
            }
            break;
        case PMIX_STRING:
            if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_string(buffer, &val->data.string, &m, PMIX_STRING))) {
                return ret;
            }
            break;
        case PMIX_SIZE:
            if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_sizet(buffer, &val->data.size, &m, PMIX_SIZE))) {
                return ret;
            }
            break;
        case PMIX_PID:
            if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_pid(buffer, &val->data.pid, &m, PMIX_PID))) {
                return ret;
            }
            break;
        case PMIX_INT:
            if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_int(buffer, &val->data.integer, &m, PMIX_INT))) {
                return ret;
            }
            break;
        case PMIX_INT8:
            if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_byte(buffer, &val->data.int8, &m, PMIX_INT8))) {
                return ret;
            }
            break;
        case PMIX_INT16:
            if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_int16(buffer, &val->data.int16, &m, PMIX_INT16))) {
                return ret;
            }
            break;
        case PMIX_INT32:
            if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_int32(buffer, &val->data.int32, &m, PMIX_INT32))) {
                return ret;
            }
            break;
        case PMIX_INT64:
            if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_int64(buffer, &val->data.int64, &m, PMIX_INT64))) {
                return ret;
            }
            break;
        case PMIX_UINT:
            if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_int(buffer, &val->data.uint, &m, PMIX_UINT))) {
                return ret;
            }
            break;
        case PMIX_UINT8:
            if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_byte(buffer, &val->data.uint8, &m, PMIX_UINT8))) {
                return ret;
            }
            break;
        case PMIX_UINT16:
            if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_int16(buffer, &val->data.uint16, &m, PMIX_UINT16))) {
                return ret;
            }
            break;
        case PMIX_UINT32:
            if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_int32(buffer, &val->data.uint32, &m, PMIX_UINT32))) {
                return ret;
            }
            break;
        case PMIX_UINT64:
            if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_int64(buffer, &val->data.uint64, &m, PMIX_UINT64))) {
                return ret;
            }
            break;
        case PMIX_FLOAT:
            if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_float(buffer, &val->data.fval, &m, PMIX_FLOAT))) {
                return ret;
            }
            break;
        case PMIX_DOUBLE:
            if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_double(buffer, &val->data.dval, &m, PMIX_DOUBLE))) {
                return ret;
            }
            break;
        case PMIX_TIMEVAL:
            if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_timeval(buffer, &val->data.tv, &m, PMIX_TIMEVAL))) {
                return ret;
            }
            break;
        case PMIX_TIME:
            if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_time(buffer, &val->data.time, &m, PMIX_TIME))) {
                return ret;
            }
            break;
        case PMIX_STATUS:
             if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_status(buffer, &val->data.status, &m, PMIX_STATUS))) {
                 return ret;
             }
             break;
        case PMIX_PROC:
            /* this field is now a pointer, so we must allocate storage for it */
            PMIX_PROC_CREATE(val->data.proc, m);
            if (NULL == val->data.proc) {
                return PMIX_ERR_NOMEM;
            }
            if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_proc(buffer, val->data.proc, &m, PMIX_PROC))) {
                return ret;
            }
            break;
        case PMIX_PROC_RANK:
            if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_rank(buffer, &val->data.rank, &m, PMIX_PROC_RANK))) {
                return ret;
            }
            break;
        case PMIX_BYTE_OBJECT:
        case PMIX_COMPRESSED_STRING:
            if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_bo(buffer, &val->data.bo, &m, PMIX_BYTE_OBJECT))) {
                return ret;
            }
            break;
        case PMIX_PERSIST:
            if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_persist(buffer, &val->data.proc, &m, PMIX_PERSIST))) {
                return ret;
            }
            break;
        case PMIX_POINTER:
            if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_ptr(buffer, &val->data.ptr, &m, PMIX_POINTER))) {
                return ret;
            }
            break;
        case PMIX_SCOPE:
            if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_scope(buffer, &val->data.scope, &m, PMIX_SCOPE))) {
                return ret;
            }
            break;
        case PMIX_DATA_RANGE:
            if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_range(buffer, &val->data.range, &m, PMIX_DATA_RANGE))) {
                return ret;
            }
            break;
        case PMIX_PROC_STATE:
            if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_pstate(buffer, &val->data.state, &m, PMIX_PROC_STATE))) {
                return ret;
            }
            break;
        case PMIX_PROC_INFO:
            /* this is now a pointer, so allocate storage for it */
            PMIX_PROC_INFO_CREATE(val->data.pinfo, 1);
            if (NULL == val->data.pinfo) {
                return PMIX_ERR_NOMEM;
            }
            if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_pinfo(buffer, val->data.pinfo, &m, PMIX_PROC_INFO))) {
                return ret;
            }
            break;
        case PMIX_DATA_ARRAY:
            /* this is now a pointer, so allocate storage for it */
            val->data.darray = (pmix_data_array_t*)malloc(sizeof(pmix_data_array_t));
            if (NULL == val->data.darray) {
                return PMIX_ERR_NOMEM;
            }
            if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_darray(buffer, val->data.darray, &m, PMIX_DATA_ARRAY))) {
                return ret;
            }
            break;
        case PMIX_ALLOC_DIRECTIVE:
            if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_alloc_directive(buffer, &val->data.adir, &m, PMIX_ALLOC_DIRECTIVE))) {
                return ret;
            }
            break;
        case PMIX_ENVAR:
            if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_envar(buffer, &val->data.envar, &m, PMIX_ENVAR))) {
                return ret;
            }
            break;
        /**** DEPRECATED ****/
        case PMIX_INFO_ARRAY:
            /* this field is now a pointer, so we must allocate storage for it */
            val->data.array = (pmix_info_array_t*)malloc(sizeof(pmix_info_array_t));
            if (NULL == val->data.array) {
                return PMIX_ERR_NOMEM;
            }
            if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_array(buffer, val->data.array, &m, PMIX_INFO_ARRAY))) {
                return ret;
            }
            break;
            /********************/
        default:
        pmix_output(0, "UNPACK-PMIX-VALUE: UNSUPPORTED TYPE %d", (int)val->type);
        return PMIX_ERROR;
    }

    return PMIX_SUCCESS;
}

pmix_status_t pmix_bfrops_base_unpack_value(pmix_buffer_t *buffer, void *dest,
                                            int32_t *num_vals, pmix_data_type_t type)
{
    pmix_value_t *ptr;
    int32_t i, n;
    pmix_status_t ret;

    ptr = (pmix_value_t *) dest;
    n = *num_vals;

    for (i = 0; i < n; ++i) {
        /* unpack the type */
        if (PMIX_SUCCESS != (ret = pmix_bfrop_get_data_type(buffer, &ptr[i].type))) {
            PMIX_ERROR_LOG(ret);
            return ret;
        }
        /* unpack value */
        if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_val(buffer, &ptr[i])) ) {
            PMIX_ERROR_LOG(ret);
            return ret;
        }
    }
    return PMIX_SUCCESS;
}

pmix_status_t pmix_bfrops_base_unpack_info(pmix_buffer_t *buffer, void *dest,
                                           int32_t *num_vals, pmix_data_type_t type)
{
    pmix_info_t *ptr;
    int32_t i, n, m;
    pmix_status_t ret;
    char *tmp;

    pmix_output_verbose(20, pmix_bfrops_base_framework.framework_output,
                        "pmix_bfrop_unpack: %d info", *num_vals);

    ptr = (pmix_info_t *) dest;
    n = *num_vals;

    for (i = 0; i < n; ++i) {
        memset(ptr[i].key, 0, sizeof(ptr[i].key));
        memset(&ptr[i].value, 0, sizeof(pmix_value_t));
        /* unpack key */
        m=1;
        tmp = NULL;
        if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_string(buffer, &tmp, &m, PMIX_STRING))) {
            PMIX_ERROR_LOG(ret);
            return ret;
        }
        if (NULL == tmp) {
            return PMIX_ERROR;
        }
        (void)strncpy(ptr[i].key, tmp, PMIX_MAX_KEYLEN);
        free(tmp);
        /* unpack the directives */
        m=1;
        if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_info_directives(buffer, &ptr[i].flags, &m, PMIX_INFO_DIRECTIVES))) {
            return ret;
        }
        /* unpack value - since the value structure is statically-defined
         * instead of a pointer in this struct, we directly unpack it to
         * avoid the malloc */
         if (PMIX_SUCCESS != (ret = pmix_bfrop_get_data_type(buffer, &ptr[i].value.type))) {
            return ret;
        }
        pmix_output_verbose(20, pmix_bfrops_base_framework.framework_output,
                            "pmix_bfrop_unpack: info type %d", ptr[i].value.type);
        m=1;
        if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_val(buffer, &ptr[i].value))) {
            return ret;
        }
    }
    return PMIX_SUCCESS;
}

pmix_status_t pmix_bfrops_base_unpack_pdata(pmix_buffer_t *buffer, void *dest,
                                            int32_t *num_vals, pmix_data_type_t type)
{
    pmix_pdata_t *ptr;
    int32_t i, n, m;
    pmix_status_t ret;
    char *tmp;

    pmix_output_verbose(20, pmix_bfrops_base_framework.framework_output,
                        "pmix_bfrop_unpack: %d pdata", *num_vals);

    ptr = (pmix_pdata_t *) dest;
    n = *num_vals;

    for (i = 0; i < n; ++i) {
        PMIX_PDATA_CONSTRUCT(&ptr[i]);
        /* unpack the proc */
        m=1;
        if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_proc(buffer, &ptr[i].proc, &m, PMIX_PROC))) {
            return ret;
        }
        /* unpack key */
        m=1;
        tmp = NULL;
        if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_string(buffer, &tmp, &m, PMIX_STRING))) {
            return ret;
        }
        if (NULL == tmp) {
            PMIX_ERROR_LOG(PMIX_ERROR);
            return PMIX_ERROR;
        }
        (void)strncpy(ptr[i].key, tmp, PMIX_MAX_KEYLEN);
        free(tmp);
        /* unpack value - since the value structure is statically-defined
         * instead of a pointer in this struct, we directly unpack it to
         * avoid the malloc */
         if (PMIX_SUCCESS != (ret = pmix_bfrop_get_data_type(buffer, &ptr[i].value.type))) {
            PMIX_ERROR_LOG(ret);
            return ret;
        }
        pmix_output_verbose(20, pmix_bfrops_base_framework.framework_output,
                            "pmix_bfrop_unpack: pdata type %d %s", ptr[i].value.type, ptr[i].value.data.string);
        m=1;
        if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_val(buffer, &ptr[i].value))) {
            PMIX_ERROR_LOG(ret);
            return ret;
        }
    }
    return PMIX_SUCCESS;
}

pmix_status_t pmix_bfrops_base_unpack_buf(pmix_buffer_t *buffer, void *dest,
                                          int32_t *num_vals, pmix_data_type_t type)
{
    pmix_buffer_t *ptr;
    int32_t i, n, m;
    pmix_status_t ret;
    size_t nbytes;

    ptr = (pmix_buffer_t *) dest;
    n = *num_vals;

    for (i = 0; i < n; ++i) {
        PMIX_CONSTRUCT(&ptr[i], pmix_buffer_t);
        /* unpack the type of buffer */
        m=1;
        if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_byte(buffer, &ptr[i].type, &m, PMIX_BYTE))) {
            return ret;
        }
        /* unpack the number of bytes */
        m=1;
        if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_sizet(buffer, &nbytes, &m, PMIX_SIZE))) {
            return ret;
        }
        m = nbytes;
        /* setup the buffer's data region */
        if (0 < nbytes) {
            ptr[i].base_ptr = (char*)malloc(nbytes);
            if (NULL == ptr[i].base_ptr) {
                return PMIX_ERR_NOMEM;
            }
            /* unpack the bytes */
            if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_byte(buffer, ptr[i].base_ptr, &m, PMIX_BYTE))) {
                return ret;
            }
        }
        ptr[i].pack_ptr = ptr[i].base_ptr + m;
        ptr[i].unpack_ptr = ptr[i].base_ptr;
        ptr[i].bytes_allocated = nbytes;
        ptr[i].bytes_used = m;
    }
    return PMIX_SUCCESS;
}

pmix_status_t pmix_bfrops_base_unpack_proc(pmix_buffer_t *buffer, void *dest,
                                           int32_t *num_vals, pmix_data_type_t type)
{
    pmix_proc_t *ptr;
    int32_t i, n, m;
    pmix_status_t ret;
    char *tmp;

    pmix_output_verbose(20, pmix_bfrops_base_framework.framework_output,
                        "pmix_bfrop_unpack: %d procs", *num_vals);

    ptr = (pmix_proc_t *) dest;
    n = *num_vals;

    for (i = 0; i < n; ++i) {
        pmix_output_verbose(20, pmix_bfrops_base_framework.framework_output,
                            "pmix_bfrop_unpack: init proc[%d]", i);
        memset(&ptr[i], 0, sizeof(pmix_proc_t));
        /* unpack nspace */
        m=1;
        tmp = NULL;
        if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_string(buffer, &tmp, &m, PMIX_STRING))) {
            PMIX_ERROR_LOG(ret);
            return ret;
        }
        if (NULL == tmp) {
            PMIX_ERROR_LOG(PMIX_ERROR);
            return PMIX_ERROR;
        }
        (void)strncpy(ptr[i].nspace, tmp, PMIX_MAX_NSLEN);
        free(tmp);
        /* unpack the rank */
        m=1;
        if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_rank(buffer, &ptr[i].rank, &m, PMIX_PROC_RANK))) {
            PMIX_ERROR_LOG(ret);
            return ret;
        }
    }
    return PMIX_SUCCESS;
}

pmix_status_t pmix_bfrops_base_unpack_app(pmix_buffer_t *buffer, void *dest,
                                          int32_t *num_vals, pmix_data_type_t type)
{
    pmix_app_t *ptr;
    int32_t i, k, n, m;
    pmix_status_t ret;
    int32_t nval;
    char *tmp;

    pmix_output_verbose(20, pmix_bfrops_base_framework.framework_output,
                        "pmix_bfrop_unpack: %d apps", *num_vals);

    ptr = (pmix_app_t *) dest;
    n = *num_vals;

    for (i = 0; i < n; ++i) {
        /* initialize the fields */
        PMIX_APP_CONSTRUCT(&ptr[i]);
        /* unpack cmd */
        m=1;
        if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_string(buffer, &ptr[i].cmd, &m, PMIX_STRING))) {
            return ret;
        }
        /* unpack argc */
        m=1;
        if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_int(buffer, &nval, &m, PMIX_INT32))) {
            return ret;
        }
        /* unpack argv */
        for (k=0; k < nval; k++) {
            m=1;
            tmp = NULL;
            if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_string(buffer, &tmp, &m, PMIX_STRING))) {
                return ret;
            }
            if (NULL == tmp) {
                return PMIX_ERROR;
            }
            pmix_argv_append_nosize(&ptr[i].argv, tmp);
            free(tmp);
        }
        /* unpack env */
        m=1;
        if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_int32(buffer, &nval, &m, PMIX_INT32))) {
            return ret;
        }
        for (k=0; k < nval; k++) {
            m=1;
            tmp = NULL;
            if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_string(buffer, &tmp, &m, PMIX_STRING))) {
                return ret;
            }
            if (NULL == tmp) {
                return PMIX_ERROR;
            }
            pmix_argv_append_nosize(&ptr[i].env, tmp);
            free(tmp);
        }
        /* unpack cwd */
        m=1;
        if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_string(buffer, &ptr[i].cwd, &m, PMIX_STRING))) {
            return ret;
        }
        /* unpack maxprocs */
        m=1;
        if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_int(buffer, &ptr[i].maxprocs, &m, PMIX_INT))) {
            return ret;
        }
        /* unpack info array */
        m=1;
        if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_sizet(buffer, &ptr[i].ninfo, &m, PMIX_SIZE))) {
            return ret;
        }
        if (0 < ptr[i].ninfo) {
            PMIX_INFO_CREATE(ptr[i].info, ptr[i].ninfo);
            m = ptr[i].ninfo;
            if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_info(buffer, ptr[i].info, &m, PMIX_INFO))) {
                return ret;
            }
        }
    }
    return PMIX_SUCCESS;
}

pmix_status_t pmix_bfrops_base_unpack_kval(pmix_buffer_t *buffer, void *dest,
                                           int32_t *num_vals, pmix_data_type_t type)
{
    pmix_kval_t *ptr;
    int32_t i, n, m;
    pmix_status_t ret;

    pmix_output_verbose(20, pmix_bfrops_base_framework.framework_output,
                        "pmix_bfrop_unpack: %d kvals", *num_vals);

    ptr = (pmix_kval_t*) dest;
    n = *num_vals;

    for (i = 0; i < n; ++i) {
        PMIX_CONSTRUCT(&ptr[i], pmix_kval_t);
        /* unpack the key */
        m = 1;
        if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_string(buffer, &ptr[i].key, &m, PMIX_STRING))) {
            PMIX_ERROR_LOG(ret);
            return ret;
        }
        /* allocate the space */
        ptr[i].value = (pmix_value_t*)malloc(sizeof(pmix_value_t));
        /* unpack the value */
        m = 1;
        if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_value(buffer, ptr[i].value, &m, PMIX_VALUE))) {
            PMIX_ERROR_LOG(ret);
            return ret;
        }
    }
    return PMIX_SUCCESS;
}

pmix_status_t pmix_bfrops_base_unpack_modex(pmix_buffer_t *buffer, void *dest,
                                            int32_t *num_vals, pmix_data_type_t type)
{
    pmix_modex_data_t *ptr;
    int32_t i, n, m;
    pmix_status_t ret;

    pmix_output_verbose(20, pmix_bfrops_base_framework.framework_output,
                        "pmix_bfrop_unpack: %d modex", *num_vals);

    ptr = (pmix_modex_data_t *) dest;
    n = *num_vals;

    for (i = 0; i < n; ++i) {
        memset(&ptr[i], 0, sizeof(pmix_modex_data_t));
        /* unpack the number of bytes */
        m=1;
        if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_sizet(buffer, &ptr[i].size, &m, PMIX_SIZE))) {
            return ret;
        }
        if (0 < ptr[i].size) {
            ptr[i].blob = (uint8_t*)malloc(ptr[i].size * sizeof(uint8_t));
            m=ptr[i].size;
            if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_byte(buffer, ptr[i].blob, &m, PMIX_UINT8))) {
                return ret;
            }
        }
    }
    return PMIX_SUCCESS;
}


pmix_status_t pmix_bfrops_base_unpack_persist(pmix_buffer_t *buffer, void *dest,
                                              int32_t *num_vals, pmix_data_type_t type)
{
    return pmix_bfrops_base_unpack_byte(buffer, dest, num_vals, PMIX_UINT8);
}

pmix_status_t pmix_bfrops_base_unpack_bo(pmix_buffer_t *buffer, void *dest,
                                         int32_t *num_vals, pmix_data_type_t type)
{
    pmix_byte_object_t *ptr;
    int32_t i, n, m;
    pmix_status_t ret;

    pmix_output_verbose(20, pmix_bfrops_base_framework.framework_output,
                        "pmix_bfrop_unpack: %d byte_object", *num_vals);

    ptr = (pmix_byte_object_t *) dest;
    n = *num_vals;

    for (i = 0; i < n; ++i) {
        memset(&ptr[i], 0, sizeof(pmix_byte_object_t));
        /* unpack the number of bytes */
        m=1;
        if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_sizet(buffer, &ptr[i].size, &m, PMIX_SIZE))) {
            return ret;
        }
        if (0 < ptr[i].size) {
            ptr[i].bytes = (char*)malloc(ptr[i].size * sizeof(char));
            m=ptr[i].size;
            if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_byte(buffer, ptr[i].bytes, &m, PMIX_BYTE))) {
                return ret;
            }
        }
    }
    return PMIX_SUCCESS;
}

pmix_status_t pmix_bfrops_base_unpack_ptr(pmix_buffer_t *buffer, void *dest,
                                          int32_t *num_vals, pmix_data_type_t type)
{
    uint8_t foo=1;
    int32_t cnt=1;

    /* it obviously makes no sense to pack a pointer and
     * send it somewhere else, so we just unpack the sentinel */
    return pmix_bfrops_base_unpack_byte(buffer, &foo, &cnt, PMIX_UINT8);
}

pmix_status_t pmix_bfrops_base_unpack_scope(pmix_buffer_t *buffer, void *dest,
                                            int32_t *num_vals, pmix_data_type_t type)
{
    return pmix_bfrops_base_unpack_byte(buffer, dest, num_vals, PMIX_UINT8);
}

pmix_status_t pmix_bfrops_base_unpack_range(pmix_buffer_t *buffer, void *dest,
                                            int32_t *num_vals, pmix_data_type_t type)
{
    return pmix_bfrops_base_unpack_byte(buffer, dest, num_vals, PMIX_UINT8);
}

pmix_status_t pmix_bfrops_base_unpack_cmd(pmix_buffer_t *buffer, void *dest,
                                          int32_t *num_vals, pmix_data_type_t type)
{
    return pmix_bfrops_base_unpack_byte(buffer, dest, num_vals, PMIX_UINT8);
}

pmix_status_t pmix_bfrops_base_unpack_info_directives(pmix_buffer_t *buffer, void *dest,
                                                      int32_t *num_vals, pmix_data_type_t type)
{
    return pmix_bfrops_base_unpack_int32(buffer, dest, num_vals, PMIX_UINT32);
}

pmix_status_t pmix_bfrops_base_unpack_pstate(pmix_buffer_t *buffer, void *dest,
                                             int32_t *num_vals, pmix_data_type_t type)
{
    return pmix_bfrops_base_unpack_byte(buffer, dest, num_vals, PMIX_UINT8);
}


pmix_status_t pmix_bfrops_base_unpack_pinfo(pmix_buffer_t *buffer, void *dest,
                                            int32_t *num_vals, pmix_data_type_t type)
{
    pmix_proc_info_t *ptr;
    int32_t i, n, m;
    pmix_status_t ret;

    pmix_output_verbose(20, pmix_bfrops_base_framework.framework_output,
                        "pmix_bfrop_unpack: %d pinfo", *num_vals);

    ptr = (pmix_proc_info_t *) dest;
    n = *num_vals;

    for (i = 0; i < n; ++i) {
        PMIX_PROC_INFO_CONSTRUCT(&ptr[i]);
        /* unpack the proc */
        m=1;
        if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_proc(buffer, &ptr[i].proc, &m, PMIX_PROC))) {
            return ret;
        }
        /* unpack the hostname */
        m=1;
        if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_string(buffer, &ptr[i].hostname, &m, PMIX_STRING))) {
            return ret;
        }
        /* unpack the executable */
        m=1;
        if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_string(buffer, &ptr[i].executable_name, &m, PMIX_STRING))) {
            return ret;
        }
        /* unpack pid */
         m=1;
         if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_pid(buffer, &ptr[i].pid, &m, PMIX_PID))) {
            return ret;
        }
        /* unpack state */
         m=1;
         if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_pstate(buffer, &ptr[i].state, &m, PMIX_PROC_STATE))) {
            return ret;
        }
    }
    return PMIX_SUCCESS;
}

pmix_status_t pmix_bfrops_base_unpack_darray(pmix_buffer_t *buffer, void *dest,
                                             int32_t *num_vals, pmix_data_type_t type)
{
    pmix_data_array_t *ptr;
    int32_t i, n, m;
    pmix_status_t ret;

    pmix_output_verbose(20, pmix_bfrops_base_framework.framework_output,
                        "pmix_bfrop_unpack: %d data arrays", *num_vals);

    ptr = (pmix_data_array_t *) dest;
    n = *num_vals;

    for (i = 0; i < n; ++i) {
        memset(&ptr[i], 0, sizeof(pmix_data_array_t));
        /* unpack the type */
        m=1;
        if (PMIX_SUCCESS != (ret = pmix_bfrop_get_data_type(buffer, &ptr[i].type))) {
            return ret;
        }
        /* unpack the number of array elements */
        m=1;
        if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_sizet(buffer, &ptr[i].size, &m, PMIX_SIZE))) {
            return ret;
        }
        if (0 == ptr[i].size || PMIX_UNDEF == ptr[i].type) {
            /* nothing else to do */
            continue;
        }
        /* allocate storage for the array and unpack the array elements */
        m = ptr[i].size;
        switch(ptr[i].type) {
            case PMIX_BOOL:
                ptr[i].array = (bool*)malloc(m * sizeof(bool));
                if (NULL == ptr[i].array) {
                    return PMIX_ERR_NOMEM;
                }
                if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_bool(buffer, ptr[i].array, &m, PMIX_BOOL))) {
                    return ret;
                }
                break;
            case PMIX_BYTE:
            case PMIX_INT8:
            case PMIX_UINT8:
                ptr[i].array = (uint8_t*)malloc(m * sizeof(uint8_t));
                if (NULL == ptr[i].array) {
                    return PMIX_ERR_NOMEM;
                }
                if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_byte(buffer, ptr[i].array, &m, ptr[i].type))) {
                    return ret;
                }
                break;
            case PMIX_INT16:
            case PMIX_UINT16:
                ptr[i].array = (uint16_t*)malloc(m * sizeof(uint16_t));
                if (NULL == ptr[i].array) {
                    return PMIX_ERR_NOMEM;
                }
                if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_int16(buffer, ptr[i].array, &m, ptr[i].type))) {
                    return ret;
                }
                break;
            case PMIX_INT32:
            case PMIX_UINT32:
                ptr[i].array = (uint32_t*)malloc(m * sizeof(uint32_t));
                if (NULL == ptr[i].array) {
                    return PMIX_ERR_NOMEM;
                }
                if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_int32(buffer, ptr[i].array, &m, ptr[i].type))) {
                    return ret;
                }
                break;
            case PMIX_INT64:
            case PMIX_UINT64:
                ptr[i].array = (uint64_t*)malloc(m * sizeof(uint64_t));
                if (NULL == ptr[i].array) {
                    return PMIX_ERR_NOMEM;
                }
                if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_int64(buffer, ptr[i].array, &m, ptr[i].type))) {
                    return ret;
                }
                break;
            case PMIX_STRING:
                ptr[i].array = (char**)malloc(m * sizeof(char*));
                if (NULL == ptr[i].array) {
                    return PMIX_ERR_NOMEM;
                }
                if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_string(buffer, ptr[i].array, &m, ptr[i].type))) {
                    return ret;
                }
                break;
            case PMIX_SIZE:
                ptr[i].array = (size_t*)malloc(m * sizeof(size_t));
                if (NULL == ptr[i].array) {
                    return PMIX_ERR_NOMEM;
                }
                if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_sizet(buffer, ptr[i].array, &m, ptr[i].type))) {
                    return ret;
                }
                break;
            case PMIX_PID:
                ptr[i].array = (pid_t*)malloc(m * sizeof(pid_t));
                if (NULL == ptr[i].array) {
                    return PMIX_ERR_NOMEM;
                }
                if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_pid(buffer, ptr[i].array, &m, ptr[i].type))) {
                    return ret;
                }
                break;
            case PMIX_INT:
            case PMIX_UINT:
                ptr[i].array = (int*)malloc(m * sizeof(int));
                if (NULL == ptr[i].array) {
                    return PMIX_ERR_NOMEM;
                }
                if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_int(buffer, ptr[i].array, &m, ptr[i].type))) {
                    return ret;
                }
                break;
            case PMIX_FLOAT:
                ptr[i].array = (float*)malloc(m * sizeof(float));
                if (NULL == ptr[i].array) {
                    return PMIX_ERR_NOMEM;
                }
                if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_float(buffer, ptr[i].array, &m, ptr[i].type))) {
                    return ret;
                }
                break;
            case PMIX_DOUBLE:
                ptr[i].array = (double*)malloc(m * sizeof(double));
                if (NULL == ptr[i].array) {
                    return PMIX_ERR_NOMEM;
                }
                if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_double(buffer, ptr[i].array, &m, ptr[i].type))) {
                    return ret;
                }
                break;
            case PMIX_TIMEVAL:
                ptr[i].array = (struct timeval *)malloc(m * sizeof(struct timeval));
                if (NULL == ptr[i].array) {
                    return PMIX_ERR_NOMEM;
                }
                if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_timeval(buffer, ptr[i].array, &m, ptr[i].type))) {
                    return ret;
                }
                break;
            case PMIX_TIME:
                ptr[i].array = (time_t*)malloc(m * sizeof(time_t));
                if (NULL == ptr[i].array) {
                    return PMIX_ERR_NOMEM;
                }
                if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_time(buffer, ptr[i].array, &m, ptr[i].type))) {
                    return ret;
                }
                break;
            case PMIX_STATUS:
                ptr[i].array = (pmix_status_t*)malloc(m * sizeof(pmix_status_t));
                if (NULL == ptr[i].array) {
                    return PMIX_ERR_NOMEM;
                }
                if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_status(buffer, ptr[i].array, &m, ptr[i].type))) {
                    return ret;
                }
                break;
            case PMIX_INFO:
                ptr[i].array = (pmix_info_t*)malloc(m * sizeof(pmix_info_t));
                if (NULL == ptr[i].array) {
                    return PMIX_ERR_NOMEM;
                }
                if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_info(buffer, ptr[i].array, &m, ptr[i].type))) {
                    return ret;
                }
                break;
            case PMIX_PROC:
                ptr[i].array = (pmix_proc_t*)malloc(m * sizeof(pmix_proc_t));
                if (NULL == ptr[i].array) {
                    return PMIX_ERR_NOMEM;
                }
                if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_proc(buffer, ptr[i].array, &m, ptr[i].type))) {
                    return ret;
                }
                break;
            case PMIX_BYTE_OBJECT:
            case PMIX_COMPRESSED_STRING:
                ptr[i].array = (pmix_byte_object_t*)malloc(m * sizeof(pmix_byte_object_t));
                if (NULL == ptr[i].array) {
                    return PMIX_ERR_NOMEM;
                }
                if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_bo(buffer, ptr[i].array, &m, ptr[i].type))) {
                    return ret;
                }
                break;
            case PMIX_PERSIST:
                ptr[i].array = (pmix_persistence_t*)malloc(m * sizeof(pmix_persistence_t));
                if (NULL == ptr[i].array) {
                    return PMIX_ERR_NOMEM;
                }
                if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_persist(buffer, ptr[i].array, &m, ptr[i].type))) {
                    return ret;
                }
                break;
            case PMIX_POINTER:
                ptr[i].array = (char*)malloc(m * sizeof(char*));
                if (NULL == ptr[i].array) {
                    return PMIX_ERR_NOMEM;
                }
                if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_ptr(buffer, ptr[i].array, &m, PMIX_POINTER))) {
                    return ret;
                }
                break;
            case PMIX_SCOPE:
                ptr[i].array = (pmix_scope_t*)malloc(m * sizeof(pmix_scope_t));
                if (NULL == ptr[i].array) {
                    return PMIX_ERR_NOMEM;
                }
                if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_scope(buffer, ptr[i].array, &m, ptr[i].type))) {
                    return ret;
                }
                break;
            case PMIX_DATA_RANGE:
                ptr[i].array = (pmix_data_range_t*)malloc(m * sizeof(pmix_data_range_t));
                if (NULL == ptr[i].array) {
                    return PMIX_ERR_NOMEM;
                }
                if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_range(buffer, ptr[i].array, &m, ptr[i].type))) {
                    return ret;
                }
                break;
            case PMIX_PROC_STATE:
                ptr[i].array = (pmix_proc_state_t*)malloc(m * sizeof(pmix_proc_state_t));
                if (NULL == ptr[i].array) {
                    return PMIX_ERR_NOMEM;
                }
                if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_pstate(buffer, ptr[i].array, &m, ptr[i].type))) {
                    return ret;
                }
                break;
            case PMIX_PROC_INFO:
                ptr[i].array = (pmix_proc_info_t*)malloc(m * sizeof(pmix_proc_info_t));
                if (NULL == ptr[i].array) {
                    return PMIX_ERR_NOMEM;
                }
                if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_pinfo(buffer, ptr[i].array, &m, ptr[i].type))) {
                    return ret;
                }
                break;
            case PMIX_QUERY:
                ptr[i].array = (pmix_query_t*)malloc(m * sizeof(pmix_query_t));
                if (NULL == ptr[i].array) {
                    return PMIX_ERR_NOMEM;
                }
                if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_query(buffer, ptr[i].array, &m, ptr[i].type))) {
                    return ret;
                }
                break;
            case PMIX_VALUE:
                ptr[i].array = (pmix_value_t*)malloc(m * sizeof(pmix_value_t));
                if (NULL == ptr[i].array) {
                    return PMIX_ERR_NOMEM;
                }
                if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_value(buffer, ptr[i].array, &m, ptr[i].type))) {
                    return ret;
                }
                break;
            case PMIX_ENVAR:
                ptr[i].array = (pmix_envar_t*)malloc(m * sizeof(pmix_envar_t));
                if (NULL == ptr[i].array) {
                    return PMIX_ERR_NOMEM;
                }
                if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_envar(buffer, ptr[i].array, &m, ptr[i].type))) {
                    return ret;
                }
                break;
            /**** DEPRECATED ****/
            case PMIX_INFO_ARRAY:
                ptr[i].array = (pmix_info_array_t*)malloc(m * sizeof(pmix_info_array_t));
                if (NULL == ptr[i].array) {
                    return PMIX_ERR_NOMEM;
                }
                if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_array(buffer, ptr[i].array, &m, ptr[i].type))) {
                    return ret;
                }
                break;
            /********************/
            default:
                return PMIX_ERR_NOT_SUPPORTED;
        }
    }
    return PMIX_SUCCESS;
}

pmix_status_t pmix_bfrops_base_unpack_rank(pmix_buffer_t *buffer, void *dest,
                                           int32_t *num_vals, pmix_data_type_t type)
{
    return pmix_bfrops_base_unpack_int32(buffer, dest, num_vals, PMIX_UINT32);
}

pmix_status_t pmix_bfrops_base_unpack_query(pmix_buffer_t *buffer, void *dest,
                                            int32_t *num_vals, pmix_data_type_t type)
{
    pmix_query_t *ptr;
    int32_t i, n, m;
    pmix_status_t ret;
    int32_t nkeys;

    pmix_output_verbose(20, pmix_bfrops_base_framework.framework_output,
                        "pmix_bfrop_unpack: %d queries", *num_vals);

    ptr = (pmix_query_t *) dest;
    n = *num_vals;

    for (i = 0; i < n; ++i) {
        PMIX_QUERY_CONSTRUCT(&ptr[i]);
        /* unpack the number of keys */
        m=1;
        if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_int32(buffer, &nkeys, &m, PMIX_INT32))) {
            return ret;
        }
        if (0 < nkeys) {
            /* unpack the keys */
            if (NULL == (ptr[i].keys = (char**)calloc(nkeys+1, sizeof(char*)))) {
                return PMIX_ERR_NOMEM;
            }
            /* unpack keys */
            m=nkeys;
            if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_string(buffer, ptr[i].keys, &m, PMIX_STRING))) {
                return ret;
            }
        }
        /* unpack the number of qualifiers */
        m=1;
        if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_sizet(buffer, &ptr[i].nqual, &m, PMIX_SIZE))) {
            return ret;
        }
        if (0 < ptr[i].nqual) {
            /* unpack the qualifiers */
            PMIX_INFO_CREATE(ptr[i].qualifiers, ptr[i].nqual);
            m =  ptr[i].nqual;
            if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_info(buffer, ptr[i].qualifiers, &m, PMIX_INFO))) {
                return ret;
            }
        }
    }
    return PMIX_SUCCESS;
}

pmix_status_t pmix_bfrops_base_unpack_alloc_directive(pmix_buffer_t *buffer, void *dest,
                                                      int32_t *num_vals, pmix_data_type_t type)
{
    return pmix_bfrops_base_unpack_byte(buffer, dest, num_vals, PMIX_UINT8);
}

pmix_status_t pmix_bfrops_base_unpack_iof_channel(pmix_buffer_t *buffer, void *dest,
                                                  int32_t *num_vals, pmix_data_type_t type)
{
    return pmix_bfrops_base_unpack_int16(buffer, dest, num_vals, PMIX_UINT16);
}

pmix_status_t pmix_bfrops_base_unpack_envar(pmix_buffer_t *buffer, void *dest,
                                            int32_t *num_vals, pmix_data_type_t type)
{
    pmix_envar_t *ptr;
    int32_t i, n, m;
    pmix_status_t ret;

    pmix_output_verbose(20, pmix_bfrops_base_framework.framework_output,
                        "pmix_bfrop_unpack: %d envars", *num_vals);

    ptr = (pmix_envar_t *) dest;
    n = *num_vals;

    for (i = 0; i < n; ++i) {
        PMIX_ENVAR_CONSTRUCT(&ptr[i]);
        /* unpack the name */
        m=1;
        if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_string(buffer, &ptr[i].envar, &m, PMIX_STRING))) {
            return ret;
        }
        /* unpack the value */
        m=1;
        if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_string(buffer, &ptr[i].value, &m, PMIX_STRING))) {
            return ret;
        }
        /* unpack the separator */
        m=1;
        if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_byte(buffer, &ptr[i].separator, &m, PMIX_BYTE))) {
            return ret;
        }
    }
    return PMIX_SUCCESS;
}

/**** DEPRECATED ****/
pmix_status_t pmix_bfrops_base_unpack_array(pmix_buffer_t *buffer, void *dest,
                                            int32_t *num_vals, pmix_data_type_t type)
{
    pmix_info_array_t *ptr;
    int32_t i, n, m;
    pmix_status_t ret;

    pmix_output_verbose(20, pmix_bfrops_base_framework.framework_output,
                        "pmix_bfrop_unpack: %d info arrays", *num_vals);

    ptr = (pmix_info_array_t*) dest;
    n = *num_vals;

    for (i = 0; i < n; ++i) {
        pmix_output_verbose(20, pmix_bfrops_base_framework.framework_output,
                            "pmix_bfrop_unpack: init array[%d]", i);
        memset(&ptr[i], 0, sizeof(pmix_info_array_t));
        /* unpack the size of this array */
        m=1;
        if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_sizet(buffer, &ptr[i].size, &m, PMIX_SIZE))) {
            return ret;
        }
        if (0 < ptr[i].size) {
            ptr[i].array = (pmix_info_t*)malloc(ptr[i].size * sizeof(pmix_info_t));
            m=ptr[i].size;
            if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_value(buffer, ptr[i].array, &m, PMIX_INFO))) {
                return ret;
            }
        }
    }
    return PMIX_SUCCESS;
}
