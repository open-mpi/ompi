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
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015-2019 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2016-2019 Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "src/include/pmix_config.h"

#include "src/hwloc/pmix_hwloc.h"
#include "src/include/pmix_globals.h"
#include "src/mca/preg/preg.h"
#include "src/util/pmix_argv.h"
#include "src/util/pmix_error.h"
#include "src/util/pmix_output.h"

#include "src/mca/bfrops/base/base.h"
#include "src/mca/bfrops/bfrops_types.h"

static pmix_status_t pmix_bfrops_base_unpack_buffer(pmix_pointer_array_t *regtypes,
                                                    pmix_buffer_t *buffer, void *dst,
                                                    int32_t *num_vals, pmix_data_type_t type)
{
    pmix_status_t rc;
    pmix_data_type_t local_type;

    pmix_output_verbose(20, pmix_bfrops_base_framework.framework_output,
                        "pmix_bfrops_base_unpack_buffer( %p, %p, %lu, %d )\n", (void *) buffer, dst,
                        (long unsigned int) *num_vals, (int) type);

    /** Unpack the declared data type */
    if (PMIX_BFROP_BUFFER_FULLY_DESC == buffer->type) {
        if (PMIX_SUCCESS != (rc = pmix_bfrop_get_data_type(regtypes, buffer, &local_type))) {
            PMIX_ERROR_LOG(rc);
            return rc;
        }
        /* if the data types don't match, then return an error */
        if (type != local_type) {
            pmix_output_verbose(20, pmix_bfrops_base_framework.framework_output,
                                "PMIX bfrop:unpack: got type %s when expecting type %s",
                                PMIx_Data_type_string(local_type),
                                PMIx_Data_type_string(type));
            return PMIX_ERR_PACK_MISMATCH;
        }
    }
    PMIX_BFROPS_UNPACK_TYPE(rc, buffer, dst, num_vals, type, regtypes);
    return rc;
}

pmix_status_t pmix_bfrops_base_unpack(pmix_pointer_array_t *regtypes, pmix_buffer_t *buffer,
                                      void *dst, int32_t *num_vals, pmix_data_type_t type)
{
    pmix_status_t rc, ret;
    int32_t local_num, n = 1;
    pmix_data_type_t local_type;

    /* check for error */
    if (NULL == buffer || NULL == dst || NULL == num_vals) {
        pmix_output(0, "SOMEONE IS NULL: buffer %s dst %s num_vals %s",
            (NULL == buffer) ? "NULL" : "GOOD",
            (NULL == dst) ? "NULL" : "GOOD",
            (NULL == num_vals) ? "NULL" : "GOOD");
        return PMIX_ERR_BAD_PARAM;
    }

    /* if user provides a zero for num_vals, then there is no storage allocated
     * so return an appropriate error
     */
    if (0 == *num_vals) {
        pmix_output_verbose(20, pmix_bfrops_base_framework.framework_output,
                            "pmix_bfrop_unpack: inadequate space ( %p, %p, %lu, %d )\n",
                            (void *) buffer, dst, (long unsigned int) *num_vals, (int) type);
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
        if (PMIX_SUCCESS != (rc = pmix_bfrop_get_data_type(regtypes, buffer, &local_type))) {
            *num_vals = 0;
            /* don't error log here as the user may be unpacking past
             * the end of the buffer, which isn't necessarily an error */
            return rc;
        }
        if (PMIX_INT32 != local_type) { /* if the length wasn't first, then error */
            *num_vals = 0;
            return PMIX_ERR_UNPACK_FAILURE;
        }
    }

    n = 1;
    PMIX_BFROPS_UNPACK_TYPE(rc, buffer, &local_num, &n, PMIX_INT32, regtypes);
    if (PMIX_SUCCESS != rc) {
        *num_vals = 0;
        /* don't error log here as the user may be unpacking past
         * the end of the buffer, which isn't necessarily an error */
        return rc;
    }
    pmix_output_verbose(20, pmix_bfrops_base_framework.framework_output,
                        "pmix_bfrop_unpack: found %d values for %d provided storage", local_num,
                        *num_vals);

    /** if the storage provided is inadequate, set things up
     * to unpack as much as we can and to return an error code
     * indicating that everything was not unpacked - the buffer
     * is left in a state where it can not be further unpacked.
     */
    if (local_num > *num_vals) {
        local_num = *num_vals;
        pmix_output_verbose(20, pmix_bfrops_base_framework.framework_output,
                            "pmix_bfrop_unpack: inadequate space ( %p, %p, %lu, %d )\n",
                            (void *) buffer, dst, (long unsigned int) *num_vals, (int) type);
        ret = PMIX_ERR_UNPACK_INADEQUATE_SPACE;
    } else {                   /** enough or more than enough storage */
        *num_vals = local_num; /** let the user know how many we actually unpacked */
        ret = PMIX_SUCCESS;
    }

    /** Unpack the value(s) */
    rc = pmix_bfrops_base_unpack_buffer(regtypes, buffer, dst, &local_num, type);
    if (PMIX_SUCCESS != rc) {
        *num_vals = 0;
        ret = rc;
    }

    return ret;
}

/* UNPACK GENERIC SYSTEM TYPES */

/*
 * BOOL
 */
pmix_status_t pmix_bfrops_base_unpack_bool(pmix_pointer_array_t *regtypes, pmix_buffer_t *buffer,
                                           void *dest, int32_t *num_vals, pmix_data_type_t type)
{
    int32_t i;
    uint8_t *src;
    bool *dst;

    pmix_output_verbose(20, pmix_bfrops_base_framework.framework_output,
                        "pmix_bfrop_unpack_bool * %d\n", (int) *num_vals);

    PMIX_HIDE_UNUSED_PARAMS(regtypes, type);

    /* check to see if there's enough data in buffer */
    if (pmix_bfrop_too_small(buffer, *num_vals)) {
        return PMIX_ERR_UNPACK_READ_PAST_END_OF_BUFFER;
    }

    /* unpack the data */
    src = (uint8_t *) buffer->unpack_ptr;
    dst = (bool *) dest;

    for (i = 0; i < *num_vals; i++) {
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
pmix_status_t pmix_bfrops_base_unpack_int(pmix_pointer_array_t *regtypes, pmix_buffer_t *buffer,
                                          void *dest, int32_t *num_vals, pmix_data_type_t type)
{
    pmix_status_t ret;
    pmix_data_type_t remote_type;

    PMIX_HIDE_UNUSED_PARAMS(type);

    if (PMIX_SUCCESS != (ret = pmix_bfrop_get_data_type(regtypes, buffer, &remote_type))) {
        return ret;
    }

    if (remote_type == BFROP_TYPE_INT) {
        /* fast path it if the sizes are the same */
        /* Turn around and unpack the real type */
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, dest, num_vals, BFROP_TYPE_INT, regtypes);
    } else {
        /* slow path - types are different sizes */
        PMIX_BFROP_UNPACK_SIZE_MISMATCH(regtypes, int, remote_type, ret);
    }

    return ret;
}

/*
 * SIZE_T
 */
pmix_status_t pmix_bfrops_base_unpack_sizet(pmix_pointer_array_t *regtypes, pmix_buffer_t *buffer,
                                            void *dest, int32_t *num_vals, pmix_data_type_t type)
{
    pmix_status_t ret;
    pmix_data_type_t remote_type;

    if (PMIX_SIZE != type) {
        return PMIX_ERR_BAD_PARAM;
    }

    PMIX_HIDE_UNUSED_PARAMS(type);

    if (PMIX_SUCCESS != (ret = pmix_bfrop_get_data_type(regtypes, buffer, &remote_type))) {
        PMIX_ERROR_LOG(ret);
        return ret;
    }

    if (remote_type == BFROP_TYPE_SIZE_T) {
        /* fast path it if the sizes are the same */
        /* Turn around and unpack the real type */
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, dest, num_vals, BFROP_TYPE_SIZE_T, regtypes);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
        }
    } else {
        /* slow path - types are different sizes */
        PMIX_BFROP_UNPACK_SIZE_MISMATCH(regtypes, size_t, remote_type, ret);
    }
    return ret;
}

/*
 * PID_T
 */
pmix_status_t pmix_bfrops_base_unpack_pid(pmix_pointer_array_t *regtypes, pmix_buffer_t *buffer,
                                          void *dest, int32_t *num_vals, pmix_data_type_t type)
{
    pmix_status_t ret;
    pmix_data_type_t remote_type;

    PMIX_HIDE_UNUSED_PARAMS(type);

    if (PMIX_SUCCESS != (ret = pmix_bfrop_get_data_type(regtypes, buffer, &remote_type))) {
        return ret;
    }

    if (remote_type == BFROP_TYPE_PID_T) {
        /* fast path it if the sizes are the same */
        /* Turn around and unpack the real type */
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, dest, num_vals, BFROP_TYPE_PID_T, regtypes);
    } else {
        /* slow path - types are different sizes */
        PMIX_BFROP_UNPACK_SIZE_MISMATCH(regtypes, pid_t, remote_type, ret);
    }

    return ret;
}

/* UNPACK FUNCTIONS FOR NON-GENERIC SYSTEM TYPES */

/*
 * BYTE, CHAR, INT8
 */
pmix_status_t pmix_bfrops_base_unpack_byte(pmix_pointer_array_t *regtypes, pmix_buffer_t *buffer,
                                           void *dest, int32_t *num_vals, pmix_data_type_t type)
{
    pmix_output_verbose(20, pmix_bfrops_base_framework.framework_output,
                        "pmix_bfrop_unpack_byte * %d\n", (int) *num_vals);

    PMIX_HIDE_UNUSED_PARAMS(regtypes, type);

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

pmix_status_t pmix_bfrops_base_unpack_int16(pmix_pointer_array_t *regtypes, pmix_buffer_t *buffer,
                                            void *dest, int32_t *num_vals, pmix_data_type_t type)
{
    int32_t i;
    uint16_t tmp, *desttmp = (uint16_t *) dest;

    pmix_output_verbose(20, pmix_bfrops_base_framework.framework_output,
                        "pmix_bfrop_unpack_int16 * %d\n", (int) *num_vals);

    PMIX_HIDE_UNUSED_PARAMS(regtypes, type);

    /* check to see if there's enough data in buffer */
    if (pmix_bfrop_too_small(buffer, (*num_vals) * sizeof(tmp))) {
        return PMIX_ERR_UNPACK_READ_PAST_END_OF_BUFFER;
    }

    /* unpack the data */
    for (i = 0; i < (*num_vals); ++i) {
        memcpy(&(tmp), buffer->unpack_ptr, sizeof(tmp));
        tmp = pmix_ntohs(tmp);
        memcpy(&desttmp[i], &tmp, sizeof(tmp));
        buffer->unpack_ptr += sizeof(tmp);
    }

    return PMIX_SUCCESS;
}

pmix_status_t pmix_bfrops_base_unpack_int32(pmix_pointer_array_t *regtypes, pmix_buffer_t *buffer,
                                            void *dest, int32_t *num_vals, pmix_data_type_t type)
{
    int32_t i;
    uint32_t tmp, *desttmp = (uint32_t *) dest;

    pmix_output_verbose(20, pmix_bfrops_base_framework.framework_output,
                        "pmix_bfrop_unpack_int32 * %d\n", (int) *num_vals);

    PMIX_HIDE_UNUSED_PARAMS(regtypes, type);

    /* check to see if there's enough data in buffer */
    if (pmix_bfrop_too_small(buffer, (*num_vals) * sizeof(tmp))) {
        return PMIX_ERR_UNPACK_READ_PAST_END_OF_BUFFER;
    }

    /* unpack the data */
    for (i = 0; i < (*num_vals); ++i) {
        memcpy(&(tmp), buffer->unpack_ptr, sizeof(tmp));
        tmp = ntohl(tmp);
        memcpy(&desttmp[i], &tmp, sizeof(tmp));
        buffer->unpack_ptr += sizeof(tmp);
    }

    return PMIX_SUCCESS;
}

pmix_status_t pmix_bfrops_base_unpack_datatype(pmix_pointer_array_t *regtypes,
                                               pmix_buffer_t *buffer, void *dest, int32_t *num_vals,
                                               pmix_data_type_t type)
{
    pmix_status_t ret;

    PMIX_HIDE_UNUSED_PARAMS(type);

    PMIX_BFROPS_UNPACK_TYPE(ret, buffer, dest, num_vals, PMIX_INT16, regtypes);
    return ret;
}

pmix_status_t pmix_bfrops_base_unpack_int64(pmix_pointer_array_t *regtypes, pmix_buffer_t *buffer,
                                            void *dest, int32_t *num_vals, pmix_data_type_t type)
{
    int32_t i;
    uint64_t tmp, *desttmp = (uint64_t *) dest;

    pmix_output_verbose(20, pmix_bfrops_base_framework.framework_output,
                        "pmix_bfrop_unpack_int64 * %d\n", (int) *num_vals);

    PMIX_HIDE_UNUSED_PARAMS(regtypes, type);

    /* check to see if there's enough data in buffer */
    if (pmix_bfrop_too_small(buffer, (*num_vals) * sizeof(tmp))) {
        return PMIX_ERR_UNPACK_READ_PAST_END_OF_BUFFER;
    }

    /* unpack the data */
    for (i = 0; i < (*num_vals); ++i) {
        memcpy(&(tmp), buffer->unpack_ptr, sizeof(tmp));
        tmp = pmix_ntoh64(tmp);
        memcpy(&desttmp[i], &tmp, sizeof(tmp));
        buffer->unpack_ptr += sizeof(tmp);
    }

    return PMIX_SUCCESS;
}

pmix_status_t pmix_bfrops_base_unpack_string(pmix_pointer_array_t *regtypes, pmix_buffer_t *buffer,
                                             void *dest, int32_t *num_vals, pmix_data_type_t type)
{
    pmix_status_t ret;
    int32_t i, len, n = 1;
    char **sdest = (char **) dest;

    PMIX_HIDE_UNUSED_PARAMS(type);

    for (i = 0; i < (*num_vals); ++i) {
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &len, &n, PMIX_INT32, regtypes);
        if (PMIX_SUCCESS != ret) {
            return ret;
        }
        if (0 == len) { /* zero-length string - unpack the NULL */
            sdest[i] = NULL;
        } else {
            sdest[i] = (char *) malloc(len); // NULL terminator is included
            if (NULL == sdest[i]) {
                return PMIX_ERR_OUT_OF_RESOURCE;
            }
            PMIX_BFROPS_UNPACK_TYPE(ret, buffer, sdest[i], &len, PMIX_BYTE, regtypes);
            if (PMIX_SUCCESS != ret) {
                return ret;
            }
        }
    }

    return PMIX_SUCCESS;
}

pmix_status_t pmix_bfrops_base_unpack_float(pmix_pointer_array_t *regtypes, pmix_buffer_t *buffer,
                                            void *dest, int32_t *num_vals, pmix_data_type_t type)
{
    int32_t i, n;
    float *desttmp = (float *) dest, tmp;
    pmix_status_t ret;
    char *convert;

    pmix_output_verbose(20, pmix_bfrops_base_framework.framework_output,
                        "pmix_bfrop_unpack_float * %d\n", (int) *num_vals);

    PMIX_HIDE_UNUSED_PARAMS(type);

    /* unpack the data */
    for (i = 0; i < (*num_vals); ++i) {
        n = 1;
        convert = NULL;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &convert, &n, PMIX_STRING, regtypes);
        if (PMIX_SUCCESS != ret) {
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

pmix_status_t pmix_bfrops_base_unpack_double(pmix_pointer_array_t *regtypes, pmix_buffer_t *buffer,
                                             void *dest, int32_t *num_vals, pmix_data_type_t type)
{
    int32_t i, n;
    double *desttmp = (double *) dest, tmp;
    pmix_status_t ret;
    char *convert;

    pmix_output_verbose(20, pmix_bfrops_base_framework.framework_output,
                        "pmix_bfrop_unpack_double * %d\n", (int) *num_vals);

    PMIX_HIDE_UNUSED_PARAMS(type);

    /* unpack the data */
    for (i = 0; i < (*num_vals); ++i) {
        n = 1;
        convert = NULL;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &convert, &n, PMIX_STRING, regtypes);
        if (PMIX_SUCCESS != ret) {
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

pmix_status_t pmix_bfrops_base_unpack_timeval(pmix_pointer_array_t *regtypes, pmix_buffer_t *buffer,
                                              void *dest, int32_t *num_vals, pmix_data_type_t type)
{
    int32_t i, n;
    int64_t tmp[2];
    struct timeval *desttmp = (struct timeval *) dest, tt;
    pmix_status_t ret;

    pmix_output_verbose(20, pmix_bfrops_base_framework.framework_output,
                        "pmix_bfrop_unpack_timeval * %d\n", (int) *num_vals);

    PMIX_HIDE_UNUSED_PARAMS(type);

    /* unpack the data */
    for (i = 0; i < (*num_vals); ++i) {
        n = 2;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, tmp, &n, PMIX_INT64, regtypes);
        if (PMIX_SUCCESS != ret) {
            return ret;
        }
        tt.tv_sec = tmp[0];
        tt.tv_usec = tmp[1];
        memcpy(&desttmp[i], &tt, sizeof(tt));
    }
    return PMIX_SUCCESS;
}

pmix_status_t pmix_bfrops_base_unpack_time(pmix_pointer_array_t *regtypes, pmix_buffer_t *buffer,
                                           void *dest, int32_t *num_vals, pmix_data_type_t type)
{
    int32_t i, n;
    time_t *desttmp = (time_t *) dest, tmp;
    pmix_status_t ret;
    uint64_t ui64;

    /* time_t is a system-dependent size, so cast it
     * to uint64_t as a generic safe size
     */

    pmix_output_verbose(20, pmix_bfrops_base_framework.framework_output,
                        "pmix_bfrop_unpack_time * %d\n", (int) *num_vals);

    PMIX_HIDE_UNUSED_PARAMS(type);

    /* unpack the data */
    for (i = 0; i < (*num_vals); ++i) {
        n = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ui64, &n, PMIX_UINT64, regtypes);
        if (PMIX_SUCCESS != ret) {
            return ret;
        }
        tmp = (time_t) ui64;
        memcpy(&desttmp[i], &tmp, sizeof(tmp));
    }
    return PMIX_SUCCESS;
}

pmix_status_t pmix_bfrops_base_unpack_status(pmix_pointer_array_t *regtypes, pmix_buffer_t *buffer,
                                             void *dest, int32_t *num_vals, pmix_data_type_t type)
{
    pmix_status_t ret;

    pmix_output_verbose(20, pmix_bfrops_base_framework.framework_output,
                        "pmix_bfrop_unpack_status * %d\n", (int) *num_vals);

    PMIX_HIDE_UNUSED_PARAMS(type);

    /* unpack the data */
    PMIX_BFROPS_UNPACK_TYPE(ret, buffer, dest, num_vals, PMIX_INT32, regtypes);
    return ret;
}

/* UNPACK FUNCTIONS FOR GENERIC PMIX TYPES */

/*
 * PMIX_VALUE
 */
pmix_status_t pmix_bfrops_base_unpack_val(pmix_pointer_array_t *regtypes, pmix_buffer_t *buffer,
                                          pmix_value_t *val)
{
    int m;
    pmix_status_t ret = PMIX_SUCCESS;

    m = 1;
    switch (val->type) {
        case PMIX_UNDEF:
            break;
        case PMIX_PROC:
            /* this field is now a pointer, so we must allocate storage for it */
            PMIX_PROC_CREATE(val->data.proc, m);
            if (NULL == val->data.proc) {
                return PMIX_ERR_NOMEM;
            }
            PMIX_BFROPS_UNPACK_TYPE(ret, buffer, val->data.proc, &m, PMIX_PROC, regtypes);
            break;
        case PMIX_PROC_INFO:
            /* this is now a pointer, so allocate storage for it */
            PMIX_PROC_INFO_CREATE(val->data.pinfo, 1);
            if (NULL == val->data.pinfo) {
                return PMIX_ERR_NOMEM;
            }
            PMIX_BFROPS_UNPACK_TYPE(ret, buffer, val->data.pinfo, &m, PMIX_PROC_INFO, regtypes);
            break;
        case PMIX_DATA_ARRAY:
            /* this is now a pointer, so allocate storage for it */
            val->data.darray = (pmix_data_array_t *) malloc(sizeof(pmix_data_array_t));
            if (NULL == val->data.darray) {
                return PMIX_ERR_NOMEM;
            }
            PMIX_BFROPS_UNPACK_TYPE(ret, buffer, val->data.darray, &m, PMIX_DATA_ARRAY, regtypes);
            break;
        case PMIX_REGATTR:
            val->data.ptr = (pmix_regattr_t *) calloc(1, sizeof(pmix_regattr_t));
            PMIX_BFROPS_UNPACK_TYPE(ret, buffer, val->data.ptr, &m, PMIX_REGATTR, regtypes);
            return ret;
        case PMIX_COORD:
            val->data.coord = (pmix_coord_t *) calloc(1, sizeof(pmix_coord_t));
            if (NULL == val->data.coord) {
                return PMIX_ERR_NOMEM;
            }
            PMIX_BFROPS_UNPACK_TYPE(ret, buffer, val->data.coord, &m, PMIX_COORD, regtypes);
            return ret;
        case PMIX_TOPO:
            PMIX_TOPOLOGY_CREATE(val->data.topo, 1);
            if (NULL == val->data.topo) {
                return PMIX_ERR_NOMEM;
            }
            PMIX_BFROPS_UNPACK_TYPE(ret, buffer, val->data.topo, &m, PMIX_TOPO, regtypes);
            return ret;
        case PMIX_PROC_CPUSET:
            PMIX_CPUSET_CREATE(val->data.cpuset, 1);
            if (NULL == val->data.cpuset) {
                return PMIX_ERR_NOMEM;
            }
            PMIX_BFROPS_UNPACK_TYPE(ret, buffer, val->data.cpuset, &m, PMIX_PROC_CPUSET, regtypes);
            return ret;
        case PMIX_GEOMETRY:
            PMIX_GEOMETRY_CREATE(val->data.geometry, 1);
            if (NULL == val->data.geometry) {
                return PMIX_ERR_NOMEM;
            }
            PMIX_BFROPS_UNPACK_TYPE(ret, buffer, val->data.geometry, &m, PMIX_GEOMETRY, regtypes);
            return ret;
        case PMIX_DEVICE_DIST:
            PMIX_DEVICE_DIST_CREATE(val->data.devdist, 1);
            if (NULL == val->data.devdist) {
                return PMIX_ERR_NOMEM;
            }
            PMIX_BFROPS_UNPACK_TYPE(ret, buffer, val->data.devdist, &m, PMIX_DEVICE_DIST, regtypes);
            return ret;
        case PMIX_ENDPOINT:
            PMIX_ENDPOINT_CREATE(val->data.endpoint, 1);
            if (NULL == val->data.endpoint) {
                return PMIX_ERR_NOMEM;
            }
            PMIX_BFROPS_UNPACK_TYPE(ret, buffer, val->data.endpoint, &m, PMIX_ENDPOINT, regtypes);
            return ret;
        case PMIX_PROC_NSPACE:
            PMIX_PROC_CREATE(val->data.proc, 1);
            if (NULL == val->data.proc) {
                return PMIX_ERR_NOMEM;
            }
            PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &val->data.proc->nspace, &m, PMIX_PROC_NSPACE,
                                    regtypes);
            return ret;
        case PMIX_PROC_STATS:
            PMIX_PROC_STATS_CREATE(val->data.pstats, 1);
            if (NULL == val->data.pstats) {
                return PMIX_ERR_NOMEM;
            }
            PMIX_BFROPS_UNPACK_TYPE(ret, buffer, val->data.pstats, &m, PMIX_PROC_STATS, regtypes);
            return ret;
        case PMIX_DISK_STATS:
            PMIX_DISK_STATS_CREATE(val->data.dkstats, 1);
            if (NULL == val->data.dkstats) {
                return PMIX_ERR_NOMEM;
            }
            PMIX_BFROPS_UNPACK_TYPE(ret, buffer, val->data.dkstats, &m, PMIX_DISK_STATS, regtypes);
            return ret;
        case PMIX_NET_STATS:
            PMIX_NET_STATS_CREATE(val->data.netstats, 1);
            if (NULL == val->data.netstats) {
                return PMIX_ERR_NOMEM;
            }
            PMIX_BFROPS_UNPACK_TYPE(ret, buffer, val->data.netstats, &m, PMIX_NET_STATS, regtypes);
            return ret;
        case PMIX_NODE_STATS:
            PMIX_NODE_STATS_CREATE(val->data.ndstats, 1);
            if (NULL == val->data.ndstats) {
                return PMIX_ERR_NOMEM;
            }
            PMIX_BFROPS_UNPACK_TYPE(ret, buffer, val->data.ndstats, &m, PMIX_NODE_STATS, regtypes);
            return ret;
        case PMIX_DATA_BUFFER:
            PMIX_DATA_BUFFER_CREATE(val->data.ptr);
            if (NULL == val->data.ptr) {
                return PMIX_ERR_NOMEM;
            }
            PMIX_BFROPS_UNPACK_TYPE(ret, buffer, val->data.ptr, &m, PMIX_DATA_BUFFER, regtypes);
            return ret;
        default:
            PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &val->data, &m, val->type, regtypes);
            if (PMIX_ERR_UNKNOWN_DATA_TYPE == ret) {
                pmix_output(0, "UNPACK-PMIX-VALUE: UNSUPPORTED TYPE %d", (int) val->type);
            }
    }

    return ret;
}

pmix_status_t pmix_bfrops_base_unpack_value(pmix_pointer_array_t *regtypes, pmix_buffer_t *buffer,
                                            void *dest, int32_t *num_vals, pmix_data_type_t type)
{
    pmix_value_t *ptr;
    int32_t i, n;
    pmix_status_t ret;

    ptr = (pmix_value_t *) dest;
    n = *num_vals;

    PMIX_HIDE_UNUSED_PARAMS(type);

    for (i = 0; i < n; ++i) {
        /* unpack the type */
        if (PMIX_SUCCESS != (ret = pmix_bfrop_get_data_type(regtypes, buffer, &ptr[i].type))) {
            PMIX_ERROR_LOG(ret);
            return ret;
        }
        /* unpack value */
        if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_val(regtypes, buffer, &ptr[i]))) {
            PMIX_ERROR_LOG(ret);
            return ret;
        }
    }
    return PMIX_SUCCESS;
}

pmix_status_t pmix_bfrops_base_unpack_info(pmix_pointer_array_t *regtypes, pmix_buffer_t *buffer,
                                           void *dest, int32_t *num_vals, pmix_data_type_t type)
{
    pmix_info_t *ptr;
    int32_t i, n, m;
    pmix_status_t ret;
    char *tmp;

    pmix_output_verbose(20, pmix_bfrops_base_framework.framework_output,
                        "pmix_bfrop_unpack: %d info", *num_vals);

    PMIX_HIDE_UNUSED_PARAMS(type);

    ptr = (pmix_info_t *) dest;
    n = *num_vals;

    for (i = 0; i < n; ++i) {
        memset(ptr[i].key, 0, sizeof(ptr[i].key));
        memset(&ptr[i].value, 0, sizeof(pmix_value_t));
        /* unpack key */
        m = 1;
        tmp = NULL;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &tmp, &m, PMIX_STRING, regtypes);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            return ret;
        }
        if (NULL == tmp) {
            return PMIX_ERROR;
        }
        pmix_strncpy(ptr[i].key, tmp, PMIX_MAX_KEYLEN);
        free(tmp);
        /* unpack the directives */
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].flags, &m, PMIX_INFO_DIRECTIVES, regtypes);
        if (PMIX_SUCCESS != ret) {
            return ret;
        }
        /* unpack value - since the value structure is statically-defined
         * instead of a pointer in this struct, we directly unpack it to
         * avoid the malloc */
        if (PMIX_SUCCESS
            != (ret = pmix_bfrop_get_data_type(regtypes, buffer, &ptr[i].value.type))) {
            return ret;
        }
        pmix_output_verbose(20, pmix_bfrops_base_framework.framework_output,
                            "pmix_bfrop_unpack: info type %d", ptr[i].value.type);
        m = 1;
        if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_val(regtypes, buffer, &ptr[i].value))) {
            return ret;
        }
    }
    return PMIX_SUCCESS;
}

pmix_status_t pmix_bfrops_base_unpack_pdata(pmix_pointer_array_t *regtypes, pmix_buffer_t *buffer,
                                            void *dest, int32_t *num_vals, pmix_data_type_t type)
{
    pmix_pdata_t *ptr;
    int32_t i, n, m;
    pmix_status_t ret;
    char *tmp;

    pmix_output_verbose(20, pmix_bfrops_base_framework.framework_output,
                        "pmix_bfrop_unpack: %d pdata", *num_vals);

    PMIX_HIDE_UNUSED_PARAMS(type);

    ptr = (pmix_pdata_t *) dest;
    n = *num_vals;

    for (i = 0; i < n; ++i) {
        PMIX_PDATA_CONSTRUCT(&ptr[i]);
        /* unpack the proc */
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].proc, &m, PMIX_PROC, regtypes);
        if (PMIX_SUCCESS != ret) {
            return ret;
        }
        /* unpack key */
        m = 1;
        tmp = NULL;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &tmp, &m, PMIX_STRING, regtypes);
        if (PMIX_SUCCESS != ret) {
            return ret;
        }
        if (NULL == tmp) {
            PMIX_ERROR_LOG(PMIX_ERROR);
            return PMIX_ERROR;
        }
        pmix_strncpy(ptr[i].key, tmp, PMIX_MAX_KEYLEN);
        free(tmp);
        /* unpack value - since the value structure is statically-defined
         * instead of a pointer in this struct, we directly unpack it to
         * avoid the malloc */
        if (PMIX_SUCCESS
            != (ret = pmix_bfrop_get_data_type(regtypes, buffer, &ptr[i].value.type))) {
            PMIX_ERROR_LOG(ret);
            return ret;
        }
        pmix_output_verbose(20, pmix_bfrops_base_framework.framework_output,
                            "pmix_bfrop_unpack: pdata type %d %s", ptr[i].value.type,
                            ptr[i].value.data.string);
        m = 1;
        if (PMIX_SUCCESS != (ret = pmix_bfrops_base_unpack_val(regtypes, buffer, &ptr[i].value))) {
            PMIX_ERROR_LOG(ret);
            return ret;
        }
    }
    return PMIX_SUCCESS;
}

pmix_status_t pmix_bfrops_base_unpack_buf(pmix_pointer_array_t *regtypes, pmix_buffer_t *buffer,
                                          void *dest, int32_t *num_vals, pmix_data_type_t type)
{
    pmix_buffer_t *ptr;
    int32_t i, n, m;
    pmix_status_t ret;
    size_t nbytes;

    ptr = (pmix_buffer_t *) dest;
    n = *num_vals;

    PMIX_HIDE_UNUSED_PARAMS(type);

    for (i = 0; i < n; ++i) {
        PMIX_CONSTRUCT(&ptr[i], pmix_buffer_t);
        /* unpack the type of buffer */
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].type, &m, PMIX_BYTE, regtypes);
        if (PMIX_SUCCESS != ret) {
            return ret;
        }
        /* unpack the number of bytes */
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &nbytes, &m, PMIX_SIZE, regtypes);
        if (PMIX_SUCCESS != ret) {
            return ret;
        }
        m = nbytes;
        /* setup the buffer's data region */
        if (0 < nbytes) {
            ptr[i].base_ptr = (char *) malloc(nbytes);
            if (NULL == ptr[i].base_ptr) {
                return PMIX_ERR_NOMEM;
            }
            /* unpack the bytes */
            PMIX_BFROPS_UNPACK_TYPE(ret, buffer, ptr[i].base_ptr, &m, PMIX_BYTE, regtypes);
            if (PMIX_SUCCESS != ret) {
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

pmix_status_t pmix_bfrops_base_unpack_proc(pmix_pointer_array_t *regtypes, pmix_buffer_t *buffer,
                                           void *dest, int32_t *num_vals, pmix_data_type_t type)
{
    pmix_proc_t *ptr;
    int32_t i, n, m;
    pmix_status_t ret;
    char *tmp;

    pmix_output_verbose(20, pmix_bfrops_base_framework.framework_output,
                        "pmix_bfrop_unpack: %d procs", *num_vals);

    PMIX_HIDE_UNUSED_PARAMS(type);

    ptr = (pmix_proc_t *) dest;
    n = *num_vals;

    for (i = 0; i < n; ++i) {
        pmix_output_verbose(20, pmix_bfrops_base_framework.framework_output,
                            "pmix_bfrop_unpack: init proc[%d]", i);
        memset(&ptr[i], 0, sizeof(pmix_proc_t));
        /* unpack nspace */
        m = 1;
        tmp = NULL;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &tmp, &m, PMIX_STRING, regtypes);
        if (PMIX_SUCCESS != ret) {
            return ret;
        }
        if (NULL == tmp) {
            PMIX_ERROR_LOG(PMIX_ERROR);
            return PMIX_ERROR;
        }
        pmix_strncpy(ptr[i].nspace, tmp, PMIX_MAX_NSLEN);
        free(tmp);
        /* unpack the rank */
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].rank, &m, PMIX_PROC_RANK, regtypes);
        if (PMIX_SUCCESS != ret) {
            return ret;
        }
    }
    return PMIX_SUCCESS;
}

pmix_status_t pmix_bfrops_base_unpack_app(pmix_pointer_array_t *regtypes, pmix_buffer_t *buffer,
                                          void *dest, int32_t *num_vals, pmix_data_type_t type)
{
    pmix_app_t *ptr;
    int32_t i, k, n, m;
    pmix_status_t ret;
    int32_t nval;
    char *tmp;

    pmix_output_verbose(20, pmix_bfrops_base_framework.framework_output,
                        "pmix_bfrop_unpack: %d apps", *num_vals);

    PMIX_HIDE_UNUSED_PARAMS(type);

    ptr = (pmix_app_t *) dest;
    n = *num_vals;

    for (i = 0; i < n; ++i) {
        /* initialize the fields */
        PMIX_APP_CONSTRUCT(&ptr[i]);
        /* unpack cmd */
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].cmd, &m, PMIX_STRING, regtypes);
        if (PMIX_SUCCESS != ret) {
            return ret;
        }
        /* unpack argc */
        m = 1;
        /* although nval is technically an int32, we have to unpack it
         * as a generic int due to a typo in earlier release series. This
         * preserves the ordering of bytes in the packed buffer as it
         * includes a tag indicating the actual size of the value. No
         * harm is done as generic int is equivalent to int32 on all
         * current systems - just something to watch out for in the
         * future should someone someday change the size of "int" */
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &nval, &m, PMIX_INT, regtypes);
        if (PMIX_SUCCESS != ret) {
            return ret;
        }
        /* unpack argv */
        for (k = 0; k < nval; k++) {
            m = 1;
            tmp = NULL;
            PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &tmp, &m, PMIX_STRING, regtypes);
            if (PMIX_SUCCESS != ret) {
                return ret;
            }
            if (NULL == tmp) {
                return PMIX_ERROR;
            }
            PMIx_Argv_append_nosize(&ptr[i].argv, tmp);
            free(tmp);
        }
        /* unpack env */
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &nval, &m, PMIX_INT32, regtypes);
        if (PMIX_SUCCESS != ret) {
            return ret;
        }
        for (k = 0; k < nval; k++) {
            m = 1;
            tmp = NULL;
            PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &tmp, &m, PMIX_STRING, regtypes);
            if (PMIX_SUCCESS != ret) {
                return ret;
            }
            if (NULL == tmp) {
                return PMIX_ERROR;
            }
            PMIx_Argv_append_nosize(&ptr[i].env, tmp);
            free(tmp);
        }
        /* unpack cwd */
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].cwd, &m, PMIX_STRING, regtypes);
        if (PMIX_SUCCESS != ret) {
            return ret;
        }
        /* unpack maxprocs */
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].maxprocs, &m, PMIX_INT, regtypes);
        if (PMIX_SUCCESS != ret) {
            return ret;
        }
        /* unpack info array */
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].ninfo, &m, PMIX_SIZE, regtypes);
        if (PMIX_SUCCESS != ret) {
            return ret;
        }
        if (0 < ptr[i].ninfo) {
            PMIX_INFO_CREATE(ptr[i].info, ptr[i].ninfo);
            m = ptr[i].ninfo;
            PMIX_BFROPS_UNPACK_TYPE(ret, buffer, ptr[i].info, &m, PMIX_INFO, regtypes);
            if (PMIX_SUCCESS != ret) {
                return ret;
            }
        }
    }
    return PMIX_SUCCESS;
}

pmix_status_t pmix_bfrops_base_unpack_kval(pmix_pointer_array_t *regtypes, pmix_buffer_t *buffer,
                                           void *dest, int32_t *num_vals, pmix_data_type_t type)
{
    pmix_kval_t *ptr;
    int32_t i, n, m;
    pmix_status_t ret;

    pmix_output_verbose(20, pmix_bfrops_base_framework.framework_output,
                        "pmix_bfrop_unpack: %d kvals", *num_vals);

    PMIX_HIDE_UNUSED_PARAMS(type);

    ptr = (pmix_kval_t *) dest;
    n = *num_vals;

    for (i = 0; i < n; ++i) {
        PMIX_CONSTRUCT(&ptr[i], pmix_kval_t);
        /* unpack the key */
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].key, &m, PMIX_STRING, regtypes);
        if (PMIX_SUCCESS != ret) {
            return ret;
        }
        /* allocate the space */
        ptr[i].value = (pmix_value_t *) malloc(sizeof(pmix_value_t));
        /* unpack the value */
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, ptr[i].value, &m, PMIX_VALUE, regtypes);
        if (PMIX_SUCCESS != ret) {
            return ret;
        }
    }
    return PMIX_SUCCESS;
}

pmix_status_t pmix_bfrops_base_unpack_persist(pmix_pointer_array_t *regtypes, pmix_buffer_t *buffer,
                                              void *dest, int32_t *num_vals, pmix_data_type_t type)
{
    pmix_status_t ret;

    PMIX_HIDE_UNUSED_PARAMS(type);

    PMIX_BFROPS_UNPACK_TYPE(ret, buffer, dest, num_vals, PMIX_BYTE, regtypes);
    return ret;
}

pmix_status_t pmix_bfrops_base_unpack_bo(pmix_pointer_array_t *regtypes, pmix_buffer_t *buffer,
                                         void *dest, int32_t *num_vals, pmix_data_type_t type)
{
    pmix_byte_object_t *ptr;
    int32_t i, n, m;
    pmix_status_t ret;

    pmix_output_verbose(20, pmix_bfrops_base_framework.framework_output,
                        "pmix_bfrop_unpack: %d byte_object", *num_vals);

    PMIX_HIDE_UNUSED_PARAMS(type);

    ptr = (pmix_byte_object_t *) dest;
    n = *num_vals;

    for (i = 0; i < n; ++i) {
        memset(&ptr[i], 0, sizeof(pmix_byte_object_t));
        /* unpack the number of bytes */
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].size, &m, PMIX_SIZE, regtypes);
        if (PMIX_SUCCESS != ret) {
            return ret;
        }
        if (0 < ptr[i].size) {
            ptr[i].bytes = (char *) malloc(ptr[i].size * sizeof(char));
            if (NULL == ptr[i].bytes) {
                return PMIX_ERR_NOMEM;
            }
            m = ptr[i].size;
            PMIX_BFROPS_UNPACK_TYPE(ret, buffer, ptr[i].bytes, &m, PMIX_BYTE, regtypes);
            if (PMIX_SUCCESS != ret) {
                return ret;
            }
        }
    }
    return PMIX_SUCCESS;
}

pmix_status_t pmix_bfrops_base_unpack_ptr(pmix_pointer_array_t *regtypes, pmix_buffer_t *buffer,
                                          void *dest, int32_t *num_vals, pmix_data_type_t type)
{
    uint8_t foo = 1;
    int32_t cnt = 1;
    pmix_status_t ret;

    PMIX_HIDE_UNUSED_PARAMS(dest, num_vals, type);

    /* it obviously makes no sense to pack a pointer and
     * send it somewhere else, so we just unpack the sentinel */
    PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &foo, &cnt, PMIX_UINT8, regtypes);
    return ret;
}

pmix_status_t pmix_bfrops_base_unpack_scope(pmix_pointer_array_t *regtypes, pmix_buffer_t *buffer,
                                            void *dest, int32_t *num_vals, pmix_data_type_t type)
{
    pmix_status_t ret;

    PMIX_HIDE_UNUSED_PARAMS(type);

    PMIX_BFROPS_UNPACK_TYPE(ret, buffer, dest, num_vals, PMIX_UINT8, regtypes);
    return ret;
}

pmix_status_t pmix_bfrops_base_unpack_range(pmix_pointer_array_t *regtypes, pmix_buffer_t *buffer,
                                            void *dest, int32_t *num_vals, pmix_data_type_t type)
{
    pmix_status_t ret;

    PMIX_HIDE_UNUSED_PARAMS(type);

    PMIX_BFROPS_UNPACK_TYPE(ret, buffer, dest, num_vals, PMIX_UINT8, regtypes);
    return ret;
}

pmix_status_t pmix_bfrops_base_unpack_cmd(pmix_pointer_array_t *regtypes, pmix_buffer_t *buffer,
                                          void *dest, int32_t *num_vals, pmix_data_type_t type)
{
    pmix_status_t ret;

    PMIX_HIDE_UNUSED_PARAMS(type);

    PMIX_BFROPS_UNPACK_TYPE(ret, buffer, dest, num_vals, PMIX_UINT8, regtypes);
    return ret;
}

pmix_status_t pmix_bfrops_base_unpack_info_directives(pmix_pointer_array_t *regtypes,
                                                      pmix_buffer_t *buffer, void *dest,
                                                      int32_t *num_vals, pmix_data_type_t type)
{
    pmix_status_t ret;

    PMIX_HIDE_UNUSED_PARAMS(type);

    PMIX_BFROPS_UNPACK_TYPE(ret, buffer, dest, num_vals, PMIX_UINT32, regtypes);
    return ret;
}

pmix_status_t pmix_bfrops_base_unpack_pstate(pmix_pointer_array_t *regtypes, pmix_buffer_t *buffer,
                                             void *dest, int32_t *num_vals, pmix_data_type_t type)
{
    pmix_status_t ret;

    PMIX_HIDE_UNUSED_PARAMS(type);

    PMIX_BFROPS_UNPACK_TYPE(ret, buffer, dest, num_vals, PMIX_UINT8, regtypes);
    return ret;
}

pmix_status_t pmix_bfrops_base_unpack_pinfo(pmix_pointer_array_t *regtypes, pmix_buffer_t *buffer,
                                            void *dest, int32_t *num_vals, pmix_data_type_t type)
{
    pmix_proc_info_t *ptr;
    int32_t i, n, m;
    pmix_status_t ret;

    pmix_output_verbose(20, pmix_bfrops_base_framework.framework_output,
                        "pmix_bfrop_unpack: %d pinfo", *num_vals);

    PMIX_HIDE_UNUSED_PARAMS(type);

    ptr = (pmix_proc_info_t *) dest;
    n = *num_vals;

    for (i = 0; i < n; ++i) {
        PMIX_PROC_INFO_CONSTRUCT(&ptr[i]);
        /* unpack the proc */
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].proc, &m, PMIX_PROC, regtypes);
        if (PMIX_SUCCESS != ret) {
            return ret;
        }
        /* unpack the hostname */
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].hostname, &m, PMIX_STRING, regtypes);
        if (PMIX_SUCCESS != ret) {
            return ret;
        }
        /* unpack the executable */
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].executable_name, &m, PMIX_STRING, regtypes);
        if (PMIX_SUCCESS != ret) {
            return ret;
        }
        /* unpack pid */
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].pid, &m, PMIX_PID, regtypes);
        if (PMIX_SUCCESS != ret) {
            return ret;
        }
        /* unpack state */
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].state, &m, PMIX_PROC_STATE, regtypes);
        if (PMIX_SUCCESS != ret) {
            return ret;
        }
    }
    return PMIX_SUCCESS;
}

pmix_status_t pmix_bfrops_base_unpack_darray(pmix_pointer_array_t *regtypes, pmix_buffer_t *buffer,
                                             void *dest, int32_t *num_vals, pmix_data_type_t type)
{
    pmix_data_array_t *ptr;
    int32_t i, n, m;
    pmix_status_t ret;
    pmix_data_type_t t;
    size_t sm;

    pmix_output_verbose(20, pmix_bfrops_base_framework.framework_output,
                        "pmix_bfrop_unpack: %d data arrays", *num_vals);

    PMIX_HIDE_UNUSED_PARAMS(type);

    ptr = (pmix_data_array_t *) dest;
    n = *num_vals;

    for (i = 0; i < n; ++i) {
        memset(&ptr[i], 0, sizeof(pmix_data_array_t));
        /* unpack the type */
        m = 1;
        if (PMIX_SUCCESS != (ret = pmix_bfrop_get_data_type(regtypes, buffer, &ptr[i].type))) {
            return ret;
        }
        /* unpack the number of array elements */
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].size, &m, PMIX_SIZE, regtypes);
        if (PMIX_SUCCESS != ret) {
            return ret;
        }
        if (0 == ptr[i].size || PMIX_UNDEF == ptr[i].type) {
            /* nothing else to do */
            continue;
        }
        /* allocate storage for the array and unpack the array elements */
        sm = ptr[i].size;
        t = ptr[i].type;

        PMIX_DATA_ARRAY_CONSTRUCT(&ptr[i], sm, t);
        if (NULL == ptr[i].array) {
            return PMIX_ERR_NOMEM;
        }
        m = sm;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, ptr[i].array, &m, t, regtypes);
        if (PMIX_SUCCESS != ret) {
            return ret;
        }
    }
    return PMIX_SUCCESS;
}

pmix_status_t pmix_bfrops_base_unpack_rank(pmix_pointer_array_t *regtypes, pmix_buffer_t *buffer,
                                           void *dest, int32_t *num_vals, pmix_data_type_t type)
{
    pmix_status_t ret;

    PMIX_HIDE_UNUSED_PARAMS(type);

    PMIX_BFROPS_UNPACK_TYPE(ret, buffer, dest, num_vals, PMIX_UINT32, regtypes);
    return ret;
}

pmix_status_t pmix_bfrops_base_unpack_query(pmix_pointer_array_t *regtypes, pmix_buffer_t *buffer,
                                            void *dest, int32_t *num_vals, pmix_data_type_t type)
{
    pmix_query_t *ptr;
    int32_t i, n, m;
    pmix_status_t ret;
    int32_t nkeys;

    pmix_output_verbose(20, pmix_bfrops_base_framework.framework_output,
                        "pmix_bfrop_unpack: %d queries", *num_vals);

    PMIX_HIDE_UNUSED_PARAMS(type);

    ptr = (pmix_query_t *) dest;
    n = *num_vals;

    for (i = 0; i < n; ++i) {
        PMIX_QUERY_CONSTRUCT(&ptr[i]);
        /* unpack the number of keys */
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &nkeys, &m, PMIX_INT32, regtypes);
        if (PMIX_SUCCESS != ret) {
            return ret;
        }
        if (0 < nkeys) {
            /* unpack the keys */
            if (NULL == (ptr[i].keys = (char **) calloc(nkeys + 1, sizeof(char *)))) {
                return PMIX_ERR_NOMEM;
            }
            /* unpack keys */
            m = nkeys;
            PMIX_BFROPS_UNPACK_TYPE(ret, buffer, ptr[i].keys, &m, PMIX_STRING, regtypes);
            if (PMIX_SUCCESS != ret) {
                return ret;
            }
        }
        /* unpack the number of qualifiers */
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].nqual, &m, PMIX_SIZE, regtypes);
        if (PMIX_SUCCESS != ret) {
            return ret;
        }
        if (0 < ptr[i].nqual) {
            /* unpack the qualifiers */
            PMIX_INFO_CREATE(ptr[i].qualifiers, ptr[i].nqual);
            m = ptr[i].nqual;
            PMIX_BFROPS_UNPACK_TYPE(ret, buffer, ptr[i].qualifiers, &m, PMIX_INFO, regtypes);
            if (PMIX_SUCCESS != ret) {
                return ret;
            }
        }
    }
    return PMIX_SUCCESS;
}

pmix_status_t pmix_bfrops_base_unpack_alloc_directive(pmix_pointer_array_t *regtypes,
                                                      pmix_buffer_t *buffer, void *dest,
                                                      int32_t *num_vals, pmix_data_type_t type)
{
    pmix_status_t ret;

    PMIX_HIDE_UNUSED_PARAMS(type);

    PMIX_BFROPS_UNPACK_TYPE(ret, buffer, dest, num_vals, PMIX_UINT8, regtypes);
    return ret;
}

pmix_status_t pmix_bfrops_base_unpack_iof_channel(pmix_pointer_array_t *regtypes,
                                                  pmix_buffer_t *buffer, void *dest,
                                                  int32_t *num_vals, pmix_data_type_t type)
{
    pmix_status_t ret;

    PMIX_HIDE_UNUSED_PARAMS(type);

    PMIX_BFROPS_UNPACK_TYPE(ret, buffer, dest, num_vals, PMIX_UINT16, regtypes);
    return ret;
}

pmix_status_t pmix_bfrops_base_unpack_envar(pmix_pointer_array_t *regtypes, pmix_buffer_t *buffer,
                                            void *dest, int32_t *num_vals, pmix_data_type_t type)
{
    pmix_envar_t *ptr;
    int32_t i, n, m;
    pmix_status_t ret;

    pmix_output_verbose(20, pmix_bfrops_base_framework.framework_output,
                        "pmix_bfrop_unpack: %d envars", *num_vals);

    PMIX_HIDE_UNUSED_PARAMS(type);

    ptr = (pmix_envar_t *) dest;
    n = *num_vals;

    for (i = 0; i < n; ++i) {
        PMIX_ENVAR_CONSTRUCT(&ptr[i]);
        /* unpack the name */
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].envar, &m, PMIX_STRING, regtypes);
        if (PMIX_SUCCESS != ret) {
            return ret;
        }
        /* unpack the value */
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].value, &m, PMIX_STRING, regtypes);
        if (PMIX_SUCCESS != ret) {
            return ret;
        }
        /* unpack the separator */
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].separator, &m, PMIX_BYTE, regtypes);
        if (PMIX_SUCCESS != ret) {
            return ret;
        }
    }
    return PMIX_SUCCESS;
}

pmix_status_t pmix_bfrops_base_unpack_coord(pmix_pointer_array_t *regtypes, pmix_buffer_t *buffer,
                                            void *dest, int32_t *num_vals, pmix_data_type_t type)
{
    pmix_coord_t *ptr;
    int32_t i, n, m;
    pmix_status_t ret;

    pmix_output_verbose(20, pmix_bfrops_base_framework.framework_output,
                        "pmix_bfrop_unpack: %d coordinates", *num_vals);

    PMIX_HIDE_UNUSED_PARAMS(type);

    ptr = (pmix_coord_t *) dest;
    n = *num_vals;

    for (i = 0; i < n; ++i) {
        PMIX_COORD_CONSTRUCT(&ptr[i]);
        /* unpack the view */
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].view, &m, PMIX_UINT8, regtypes);
        if (PMIX_SUCCESS != ret) {
            return ret;
        }
        /* unpack the dims */
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].dims, &m, PMIX_SIZE, regtypes);
        if (PMIX_SUCCESS != ret) {
            return ret;
        }
        if (0 < ptr[i].dims) {
            ptr[i].coord = (uint32_t *) malloc(ptr[i].dims * sizeof(uint32_t));
            /* unpack the coords */
            m = ptr[i].dims;
            PMIX_BFROPS_UNPACK_TYPE(ret, buffer, ptr[i].coord, &m, PMIX_UINT32, regtypes);
            if (PMIX_SUCCESS != ret) {
                return ret;
            }
        }
    }
    return PMIX_SUCCESS;
}

pmix_status_t pmix_bfrops_base_unpack_regattr(pmix_pointer_array_t *regtypes, pmix_buffer_t *buffer,
                                              void *dest, int32_t *num_vals, pmix_data_type_t type)
{
    pmix_regattr_t *ptr;
    int32_t i, n, m, nd;
    pmix_status_t ret;
    char *tmp;

    pmix_output_verbose(20, pmix_bfrops_base_framework.framework_output,
                        "pmix_bfrop_unpack: %d regattrs", *num_vals);

    PMIX_HIDE_UNUSED_PARAMS(type);

    ptr = (pmix_regattr_t *) dest;
    n = *num_vals;

    for (i = 0; i < n; ++i) {
        PMIX_REGATTR_CONSTRUCT(&ptr[i]);
        /* unpack the name */
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].name, &m, PMIX_STRING, regtypes);
        if (PMIX_SUCCESS != ret) {
            return ret;
        }
        /* unpack the string */
        m = 1;
        tmp = NULL;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &tmp, &m, PMIX_STRING, regtypes);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            return ret;
        }
        if (NULL == tmp) {
            return PMIX_ERROR;
        }
        pmix_strncpy(ptr[i].string, tmp, PMIX_MAX_KEYLEN);
        free(tmp);
        /* unpack the type */
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].type, &m, PMIX_DATA_TYPE, regtypes);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            return ret;
        }
        /* unpack the description */
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &nd, &m, PMIX_INT32, regtypes);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            return ret;
        }
        if (0 < nd) {
            /* unpack the description */
            if (NULL == (ptr[i].description = (char **) calloc(nd + 1, sizeof(char *)))) {
                return PMIX_ERR_NOMEM;
            }
            m = nd;
            PMIX_BFROPS_UNPACK_TYPE(ret, buffer, ptr[i].description, &m, PMIX_STRING, regtypes);
            if (PMIX_SUCCESS != ret) {
                PMIX_ERROR_LOG(ret);
                return ret;
            }
        }
    }
    return PMIX_SUCCESS;
}

pmix_status_t pmix_bfrops_base_unpack_regex(pmix_pointer_array_t *regtypes, pmix_buffer_t *buffer,
                                            void *dest, int32_t *num_vals, pmix_data_type_t type)
{
    char **ptr;
    int32_t i, n;
    pmix_status_t ret;

    pmix_output_verbose(20, pmix_bfrops_base_framework.framework_output,
                        "pmix_bfrop_unpack: %d regex", *num_vals);

    PMIX_HIDE_UNUSED_PARAMS(regtypes, type);

    ptr = (char **) dest;
    n = *num_vals;

    for (i = 0; i < n; ++i) {
        ret = pmix_preg.unpack(buffer, &ptr[i]);
        if (PMIX_SUCCESS != ret) {
            *num_vals = 0;
            return ret;
        }
    }
    return PMIX_SUCCESS;
}

pmix_status_t pmix_bfrops_base_unpack_jobstate(pmix_pointer_array_t *regtypes,
                                               pmix_buffer_t *buffer, void *dest, int32_t *num_vals,
                                               pmix_data_type_t type)
{
    pmix_status_t ret;

    PMIX_HIDE_UNUSED_PARAMS(type);

    PMIX_BFROPS_UNPACK_TYPE(ret, buffer, dest, num_vals, PMIX_UINT8, regtypes);
    return ret;
}

pmix_status_t pmix_bfrops_base_unpack_linkstate(pmix_pointer_array_t *regtypes,
                                                pmix_buffer_t *buffer, void *dest,
                                                int32_t *num_vals, pmix_data_type_t type)
{
    pmix_status_t ret;

    PMIX_HIDE_UNUSED_PARAMS(type);

    PMIX_BFROPS_UNPACK_TYPE(ret, buffer, dest, num_vals, PMIX_UINT8, regtypes);
    return ret;
}

pmix_status_t pmix_bfrops_base_unpack_cpuset(pmix_pointer_array_t *regtypes, pmix_buffer_t *buffer,
                                             void *dest, int32_t *num_vals, pmix_data_type_t type)
{
    pmix_cpuset_t *ptr;
    int32_t i, n;
    pmix_status_t ret;

    pmix_output_verbose(20, pmix_bfrops_base_framework.framework_output,
                        "pmix_bfrop_unpack: %d cpuset", *num_vals);

    PMIX_HIDE_UNUSED_PARAMS(type);

    ptr = (pmix_cpuset_t *) dest;
    n = *num_vals;

    for (i = 0; i < n; ++i) {
        ret = pmix_hwloc_unpack_cpuset(buffer, &ptr[i], regtypes);
        if (PMIX_SUCCESS != ret) {
            *num_vals = 0;
            return ret;
        }
    }
    return PMIX_SUCCESS;
}

pmix_status_t pmix_bfrops_base_unpack_geometry(pmix_pointer_array_t *regtypes,
                                               pmix_buffer_t *buffer, void *dest, int32_t *num_vals,
                                               pmix_data_type_t type)
{
    pmix_geometry_t *ptr;
    int32_t i, n, m;
    pmix_status_t ret;

    pmix_output_verbose(20, pmix_bfrops_base_framework.framework_output,
                        "pmix_bfrop_unpack: %d geometry", *num_vals);

    PMIX_HIDE_UNUSED_PARAMS(type);

    ptr = (pmix_geometry_t *) dest;
    n = *num_vals;

    for (i = 0; i < n; ++i) {
        PMIX_GEOMETRY_CONSTRUCT(&ptr[i]);
        /* unpack the fabric id */
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].fabric, &m, PMIX_SIZE, regtypes);
        if (PMIX_SUCCESS != ret) {
            return ret;
        }
        /* unpack the uuid */
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].uuid, &m, PMIX_STRING, regtypes);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            return ret;
        }
        /* unpack the osname */
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].osname, &m, PMIX_STRING, regtypes);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            return ret;
        }
        /* get the number of coords */
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].ncoords, &m, PMIX_SIZE, regtypes);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            return ret;
        }
        if (0 < ptr[i].ncoords) {
            /* allocate the coords */
            ptr[i].coordinates = (pmix_coord_t *) calloc(ptr[i].ncoords, sizeof(pmix_coord_t));
            /* unpack them */
            m = ptr[i].ncoords;
            PMIX_BFROPS_UNPACK_TYPE(ret, buffer, ptr[i].coordinates, &m, PMIX_COORD, regtypes);
            if (PMIX_SUCCESS != ret) {
                PMIX_ERROR_LOG(ret);
                return ret;
            }
        }
    }
    return PMIX_SUCCESS;
}

pmix_status_t pmix_bfrops_base_unpack_devdist(pmix_pointer_array_t *regtypes, pmix_buffer_t *buffer,
                                              void *dest, int32_t *num_vals, pmix_data_type_t type)
{
    pmix_device_distance_t *ptr;
    int32_t i, n, m;
    pmix_status_t ret;

    pmix_output_verbose(20, pmix_bfrops_base_framework.framework_output,
                        "pmix_bfrop_unpack: %d device distances", *num_vals);

    PMIX_HIDE_UNUSED_PARAMS(type);

    ptr = (pmix_device_distance_t *) dest;
    n = *num_vals;

    for (i = 0; i < n; ++i) {
        PMIX_DEVICE_DIST_CONSTRUCT(&ptr[i]);
        /* unpack the uuid */
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].uuid, &m, PMIX_STRING, regtypes);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            return ret;
        }
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].osname, &m, PMIX_STRING, regtypes);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            return ret;
        }
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].type, &m, PMIX_DEVTYPE, regtypes);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            return ret;
        }
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].mindist, &m, PMIX_UINT16, regtypes);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            return ret;
        }
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].maxdist, &m, PMIX_UINT16, regtypes);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            return ret;
        }
    }
    return PMIX_SUCCESS;
}

pmix_status_t pmix_bfrops_base_unpack_endpoint(pmix_pointer_array_t *regtypes,
                                               pmix_buffer_t *buffer, void *dest, int32_t *num_vals,
                                               pmix_data_type_t type)
{
    pmix_endpoint_t *ptr;
    int32_t i, n, m;
    pmix_status_t ret;

    pmix_output_verbose(20, pmix_bfrops_base_framework.framework_output,
                        "pmix_bfrop_unpack: %d endpts", *num_vals);

    PMIX_HIDE_UNUSED_PARAMS(type);

    ptr = (pmix_endpoint_t *) dest;
    n = *num_vals;

    for (i = 0; i < n; ++i) {
        PMIX_ENDPOINT_CONSTRUCT(&ptr[i]);
        /* unpack the uuid */
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].uuid, &m, PMIX_STRING, regtypes);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            return ret;
        }
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].osname, &m, PMIX_STRING, regtypes);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            return ret;
        }
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].endpt.size, &m, PMIX_SIZE, regtypes);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            return ret;
        }
        if (0 < ptr[i].endpt.size) {
            ptr[i].endpt.bytes = (char *) malloc(ptr[i].endpt.size);
            m = ptr[i].endpt.size;
            PMIX_BFROPS_UNPACK_TYPE(ret, buffer, ptr[i].endpt.bytes, &m, PMIX_BYTE, regtypes);
            if (PMIX_SUCCESS != ret) {
                PMIX_ERROR_LOG(ret);
                return ret;
            }
        }
    }
    return PMIX_SUCCESS;
}

pmix_status_t pmix_bfrops_base_unpack_topology(pmix_pointer_array_t *regtypes,
                                               pmix_buffer_t *buffer, void *dest, int32_t *num_vals,
                                               pmix_data_type_t type)
{
    pmix_topology_t *ptr;
    int32_t i, n;
    pmix_status_t ret;

    pmix_output_verbose(20, pmix_bfrops_base_framework.framework_output,
                        "pmix_bfrop_unpack: %d topology", *num_vals);

    PMIX_HIDE_UNUSED_PARAMS(type);

    ptr = (pmix_topology_t *) dest;
    n = *num_vals;

    for (i = 0; i < n; ++i) {
        ret = pmix_hwloc_unpack_topology(buffer, &ptr[i], regtypes);
        if (PMIX_SUCCESS != ret) {
            *num_vals = 0;
            return ret;
        }
    }
    return PMIX_SUCCESS;
}

pmix_status_t pmix_bfrops_base_unpack_devtype(pmix_pointer_array_t *regtypes, pmix_buffer_t *buffer,
                                              void *dest, int32_t *num_vals, pmix_data_type_t type)
{
    pmix_status_t ret;

    pmix_output_verbose(20, pmix_bfrops_base_framework.framework_output,
                        "pmix_bfrop_unpack: %d device types", *num_vals);

    PMIX_HIDE_UNUSED_PARAMS(type);

    PMIX_BFROPS_UNPACK_TYPE(ret, buffer, dest, num_vals, PMIX_UINT64, regtypes);

    return ret;
}

pmix_status_t pmix_bfrops_base_unpack_locality(pmix_pointer_array_t *regtypes,
                                               pmix_buffer_t *buffer, void *dest, int32_t *num_vals,
                                               pmix_data_type_t type)
{
    pmix_status_t ret;

    pmix_output_verbose(20, pmix_bfrops_base_framework.framework_output,
                        "pmix_bfrop_unpack: %d locality", *num_vals);

    PMIX_HIDE_UNUSED_PARAMS(type);

    PMIX_BFROPS_UNPACK_TYPE(ret, buffer, dest, num_vals, PMIX_UINT16, regtypes);

    return ret;
}

pmix_status_t pmix_bfrops_base_unpack_nspace(pmix_pointer_array_t *regtypes, pmix_buffer_t *buffer,
                                             void *dest, int32_t *num_vals, pmix_data_type_t type)
{
    pmix_nspace_t *ptr;
    int32_t i, n, m;
    pmix_status_t ret;
    char *p;

    pmix_output_verbose(20, pmix_bfrops_base_framework.framework_output,
                        "pmix_bfrop_unpack: %d nspace", *num_vals);

    PMIX_HIDE_UNUSED_PARAMS(type);

    ptr = (pmix_nspace_t *) dest;
    n = *num_vals;

    for (i = 0; i < n; ++i) {
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &p, &m, PMIX_STRING, regtypes);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            return ret;
        }
        PMIX_LOAD_NSPACE(ptr[i], p);
        free(p);
    }
    return PMIX_SUCCESS;
}

pmix_status_t pmix_bfrops_base_unpack_pstats(pmix_pointer_array_t *regtypes, pmix_buffer_t *buffer,
                                             void *dest, int32_t *num_vals, pmix_data_type_t type)
{
    pmix_proc_stats_t *ptr;
    int32_t i, n, m;
    pmix_status_t ret;

    ptr = (pmix_proc_stats_t *) dest;
    n = *num_vals;

    PMIX_HIDE_UNUSED_PARAMS(type);

    for (i = 0; i < n; ++i) {
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].node, &m, PMIX_STRING, regtypes);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            return ret;
        }
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].proc, &m, PMIX_PROC, regtypes);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            return ret;
        }
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].pid, &m, PMIX_PID, regtypes);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            return ret;
        }
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].cmd, &m, PMIX_STRING, regtypes);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            return ret;
        }
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].state, &m, PMIX_BYTE, regtypes);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            return ret;
        }
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].time, &m, PMIX_TIMEVAL, regtypes);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            return ret;
        }
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].priority, &m, PMIX_INT32, regtypes);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            return ret;
        }
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].num_threads, &m, PMIX_INT16, regtypes);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            return ret;
        }
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].pss, &m, PMIX_FLOAT, regtypes);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            return ret;
        }
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].vsize, &m, PMIX_FLOAT, regtypes);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            return ret;
        }
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].rss, &m, PMIX_FLOAT, regtypes);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            return ret;
        }
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].peak_vsize, &m, PMIX_FLOAT, regtypes);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            return ret;
        }
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].processor, &m, PMIX_INT16, regtypes);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            return ret;
        }
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].sample_time, &m, PMIX_TIMEVAL, regtypes);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            return ret;
        }
    }

    return PMIX_SUCCESS;
}

pmix_status_t pmix_bfrops_base_unpack_dkstats(pmix_pointer_array_t *regtypes, pmix_buffer_t *buffer,
                                              void *dest, int32_t *num_vals, pmix_data_type_t type)
{
    int32_t i, m, n;
    pmix_status_t ret;
    pmix_disk_stats_t *ptr = (pmix_disk_stats_t *) dest;

    PMIX_HIDE_UNUSED_PARAMS(type);

    /* unpack them */
    n = *num_vals;
    for (i = 0; i < n; i++) {
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].disk, &m, PMIX_STRING, regtypes);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            return ret;
        }
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].num_reads_completed, &m, PMIX_UINT64,
                                regtypes);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            return ret;
        }
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].num_reads_merged, &m, PMIX_UINT64, regtypes);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            return ret;
        }
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].num_sectors_read, &m, PMIX_UINT64, regtypes);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            return ret;
        }
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].milliseconds_reading, &m, PMIX_UINT64,
                                regtypes);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            return ret;
        }
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].num_writes_completed, &m, PMIX_UINT64,
                                regtypes);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            return ret;
        }
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].num_writes_merged, &m, PMIX_UINT64, regtypes);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            return ret;
        }
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].num_sectors_written, &m, PMIX_UINT64,
                                regtypes);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            return ret;
        }
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].milliseconds_writing, &m, PMIX_UINT64,
                                regtypes);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            return ret;
        }
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].num_ios_in_progress, &m, PMIX_UINT64,
                                regtypes);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            return ret;
        }
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].milliseconds_io, &m, PMIX_UINT64, regtypes);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            return ret;
        }
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].weighted_milliseconds_io, &m, PMIX_UINT64,
                                regtypes);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            return ret;
        }
    }
    return PMIX_SUCCESS;
}

pmix_status_t pmix_bfrops_base_unpack_netstats(pmix_pointer_array_t *regtypes,
                                               pmix_buffer_t *buffer, void *dest, int32_t *num_vals,
                                               pmix_data_type_t type)
{
    pmix_net_stats_t *ptr = (pmix_net_stats_t *) dest;
    int32_t i, m, n;
    int ret;

    PMIX_HIDE_UNUSED_PARAMS(type);

    n = *num_vals;
    for (i = 0; i < n; i++) {
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].net_interface, &m, PMIX_STRING, regtypes);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            return ret;
        }
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].num_bytes_recvd, &m, PMIX_UINT64, regtypes);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            return ret;
        }
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].num_packets_recvd, &m, PMIX_UINT64, regtypes);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            return ret;
        }
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].num_recv_errs, &m, PMIX_UINT64, regtypes);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            return ret;
        }
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].num_bytes_sent, &m, PMIX_UINT64, regtypes);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            return ret;
        }
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].num_packets_sent, &m, PMIX_UINT64, regtypes);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            return ret;
        }
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].num_send_errs, &m, PMIX_UINT64, regtypes);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            return ret;
        }
    }
    return PMIX_SUCCESS;
}

pmix_status_t pmix_bfrops_base_unpack_ndstats(pmix_pointer_array_t *regtypes, pmix_buffer_t *buffer,
                                              void *dest, int32_t *num_vals, pmix_data_type_t type)
{
    pmix_node_stats_t *ptr = (pmix_node_stats_t *) dest;
    int32_t i, m, n;
    int ret;

    PMIX_HIDE_UNUSED_PARAMS(type);

    n = *num_vals;
    for (i = 0; i < n; i++) {
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].node, &m, PMIX_STRING, regtypes);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            return ret;
        }
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].la, &m, PMIX_FLOAT, regtypes);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            return ret;
        }
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].la5, &m, PMIX_FLOAT, regtypes);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            return ret;
        }
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].la15, &m, PMIX_FLOAT, regtypes);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            return ret;
        }
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].total_mem, &m, PMIX_FLOAT, regtypes);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            return ret;
        }
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].free_mem, &m, PMIX_FLOAT, regtypes);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            return ret;
        }
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].buffers, &m, PMIX_FLOAT, regtypes);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            return ret;
        }
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].cached, &m, PMIX_FLOAT, regtypes);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            return ret;
        }
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].swap_cached, &m, PMIX_FLOAT, regtypes);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            return ret;
        }
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].swap_total, &m, PMIX_FLOAT, regtypes);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            return ret;
        }
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].swap_free, &m, PMIX_FLOAT, regtypes);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            return ret;
        }
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].mapped, &m, PMIX_FLOAT, regtypes);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            return ret;
        }
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].sample_time, &m, PMIX_TIMEVAL, regtypes);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            return ret;
        }
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].ndiskstats, &m, PMIX_SIZE, regtypes);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            return ret;
        }
        if (0 < ptr[i].ndiskstats) {
            m = ptr[i].ndiskstats;
            PMIX_DISK_STATS_CREATE(ptr[i].diskstats, ptr[i].ndiskstats);
            PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].diskstats, &m, PMIX_DISK_STATS, regtypes);
            if (PMIX_SUCCESS != ret) {
                PMIX_DISK_STATS_FREE(ptr[i].diskstats, ptr[i].ndiskstats);
                PMIX_ERROR_LOG(ret);
                return ret;
            }
        }
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].nnetstats, &m, PMIX_SIZE, regtypes);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            return ret;
        }
        if (0 < ptr[i].nnetstats) {
            m = ptr[i].nnetstats;
            PMIX_NET_STATS_CREATE(ptr[i].netstats, ptr[i].nnetstats);
            PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].netstats, &m, PMIX_NET_STATS, regtypes);
            if (PMIX_SUCCESS != ret) {
                PMIX_NET_STATS_FREE(ptr[i].netstats, ptr[i].nnetstats);
                PMIX_ERROR_LOG(ret);
                return ret;
            }
        }
    }
    return PMIX_SUCCESS;
}

pmix_status_t pmix_bfrops_base_unpack_dbuf(pmix_pointer_array_t *regtypes, pmix_buffer_t *buffer,
                                           void *dest, int32_t *num_vals, pmix_data_type_t type)
{
    pmix_data_buffer_t *ptr = (pmix_data_buffer_t *) dest;
    int32_t i, m, n;
    pmix_status_t ret;

    PMIX_HIDE_UNUSED_PARAMS(type);

    n = *num_vals;
    for (i = 0; i < n; ++i) {
        m = 1;
        PMIX_BFROPS_UNPACK_TYPE(ret, buffer, &ptr[i].bytes_used, &m, PMIX_SIZE, regtypes);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            return ret;
        }
        if (0 < ptr[i].bytes_used) {
            ptr[i].base_ptr = malloc(ptr[i].bytes_used);
            m = ptr[i].bytes_used;
            PMIX_BFROPS_UNPACK_TYPE(ret, buffer, ptr[i].base_ptr, &m, PMIX_BYTE, regtypes);
            if (PMIX_SUCCESS != ret) {
                return ret;
            }
        }
    }
    return PMIX_SUCCESS;
}

pmix_status_t pmix_bfrops_base_unpack_smed(pmix_pointer_array_t *regtypes, pmix_buffer_t *buffer,
                                           void *dest, int32_t *num_vals, pmix_data_type_t type)
{
    pmix_status_t ret;

    pmix_output_verbose(20, pmix_bfrops_base_framework.framework_output,
                        "pmix_bfrop_unpack: %d storage medium", *num_vals);

    PMIX_HIDE_UNUSED_PARAMS(type);

    PMIX_BFROPS_UNPACK_TYPE(ret, buffer, dest, num_vals, PMIX_UINT64, regtypes);

    return ret;
}

pmix_status_t pmix_bfrops_base_unpack_sacc(pmix_pointer_array_t *regtypes, pmix_buffer_t *buffer,
                                           void *dest, int32_t *num_vals, pmix_data_type_t type)
{
    pmix_status_t ret;

    pmix_output_verbose(20, pmix_bfrops_base_framework.framework_output,
                        "pmix_bfrop_unpack: %d storage access", *num_vals);

    PMIX_HIDE_UNUSED_PARAMS(type);

    PMIX_BFROPS_UNPACK_TYPE(ret, buffer, dest, num_vals, PMIX_UINT64, regtypes);

    return ret;
}

pmix_status_t pmix_bfrops_base_unpack_spers(pmix_pointer_array_t *regtypes, pmix_buffer_t *buffer,
                                            void *dest, int32_t *num_vals, pmix_data_type_t type)
{
    pmix_status_t ret;

    pmix_output_verbose(20, pmix_bfrops_base_framework.framework_output,
                        "pmix_bfrop_unpack: %d storage persistence", *num_vals);

    PMIX_HIDE_UNUSED_PARAMS(type);

    PMIX_BFROPS_UNPACK_TYPE(ret, buffer, dest, num_vals, PMIX_UINT64, regtypes);

    return ret;
}

pmix_status_t pmix_bfrops_base_unpack_satyp(pmix_pointer_array_t *regtypes, pmix_buffer_t *buffer,
                                            void *dest, int32_t *num_vals, pmix_data_type_t type)
{
    pmix_status_t ret;

    pmix_output_verbose(20, pmix_bfrops_base_framework.framework_output,
                        "pmix_bfrop_unpack: %d storage access type", *num_vals);

    PMIX_HIDE_UNUSED_PARAMS(type);

    PMIX_BFROPS_UNPACK_TYPE(ret, buffer, dest, num_vals, PMIX_UINT16, regtypes);

    return ret;
}
