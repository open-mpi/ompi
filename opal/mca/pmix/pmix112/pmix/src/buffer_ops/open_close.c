/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2009 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2012-2013 Los Alamos National Security, Inc.  All rights reserved.
 * Copyright (c) 2014-2015 Intel, Inc. All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/** @file:
 *
 */
#include <private/autogen/config.h>
#include <pmix/rename.h>
#include <pmix/pmix_common.h>

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "src/util/argv.h"
#include "src/buffer_ops/internal.h"


/**
 * globals
 */
bool pmix_bfrop_initialized = false;
int pmix_bfrop_initial_size = 0;
int pmix_bfrop_threshold_size = 0;
pmix_pointer_array_t pmix_bfrop_types = {{0}};
pmix_data_type_t pmix_bfrop_num_reg_types = PMIX_UNDEF;
static pmix_bfrop_buffer_type_t pmix_default_buf_type = PMIX_BFROP_BUFFER_NON_DESC;

pmix_bfrop_t pmix_bfrop = {
    pmix_bfrop_pack,
    pmix_bfrop_unpack,
    pmix_bfrop_copy,
    pmix_bfrop_print,
    pmix_bfrop_copy_payload,
};

/**
 * Object constructors, destructors, and instantiations
 */
/** Value **/
static void pmix_buffer_construct (pmix_buffer_t* buffer)
{
    /** set the default buffer type */
    buffer->type = pmix_default_buf_type;

    /* Make everything NULL to begin with */
    buffer->base_ptr = buffer->pack_ptr = buffer->unpack_ptr = NULL;
    buffer->bytes_allocated = buffer->bytes_used = 0;
}

static void pmix_buffer_destruct (pmix_buffer_t* buffer)
{
    if (NULL != buffer->base_ptr) {
        free (buffer->base_ptr);
    }
}

PMIX_CLASS_INSTANCE(pmix_buffer_t,
                   pmix_object_t,
                   pmix_buffer_construct,
                   pmix_buffer_destruct);


static void pmix_bfrop_type_info_construct(pmix_bfrop_type_info_t *obj)
{
    obj->odti_name = NULL;
    obj->odti_pack_fn = NULL;
    obj->odti_unpack_fn = NULL;
    obj->odti_copy_fn = NULL;
    obj->odti_print_fn = NULL;
}

static void pmix_bfrop_type_info_destruct(pmix_bfrop_type_info_t *obj)
{
    if (NULL != obj->odti_name) {
        free(obj->odti_name);
    }
}

PMIX_CLASS_INSTANCE(pmix_bfrop_type_info_t, pmix_object_t,
                   pmix_bfrop_type_info_construct,
                   pmix_bfrop_type_info_destruct);

static void kvcon(pmix_kval_t *k)
{
    k->key = NULL;
    k->value = NULL;
}
static void kvdes(pmix_kval_t *k)
{
    if (NULL != k->key) {
        free(k->key);
    }
    if (NULL != k->value) {
        PMIX_VALUE_RELEASE(k->value);
    }
}
PMIX_CLASS_INSTANCE(pmix_kval_t,
                   pmix_list_item_t,
                   kvcon, kvdes);

static void rcon(pmix_regex_range_t *p)
{
    p->start = 0;
    p->cnt = 0;
}
PMIX_CLASS_INSTANCE(pmix_regex_range_t,
                    pmix_list_item_t,
                    rcon, NULL);

static void rvcon(pmix_regex_value_t *p)
{
    p->prefix = NULL;
    p->suffix = NULL;
    p->num_digits = 0;
    PMIX_CONSTRUCT(&p->ranges, pmix_list_t);
}
static void rvdes(pmix_regex_value_t *p)
{
    if (NULL != p->prefix) {
        free(p->prefix);
    }
    if (NULL != p->suffix) {
        free(p->suffix);
    }
    PMIX_LIST_DESTRUCT(&p->ranges);
}
PMIX_CLASS_INSTANCE(pmix_regex_value_t,
                    pmix_list_item_t,
                    rvcon, rvdes);

pmix_status_t pmix_bfrop_open(void)
{
    pmix_status_t rc;

    if (pmix_bfrop_initialized) {
        return PMIX_SUCCESS;
    }

    /** set the default buffer type. If we are in debug mode, then we default
     * to fully described buffers. Otherwise, we default to non-described for brevity
     * and performance
     */
#if PMIX_ENABLE_DEBUG
    pmix_default_buf_type = PMIX_BFROP_BUFFER_FULLY_DESC;
#else
    pmix_default_buf_type = PMIX_BFROP_BUFFER_NON_DESC;
#endif

    /* Setup the types array */
    PMIX_CONSTRUCT(&pmix_bfrop_types, pmix_pointer_array_t);
    if (PMIX_SUCCESS != (rc = pmix_pointer_array_init(&pmix_bfrop_types, 64, 255, 64))) {
        return rc;
    }
    pmix_bfrop_num_reg_types = PMIX_UNDEF;
    pmix_bfrop_threshold_size = PMIX_BFROP_DEFAULT_THRESHOLD_SIZE;
    pmix_bfrop_initial_size = 1;

    /* Register all the supported types */
    PMIX_REGISTER_TYPE("PMIX_BOOL", PMIX_BOOL,
                       pmix_bfrop_pack_bool,
                       pmix_bfrop_unpack_bool,
                       pmix_bfrop_std_copy,
                       pmix_bfrop_print_bool);

    PMIX_REGISTER_TYPE("PMIX_BYTE", PMIX_BYTE,
                       pmix_bfrop_pack_byte,
                       pmix_bfrop_unpack_byte,
                       pmix_bfrop_std_copy,
                       pmix_bfrop_print_byte);

    PMIX_REGISTER_TYPE("PMIX_STRING", PMIX_STRING,
                       pmix_bfrop_pack_string,
                       pmix_bfrop_unpack_string,
                       pmix_bfrop_copy_string,
                       pmix_bfrop_print_string);

    PMIX_REGISTER_TYPE("PMIX_SIZE", PMIX_SIZE,
                       pmix_bfrop_pack_sizet,
                       pmix_bfrop_unpack_sizet,
                       pmix_bfrop_std_copy,
                       pmix_bfrop_print_size);

    PMIX_REGISTER_TYPE("PMIX_PID", PMIX_PID,
                       pmix_bfrop_pack_pid,
                       pmix_bfrop_unpack_pid,
                       pmix_bfrop_std_copy,
                       pmix_bfrop_print_pid);

    PMIX_REGISTER_TYPE("PMIX_INT", PMIX_INT,
                       pmix_bfrop_pack_int,
                       pmix_bfrop_unpack_int,
                       pmix_bfrop_std_copy,
                       pmix_bfrop_print_int);

    PMIX_REGISTER_TYPE("PMIX_INT8", PMIX_INT8,
                       pmix_bfrop_pack_byte,
                       pmix_bfrop_unpack_byte,
                       pmix_bfrop_std_copy,
                       pmix_bfrop_print_int8);

    PMIX_REGISTER_TYPE("PMIX_INT16", PMIX_INT16,
                       pmix_bfrop_pack_int16,
                       pmix_bfrop_unpack_int16,
                       pmix_bfrop_std_copy,
                       pmix_bfrop_print_int16);

    PMIX_REGISTER_TYPE("PMIX_INT32", PMIX_INT32,
                       pmix_bfrop_pack_int32,
                       pmix_bfrop_unpack_int32,
                       pmix_bfrop_std_copy,
                       pmix_bfrop_print_int32);

    PMIX_REGISTER_TYPE("PMIX_INT64", PMIX_INT64,
                       pmix_bfrop_pack_int64,
                       pmix_bfrop_unpack_int64,
                       pmix_bfrop_std_copy,
                       pmix_bfrop_print_int64);

    PMIX_REGISTER_TYPE("PMIX_UINT", PMIX_UINT,
                       pmix_bfrop_pack_int,
                       pmix_bfrop_unpack_int,
                       pmix_bfrop_std_copy,
                       pmix_bfrop_print_uint);

    PMIX_REGISTER_TYPE("PMIX_UINT8", PMIX_UINT8,
                       pmix_bfrop_pack_byte,
                       pmix_bfrop_unpack_byte,
                       pmix_bfrop_std_copy,
                       pmix_bfrop_print_uint8);

    PMIX_REGISTER_TYPE("PMIX_UINT16", PMIX_UINT16,
                       pmix_bfrop_pack_int16,
                       pmix_bfrop_unpack_int16,
                       pmix_bfrop_std_copy,
                       pmix_bfrop_print_uint16);

    PMIX_REGISTER_TYPE("PMIX_UINT32", PMIX_UINT32,
                       pmix_bfrop_pack_int32,
                       pmix_bfrop_unpack_int32,
                       pmix_bfrop_std_copy,
                       pmix_bfrop_print_uint32);

    PMIX_REGISTER_TYPE("PMIX_UINT64", PMIX_UINT64,
                       pmix_bfrop_pack_int64,
                       pmix_bfrop_unpack_int64,
                       pmix_bfrop_std_copy,
                       pmix_bfrop_print_uint64);

    PMIX_REGISTER_TYPE("PMIX_FLOAT", PMIX_FLOAT,
                       pmix_bfrop_pack_float,
                       pmix_bfrop_unpack_float,
                       pmix_bfrop_std_copy,
                       pmix_bfrop_print_float);

    PMIX_REGISTER_TYPE("PMIX_DOUBLE", PMIX_DOUBLE,
                       pmix_bfrop_pack_double,
                       pmix_bfrop_unpack_double,
                       pmix_bfrop_std_copy,
                       pmix_bfrop_print_double);

    PMIX_REGISTER_TYPE("PMIX_TIMEVAL", PMIX_TIMEVAL,
                       pmix_bfrop_pack_timeval,
                       pmix_bfrop_unpack_timeval,
                       pmix_bfrop_std_copy,
                       pmix_bfrop_print_timeval);

    PMIX_REGISTER_TYPE("PMIX_TIME", PMIX_TIME,
                       pmix_bfrop_pack_time,
                       pmix_bfrop_unpack_time,
                       pmix_bfrop_std_copy,
                       pmix_bfrop_print_time);

#if PMIX_HAVE_HWLOC
    PMIX_REGISTER_TYPE("PMIX_HWLOC_TOPO", PMIX_HWLOC_TOPO,
                       pmix_bfrop_pack_topo,
                       pmix_bfrop_unpack_topo,
                       pmix_bfrop_copy_topo,
                       pmix_bfrop_print_topo);
#endif

    PMIX_REGISTER_TYPE("PMIX_VALUE", PMIX_VALUE,
                       pmix_bfrop_pack_value,
                       pmix_bfrop_unpack_value,
                       pmix_bfrop_copy_value,
                       pmix_bfrop_print_value);

    PMIX_REGISTER_TYPE("PMIX_INFO_ARRAY", PMIX_INFO_ARRAY,
                       pmix_bfrop_pack_array,
                       pmix_bfrop_unpack_array,
                       pmix_bfrop_copy_array,
                       pmix_bfrop_print_array);

    PMIX_REGISTER_TYPE("PMIX_PROC", PMIX_PROC,
                       pmix_bfrop_pack_proc,
                       pmix_bfrop_unpack_proc,
                       pmix_bfrop_copy_proc,
                       pmix_bfrop_print_proc);

    PMIX_REGISTER_TYPE("PMIX_APP", PMIX_APP,
                       pmix_bfrop_pack_app,
                       pmix_bfrop_unpack_app,
                       pmix_bfrop_copy_app,
                       pmix_bfrop_print_app);

    PMIX_REGISTER_TYPE("PMIX_INFO", PMIX_INFO,
                       pmix_bfrop_pack_info,
                       pmix_bfrop_unpack_info,
                       pmix_bfrop_copy_info,
                       pmix_bfrop_print_info);

    PMIX_REGISTER_TYPE("PMIX_PDATA", PMIX_PDATA,
                       pmix_bfrop_pack_pdata,
                       pmix_bfrop_unpack_pdata,
                       pmix_bfrop_copy_pdata,
                       pmix_bfrop_print_pdata);

    PMIX_REGISTER_TYPE("PMIX_BUFFER", PMIX_BUFFER,
                       pmix_bfrop_pack_buf,
                       pmix_bfrop_unpack_buf,
                       pmix_bfrop_copy_buf,
                       pmix_bfrop_print_buf);

    PMIX_REGISTER_TYPE("PMIX_BYTE_OBJECT", PMIX_BYTE_OBJECT,
                       pmix_bfrop_pack_bo,
                       pmix_bfrop_unpack_bo,
                       pmix_bfrop_copy_bo,
                       pmix_bfrop_print_bo);

    PMIX_REGISTER_TYPE("PMIX_KVAL", PMIX_KVAL,
                       pmix_bfrop_pack_kval,
                       pmix_bfrop_unpack_kval,
                       pmix_bfrop_copy_kval,
                       pmix_bfrop_print_kval);

    PMIX_REGISTER_TYPE("PMIX_MODEX", PMIX_MODEX,
                       pmix_bfrop_pack_modex,
                       pmix_bfrop_unpack_modex,
                       pmix_bfrop_copy_modex,
                       pmix_bfrop_print_modex);

    PMIX_REGISTER_TYPE("PMIX_PERSIST", PMIX_PERSIST,
                       pmix_bfrop_pack_persist,
                       pmix_bfrop_unpack_persist,
                       pmix_bfrop_copy_persist,
                       pmix_bfrop_print_persist);

    /* All done */
    pmix_bfrop_initialized = true;
    return PMIX_SUCCESS;
}


pmix_status_t pmix_bfrop_close(void)
{
    int32_t i;

    if (!pmix_bfrop_initialized) {
        return PMIX_SUCCESS;
    }
    pmix_bfrop_initialized = false;

    for (i = 0 ; i < pmix_pointer_array_get_size(&pmix_bfrop_types) ; ++i) {
        pmix_bfrop_type_info_t *info = (pmix_bfrop_type_info_t*)pmix_pointer_array_get_item(&pmix_bfrop_types, i);
        if (NULL != info) {
            pmix_pointer_array_set_item(&pmix_bfrop_types, i, NULL);
            PMIX_RELEASE(info);
        }
    }

    PMIX_DESTRUCT(&pmix_bfrop_types);

    return PMIX_SUCCESS;
}

/**** UTILITY SUPPORT ****/
void pmix_value_load(pmix_value_t *v, void *data,
                     pmix_data_type_t type)
{
    v->type = type;
    if (NULL == data) {
        /* just set the fields to zero */
        memset(&v->data, 0, sizeof(v->data));
    } else {
        switch(type) {
        case PMIX_UNDEF:
            break;
        case PMIX_BOOL:
            memcpy(&(v->data.flag), data, 1);
            break;
        case PMIX_BYTE:
            memcpy(&(v->data.byte), data, 1);
            break;
        case PMIX_STRING:
            v->data.string = strdup(data);
            break;
        case PMIX_SIZE:
            memcpy(&(v->data.size), data, sizeof(size_t));
            break;
        case PMIX_PID:
            memcpy(&(v->data.pid), data, sizeof(pid_t));
            break;
        case PMIX_INT:
            memcpy(&(v->data.integer), data, sizeof(int));
            break;
        case PMIX_INT8:
            memcpy(&(v->data.int8), data, 1);
            break;
        case PMIX_INT16:
            memcpy(&(v->data.int16), data, 2);
            break;
        case PMIX_INT32:
            memcpy(&(v->data.int32), data, 4);
            break;
        case PMIX_INT64:
            memcpy(&(v->data.int64), data, 8);
            break;
        case PMIX_UINT:
            memcpy(&(v->data.uint), data, sizeof(int));
            break;
        case PMIX_UINT8:
            memcpy(&(v->data.uint8), data, 1);
            break;
        case PMIX_UINT16:
            memcpy(&(v->data.uint16), data, 2);
            break;
        case PMIX_UINT32:
            memcpy(&(v->data.uint32), data, 4);
            break;
        case PMIX_UINT64:
            memcpy(&(v->data.uint64), data, 8);
            break;
        case PMIX_FLOAT:
            memcpy(&(v->data.fval), data, sizeof(float));
            break;
        case PMIX_DOUBLE:
            memcpy(&(v->data.dval), data, sizeof(double));
            break;
        case PMIX_TIMEVAL:
            memcpy(&(v->data.tv), data, sizeof(struct timeval));
            break;
        case PMIX_BYTE_OBJECT:
            v->data.bo.bytes = data;
            memcpy(&(v->data.bo.size), data, sizeof(size_t));
            break;
        case PMIX_TIME:
        case PMIX_HWLOC_TOPO:
        case PMIX_VALUE:
        case PMIX_INFO_ARRAY:
        case PMIX_APP:
        case PMIX_INFO:
        case PMIX_PDATA:
        case PMIX_BUFFER:
        case PMIX_KVAL:
        case PMIX_MODEX:
        case PMIX_PERSIST:
        case PMIX_PROC:
            /* silence warnings */
            break;
        }
    }
}

pmix_status_t pmix_value_unload(pmix_value_t *kv, void **data,
                                size_t *sz, pmix_data_type_t type)
{
    pmix_status_t rc;

    rc = PMIX_SUCCESS;
    if (type != kv->type) {
        rc = PMIX_ERR_TYPE_MISMATCH;
    } else if (NULL == data ||
               (NULL == *data && PMIX_STRING != type && PMIX_BYTE_OBJECT != type)) {
        rc = PMIX_ERR_BAD_PARAM;
    } else {
        switch(type) {
        case PMIX_UNDEF:
            rc = PMIX_ERR_UNKNOWN_DATA_TYPE;
            break;
        case PMIX_BOOL:
            memcpy(*data, &(kv->data.flag), 1);
            *sz = 1;
            break;
        case PMIX_BYTE:
            memcpy(*data, &(kv->data.byte), 1);
            *sz = 1;
            break;
        case PMIX_STRING:
            if (NULL != kv->data.string) {
                *data = strdup(kv->data.string);
                *sz = strlen(kv->data.string);
            }
            break;
        case PMIX_SIZE:
            memcpy(*data, &(kv->data.size), sizeof(size_t));
            *sz = sizeof(size_t);
            break;
        case PMIX_PID:
            memcpy(*data, &(kv->data.pid), sizeof(pid_t));
            *sz = sizeof(pid_t);
            break;
        case PMIX_INT:
            memcpy(*data, &(kv->data.integer), sizeof(int));
            *sz = sizeof(int);
            break;
        case PMIX_INT8:
            memcpy(*data, &(kv->data.int8), 1);
            *sz = 1;
            break;
        case PMIX_INT16:
            memcpy(*data, &(kv->data.int16), 2);
            *sz = 2;
            break;
        case PMIX_INT32:
            memcpy(*data, &(kv->data.int32), 4);
            *sz = 4;
            break;
        case PMIX_INT64:
            memcpy(*data, &(kv->data.int64), 8);
            *sz = 8;
            break;
        case PMIX_UINT:
            memcpy(*data, &(kv->data.uint), sizeof(int));
            *sz = sizeof(int);
            break;
        case PMIX_UINT8:
            memcpy(*data, &(kv->data.uint8), 1);
            *sz = 1;
            break;
        case PMIX_UINT16:
            memcpy(*data, &(kv->data.uint16), 2);
            *sz = 2;
            break;
        case PMIX_UINT32:
            memcpy(*data, &(kv->data.uint32), 4);
            *sz = 4;
            break;
        case PMIX_UINT64:
            memcpy(*data, &(kv->data.uint64), 8);
            *sz = 8;
            break;
        case PMIX_FLOAT:
            memcpy(*data, &(kv->data.fval), sizeof(float));
            *sz = sizeof(float);
            break;
        case PMIX_DOUBLE:
            memcpy(*data, &(kv->data.dval), sizeof(double));
            *sz = sizeof(double);
            break;
        case PMIX_TIMEVAL:
            memcpy(*data, &(kv->data.tv), sizeof(struct timeval));
            *sz = sizeof(struct timeval);
            break;
        case PMIX_BYTE_OBJECT:
            if (NULL != kv->data.bo.bytes && 0 < kv->data.bo.size) {
                *data = kv->data.bo.bytes;
                *sz = kv->data.bo.size;
            } else {
                *data = NULL;
                *sz = 0;
            }
            break;
        case PMIX_TIME:
        case PMIX_HWLOC_TOPO:
        case PMIX_VALUE:
        case PMIX_INFO_ARRAY:
        case PMIX_APP:
        case PMIX_INFO:
        case PMIX_PDATA:
        case PMIX_BUFFER:
        case PMIX_KVAL:
        case PMIX_MODEX:
        case PMIX_PERSIST:
        case PMIX_PROC:
            /* silence warnings */
            rc = PMIX_ERROR;
            break;
        }
    }
    return rc;
}

