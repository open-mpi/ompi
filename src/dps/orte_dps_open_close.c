/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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
/** @file:
 *
 */
#include "orte_config.h"
#include "include/orte_types.h"

#include "mca/base/mca_base_param.h"
#include "mca/errmgr/errmgr.h"
#include "mca/ns/ns.h"

#include "dps/dps_internal.h"

/**
 * globals
 */
bool orte_dps_initialized = false;
bool orte_dps_debug = false;
int orte_dps_page_size;

orte_dps_t orte_dps = {
    orte_dps_pack,
    orte_dps_unpack,
    orte_dps_peek,
    orte_dps_unload,
    orte_dps_load
};

/**
 * Object constructors, destructors, and instantiations
 */
static void orte_buffer_construct (orte_buffer_t* buffer)
{
    /* Make everything NULL to begin with */

    buffer->base_ptr = buffer->pack_ptr = buffer->unpack_ptr = NULL;
    buffer->bytes_allocated = buffer->bytes_used = buffer->bytes_avail = 0;
}

static void orte_buffer_destruct (orte_buffer_t* buffer)
{
    if (NULL != buffer) {
        if (NULL != buffer->base_ptr) {
            free (buffer->base_ptr);
        }
    }
}

OBJ_CLASS_INSTANCE(orte_buffer_t,
                   ompi_object_t,
                   orte_buffer_construct,
                   orte_buffer_destruct);


int orte_dps_open(void)
{
    char *enviro_val;
    int id, page_size, rc;

    if (orte_dps_initialized) {
        return ORTE_SUCCESS;
    }

    enviro_val = getenv("ORTE_dps_debug");
    if (NULL != enviro_val) {  /* debug requested */
        orte_dps_debug = true;
    } else {
        orte_dps_debug = false;
    }

    /* setup the page size, convert to bytes */
    id = mca_base_param_register_int("dps", "page", "size", NULL, ORTE_DPS_DEFAULT_PAGE_SIZE);
    mca_base_param_lookup_int(id, &page_size);
    orte_dps_page_size = 1024 * page_size;

    /* Register all the intrinsic types */

    if (ORTE_SUCCESS != (rc = orte_ns.define_data_type(orte_dps_pack_null, 
                                          orte_dps_unpack_null,
                                          "ORTE_NULL", ORTE_NULL))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    if (ORTE_SUCCESS != (rc = orte_ns.define_data_type(orte_dps_pack_byte, 
                                          orte_dps_unpack_byte,
                                          "ORTE_BYTE", ORTE_BYTE))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    if (ORTE_SUCCESS != (rc = orte_ns.define_data_type(orte_dps_pack_data_type, 
                                          orte_dps_unpack_data_type,
                                          "ORTE_DATA_TYPE", ORTE_DATA_TYPE))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    if (ORTE_SUCCESS != (rc = orte_ns.define_data_type(orte_dps_pack_bool, 
                                          orte_dps_unpack_bool,
                                          "ORTE_BOOL", ORTE_BOOL))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    if (ORTE_SUCCESS != (rc = orte_ns.define_data_type(orte_dps_pack_int, 
                                          orte_dps_unpack_int,
                                          "ORTE_INT", ORTE_INT))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    if (ORTE_SUCCESS != (rc = orte_ns.define_data_type(orte_dps_pack_int, 
                                          orte_dps_unpack_int,
                                          "ORTE_UINT", ORTE_UINT))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    if (ORTE_SUCCESS != (rc = orte_ns.define_data_type(orte_dps_pack_byte, 
                                          orte_dps_unpack_byte,
                                          "ORTE_INT8", ORTE_INT8))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    if (ORTE_SUCCESS != (rc = orte_ns.define_data_type(orte_dps_pack_byte, 
                                          orte_dps_unpack_byte,
                                          "ORTE_UINT8", ORTE_UINT8))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    if (ORTE_SUCCESS != (rc = orte_ns.define_data_type(orte_dps_pack_int16, 
                                          orte_dps_unpack_int16,
                                          "ORTE_INT16", ORTE_INT16))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    if (ORTE_SUCCESS != (rc = orte_ns.define_data_type(orte_dps_pack_int16, 
                                          orte_dps_unpack_int16,
                                          "ORTE_UINT16", ORTE_UINT16))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    if (ORTE_SUCCESS != (rc = orte_ns.define_data_type(orte_dps_pack_int32, 
                                          orte_dps_unpack_int32,
                                          "ORTE_INT32", ORTE_INT32))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    if (ORTE_SUCCESS != (rc = orte_ns.define_data_type(orte_dps_pack_int32, 
                                          orte_dps_unpack_int32,
                                          "ORTE_UINT32", ORTE_UINT32))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    if (ORTE_SUCCESS != (rc = orte_ns.define_data_type(orte_dps_pack_int64, 
                                          orte_dps_unpack_int64,
                                          "ORTE_INT64", ORTE_INT64))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    if (ORTE_SUCCESS != (rc = orte_ns.define_data_type(orte_dps_pack_int64, 
                                          orte_dps_unpack_int64,
                                          "ORTE_UINT64", ORTE_UINT64))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    if (ORTE_SUCCESS != (rc = orte_ns.define_data_type(orte_dps_pack_sizet, 
                                          orte_dps_unpack_sizet,
                                          "ORTE_SIZE", ORTE_SIZE))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    if (ORTE_SUCCESS != (rc = orte_ns.define_data_type(orte_dps_pack_string, 
                                          orte_dps_unpack_string,
                                          "ORTE_STRING", ORTE_STRING))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    if (ORTE_SUCCESS != (rc = orte_ns.define_data_type(orte_dps_pack_byte_object, 
                                          orte_dps_unpack_byte_object,
                                          "ORTE_BYTE_OBJECT", ORTE_BYTE_OBJECT))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /* All done */
    
    return ORTE_SUCCESS;
}


int orte_dps_close(void)
{
    size_t i;
    orte_dps_type_info_t *info;

    for (i = 0; i < orte_value_array_get_size(&orte_dps_types); ++i) {
        info = orte_value_array_get_item(&orte_dps_types, i);
        if (NULL != info->odti_name) {
            free(info->odti_name);
        }
    }
    OBJ_DESTRUCT(&orte_dps_types);

    orte_dps_initialized = false;

    return ORTE_SUCCESS;
}
