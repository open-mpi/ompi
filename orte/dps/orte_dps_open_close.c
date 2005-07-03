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

/* definitions
 */
#define ORTE_DPS_TABLE_PAGE_SIZE 10

/**
 * globals
 */
bool orte_dps_initialized = false;
bool orte_dps_debug = false;
int orte_dps_verbose = -1;  /* by default disabled */
int orte_dps_page_size;
orte_pointer_array_t *orte_dps_types;

OMPI_DECLSPEC orte_dps_t orte_dps = {
    orte_dps_pack,
    orte_dps_unpack,
    orte_dps_peek,
    orte_dps_unload,
    orte_dps_load,
    orte_dps_register,
    orte_dps_lookup_data_type
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
                   opal_object_t,
                   orte_buffer_construct,
                   orte_buffer_destruct);


int orte_dps_open(void)
{
    char *enviro_val;
    int id, page_size, rc;
    orte_data_type_t tmp;

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

    /* Setup the value array */

    if (ORTE_SUCCESS != (rc = orte_pointer_array_init(&orte_dps_types,
                                                      ORTE_DPS_ID_DYNAMIC,
                                                      ORTE_DPS_ID_MAX,
                                                      ORTE_DPS_TABLE_PAGE_SIZE))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* Register all the intrinsic types */

    tmp = ORTE_NULL;
    if (ORTE_SUCCESS != (rc = orte_dps.register_type(orte_dps_pack_null, 
                                          orte_dps_unpack_null,
                                          "ORTE_NULL", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    tmp = ORTE_BYTE;
    if (ORTE_SUCCESS != (rc = orte_dps.register_type(orte_dps_pack_byte, 
                                          orte_dps_unpack_byte,
                                          "ORTE_BYTE", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    tmp = ORTE_DATA_TYPE;
    if (ORTE_SUCCESS != (rc = orte_dps.register_type(orte_dps_pack_data_type, 
                                          orte_dps_unpack_data_type,
                                          "ORTE_DATA_TYPE", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    tmp = ORTE_BOOL;
    if (ORTE_SUCCESS != (rc = orte_dps.register_type(orte_dps_pack_bool, 
                                          orte_dps_unpack_bool,
                                          "ORTE_BOOL", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    tmp = ORTE_INT;
    if (ORTE_SUCCESS != (rc = orte_dps.register_type(orte_dps_pack_int, 
                                          orte_dps_unpack_int,
                                          "ORTE_INT", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    tmp = ORTE_UINT;
    if (ORTE_SUCCESS != (rc = orte_dps.register_type(orte_dps_pack_int, 
                                          orte_dps_unpack_int,
                                          "ORTE_UINT", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    tmp = ORTE_INT8;
    if (ORTE_SUCCESS != (rc = orte_dps.register_type(orte_dps_pack_byte, 
                                          orte_dps_unpack_byte,
                                          "ORTE_INT8", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    tmp = ORTE_UINT8;
    if (ORTE_SUCCESS != (rc = orte_dps.register_type(orte_dps_pack_byte, 
                                          orte_dps_unpack_byte,
                                          "ORTE_UINT8", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    tmp = ORTE_INT16;
    if (ORTE_SUCCESS != (rc = orte_dps.register_type(orte_dps_pack_int16, 
                                          orte_dps_unpack_int16,
                                          "ORTE_INT16", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    tmp = ORTE_UINT16;
    if (ORTE_SUCCESS != (rc = orte_dps.register_type(orte_dps_pack_int16, 
                                          orte_dps_unpack_int16,
                                          "ORTE_UINT16", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    tmp = ORTE_INT32;
    if (ORTE_SUCCESS != (rc = orte_dps.register_type(orte_dps_pack_int32, 
                                          orte_dps_unpack_int32,
                                          "ORTE_INT32", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    tmp = ORTE_UINT32;
    if (ORTE_SUCCESS != (rc = orte_dps.register_type(orte_dps_pack_int32, 
                                          orte_dps_unpack_int32,
                                          "ORTE_UINT32", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    tmp = ORTE_INT64;
    if (ORTE_SUCCESS != (rc = orte_dps.register_type(orte_dps_pack_int64, 
                                          orte_dps_unpack_int64,
                                          "ORTE_INT64", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    tmp = ORTE_UINT64;
    if (ORTE_SUCCESS != (rc = orte_dps.register_type(orte_dps_pack_int64, 
                                          orte_dps_unpack_int64,
                                          "ORTE_UINT64", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    tmp = ORTE_SIZE;
    if (ORTE_SUCCESS != (rc = orte_dps.register_type(orte_dps_pack_sizet, 
                                          orte_dps_unpack_sizet,
                                          "ORTE_SIZE", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    tmp = ORTE_PID;
    if (ORTE_SUCCESS != (rc = orte_dps.register_type(orte_dps_pack_pid, 
                                          orte_dps_unpack_pid,
                                          "ORTE_PID", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    tmp = ORTE_STRING;
    if (ORTE_SUCCESS != (rc = orte_dps.register_type(orte_dps_pack_string, 
                                          orte_dps_unpack_string,
                                          "ORTE_STRING", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    tmp = ORTE_BYTE_OBJECT;
    if (ORTE_SUCCESS != (rc = orte_dps.register_type(orte_dps_pack_byte_object, 
                                          orte_dps_unpack_byte_object,
                                          "ORTE_BYTE_OBJECT", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /* All done */
    
    return ORTE_SUCCESS;
}


int orte_dps_close(void)
{
    orte_dps_initialized = false;

    return ORTE_SUCCESS;
}
