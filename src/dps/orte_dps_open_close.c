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

#include "mca/base/mca_base_param.h"

#include "dps/dps_internal.h"

/**
 * globals
 */
bool orte_dps_initialized = false;
bool orte_dps_debug = false;
int orte_dps_page_size;
orte_value_array_t orte_dps_types;

orte_data_type_t ORTE_NULL = -1;
orte_data_type_t ORTE_BYTE = -1;
orte_data_type_t ORTE_DATATYPE = -1;

orte_data_type_t ORTE_BOOL = -1;

orte_data_type_t ORTE_INT = -1;
orte_data_type_t ORTE_UINT = -1;
orte_data_type_t ORTE_INT8 = -1;
orte_data_type_t ORTE_UINT8 = -1;
orte_data_type_t ORTE_INT16 = -1;
orte_data_type_t ORTE_UINT16 = -1;
orte_data_type_t ORTE_INT32 = -1;
orte_data_type_t ORTE_UINT32 = -1;
orte_data_type_t ORTE_INT64 = -1;
orte_data_type_t ORTE_UINT64 = -1;

orte_data_type_t ORTE_SIZE = -1;

orte_data_type_t ORTE_STRING = -1;

orte_dps_t orte_dps = {
    orte_dps_pack,
    orte_dps_unpack,
    orte_dps_peek,
    orte_dps_unload,
    orte_dps_load,
    orte_dps_register
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
    int id, page_size;

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

    OBJ_CONSTRUCT(&orte_dps_types, orte_value_array_t);
    orte_value_array_init(&orte_dps_types, sizeof(orte_dps_type_info_t));

    /* Register all the intrinsic types */

    if (ORTE_SUCCESS != orte_dps_register(orte_dps_pack_null, 
                                          orte_dps_unpack_null,
                                          "ORTE_NULL", &ORTE_NULL) ||
        ORTE_SUCCESS != orte_dps_register(orte_dps_pack_byte, 
                                          orte_dps_unpack_byte,
                                          "ORTE_BYTE", &ORTE_BYTE) ||
        ORTE_SUCCESS != orte_dps_register(orte_dps_pack_byte, 
                                          orte_dps_unpack_byte,
                                          "ORTE_DATA_TYPE", &ORTE_DATATYPE) ||

        ORTE_SUCCESS != orte_dps_register(orte_dps_pack_bool, 
                                          orte_dps_unpack_bool,
                                          "ORTE_BOOL", &ORTE_BOOL) ||

        ORTE_SUCCESS != orte_dps_register(orte_dps_pack_int, 
                                          orte_dps_unpack_int,
                                          "ORTE_INT", &ORTE_INT) ||
        ORTE_SUCCESS != orte_dps_register(orte_dps_pack_int, 
                                          orte_dps_unpack_int,
                                          "ORTE_UINT", &ORTE_UINT) ||
        ORTE_SUCCESS != orte_dps_register(orte_dps_pack_byte, 
                                          orte_dps_unpack_byte,
                                          "ORTE_INT8", &ORTE_INT8) ||
        ORTE_SUCCESS != orte_dps_register(orte_dps_pack_byte, 
                                          orte_dps_unpack_byte,
                                          "ORTE_UINT8", &ORTE_UINT8) ||
        ORTE_SUCCESS != orte_dps_register(orte_dps_pack_int16, 
                                          orte_dps_unpack_int16,
                                          "ORTE_INT16", &ORTE_INT16) ||
        ORTE_SUCCESS != orte_dps_register(orte_dps_pack_int16, 
                                          orte_dps_unpack_int16,
                                          "ORTE_UINT16", &ORTE_UINT16) ||
        ORTE_SUCCESS != orte_dps_register(orte_dps_pack_int32, 
                                          orte_dps_unpack_int32,
                                          "ORTE_INT32", &ORTE_INT32) ||
        ORTE_SUCCESS != orte_dps_register(orte_dps_pack_int32, 
                                          orte_dps_unpack_int32,
                                          "ORTE_UINT32", &ORTE_UINT32) ||
        ORTE_SUCCESS != orte_dps_register(orte_dps_pack_int64, 
                                          orte_dps_unpack_int64,
                                          "ORTE_INT64", &ORTE_INT64) ||
        ORTE_SUCCESS != orte_dps_register(orte_dps_pack_int64, 
                                          orte_dps_unpack_int64,
                                          "ORTE_UINT64", &ORTE_UINT64) ||

        ORTE_SUCCESS != orte_dps_register(orte_dps_pack_sizet, 
                                          orte_dps_unpack_sizet,
                                          "ORTE_SIZE", &ORTE_SIZE) ||

        ORTE_SUCCESS != orte_dps_register(orte_dps_pack_string, 
                                          orte_dps_unpack_string,
                                          "ORTE_STRING", &ORTE_STRING) ||
        0) {
        return ORTE_ERROR;
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
