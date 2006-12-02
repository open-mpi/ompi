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
/** @file:
 *
 */
#include "orte_config.h"
#include "orte/orte_types.h"

#include "opal/mca/base/mca_base_param.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ns/ns.h"

#include "orte/dss/dss_internal.h"

/**
 * globals
 */
bool orte_dss_initialized = false;
bool orte_dss_debug = false;
int orte_dss_verbose = -1;  /* by default disabled */
int orte_dss_page_size;
orte_pointer_array_t *orte_dss_types;
orte_data_type_t orte_dss_num_reg_types;
orte_dss_buffer_type_t default_buf_type;

orte_dss_t orte_dss = {
    orte_dss_set,
    orte_dss_get,
    orte_dss_arith,
    orte_dss_increment,
    orte_dss_decrement,
    orte_dss_set_buffer_type,
    orte_dss_pack,
    orte_dss_unpack,
    orte_dss_copy,
    orte_dss_compare,
    orte_dss_size,
    orte_dss_print,
    orte_dss_release,
    orte_dss_peek,
    orte_dss_unload,
    orte_dss_load,
    orte_dss_register,
    orte_dss_lookup_data_type,
    orte_dss_dump_data_types,
    orte_dss_dump
};

/**
 * Object constructors, destructors, and instantiations
 */
/** Data Value **/
/* constructor - used to initialize state of data value instance */
static void orte_data_value_construct(orte_data_value_t* ptr)
{
    ptr->type = ORTE_UNDEF;
    ptr->data = NULL;
}
/* destructor - used to release data value instance */
static void orte_data_value_destruct(orte_data_value_t* ptr)
{
    if (NULL != ptr->data) {
        orte_dss.release(ptr);
    }
}

/* define instance of opal_class_t */
OBJ_CLASS_INSTANCE(
    orte_data_value_t,              /* type name */
    opal_object_t,                  /* parent "class" name */
    orte_data_value_construct,      /* constructor */
    orte_data_value_destruct);      /* destructor */


static void orte_buffer_construct (orte_buffer_t* buffer)
{
    /** set the default buffer type */
    buffer->type = default_buf_type;

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


static void orte_dss_type_info_construct(orte_dss_type_info_t *obj)
{
    obj->odti_name = NULL;
    obj->odti_pack_fn = NULL;
    obj->odti_unpack_fn = NULL;
    obj->odti_copy_fn = NULL;
    obj->odti_compare_fn = NULL;
    obj->odti_size_fn = NULL;
    obj->odti_print_fn = NULL;
    obj->odti_release_fn = NULL;
    obj->odti_structured = false;
}

static void orte_dss_type_info_destruct(orte_dss_type_info_t *obj)
{
    if (NULL != obj->odti_name) {
        free(obj->odti_name);
    }
}

OBJ_CLASS_INSTANCE(orte_dss_type_info_t, opal_object_t,
                   orte_dss_type_info_construct,
                   orte_dss_type_info_destruct);


int orte_dss_open(void)
{
    char *enviro_val;
    int id, page_size, rc;
    orte_data_type_t tmp;
    int def_type;

    if (orte_dss_initialized) {
        return ORTE_SUCCESS;
    }

    enviro_val = getenv("ORTE_dss_debug");
    if (NULL != enviro_val) {  /* debug requested */
        orte_dss_debug = true;
    } else {
        orte_dss_debug = false;
    }

    /** set the default buffer type. If we are in debug mode, then we default
     * to fully described buffers. Otherwise, we default to non-described for brevity
     * and performance
     */
#if OMPI_ENABLE_DEBUG
    def_type = ORTE_DSS_BUFFER_FULLY_DESC;
#else
    def_type = ORTE_DSS_BUFFER_NON_DESC;
#endif

    id = mca_base_param_register_int("dss", "buffer", "type",
                                     "Set the default mode for OpenRTE buffers (0=non-described, 1=described",
                                     def_type);
    mca_base_param_lookup_int(id, &rc);
    default_buf_type = rc;

    /* setup the page size -this is for use by the BUFFER system, NOT the data type
       manager that keeps track of registered data types!! It must be converted to
       bytes since the buffer system will allocate a "page_size" at a time.
    */
    id = mca_base_param_register_int("dss", "page", "size", NULL, ORTE_DSS_DEFAULT_PAGE_SIZE);
    mca_base_param_lookup_int(id, &page_size);
    orte_dss_page_size = 1024*page_size;

    /* Setup the types array */

    if (ORTE_SUCCESS != (rc = orte_pointer_array_init(&orte_dss_types,
                                                      ORTE_DSS_ID_DYNAMIC,
                                                      ORTE_DSS_ID_MAX,
                                                      ORTE_DSS_ID_MAX))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    orte_dss_num_reg_types = 0;

    /* Register all the intrinsic types */

    tmp = ORTE_NULL;
    if (ORTE_SUCCESS != (rc = orte_dss.register_type(orte_dss_pack_null,
                                          orte_dss_unpack_null,
                                          (orte_dss_copy_fn_t)orte_dss_copy_null,
                                          (orte_dss_compare_fn_t)orte_dss_compare_null,
                                          (orte_dss_size_fn_t)orte_dss_std_size,
                                          (orte_dss_print_fn_t)orte_dss_print_null,
                                          (orte_dss_release_fn_t)orte_dss_std_release,
                                          ORTE_DSS_UNSTRUCTURED,
                                          "ORTE_NULL", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    tmp = ORTE_BYTE;
    if (ORTE_SUCCESS != (rc = orte_dss.register_type(orte_dss_pack_byte,
                                          orte_dss_unpack_byte,
                                          (orte_dss_copy_fn_t)orte_dss_std_copy,
                                          (orte_dss_compare_fn_t)orte_dss_compare_byte,
                                          (orte_dss_size_fn_t)orte_dss_std_size,
                                          (orte_dss_print_fn_t)orte_dss_print_byte,
                                          (orte_dss_release_fn_t)orte_dss_std_release,
                                          ORTE_DSS_UNSTRUCTURED,
                                          "ORTE_BYTE", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    tmp = ORTE_BOOL;
    if (ORTE_SUCCESS != (rc = orte_dss.register_type(orte_dss_pack_bool,
                                          orte_dss_unpack_bool,
                                          (orte_dss_copy_fn_t)orte_dss_std_copy,
                                          (orte_dss_compare_fn_t)orte_dss_compare_bool,
                                          (orte_dss_size_fn_t)orte_dss_std_size,
                                          (orte_dss_print_fn_t)orte_dss_print_bool,
                                          (orte_dss_release_fn_t)orte_dss_std_release,
                                          ORTE_DSS_UNSTRUCTURED,
                                          "ORTE_BOOL", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    tmp = ORTE_INT;
    if (ORTE_SUCCESS != (rc = orte_dss.register_type(orte_dss_pack_int,
                                          orte_dss_unpack_int,
                                          (orte_dss_copy_fn_t)orte_dss_std_copy,
                                          (orte_dss_compare_fn_t)orte_dss_compare_int,
                                          (orte_dss_size_fn_t)orte_dss_std_size,
                                          (orte_dss_print_fn_t)orte_dss_print_int,
                                          (orte_dss_release_fn_t)orte_dss_std_release,
                                          ORTE_DSS_UNSTRUCTURED,
                                          "ORTE_INT", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    tmp = ORTE_UINT;
    if (ORTE_SUCCESS != (rc = orte_dss.register_type(orte_dss_pack_int,
                                          orte_dss_unpack_int,
                                          (orte_dss_copy_fn_t)orte_dss_std_copy,
                                          (orte_dss_compare_fn_t)orte_dss_compare_uint,
                                          (orte_dss_size_fn_t)orte_dss_std_size,
                                          (orte_dss_print_fn_t)orte_dss_print_uint,
                                          (orte_dss_release_fn_t)orte_dss_std_release,
                                          ORTE_DSS_UNSTRUCTURED,
                                          "ORTE_UINT", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    tmp = ORTE_INT8;
    if (ORTE_SUCCESS != (rc = orte_dss.register_type(orte_dss_pack_byte,
                                          orte_dss_unpack_byte,
                                          (orte_dss_copy_fn_t)orte_dss_std_copy,
                                          (orte_dss_compare_fn_t)orte_dss_compare_int8,
                                          (orte_dss_size_fn_t)orte_dss_std_size,
                                          (orte_dss_print_fn_t)orte_dss_print_int8,
                                          (orte_dss_release_fn_t)orte_dss_std_release,
                                          ORTE_DSS_UNSTRUCTURED,
                                          "ORTE_INT8", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    tmp = ORTE_UINT8;
    if (ORTE_SUCCESS != (rc = orte_dss.register_type(orte_dss_pack_byte,
                                          orte_dss_unpack_byte,
                                          (orte_dss_copy_fn_t)orte_dss_std_copy,
                                          (orte_dss_compare_fn_t)orte_dss_compare_uint8,
                                          (orte_dss_size_fn_t)orte_dss_std_size,
                                          (orte_dss_print_fn_t)orte_dss_print_uint8,
                                          (orte_dss_release_fn_t)orte_dss_std_release,
                                          ORTE_DSS_UNSTRUCTURED,
                                          "ORTE_UINT8", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    tmp = ORTE_INT16;
    if (ORTE_SUCCESS != (rc = orte_dss.register_type(orte_dss_pack_int16,
                                          orte_dss_unpack_int16,
                                          (orte_dss_copy_fn_t)orte_dss_std_copy,
                                          (orte_dss_compare_fn_t)orte_dss_compare_int16,
                                          (orte_dss_size_fn_t)orte_dss_std_size,
                                          (orte_dss_print_fn_t)orte_dss_print_int16,
                                          (orte_dss_release_fn_t)orte_dss_std_release,
                                          ORTE_DSS_UNSTRUCTURED,
                                          "ORTE_INT16", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    tmp = ORTE_UINT16;
    if (ORTE_SUCCESS != (rc = orte_dss.register_type(orte_dss_pack_int16,
                                          orte_dss_unpack_int16,
                                          (orte_dss_copy_fn_t)orte_dss_std_copy,
                                          (orte_dss_compare_fn_t)orte_dss_compare_uint16,
                                          (orte_dss_size_fn_t)orte_dss_std_size,
                                          (orte_dss_print_fn_t)orte_dss_print_uint16,
                                          (orte_dss_release_fn_t)orte_dss_std_release,
                                          ORTE_DSS_UNSTRUCTURED,
                                          "ORTE_UINT16", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    tmp = ORTE_INT32;
    if (ORTE_SUCCESS != (rc = orte_dss.register_type(orte_dss_pack_int32,
                                          orte_dss_unpack_int32,
                                          (orte_dss_copy_fn_t)orte_dss_std_copy,
                                          (orte_dss_compare_fn_t)orte_dss_compare_int32,
                                          (orte_dss_size_fn_t)orte_dss_std_size,
                                          (orte_dss_print_fn_t)orte_dss_print_int32,
                                          (orte_dss_release_fn_t)orte_dss_std_release,
                                          ORTE_DSS_UNSTRUCTURED,
                                          "ORTE_INT32", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    tmp = ORTE_UINT32;
    if (ORTE_SUCCESS != (rc = orte_dss.register_type(orte_dss_pack_int32,
                                          orte_dss_unpack_int32,
                                          (orte_dss_copy_fn_t)orte_dss_std_copy,
                                          (orte_dss_compare_fn_t)orte_dss_compare_uint32,
                                          (orte_dss_size_fn_t)orte_dss_std_size,
                                          (orte_dss_print_fn_t)orte_dss_print_uint32,
                                          (orte_dss_release_fn_t)orte_dss_std_release,
                                          ORTE_DSS_UNSTRUCTURED,
                                          "ORTE_UINT32", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    tmp = ORTE_INT64;
    if (ORTE_SUCCESS != (rc = orte_dss.register_type(orte_dss_pack_int64,
                                          orte_dss_unpack_int64,
                                          (orte_dss_copy_fn_t)orte_dss_std_copy,
                                          (orte_dss_compare_fn_t)orte_dss_compare_int64,
                                          (orte_dss_size_fn_t)orte_dss_std_size,
                                          (orte_dss_print_fn_t)orte_dss_print_int64,
                                          (orte_dss_release_fn_t)orte_dss_std_release,
                                          ORTE_DSS_UNSTRUCTURED,
                                          "ORTE_INT64", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    tmp = ORTE_UINT64;
    if (ORTE_SUCCESS != (rc = orte_dss.register_type(orte_dss_pack_int64,
                                          orte_dss_unpack_int64,
                                          (orte_dss_copy_fn_t)orte_dss_std_copy,
                                          (orte_dss_compare_fn_t)orte_dss_compare_uint64,
                                          (orte_dss_size_fn_t)orte_dss_std_size,
                                          (orte_dss_print_fn_t)orte_dss_print_uint64,
                                          (orte_dss_release_fn_t)orte_dss_std_release,
                                          ORTE_DSS_UNSTRUCTURED,
                                          "ORTE_UINT64", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    tmp = ORTE_SIZE;
    if (ORTE_SUCCESS != (rc = orte_dss.register_type(orte_dss_pack_sizet,
                                          orte_dss_unpack_sizet,
                                          (orte_dss_copy_fn_t)orte_dss_std_copy,
                                          (orte_dss_compare_fn_t)orte_dss_compare_size,
                                          (orte_dss_size_fn_t)orte_dss_std_size,
                                          (orte_dss_print_fn_t)orte_dss_print_size,
                                          (orte_dss_release_fn_t)orte_dss_std_release,
                                          ORTE_DSS_UNSTRUCTURED,
                                          "ORTE_SIZE", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    tmp = ORTE_PID;
    if (ORTE_SUCCESS != (rc = orte_dss.register_type(orte_dss_pack_pid,
                                          orte_dss_unpack_pid,
                                          (orte_dss_copy_fn_t)orte_dss_std_copy,
                                          (orte_dss_compare_fn_t)orte_dss_compare_pid,
                                          (orte_dss_size_fn_t)orte_dss_std_size,
                                          (orte_dss_print_fn_t)orte_dss_print_pid,
                                          (orte_dss_release_fn_t)orte_dss_std_release,
                                          ORTE_DSS_UNSTRUCTURED,
                                          "ORTE_PID", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    tmp = ORTE_STRING;
    if (ORTE_SUCCESS != (rc = orte_dss.register_type(orte_dss_pack_string,
                                          orte_dss_unpack_string,
                                          (orte_dss_copy_fn_t)orte_dss_copy_string,
                                          (orte_dss_compare_fn_t)orte_dss_compare_string,
                                          (orte_dss_size_fn_t)orte_dss_size_string,
                                          (orte_dss_print_fn_t)orte_dss_print_string,
                                          (orte_dss_release_fn_t)orte_dss_std_release,
                                          ORTE_DSS_STRUCTURED,
                                          "ORTE_STRING", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    tmp = ORTE_STD_CNTR;
    if (ORTE_SUCCESS != (rc = orte_dss.register_type(orte_dss_pack_std_cntr,
                                          orte_dss_unpack_std_cntr,
                                          (orte_dss_copy_fn_t)orte_dss_std_copy,
                                          (orte_dss_compare_fn_t)orte_dss_compare_std_cntr,
                                          (orte_dss_size_fn_t)orte_dss_std_size,
                                          (orte_dss_print_fn_t)orte_dss_print_std_cntr,
                                          (orte_dss_release_fn_t)orte_dss_std_release,
                                          ORTE_DSS_UNSTRUCTURED,
                                          "ORTE_STD_CNTR", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    tmp = ORTE_DATA_TYPE;
    if (ORTE_SUCCESS != (rc = orte_dss.register_type(orte_dss_pack_data_type,
                                          orte_dss_unpack_data_type,
                                          (orte_dss_copy_fn_t)orte_dss_std_copy,
                                          (orte_dss_compare_fn_t)orte_dss_compare_dt,
                                          (orte_dss_size_fn_t)orte_dss_std_size,
                                          (orte_dss_print_fn_t)orte_dss_print_data_type,
                                          (orte_dss_release_fn_t)orte_dss_std_release,
                                          ORTE_DSS_UNSTRUCTURED,
                                          "ORTE_DATA_TYPE", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    tmp = ORTE_DATA_VALUE;
    if (ORTE_SUCCESS != (rc = orte_dss.register_type(orte_dss_pack_data_value,
                                          orte_dss_unpack_data_value,
                                          (orte_dss_copy_fn_t)orte_dss_copy_data_value,
                                          (orte_dss_compare_fn_t)orte_dss_compare_data_value,
                                          (orte_dss_size_fn_t)orte_dss_size_data_value,
                                          (orte_dss_print_fn_t)orte_dss_print_data_value,
                                          (orte_dss_release_fn_t)orte_dss_std_obj_release,
                                          ORTE_DSS_STRUCTURED,
                                          "ORTE_DATA_VALUE", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    tmp = ORTE_BYTE_OBJECT;
    if (ORTE_SUCCESS != (rc = orte_dss.register_type(orte_dss_pack_byte_object,
                                          orte_dss_unpack_byte_object,
                                          (orte_dss_copy_fn_t)orte_dss_copy_byte_object,
                                          (orte_dss_compare_fn_t)orte_dss_compare_byte_object,
                                          (orte_dss_size_fn_t)orte_dss_size_byte_object,
                                          (orte_dss_print_fn_t)orte_dss_print_byte_object,
                                          (orte_dss_release_fn_t)orte_dss_release_byte_object,
                                          ORTE_DSS_STRUCTURED,
                                          "ORTE_BYTE_OBJECT", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /* All done */

    return ORTE_SUCCESS;
}


int orte_dss_close(void)
{
    orte_std_cntr_t i;

    orte_dss_initialized = false;

    for (i = 0 ; i < orte_pointer_array_get_size(orte_dss_types) ; ++i) {
        orte_dss_type_info_t *info = (orte_dss_type_info_t*)orte_pointer_array_get_item(orte_dss_types, i);
        if (NULL != info) {
            OBJ_RELEASE(info);
        }
    }

    OBJ_RELEASE(orte_dss_types);

    return ORTE_SUCCESS;
}
