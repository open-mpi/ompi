/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
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

#include "dps_internal.h"

/**
 * globals
 */
bool orte_dps_debug;
int orte_dps_page_size;

orte_dps_t orte_dps = {
    orte_dps_pack,
    orte_dps_unpack,
    orte_dps_pack_nobuffer,
    orte_dps_unpack_nobuffer,
    orte_dps_peek,
    orte_dps_unload,
    orte_dps_load,
    orte_dps_dump_buffer
};

/**
 * Object constructors, destructors, and instantiations
 */
static void orte_buffer_construct (orte_buffer_t* buffer)
{
    /* allocate the initial data space */
    buffer->base_ptr = (void *)malloc(orte_dps_page_size);
    if (NULL == buffer->base_ptr) {  /* got an error = can't init */
        buffer->base_ptr = buffer->data_ptr = buffer->from_ptr = NULL;
        buffer->pages = buffer->size = buffer->len = buffer-> space = buffer-> toend = 0;
    } else {
        buffer->data_ptr = buffer->base_ptr;
        buffer->from_ptr = buffer->base_ptr;
        buffer->pages = 1;
        buffer->size = orte_dps_page_size;
        buffer->space = orte_dps_page_size;
        buffer->len = 0;
        buffer->toend = 0;
    }
}

static void orte_buffer_destruct (orte_buffer_t* buffer)
{
    /* paranoid check */
    if (NULL != buffer) {
        if (NULL != buffer->base_ptr) free (buffer->base_ptr);
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
    
    enviro_val = getenv("ORTE_dps_debug");
    if (NULL != enviro_val) {  /* debug requested */
        orte_dps_debug = true;
    } else {
        orte_dps_debug = false;
    }

    /* setup the page size */
    id = mca_base_param_register_int("dps", "page", "size", NULL, ORTE_DPS_DEFAULT_PAGE_SIZE);
    mca_base_param_lookup_int(id, &page_size);
    orte_dps_page_size = 1000*page_size;  /* convert to bytes */
    
    return ORTE_SUCCESS;
}

int orte_dps_close(void)
{
    /* no idea what this would do right now - include it for completeness */
    return ORTE_SUCCESS;
}
