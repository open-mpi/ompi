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

#include "orte/mca/errmgr/errmgr.h"

#include "orte/dss/dss_internal.h"
#include "opal/util/output.h"

int orte_dss_copy(void **dest, void *src, orte_data_type_t type)
{
    int rc;
    orte_dss_type_info_t *info;

    /* check for error */
    if (NULL == dest) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }
    if (NULL == src && (ORTE_NULL != type && ORTE_STRING != type)) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }

   /* Lookup the copy function for this type and call it */

    if (!(type < orte_dss_types->size) ||
        (NULL == (info = (orte_dss_type_info_t*)orte_pointer_array_get_item(orte_dss_types, type)))) {
        ORTE_ERROR_LOG(ORTE_ERR_UNKNOWN_DATA_TYPE);
        return ORTE_ERR_UNKNOWN_DATA_TYPE;
    }

    if (ORTE_SUCCESS != (rc = info->odti_copy_fn(dest, src, type))) {
        ORTE_ERROR_LOG(rc);
    }


    return rc;
}

/*
 * STANDARD COPY FUNCTION - WORKS FOR EVERYTHING NON-STRUCTURED
 */
int orte_dss_std_copy(void **dest, void *src, orte_data_type_t type)
{
    size_t datasize;
    uint8_t *val;

    switch(type) {
        case ORTE_BOOL:
            datasize = sizeof(bool);
            break;

        case ORTE_INT:
        case ORTE_UINT:
            datasize = sizeof(int);
            break;

        case ORTE_SIZE:
            datasize = sizeof(size_t);
            break;

        case ORTE_PID:
            datasize = sizeof(pid_t);
            break;

        case ORTE_BYTE:
        case ORTE_INT8:
        case ORTE_UINT8:
            datasize = 1;
            break;

        case ORTE_INT16:
        case ORTE_UINT16:
            datasize = 2;
            break;

        case ORTE_INT32:
        case ORTE_UINT32:
            datasize = 4;
            break;

        case ORTE_INT64:
        case ORTE_UINT64:
            datasize = 8;
            break;

        case ORTE_STD_CNTR:
            datasize = sizeof(orte_std_cntr_t);
            break;

        case ORTE_DATA_TYPE:
            datasize = sizeof(orte_data_type_t);
            break;

        default:
            ORTE_ERROR_LOG(ORTE_ERR_UNKNOWN_DATA_TYPE);
            return ORTE_ERR_UNKNOWN_DATA_TYPE;
    }

    val = (uint8_t*)malloc(datasize);
    if (NULL == val) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    memcpy(val, src, datasize);
    *dest = val;

    return ORTE_SUCCESS;
}

/* COPY FUNCTIONS FOR NON-STANDARD SYSTEM TYPES */

/*
 * NULL
 */
int orte_dss_copy_null(char **dest, char *src, orte_data_type_t type)
{
    char *val;

    *dest = (char*)malloc(sizeof(char*));
    if (NULL == *dest) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    val = *dest;  /* save the address of the value */

    /* set the dest to null */
    *val = 0x00;

    return ORTE_SUCCESS;
}

/*
 * STRING
 */
int orte_dss_copy_string(char **dest, char *src, orte_data_type_t type)
{
    if (NULL == src) {  /* got zero-length string/NULL pointer - store NULL */
        *dest = NULL;
    } else {
        *dest = strdup(src);
    }

    return ORTE_SUCCESS;
}

/* COPY FUNCTIONS FOR GENERIC ORTE TYPES */

/*
 * ORTE_DATA_VALUE
 */
int orte_dss_copy_data_value(orte_data_value_t **dest, orte_data_value_t *src,
                             orte_data_type_t type)
{
    int rc;

    /* create the new object */
    *dest = OBJ_NEW(orte_data_value_t);
    if (NULL == *dest) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    (*dest)->type = src->type;

    /* copy the payload with its associated copy function */
    if (ORTE_SUCCESS != (rc = orte_dss.copy(&((*dest)->data), src->data, src->type))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(*dest);
        return rc;
    }

    return ORTE_SUCCESS;
}


/*
 * ORTE_BYTE_OBJECT
 */
int orte_dss_copy_byte_object(orte_byte_object_t **dest, orte_byte_object_t *src,
                              orte_data_type_t type)
{
    /* allocate space for the new object */
    *dest = (orte_byte_object_t*)malloc(sizeof(orte_byte_object_t));
    if (NULL == *dest) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    (*dest)->size = src->size;

    /* allocate the required space for the bytes */
    (*dest)->bytes = (uint8_t*)malloc(src->size);
    if (NULL == (*dest)->bytes) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_RELEASE(*dest);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    /* copy the data across */
    memcpy((*dest)->bytes, src->bytes, src->size);

    return ORTE_SUCCESS;
}
