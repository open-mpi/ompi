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

#include "orte/dss/dss_internal.h"

int orte_dss_size(size_t *size, void *src, orte_data_type_t type)
{
    int rc;
    orte_dss_type_info_t *info;

    /* check for error */
    if (NULL == size) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }

    /* Lookup the size function for this type and call it */

    if (!(type < orte_dss_types->size) ||
          (NULL == (info = (orte_dss_type_info_t*)orte_pointer_array_get_item(orte_dss_types, type)))) {
        ORTE_ERROR_LOG(ORTE_ERR_UNKNOWN_DATA_TYPE);
        return ORTE_ERR_UNKNOWN_DATA_TYPE;
    }

    if (ORTE_SUCCESS != (rc = info->odti_size_fn(size, src, type))) {
        ORTE_ERROR_LOG(rc);
    }


    return rc;
}

/*
 * STANDARD SIZE FUNCTION - WORKS FOR EVERYTHING NON-STRUCTURED
 */
int orte_dss_std_size(size_t *size, void *src, orte_data_type_t type)
{
    switch(type) {
        case ORTE_BOOL:
            *size = sizeof(bool);
            break;

        case ORTE_INT:
        case ORTE_UINT:
            *size = sizeof(int);
            break;

        case ORTE_SIZE:
            *size = sizeof(size_t);
            break;

        case ORTE_PID:
            *size = sizeof(pid_t);
            break;

        case ORTE_BYTE:
        case ORTE_INT8:
        case ORTE_UINT8:
        case ORTE_NULL:
            *size = 1;
            break;

        case ORTE_INT16:
        case ORTE_UINT16:
            *size = sizeof(uint16_t);
            break;

        case ORTE_INT32:
        case ORTE_UINT32:
            *size = sizeof(uint32_t);
            break;

        case ORTE_INT64:
        case ORTE_UINT64:
            *size = sizeof(uint64_t);
            break;

        case ORTE_STD_CNTR:
            *size = sizeof(orte_std_cntr_t);
            break;

        case ORTE_DATA_TYPE:
            *size = sizeof(orte_data_type_t);
            break;

        default:
            *size = 0;
            ORTE_ERROR_LOG(ORTE_ERR_UNKNOWN_DATA_TYPE);
            return ORTE_ERR_UNKNOWN_DATA_TYPE;
    }

    return ORTE_SUCCESS;
}

/* SIZE FUNCTIONS FOR NON-STANDARD SYSTEM TYPES */

/*
 * STRING
 */
int orte_dss_size_string(size_t *size, char *src, orte_data_type_t type)
{
    if (NULL != src) {
        *size = strlen(src) + 1;
    } else {
        *size = sizeof(char*);  /* account for NULL */
    }

    return ORTE_SUCCESS;

}

/* SIZE FUNCTIONS FOR GENERIC ORTE TYPES */

/*
 * ORTE_DATA_VALUE
 */
int orte_dss_size_data_value(size_t *size, orte_data_value_t *src, orte_data_type_t type)
{
    size_t data_size;
    int rc;

    /* account for size of object itself... */
    *size = sizeof(orte_data_value_t);

    if (NULL != src) {
        /* ...and the number of bytes in the payload, IF an actual object was provided */
        if (ORTE_SUCCESS != (rc = orte_dss.size(&data_size, src->data, src->type))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        *size += data_size;
    }

    return ORTE_SUCCESS;
}


/*
 * ORTE_BYTE_OBJECT
 */
int orte_dss_size_byte_object(size_t *size, orte_byte_object_t *src, orte_data_type_t type)
{
    /* account for size of object itself... */
    *size = sizeof(orte_byte_object_t);

    if (NULL != src) {
        /* ...and the number of bytes in the payload, IF an actual object was provided */
        *size += src->size;
    }

    return ORTE_SUCCESS;
}
