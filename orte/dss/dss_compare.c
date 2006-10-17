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
#include <stdlib.h>
#if HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif

#include "orte/mca/errmgr/errmgr.h"

#include "orte/dss/dss_internal.h"
#include "opal/util/output.h"

int orte_dss_compare(void *value1, void *value2, orte_data_type_t type)
{
    orte_dss_type_info_t *info;

    /* check for error */
    if (NULL == value1 || NULL == value2) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }

    /* Lookup the compare function for this type and call it */

    if (!(type < orte_dss_types->size) ||
         (NULL == (info = (orte_dss_type_info_t*)orte_pointer_array_get_item(orte_dss_types, type)))) {
        ORTE_ERROR_LOG(ORTE_ERR_UNKNOWN_DATA_TYPE);
        return ORTE_ERR_UNKNOWN_DATA_TYPE;
    }

    return info->odti_compare_fn(value1, value2, type);
}

/*
 * NUMERIC COMPARE FUNCTIONS
 */
int orte_dss_compare_int(int *value1, int *value2, orte_data_type_t type)
{
    if (*value1 > *value2) return ORTE_VALUE1_GREATER;

    if (*value2 > *value1) return ORTE_VALUE2_GREATER;

    return ORTE_EQUAL;
}

int orte_dss_compare_uint(uint *value1, uint *value2, orte_data_type_t type)
{
    if (*value1 > *value2) return ORTE_VALUE1_GREATER;

    if (*value2 > *value1) return ORTE_VALUE2_GREATER;

    return ORTE_EQUAL;
}

int orte_dss_compare_size(size_t *value1, size_t *value2, orte_data_type_t type)
{
    if (*value1 > *value2) return ORTE_VALUE1_GREATER;

    if (*value2 > *value1) return ORTE_VALUE2_GREATER;

    return ORTE_EQUAL;
}

int orte_dss_compare_pid(pid_t *value1, pid_t *value2, orte_data_type_t type)
{
    if (*value1 > *value2) return ORTE_VALUE1_GREATER;

    if (*value2 > *value1) return ORTE_VALUE2_GREATER;

    return ORTE_EQUAL;
}

int orte_dss_compare_byte(char *value1, char *value2, orte_data_type_t type)
{
    if (*value1 > *value2) return ORTE_VALUE1_GREATER;

    if (*value2 > *value1) return ORTE_VALUE2_GREATER;

    return ORTE_EQUAL;
}

int orte_dss_compare_char(char *value1, char *value2, orte_data_type_t type)
{
    if (*value1 > *value2) return ORTE_VALUE1_GREATER;

    if (*value2 > *value1) return ORTE_VALUE2_GREATER;

    return ORTE_EQUAL;
}

int orte_dss_compare_int8(int8_t *value1, int8_t *value2, orte_data_type_t type)
{
    if (*value1 > *value2) return ORTE_VALUE1_GREATER;

    if (*value2 > *value1) return ORTE_VALUE2_GREATER;

    return ORTE_EQUAL;
}

int orte_dss_compare_uint8(uint8_t *value1, uint8_t *value2, orte_data_type_t type)
{
    if (*value1 > *value2) return ORTE_VALUE1_GREATER;

    if (*value2 > *value1) return ORTE_VALUE2_GREATER;

    return ORTE_EQUAL;
}

int orte_dss_compare_int16(int16_t *value1, int16_t *value2, orte_data_type_t type)
{
    if (*value1 > *value2) return ORTE_VALUE1_GREATER;

    if (*value2 > *value1) return ORTE_VALUE2_GREATER;

    return ORTE_EQUAL;
}

int orte_dss_compare_uint16(uint16_t *value1, uint16_t *value2, orte_data_type_t type)
{
    if (*value1 > *value2) return ORTE_VALUE1_GREATER;

    if (*value2 > *value1) return ORTE_VALUE2_GREATER;

    return ORTE_EQUAL;
}

int orte_dss_compare_int32(int32_t *value1, int32_t *value2, orte_data_type_t type)
{
    if (*value1 > *value2) return ORTE_VALUE1_GREATER;

    if (*value2 > *value1) return ORTE_VALUE2_GREATER;

    return ORTE_EQUAL;
}

int orte_dss_compare_uint32(uint32_t *value1, uint32_t *value2, orte_data_type_t type)
{
    if (*value1 > *value2) return ORTE_VALUE1_GREATER;

    if (*value2 > *value1) return ORTE_VALUE2_GREATER;

    return ORTE_EQUAL;
}

int orte_dss_compare_int64(int64_t *value1, int64_t *value2, orte_data_type_t type)
{
    if (*value1 > *value2) return ORTE_VALUE1_GREATER;

    if (*value2 > *value1) return ORTE_VALUE2_GREATER;

    return ORTE_EQUAL;
}

int orte_dss_compare_uint64(uint64_t *value1, uint64_t *value2, orte_data_type_t type)
{
    if (*value1 > *value2) return ORTE_VALUE1_GREATER;

    if (*value2 > *value1) return ORTE_VALUE2_GREATER;

    return ORTE_EQUAL;
}

/*
 * NON-NUMERIC SYSTEM TYPES
 */

/* NULL */
int orte_dss_compare_null(char *value1, char *value2, orte_data_type_t type)
{
    return ORTE_EQUAL;
}

/* BOOL */
int orte_dss_compare_bool(bool *value1, bool *value2, orte_data_type_t type)
{
    if (*value1 && !(*value2)) return ORTE_VALUE1_GREATER;

    if (*value2 && !(*value1)) return ORTE_VALUE2_GREATER;

    return ORTE_EQUAL;

}

/* STRING */
int orte_dss_compare_string(char *value1, char *value2, orte_data_type_t type)
{
    if (0 < strcmp(value1, value2)) return ORTE_VALUE2_GREATER;

    if (0 > strcmp(value1, value2)) return ORTE_VALUE1_GREATER;

    return ORTE_EQUAL;
}

/* COMPARE FUNCTIONS FOR GENERIC ORTE TYPES */

/* ORTE_STD_CNTR */
int orte_dss_compare_std_cntr(orte_std_cntr_t *value1, orte_std_cntr_t *value2, orte_data_type_t type)
{
    if (*value1 > *value2) return ORTE_VALUE1_GREATER;

    if (*value2 > *value1) return ORTE_VALUE2_GREATER;

    return ORTE_EQUAL;
}

/* ORTE_DATA_TYPE */
int orte_dss_compare_dt(orte_data_type_t *value1, orte_data_type_t *value2, orte_data_type_t type)
{
    if (*value1 > *value2) return ORTE_VALUE1_GREATER;

    if (*value2 > *value1) return ORTE_VALUE2_GREATER;

    return ORTE_EQUAL;
}

/* ORTE_DATA_VALUE */
int orte_dss_compare_data_value(orte_data_value_t *value1, orte_data_value_t *value2, orte_data_type_t type)
{
    /* can't compare if the two types don't match */
    if (value1->type != value2->type) {
        ORTE_ERROR_LOG(ORTE_ERR_TYPE_MISMATCH);
        return ORTE_ERR_TYPE_MISMATCH;
    }

    /* okay, go ahead and compare the values themselves */
    return orte_dss.compare(value1->data, value2->data, value1->type);
}

/* ORTE_BYTE_OBJECT */
int orte_dss_compare_byte_object(orte_byte_object_t *value1, orte_byte_object_t *value2, orte_data_type_t type)
{
    int checksum, diff;
    orte_std_cntr_t i;

    /* compare the sizes first - bigger size object is "greater than" */
    if (value1->size > value2->size) return ORTE_VALUE1_GREATER;

    if (value2->size > value1->size) return ORTE_VALUE2_GREATER;

    /* get here if the two sizes are identical - now do a simple checksum-style
     * calculation to determine "biggest"
     */
    checksum = 0;

    for (i=0; i < value1->size; i++) {
        /* protect against overflows */
        diff = value1->bytes[i] - value2->bytes[i];
        if (INT_MAX-abs(checksum)-abs(diff) < 0) { /* got an overflow condition */
            checksum = 0;
        }
        checksum += diff;
    }

    if (0 > checksum) return ORTE_VALUE2_GREATER;  /* sum of value2 bytes was greater */

    if (0 < checksum) return ORTE_VALUE1_GREATER;  /* of value1 bytes was greater */

    return ORTE_EQUAL;  /* sum of both value's bytes was identical */
}
