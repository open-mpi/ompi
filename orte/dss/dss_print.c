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

int orte_dss_print(char **output, char *prefix, void *src, orte_data_type_t type)
{
    int rc;
    orte_dss_type_info_t *info;

    /* check for error */
    if (NULL == output) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }

    /* Lookup the print function for this type and call it */

    if (!(type < orte_dss_types->size) ||
          (NULL == (info = (orte_dss_type_info_t*)orte_pointer_array_get_item(orte_dss_types, type)))) {
        ORTE_ERROR_LOG(ORTE_ERR_UNKNOWN_DATA_TYPE);
        return ORTE_ERR_UNKNOWN_DATA_TYPE;
    }

    if (ORTE_SUCCESS != (rc = info->odti_print_fn(output, prefix, src, type))) {
        ORTE_ERROR_LOG(rc);
    }


    return rc;
}

/*
 * STANDARD PRINT FUNCTIONS FOR SYSTEM TYPES
 */
int orte_dss_print_byte(char **output, char *prefix, uint8_t *src, orte_data_type_t type)
{
    char *prefx;

    /* deal with NULL prefix */
    if (NULL == prefix) asprintf(&prefx, " ");
    else prefx = prefix;

    /* if src is NULL, just print data type and return */
    if (NULL == src) {
        asprintf(output, "%sData type: ORTE_BYTE\tValue: NULL pointer", prefx);
        return ORTE_SUCCESS;
    }

    asprintf(output, "%sData type: ORTE_BYTE\tValue: %x", prefix, *src);

    return ORTE_SUCCESS;
}

int orte_dss_print_string(char **output, char *prefix, char *src, orte_data_type_t type)
{
    char *prefx;

    /* deal with NULL prefix */
    if (NULL == prefix) asprintf(&prefx, " ");
    else prefx = prefix;

    /* if src is NULL, just print data type and return */
    if (NULL == src) {
        asprintf(output, "%sData type: ORTE_STRING\tValue: NULL pointer", prefx);
        return ORTE_SUCCESS;
    }

    asprintf(output, "%sData type: ORTE_STRING\tValue: %s", prefx, src);

    return ORTE_SUCCESS;
}

int orte_dss_print_size(char **output, char *prefix, size_t *src, orte_data_type_t type)
{
    char *prefx;

    /* deal with NULL prefix */
    if (NULL == prefix) asprintf(&prefx, " ");
    else prefx = prefix;

    /* if src is NULL, just print data type and return */
    if (NULL == src) {
        asprintf(output, "%sData type: ORTE_SIZE\tValue: NULL pointer", prefx);
        return ORTE_SUCCESS;
    }

    asprintf(output, "%sData type: ORTE_SIZE\tValue: %lu", prefx, (unsigned long) *src);

    return ORTE_SUCCESS;
}

int orte_dss_print_pid(char **output, char *prefix, pid_t *src, orte_data_type_t type)
{
    char *prefx;

    /* deal with NULL prefix */
    if (NULL == prefix) asprintf(&prefx, " ");
    else prefx = prefix;

    /* if src is NULL, just print data type and return */
    if (NULL == src) {
        asprintf(output, "%sData type: ORTE_PID\tValue: NULL pointer", prefx);
        return ORTE_SUCCESS;
    }

    asprintf(output, "%sData type: ORTE_PID\tValue: %lu", prefx, (unsigned long) *src);

    return ORTE_SUCCESS;
}

int orte_dss_print_bool(char **output, char *prefix, bool *src, orte_data_type_t type)
{
    char *prefx;

    /* deal with NULL prefix */
    if (NULL == prefix) asprintf(&prefx, " ");
    else prefx = prefix;

    /* if src is NULL, just print data type and return */
    if (NULL == src) {
        asprintf(output, "%sData type: ORTE_BOOL\tValue: NULL pointer", prefx);
        return ORTE_SUCCESS;
    }

    asprintf(output, "%sData type: ORTE_BOOL\tValue: %s", prefx, *src ? "TRUE" : "FALSE");

    return ORTE_SUCCESS;
}

int orte_dss_print_int(char **output, char *prefix, int *src, orte_data_type_t type)
{
    char *prefx;

    /* deal with NULL prefix */
    if (NULL == prefix) asprintf(&prefx, " ");
    else prefx = prefix;

    /* if src is NULL, just print data type and return */
    if (NULL == src) {
        asprintf(output, "%sData type: ORTE_INT\tValue: NULL pointer", prefx);
        return ORTE_SUCCESS;
    }

    asprintf(output, "%sData type: ORTE_INT\tValue: %ld", prefx, (long) *src);

    return ORTE_SUCCESS;
}

int orte_dss_print_uint(char **output, char *prefix, int *src, orte_data_type_t type)
{
    char *prefx;

    /* deal with NULL prefix */
    if (NULL == prefix) asprintf(&prefx, " ");
    else prefx = prefix;

    /* if src is NULL, just print data type and return */
    if (NULL == src) {
        asprintf(output, "%sData type: ORTE_UINT\tValue: NULL pointer", prefx);
        return ORTE_SUCCESS;
    }

    asprintf(output, "%sData type: ORTE_UINT\tValue: %lu", prefx, (unsigned long) *src);

    return ORTE_SUCCESS;
}

int orte_dss_print_uint8(char **output, char *prefix, uint8_t *src, orte_data_type_t type)
{
    char *prefx;

    /* deal with NULL prefix */
    if (NULL == prefix) asprintf(&prefx, " ");
    else prefx = prefix;

    /* if src is NULL, just print data type and return */
    if (NULL == src) {
        asprintf(output, "%sData type: ORTE_UINT8\tValue: NULL pointer", prefx);
        return ORTE_SUCCESS;
    }

    asprintf(output, "%sData type: ORTE_UINT8\tValue: %u", prefx, (unsigned int) *src);

    return ORTE_SUCCESS;
}

int orte_dss_print_uint16(char **output, char *prefix, uint16_t *src, orte_data_type_t type)
{
    char *prefx;

    /* deal with NULL prefix */
    if (NULL == prefix) asprintf(&prefx, " ");
    else prefx = prefix;

    /* if src is NULL, just print data type and return */
    if (NULL == src) {
        asprintf(output, "%sData type: ORTE_UINT16\tValue: NULL pointer", prefx);
        return ORTE_SUCCESS;
    }

    asprintf(output, "%sData type: ORTE_UINT16\tValue: %u", prefx, (unsigned int) *src);

    return ORTE_SUCCESS;
}

int orte_dss_print_uint32(char **output, char *prefix, uint32_t *src, orte_data_type_t type)
{
    char *prefx;

    /* deal with NULL prefix */
    if (NULL == prefix) asprintf(&prefx, " ");
    else prefx = prefix;

    /* if src is NULL, just print data type and return */
    if (NULL == src) {
        asprintf(output, "%sData type: ORTE_UINT32\tValue: NULL pointer", prefx);
        return ORTE_SUCCESS;
    }

    asprintf(output, "%sData type: ORTE_UINT32\tValue: %u", prefx, (unsigned int) *src);

    return ORTE_SUCCESS;
}

int orte_dss_print_int8(char **output, char *prefix, int8_t *src, orte_data_type_t type)
{
    char *prefx;

    /* deal with NULL prefix */
    if (NULL == prefix) asprintf(&prefx, " ");
    else prefx = prefix;

    /* if src is NULL, just print data type and return */
    if (NULL == src) {
        asprintf(output, "%sData type: ORTE_INT8\tValue: NULL pointer", prefx);
        return ORTE_SUCCESS;
    }

    asprintf(output, "%sData type: ORTE_INT8\tValue: %d", prefx, (int) *src);

    return ORTE_SUCCESS;
}

int orte_dss_print_int16(char **output, char *prefix, int16_t *src, orte_data_type_t type)
{
    char *prefx;

    /* deal with NULL prefix */
    if (NULL == prefix) asprintf(&prefx, " ");
    else prefx = prefix;

    /* if src is NULL, just print data type and return */
    if (NULL == src) {
        asprintf(output, "%sData type: ORTE_INT16\tValue: NULL pointer", prefx);
        return ORTE_SUCCESS;
    }

    asprintf(output, "%sData type: ORTE_INT16\tValue: %d", prefx, (int) *src);

    return ORTE_SUCCESS;
}

int orte_dss_print_int32(char **output, char *prefix, int32_t *src, orte_data_type_t type)
{
    char *prefx;

    /* deal with NULL prefix */
    if (NULL == prefix) asprintf(&prefx, " ");
    else prefx = prefix;

    /* if src is NULL, just print data type and return */
    if (NULL == src) {
        asprintf(output, "%sData type: ORTE_INT32\tValue: NULL pointer", prefx);
        return ORTE_SUCCESS;
    }

    asprintf(output, "%sData type: ORTE_INT32\tValue: %d", prefx, (int) *src);

    return ORTE_SUCCESS;
}
int orte_dss_print_uint64(char **output, char *prefix,
#ifdef HAVE_INT64_T
                          uint64_t *src,
#else
                          void *src,
#endif  /* HAVE_INT64_T */
                          orte_data_type_t type)
{
    char *prefx;

    /* deal with NULL prefix */
    if (NULL == prefix) asprintf(&prefx, " ");
    else prefx = prefix;

    /* if src is NULL, just print data type and return */
    if (NULL == src) {
        asprintf(output, "%sData type: ORTE_UINT64\tValue: NULL pointer", prefx);
        return ORTE_SUCCESS;
    }

#ifdef HAVE_INT64_T
    asprintf(output, "%sData type: ORTE_UINT64\tValue: %lu", prefx, (unsigned long) *src);
#else
    asprintf(output, "%sData type: ORTE_UINT64\tValue: unsupported", prefx);
#endif  /* HAVE_INT64_T */

    return ORTE_SUCCESS;
}

int orte_dss_print_int64(char **output, char *prefix,
#ifdef HAVE_INT64_T
                          int64_t *src,
#else
                          void *src,
#endif  /* HAVE_INT64_T */
                         orte_data_type_t type)
{
    char *prefx;

    /* deal with NULL prefix */
    if (NULL == prefix) asprintf(&prefx, " ");
    else prefx = prefix;

    /* if src is NULL, just print data type and return */
    if (NULL == src) {
        asprintf(output, "%sData type: ORTE_INT64\tValue: NULL pointer", prefx);
        return ORTE_SUCCESS;
    }

#ifdef HAVE_INT64_T
    asprintf(output, "%sData type: ORTE_INT64\tValue: %ld", prefx, (long) *src);
#else
    asprintf(output, "%sData type: ORTE_INT64\tValue: unsupported", prefx);
#endif  /* HAVE_INT64_T */

    return ORTE_SUCCESS;
}

int orte_dss_print_null(char **output, char *prefix, void *src, orte_data_type_t type)
{
    char *prefx;

    /* deal with NULL prefix */
    if (NULL == prefix) asprintf(&prefx, " ");
    else prefx = prefix;

    /* if src is NULL, just print data type and return */
    if (NULL == src) {
        asprintf(output, "%sData type: ORTE_NULL\tValue: NULL pointer", prefx);
        return ORTE_SUCCESS;
    }

    asprintf(output, "%sData type: ORTE_NULL", prefx);

    return ORTE_SUCCESS;
}


/* PRINT FUNCTIONS FOR GENERIC ORTE TYPES */
/*
 * ORTE_STD_CNTR
 */
int orte_dss_print_std_cntr(char **output, char *prefix, orte_std_cntr_t *src, orte_data_type_t type)
{
    char *prefx;

    /* deal with NULL prefix */
    if (NULL == prefix) asprintf(&prefx, " ");
    else prefx = prefix;

    /* if src is NULL, just print data type and return */
    if (NULL == src) {
        asprintf(output, "%sData type: ORTE_STD_CNTR\tValue: NULL pointer", prefx);
        return ORTE_SUCCESS;
    }

    asprintf(output, "%sData type: ORTE_STD_CNTR\tValue: %lu", prefx, (unsigned long) *src);
    return ORTE_SUCCESS;
}

/*
 * ORTE_DATA_TYPE
 */
int orte_dss_print_data_type(char **output, char *prefix, orte_data_type_t *src, orte_data_type_t type)
{
    char *prefx;

    /* deal with NULL prefix */
    if (NULL == prefix) asprintf(&prefx, " ");
    else prefx = prefix;

    /* if src is NULL, just print data type and return */
    if (NULL == src) {
        asprintf(output, "%sData type: ORTE_DATA_TYPE\tValue: NULL pointer", prefx);
        return ORTE_SUCCESS;
    }

    asprintf(output, "%sData type: ORTE_DATA_TYPE\tValue: %lu", prefx, (unsigned long) *src);
    return ORTE_SUCCESS;
}

/*
 * ORTE_DATA_VALUE
 */
int orte_dss_print_data_value(char **output, char *prefix, orte_data_value_t *src, orte_data_type_t type)
{
    char *pfx, *tmp1, *tmp2;
    int rc;

   /* if src is NULL, just print data type and return */
    if (NULL == src) {
        if (NULL != prefix) {
            asprintf(output, "%sData type: ORTE_DATA_VALUE\tValue: NULL pointer", prefix);
        } else {
            asprintf(output, "Data type: ORTE_DATA_VALUE\tValue: NULL pointer");
        }
        return ORTE_SUCCESS;
    }

    if (NULL != prefix) {
        asprintf(&pfx, "%s\t", prefix);
        asprintf(&tmp1, "%sData type: ORTE_DATA_VALUE:\n", prefix);
    } else {
        asprintf(&tmp1, "Data type: ORTE_DATA_VALUE:\n");
        asprintf(&pfx, "\t");
    }

    /* if data is included, print it */
    if (ORTE_UNDEF == src->type) { /* undefined data type - just report it */
        asprintf(&tmp2, "%sData type: ORTE_UNDEF\tValue: N/A", pfx);
    } else if (NULL != src->data) {
        if (ORTE_SUCCESS != (rc = orte_dss.print(&tmp2, pfx, src->data, src->type))) {
            ORTE_ERROR_LOG(rc);
            if (NULL != tmp1) free(tmp1);
            if (NULL != pfx) free(pfx);
            *output = NULL;
            return rc;
        }
    } else { /* indicate the data field was NULL */
        asprintf(&tmp2, "%sData field is NULL", pfx);
    }

    asprintf(output, "%s%s", tmp1, tmp2);
    free(tmp1);
    free(tmp2);
    if (NULL != pfx) free(pfx);

    return ORTE_SUCCESS;
}

/*
 * ORTE_BYTE_OBJECT
 */
int orte_dss_print_byte_object(char **output, char *prefix, orte_byte_object_t *src, orte_data_type_t type)
{
    char *prefx;

    /* deal with NULL prefix */
    if (NULL == prefix) asprintf(&prefx, " ");
    else prefx = prefix;

    /* if src is NULL, just print data type and return */
    if (NULL == src) {
        asprintf(output, "%sData type: ORTE_BYTE_OBJECT\tValue: NULL pointer", prefx);
        return ORTE_SUCCESS;
    }

    asprintf(output, "%sData type: ORTE_BYTE_OBJECT\tSize: %lu", prefx, (unsigned long) src->size);

    return ORTE_SUCCESS;
}
