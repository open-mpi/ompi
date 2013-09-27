/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2012      Los Alamos National Security, Inc.  All rights reserved. 
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include "opal_stdint.h"
#include <stdio.h>

#include "opal/dss/dss_internal.h"

int opal_dss_print(char **output, char *prefix, void *src, opal_data_type_t type)
{
    opal_dss_type_info_t *info;

    /* check for error */
    if (NULL == output) {
        return OPAL_ERR_BAD_PARAM;
    }

    /* Lookup the print function for this type and call it */

    if(NULL == (info = (opal_dss_type_info_t*)opal_pointer_array_get_item(&opal_dss_types, type))) {
        return OPAL_ERR_UNKNOWN_DATA_TYPE;
    }

    return info->odti_print_fn(output, prefix, src, type);
}

/*
 * STANDARD PRINT FUNCTIONS FOR SYSTEM TYPES
 */
int opal_dss_print_byte(char **output, char *prefix, uint8_t *src, opal_data_type_t type)
{
    char *prefx;

    /* deal with NULL prefix */
    if (NULL == prefix) asprintf(&prefx, " ");
    else prefx = prefix;

    /* if src is NULL, just print data type and return */
    if (NULL == src) {
        asprintf(output, "%sData type: OPAL_BYTE\tValue: NULL pointer", prefx);
        return OPAL_SUCCESS;
    }

    asprintf(output, "%sData type: OPAL_BYTE\tValue: %x", prefix, *src);

    return OPAL_SUCCESS;
}

int opal_dss_print_string(char **output, char *prefix, char *src, opal_data_type_t type)
{
    char *prefx;

    /* deal with NULL prefix */
    if (NULL == prefix) asprintf(&prefx, " ");
    else prefx = prefix;

    /* if src is NULL, just print data type and return */
    if (NULL == src) {
        asprintf(output, "%sData type: OPAL_STRING\tValue: NULL pointer", prefx);
        return OPAL_SUCCESS;
    }

    asprintf(output, "%sData type: OPAL_STRING\tValue: %s", prefx, src);

    return OPAL_SUCCESS;
}

int opal_dss_print_size(char **output, char *prefix, size_t *src, opal_data_type_t type)
{
    char *prefx;

    /* deal with NULL prefix */
    if (NULL == prefix) asprintf(&prefx, " ");
    else prefx = prefix;

    /* if src is NULL, just print data type and return */
    if (NULL == src) {
        asprintf(output, "%sData type: OPAL_SIZE\tValue: NULL pointer", prefx);
        return OPAL_SUCCESS;
    }

    asprintf(output, "%sData type: OPAL_SIZE\tValue: %lu", prefx, (unsigned long) *src);

    return OPAL_SUCCESS;
}

int opal_dss_print_pid(char **output, char *prefix, pid_t *src, opal_data_type_t type)
{
    char *prefx;

    /* deal with NULL prefix */
    if (NULL == prefix) asprintf(&prefx, " ");
    else prefx = prefix;

    /* if src is NULL, just print data type and return */
    if (NULL == src) {
        asprintf(output, "%sData type: OPAL_PID\tValue: NULL pointer", prefx);
        return OPAL_SUCCESS;
    }

    asprintf(output, "%sData type: OPAL_PID\tValue: %lu", prefx, (unsigned long) *src);

    return OPAL_SUCCESS;
}

int opal_dss_print_bool(char **output, char *prefix, bool *src, opal_data_type_t type)
{
    char *prefx;

    /* deal with NULL prefix */
    if (NULL == prefix) asprintf(&prefx, " ");
    else prefx = prefix;

    /* if src is NULL, just print data type and return */
    if (NULL == src) {
        asprintf(output, "%sData type: OPAL_BOOL\tValue: NULL pointer", prefx);
        return OPAL_SUCCESS;
    }

    asprintf(output, "%sData type: OPAL_BOOL\tValue: %s", prefx, *src ? "TRUE" : "FALSE");

    return OPAL_SUCCESS;
}

int opal_dss_print_int(char **output, char *prefix, int *src, opal_data_type_t type)
{
    char *prefx;

    /* deal with NULL prefix */
    if (NULL == prefix) asprintf(&prefx, " ");
    else prefx = prefix;

    /* if src is NULL, just print data type and return */
    if (NULL == src) {
        asprintf(output, "%sData type: OPAL_INT\tValue: NULL pointer", prefx);
        return OPAL_SUCCESS;
    }

    asprintf(output, "%sData type: OPAL_INT\tValue: %ld", prefx, (long) *src);

    return OPAL_SUCCESS;
}

int opal_dss_print_uint(char **output, char *prefix, int *src, opal_data_type_t type)
{
    char *prefx;

    /* deal with NULL prefix */
    if (NULL == prefix) asprintf(&prefx, " ");
    else prefx = prefix;

    /* if src is NULL, just print data type and return */
    if (NULL == src) {
        asprintf(output, "%sData type: OPAL_UINT\tValue: NULL pointer", prefx);
        return OPAL_SUCCESS;
    }

    asprintf(output, "%sData type: OPAL_UINT\tValue: %lu", prefx, (unsigned long) *src);

    return OPAL_SUCCESS;
}

int opal_dss_print_uint8(char **output, char *prefix, uint8_t *src, opal_data_type_t type)
{
    char *prefx;

    /* deal with NULL prefix */
    if (NULL == prefix) asprintf(&prefx, " ");
    else prefx = prefix;

    /* if src is NULL, just print data type and return */
    if (NULL == src) {
        asprintf(output, "%sData type: OPAL_UINT8\tValue: NULL pointer", prefx);
        return OPAL_SUCCESS;
    }

    asprintf(output, "%sData type: OPAL_UINT8\tValue: %u", prefx, (unsigned int) *src);

    return OPAL_SUCCESS;
}

int opal_dss_print_uint16(char **output, char *prefix, uint16_t *src, opal_data_type_t type)
{
    char *prefx;

    /* deal with NULL prefix */
    if (NULL == prefix) asprintf(&prefx, " ");
    else prefx = prefix;

    /* if src is NULL, just print data type and return */
    if (NULL == src) {
        asprintf(output, "%sData type: OPAL_UINT16\tValue: NULL pointer", prefx);
        return OPAL_SUCCESS;
    }

    asprintf(output, "%sData type: OPAL_UINT16\tValue: %u", prefx, (unsigned int) *src);

    return OPAL_SUCCESS;
}

int opal_dss_print_uint32(char **output, char *prefix, uint32_t *src, opal_data_type_t type)
{
    char *prefx;

    /* deal with NULL prefix */
    if (NULL == prefix) asprintf(&prefx, " ");
    else prefx = prefix;

    /* if src is NULL, just print data type and return */
    if (NULL == src) {
        asprintf(output, "%sData type: OPAL_UINT32\tValue: NULL pointer", prefx);
        return OPAL_SUCCESS;
    }

    asprintf(output, "%sData type: OPAL_UINT32\tValue: %u", prefx, (unsigned int) *src);

    return OPAL_SUCCESS;
}

int opal_dss_print_int8(char **output, char *prefix, int8_t *src, opal_data_type_t type)
{
    char *prefx;

    /* deal with NULL prefix */
    if (NULL == prefix) asprintf(&prefx, " ");
    else prefx = prefix;

    /* if src is NULL, just print data type and return */
    if (NULL == src) {
        asprintf(output, "%sData type: OPAL_INT8\tValue: NULL pointer", prefx);
        return OPAL_SUCCESS;
    }

    asprintf(output, "%sData type: OPAL_INT8\tValue: %d", prefx, (int) *src);

    return OPAL_SUCCESS;
}

int opal_dss_print_int16(char **output, char *prefix, int16_t *src, opal_data_type_t type)
{
    char *prefx;

    /* deal with NULL prefix */
    if (NULL == prefix) asprintf(&prefx, " ");
    else prefx = prefix;

    /* if src is NULL, just print data type and return */
    if (NULL == src) {
        asprintf(output, "%sData type: OPAL_INT16\tValue: NULL pointer", prefx);
        return OPAL_SUCCESS;
    }

    asprintf(output, "%sData type: OPAL_INT16\tValue: %d", prefx, (int) *src);

    return OPAL_SUCCESS;
}

int opal_dss_print_int32(char **output, char *prefix, int32_t *src, opal_data_type_t type)
{
    char *prefx;

    /* deal with NULL prefix */
    if (NULL == prefix) asprintf(&prefx, " ");
    else prefx = prefix;

    /* if src is NULL, just print data type and return */
    if (NULL == src) {
        asprintf(output, "%sData type: OPAL_INT32\tValue: NULL pointer", prefx);
        return OPAL_SUCCESS;
    }

    asprintf(output, "%sData type: OPAL_INT32\tValue: %d", prefx, (int) *src);

    return OPAL_SUCCESS;
}
int opal_dss_print_uint64(char **output, char *prefix,
#ifdef HAVE_INT64_T
                          uint64_t *src,
#else
                          void *src,
#endif  /* HAVE_INT64_T */
                          opal_data_type_t type)
{
    char *prefx;

    /* deal with NULL prefix */
    if (NULL == prefix) asprintf(&prefx, " ");
    else prefx = prefix;

    /* if src is NULL, just print data type and return */
    if (NULL == src) {
        asprintf(output, "%sData type: OPAL_UINT64\tValue: NULL pointer", prefx);
        return OPAL_SUCCESS;
    }

#ifdef HAVE_INT64_T
    asprintf(output, "%sData type: OPAL_UINT64\tValue: %lu", prefx, (unsigned long) *src);
#else
    asprintf(output, "%sData type: OPAL_UINT64\tValue: unsupported", prefx);
#endif  /* HAVE_INT64_T */

    return OPAL_SUCCESS;
}

int opal_dss_print_int64(char **output, char *prefix,
#ifdef HAVE_INT64_T
                          int64_t *src,
#else
                          void *src,
#endif  /* HAVE_INT64_T */
                         opal_data_type_t type)
{
    char *prefx;

    /* deal with NULL prefix */
    if (NULL == prefix) asprintf(&prefx, " ");
    else prefx = prefix;

    /* if src is NULL, just print data type and return */
    if (NULL == src) {
        asprintf(output, "%sData type: OPAL_INT64\tValue: NULL pointer", prefx);
        return OPAL_SUCCESS;
    }

#ifdef HAVE_INT64_T
    asprintf(output, "%sData type: OPAL_INT64\tValue: %ld", prefx, (long) *src);
#else
    asprintf(output, "%sData type: OPAL_INT64\tValue: unsupported", prefx);
#endif  /* HAVE_INT64_T */

    return OPAL_SUCCESS;
}

int opal_dss_print_float(char **output, char *prefix,
                          float *src, opal_data_type_t type)
{
    char *prefx;

    /* deal with NULL prefix */
    if (NULL == prefix) asprintf(&prefx, " ");
    else prefx = prefix;

    /* if src is NULL, just print data type and return */
    if (NULL == src) {
        asprintf(output, "%sData type: OPAL_FLOAT\tValue: NULL pointer", prefx);
        return OPAL_SUCCESS;
    }

    asprintf(output, "%sData type: OPAL_FLOAT\tValue: %f", prefx, *src);

    return OPAL_SUCCESS;
}

int opal_dss_print_timeval(char **output, char *prefix,
                          struct timeval *src, opal_data_type_t type)
{
    char *prefx;

    /* deal with NULL prefix */
    if (NULL == prefix) asprintf(&prefx, " ");
    else prefx = prefix;

    /* if src is NULL, just print data type and return */
    if (NULL == src) {
        asprintf(output, "%sData type: OPAL_TIMEVAL\tValue: NULL pointer", prefx);
        return OPAL_SUCCESS;
    }

    asprintf(output, "%sData type: OPAL_TIMEVAL\tValue: %ld.%06ld", prefx,
             (long)src->tv_sec, (long)src->tv_usec);

    return OPAL_SUCCESS;
}

int opal_dss_print_null(char **output, char *prefix, void *src, opal_data_type_t type)
{
    char *prefx;

    /* deal with NULL prefix */
    if (NULL == prefix) asprintf(&prefx, " ");
    else prefx = prefix;

    /* if src is NULL, just print data type and return */
    if (NULL == src) {
        asprintf(output, "%sData type: OPAL_NULL\tValue: NULL pointer", prefx);
        return OPAL_SUCCESS;
    }

    asprintf(output, "%sData type: OPAL_NULL", prefx);

    return OPAL_SUCCESS;
}


/* PRINT FUNCTIONS FOR GENERIC OPAL TYPES */

/*
 * OPAL_DATA_TYPE
 */
int opal_dss_print_data_type(char **output, char *prefix, opal_data_type_t *src, opal_data_type_t type)
{
    char *prefx;

    /* deal with NULL prefix */
    if (NULL == prefix) asprintf(&prefx, " ");
    else prefx = prefix;

    /* if src is NULL, just print data type and return */
    if (NULL == src) {
        asprintf(output, "%sData type: OPAL_DATA_TYPE\tValue: NULL pointer", prefx);
        return OPAL_SUCCESS;
    }

    asprintf(output, "%sData type: OPAL_DATA_TYPE\tValue: %lu", prefx, (unsigned long) *src);
    return OPAL_SUCCESS;
}

/*
 * OPAL_BYTE_OBJECT
 */
int opal_dss_print_byte_object(char **output, char *prefix, opal_byte_object_t *src, opal_data_type_t type)
{
    char *prefx;

    /* deal with NULL prefix */
    if (NULL == prefix) asprintf(&prefx, " ");
    else prefx = prefix;

    /* if src is NULL, just print data type and return */
    if (NULL == src) {
        asprintf(output, "%sData type: OPAL_BYTE_OBJECT\tValue: NULL pointer", prefx);
        return OPAL_SUCCESS;
    }

    asprintf(output, "%sData type: OPAL_BYTE_OBJECT\tSize: %lu", prefx, (unsigned long) src->size);

    return OPAL_SUCCESS;
}

/*
 * OPAL_PSTAT
 */
int opal_dss_print_pstat(char **output, char *prefix, opal_pstats_t *src, opal_data_type_t type)
{
    char *prefx;

    /* deal with NULL prefix */
    if (NULL == prefix) asprintf(&prefx, " ");
    else prefx = prefix;
    
    /* if src is NULL, just print data type and return */
    if (NULL == src) {
        asprintf(output, "%sData type: OPAL_PSTATS\tValue: NULL pointer", prefx);
        return OPAL_SUCCESS;
    }
    asprintf(output, "%sOPAL_PSTATS SAMPLED AT: %ld.%06ld\n%snode: %s rank: %d pid: %d cmd: %s state: %c pri: %d #threads: %d Processor: %d\n"
             "%s\ttime: %ld.%06ld cpu: %5.2f VMsize: %8.2f PeakVMSize: %8.2f RSS: %8.2f\n",
             prefx, (long)src->sample_time.tv_sec, (long)src->sample_time.tv_usec,
             prefx, src->node, src->rank, src->pid, src->cmd, src->state[0], src->priority, src->num_threads, src->processor,
             prefx, (long)src->time.tv_sec, (long)src->time.tv_usec, src->percent_cpu, src->vsize, src->peak_vsize, src->rss);
    
    return OPAL_SUCCESS;
}

/*
 * OPAL_NODE_STAT
 */
int opal_dss_print_node_stat(char **output, char *prefix, opal_node_stats_t *src, opal_data_type_t type)
{
    char *prefx;

    /* deal with NULL prefix */
    if (NULL == prefix) asprintf(&prefx, " ");
    else prefx = prefix;
    
    /* if src is NULL, just print data type and return */
    if (NULL == src) {
        asprintf(output, "%sData type: OPAL_NODE_STATS\tValue: NULL pointer", prefx);
        return OPAL_SUCCESS;
    }
    asprintf(output, "%sOPAL_NODE_STATS SAMPLED AT: %ld.%06ld\n%sTotal Mem: %5.2f Free Mem: %5.2f Buffers: %5.2f Cached: %5.2f\n"
                     "%sSwapCached: %5.2f SwapTotal: %5.2f SwapFree: %5.2f Mapped: %5.2f\n"
                     "%s\tla: %5.2f\tla5: %5.2f\tla15: %5.2f\n",
             prefx, (long)src->sample_time.tv_sec, (long)src->sample_time.tv_usec,
             prefx, src->total_mem, src->free_mem, src->buffers, src->cached,
             prefx, src->swap_cached, src->swap_total, src->swap_free, src->mapped,
             prefx, src->la, src->la5, src->la15);
    
    return OPAL_SUCCESS;
}

/*
 * OPAL_VALUE
 */
int opal_dss_print_value(char **output, char *prefix, opal_value_t *src, opal_data_type_t type)
{
    char *prefx;
    char *scope;

    /* deal with NULL prefix */
    if (NULL == prefix) asprintf(&prefx, " ");
    else prefx = strdup(prefix);
    
    /* if src is NULL, just print data type and return */
    if (NULL == src) {
        asprintf(output, "%sData type: OPAL_VALUE\tValue: NULL pointer", prefx);
        free(prefx);
        return OPAL_SUCCESS;
    }
    
    if (OPAL_SCOPE_UNDEF == src->scope) {
        scope = "UNDEF";
    } else if (OPAL_SCOPE_PEER == src->scope) {
        scope = "PEER";
    } else if (OPAL_SCOPE_NON_PEER == src->scope) {
        scope = "NON_PEER";
    } else if (OPAL_SCOPE_GLOBAL == src->scope) {
        scope = "GLOBAL";
    } else if (OPAL_SCOPE_INTERNAL == src->scope) {
        scope = "INTERNAL";
    } else if (OPAL_SCOPE_ALL == src->scope) {
        scope = "ALL";
    } else {
        scope = "INTERNAL";
    }

    switch (src->type) {
    case OPAL_STRING:
        asprintf(output, "%sOPAL_VALUE: Data type: OPAL_STRING\tKey: %s\tScope:%s\tValue: %s",
                 prefx, src->key, scope, src->data.string);
        break;
    case OPAL_INT16:
        asprintf(output, "%sOPAL_VALUE: Data type: OPAL_STRING\tKey: %s\tScope:%s\tValue: %d",
                 prefx, src->key, scope, (int)src->data.int16);
        break;
    case OPAL_INT32:
        asprintf(output, "%sOPAL_VALUE: Data type: OPAL_INT32\tKey: %s\tScope:%s\tValue: %d",
                 prefx, src->key, scope, src->data.int32);
        break;
    case OPAL_PID:
        asprintf(output, "%sOPAL_VALUE: Data type: OPAL_STRING\tKey: %s\tScope:%s\tValue: %lu",
                 prefx, src->key, scope, (unsigned long)src->data.pid);
        break;
    case OPAL_FLOAT:
        asprintf(output, "%sOPAL_VALUE: Data type: OPAL_FLOAT\tKey: %s\tScope:%s\tValue: %f",
                 prefx, src->key, scope, src->data.fval);
        break;
    case OPAL_TIMEVAL:
        asprintf(output, "%sOPAL_VALUE: Data type: OPAL_TIMEVAL\tKey: %s\tScope:%s\tValue: %ld.%06ld", prefx,
                 src->key, scope, (long)src->data.tv.tv_sec, (long)src->data.tv.tv_usec);
        break;
    default:
        asprintf(output, "%sOPAL_VALUE: Data type: UNKNOWN\tKey: %s\tScope:%s\tValue: UNPRINTABLE",
                 prefx, src->key, scope);
        break;
    }
    free(prefx);
    return OPAL_SUCCESS;
}

int opal_dss_print_buffer_contents(char **output, char *prefix,
                                   opal_buffer_t *src, opal_data_type_t type)
{
    return OPAL_SUCCESS;
}

