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
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * Copyright (c) 2022      Triad National Security, LLC. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "src/include/pmix_config.h"

#include "src/hwloc/pmix_hwloc.h"
#include "src/include/pmix_globals.h"
#include "src/mca/preg/preg.h"
#include "src/util/pmix_argv.h"
#include "src/util/pmix_error.h"
#include "src/util/pmix_output.h"

#include "src/mca/bfrops/base/base.h"
#include "src/mca/bfrops/base/bfrop_base_tma.h"

pmix_status_t pmix_bfrops_base_copy(pmix_pointer_array_t *regtypes, void **dest, void *src,
                                    pmix_data_type_t type)
{
    pmix_bfrop_type_info_t *info;

    /* check for error */
    if (NULL == dest || NULL == src) {
        PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
        return PMIX_ERR_BAD_PARAM;
    }

    /* Lookup the copy function for this type and call it */
    if (NULL == (info = (pmix_bfrop_type_info_t *) pmix_pointer_array_get_item(regtypes, type))) {
        PMIX_ERROR_LOG(PMIX_ERR_UNKNOWN_DATA_TYPE);
        return PMIX_ERR_UNKNOWN_DATA_TYPE;
    }

    return info->odti_copy_fn(dest, src, type);
}

pmix_status_t pmix_bfrops_base_copy_payload(pmix_buffer_t *dest, pmix_buffer_t *src)
{
    return pmix_bfrops_base_tma_copy_payload(dest, src, NULL);
}

pmix_status_t pmix_bfrops_base_embed_payload(pmix_buffer_t *dest, pmix_byte_object_t *src)
{
    return pmix_bfrops_base_tma_embed_payload(dest, src, NULL);
}

/*
 * STANDARD COPY FUNCTION - WORKS FOR EVERYTHING NON-STRUCTURED
 */
pmix_status_t pmix_bfrops_base_std_copy(void **dest, void *src, pmix_data_type_t type)
{
    size_t datasize;
    uint8_t *val = NULL;

    switch (type) {
    case PMIX_BOOL:
        datasize = sizeof(bool);
        break;

    case PMIX_INT:
    case PMIX_UINT:
        datasize = sizeof(int);
        break;

    case PMIX_SIZE:
        datasize = sizeof(size_t);
        break;

    case PMIX_PID:
        datasize = sizeof(pid_t);
        break;

    case PMIX_BYTE:
    case PMIX_INT8:
    case PMIX_UINT8:
    case PMIX_LINK_STATE:
        datasize = 1;
        break;

    case PMIX_INT16:
    case PMIX_UINT16:
    case PMIX_IOF_CHANNEL:
    case PMIX_LOCTYPE:
    case PMIX_STOR_ACCESS_TYPE:
        datasize = 2;
        break;

    case PMIX_INT32:
    case PMIX_UINT32:
        datasize = 4;
        break;

    case PMIX_INT64:
    case PMIX_UINT64:
    case PMIX_DEVTYPE:
    case PMIX_STOR_MEDIUM:
    case PMIX_STOR_ACCESS:
    case PMIX_STOR_PERSIST:
        datasize = 8;
        break;

    case PMIX_FLOAT:
        datasize = sizeof(float);
        break;

    case PMIX_TIMEVAL:
        datasize = sizeof(struct timeval);
        break;

    case PMIX_TIME:
        datasize = sizeof(time_t);
        break;

    case PMIX_STATUS:
        datasize = sizeof(pmix_status_t);
        break;

    case PMIX_PROC_RANK:
        datasize = sizeof(pmix_rank_t);
        break;

    case PMIX_PERSIST:
        datasize = sizeof(pmix_persistence_t);
        break;

    case PMIX_POINTER:
        datasize = sizeof(char *);
        break;

    case PMIX_SCOPE:
        datasize = sizeof(pmix_scope_t);
        break;

    case PMIX_DATA_RANGE:
        datasize = sizeof(pmix_data_range_t);
        break;

    case PMIX_COMMAND:
        datasize = sizeof(pmix_cmd_t);
        break;

    case PMIX_INFO_DIRECTIVES:
        datasize = sizeof(pmix_info_directives_t);
        break;

    case PMIX_PROC_STATE:
        datasize = sizeof(pmix_proc_state_t);
        break;

    case PMIX_ALLOC_DIRECTIVE:
        datasize = sizeof(pmix_alloc_directive_t);
        break;

    case PMIX_JOB_STATE:
        datasize = sizeof(pmix_job_state_t);
        break;

    default:
        return PMIX_ERR_UNKNOWN_DATA_TYPE;
    }

    val = (uint8_t *) malloc(datasize);
    if (NULL == val) {
        return PMIX_ERR_OUT_OF_RESOURCE;
    }

    memcpy(val, src, datasize);
    *dest = val;

    return PMIX_SUCCESS;
}

/* COPY FUNCTIONS FOR NON-STANDARD SYSTEM TYPES */

/*
 * STRING
 */
pmix_status_t pmix_bfrops_base_copy_string(char **dest, char *src, pmix_data_type_t type)
{
    PMIX_HIDE_UNUSED_PARAMS(type);

    if (NULL == src) { /* got zero-length string/NULL pointer - store NULL */
        *dest = NULL;
    } else {
        *dest = strdup(src);
    }

    return PMIX_SUCCESS;
}

/* PMIX_VALUE */
pmix_status_t pmix_bfrops_base_copy_value(pmix_value_t **dest, pmix_value_t *src,
                                          pmix_data_type_t type)
{
    return pmix_bfrops_base_tma_copy_value(dest, src, type, NULL);
}

pmix_status_t pmix_bfrops_base_copy_info(pmix_info_t **dest, pmix_info_t *src,
                                         pmix_data_type_t type)
{
    PMIX_HIDE_UNUSED_PARAMS(type);

    *dest = (pmix_info_t *) malloc(sizeof(pmix_info_t));
    pmix_strncpy((*dest)->key, src->key, PMIX_MAX_KEYLEN);
    (*dest)->flags = src->flags;
    return pmix_bfrops_base_value_xfer(&(*dest)->value, &src->value);
}

pmix_status_t pmix_bfrops_base_copy_buf(pmix_buffer_t **dest, pmix_buffer_t *src,
                                        pmix_data_type_t type)
{
    PMIX_HIDE_UNUSED_PARAMS(type);

    *dest = PMIX_NEW(pmix_buffer_t);
    pmix_bfrops_base_copy_payload(*dest, src);
    return PMIX_SUCCESS;
}

pmix_status_t pmix_bfrops_base_copy_app(pmix_app_t **dest, pmix_app_t *src, pmix_data_type_t type)
{
    size_t j;

    PMIX_HIDE_UNUSED_PARAMS(type);

    *dest = (pmix_app_t *) malloc(sizeof(pmix_app_t));
    (*dest)->cmd = strdup(src->cmd);
    (*dest)->argv = PMIx_Argv_copy(src->argv);
    (*dest)->env = PMIx_Argv_copy(src->env);
    if (NULL != src->cwd) {
        (*dest)->cwd = strdup(src->cwd);
    }
    (*dest)->maxprocs = src->maxprocs;
    (*dest)->ninfo = src->ninfo;
    (*dest)->info = (pmix_info_t *) malloc(src->ninfo * sizeof(pmix_info_t));
    for (j = 0; j < src->ninfo; j++) {
        pmix_strncpy((*dest)->info[j].key, src->info[j].key, PMIX_MAX_KEYLEN);
        PMIx_Value_xfer(&(*dest)->info[j].value, &src->info[j].value);
    }
    return PMIX_SUCCESS;
}

pmix_status_t pmix_bfrops_base_copy_kval(pmix_kval_t **dest, pmix_kval_t *src,
                                         pmix_data_type_t type)
{
    pmix_kval_t *p;

    PMIX_HIDE_UNUSED_PARAMS(type);

    /* create the new object */
    *dest = PMIX_NEW(pmix_kval_t);
    if (NULL == *dest) {
        return PMIX_ERR_OUT_OF_RESOURCE;
    }
    p = *dest;

    /* copy the type */
    p->value->type = src->value->type;
    /* copy the data */
    return pmix_bfrops_base_value_xfer(p->value, src->value);
}

pmix_status_t pmix_bfrops_base_copy_proc(pmix_proc_t **dest, pmix_proc_t *src,
                                         pmix_data_type_t type)
{
    PMIX_HIDE_UNUSED_PARAMS(type);

    *dest = (pmix_proc_t *) malloc(sizeof(pmix_proc_t));
    if (NULL == *dest) {
        return PMIX_ERR_OUT_OF_RESOURCE;
    }
    pmix_strncpy((*dest)->nspace, src->nspace, PMIX_MAX_NSLEN);
    (*dest)->rank = src->rank;
    return PMIX_SUCCESS;
}

pmix_status_t pmix_bfrop_base_copy_persist(pmix_persistence_t **dest, pmix_persistence_t *src,
                                           pmix_data_type_t type)
{
    PMIX_HIDE_UNUSED_PARAMS(type);

    *dest = (pmix_persistence_t *) malloc(sizeof(pmix_persistence_t));
    if (NULL == *dest) {
        return PMIX_ERR_OUT_OF_RESOURCE;
    }
    memcpy(*dest, src, sizeof(pmix_persistence_t));
    return PMIX_SUCCESS;
}

pmix_status_t pmix_bfrops_base_copy_bo(pmix_byte_object_t **dest, pmix_byte_object_t *src,
                                       pmix_data_type_t type)
{
    PMIX_HIDE_UNUSED_PARAMS(type);

    *dest = (pmix_byte_object_t *) malloc(sizeof(pmix_byte_object_t));
    if (NULL == *dest) {
        return PMIX_ERR_OUT_OF_RESOURCE;
    }
    (*dest)->bytes = (char *) malloc(src->size);
    memcpy((*dest)->bytes, src->bytes, src->size);
    (*dest)->size = src->size;
    return PMIX_SUCCESS;
}

pmix_status_t pmix_bfrops_base_copy_pdata(pmix_pdata_t **dest, pmix_pdata_t *src,
                                          pmix_data_type_t type)
{
    PMIX_HIDE_UNUSED_PARAMS(type);

    *dest = (pmix_pdata_t *) malloc(sizeof(pmix_pdata_t));
    pmix_strncpy((*dest)->proc.nspace, src->proc.nspace, PMIX_MAX_NSLEN);
    (*dest)->proc.rank = src->proc.rank;
    pmix_strncpy((*dest)->key, src->key, PMIX_MAX_KEYLEN);
    return pmix_bfrops_base_value_xfer(&(*dest)->value, &src->value);
}

pmix_status_t pmix_bfrops_base_copy_pinfo(pmix_proc_info_t **dest, pmix_proc_info_t *src,
                                          pmix_data_type_t type)
{
    return pmix_bfrops_base_tma_copy_pinfo(dest, src, type, NULL);
}

pmix_status_t pmix_bfrops_base_copy_darray(pmix_data_array_t **dest, pmix_data_array_t *src,
                                           pmix_data_type_t type)
{
    return pmix_bfrops_base_tma_copy_darray(dest, src, type, NULL);
}

pmix_status_t pmix_bfrops_base_copy_query(pmix_query_t **dest, pmix_query_t *src,
                                          pmix_data_type_t type)
{
    pmix_status_t rc;

    PMIX_HIDE_UNUSED_PARAMS(type);

    *dest = (pmix_query_t *) malloc(sizeof(pmix_query_t));
    if (NULL != src->keys) {
        (*dest)->keys = PMIx_Argv_copy(src->keys);
    }
    (*dest)->nqual = src->nqual;
    if (NULL != src->qualifiers) {
        if (PMIX_SUCCESS
            != (rc = pmix_bfrops_base_copy_info(&((*dest)->qualifiers), src->qualifiers,
                                                PMIX_INFO))) {
            free(*dest);
            return rc;
        }
    }
    return PMIX_SUCCESS;
}

pmix_status_t pmix_bfrops_base_copy_envar(pmix_envar_t **dest, pmix_envar_t *src,
                                          pmix_data_type_t type)
{
    PMIX_HIDE_UNUSED_PARAMS(type);

    PMIX_ENVAR_CREATE(*dest, 1);
    if (NULL == (*dest)) {
        return PMIX_ERR_NOMEM;
    }
    if (NULL != src->envar) {
        (*dest)->envar = strdup(src->envar);
    }
    if (NULL != src->value) {
        (*dest)->value = strdup(src->value);
    }
    (*dest)->separator = src->separator;
    return PMIX_SUCCESS;
}

pmix_status_t pmix_bfrops_base_copy_coord(pmix_coord_t **dest, pmix_coord_t *src,
                                          pmix_data_type_t type)
{
    return pmix_bfrops_base_tma_copy_coord(dest, src, type, NULL);
}

pmix_status_t pmix_bfrops_base_copy_regattr(pmix_regattr_t **dest, pmix_regattr_t *src,
                                            pmix_data_type_t type)
{
    return pmix_bfrops_base_tma_copy_regattr(dest, src, type, NULL);
}

pmix_status_t pmix_bfrops_base_copy_regex(char **dest, char *src, pmix_data_type_t type)
{
    size_t len;

    PMIX_HIDE_UNUSED_PARAMS(type);

    return pmix_preg.copy(dest, &len, src);
}

pmix_status_t pmix_bfrops_base_copy_cpuset(pmix_cpuset_t **dest, pmix_cpuset_t *src,
                                           pmix_data_type_t type)
{
    return pmix_bfrops_base_tma_copy_cpuset(dest, src, type, NULL);
}

pmix_status_t pmix_bfrops_base_copy_geometry(pmix_geometry_t **dest, pmix_geometry_t *src,
                                             pmix_data_type_t type)
{
    return pmix_bfrops_base_tma_copy_geometry(dest, src, type, NULL);
}

pmix_status_t pmix_bfrops_base_copy_devdist(pmix_device_distance_t **dest,
                                            pmix_device_distance_t *src, pmix_data_type_t type)
{
    pmix_device_distance_t *dst;

    PMIX_HIDE_UNUSED_PARAMS(type);

    PMIX_DEVICE_DIST_CREATE(dst, 1);
    if (NULL == dst) {
        return PMIX_ERR_NOMEM;
    }

    if (NULL != src->uuid) {
        dst->uuid = strdup(src->uuid);
    }
    if (NULL != src->osname) {
        dst->osname = strdup(src->osname);
    }
    dst->type = src->type;
    dst->mindist = src->mindist;
    dst->maxdist = src->maxdist;

    *dest = dst;
    return PMIX_SUCCESS;
}

pmix_status_t pmix_bfrops_base_copy_endpoint(pmix_endpoint_t **dest, pmix_endpoint_t *src,
                                             pmix_data_type_t type)
{
    return pmix_bfrops_base_tma_copy_endpoint(dest, src, type, NULL);
}

pmix_status_t pmix_bfrops_base_copy_topology(pmix_topology_t **dest, pmix_topology_t *src,
                                             pmix_data_type_t type)
{
    return pmix_bfrops_base_tma_copy_topology(dest, src, type, NULL);
}

pmix_status_t pmix_bfrops_base_copy_nspace(pmix_nspace_t **dest, pmix_nspace_t *src,
                                           pmix_data_type_t type)
{
    return pmix_bfrops_base_tma_copy_nspace(dest, src, type, NULL);
}

pmix_status_t pmix_bfrops_base_copy_pstats(pmix_proc_stats_t **dest, pmix_proc_stats_t *src,
                                           pmix_data_type_t type)
{
    return pmix_bfrops_base_tma_copy_pstats(dest, src, type, NULL);
}

pmix_status_t pmix_bfrops_base_copy_dkstats(pmix_disk_stats_t **dest, pmix_disk_stats_t *src,
                                            pmix_data_type_t type)
{
    return pmix_bfrops_base_tma_copy_dkstats(dest, src, type, NULL);
}

pmix_status_t pmix_bfrops_base_copy_netstats(pmix_net_stats_t **dest, pmix_net_stats_t *src,
                                             pmix_data_type_t type)
{
    return pmix_bfrops_base_tma_copy_netstats(dest, src, type, NULL);
}

pmix_status_t pmix_bfrops_base_copy_ndstats(pmix_node_stats_t **dest, pmix_node_stats_t *src,
                                            pmix_data_type_t type)
{
    return pmix_bfrops_base_tma_copy_ndstats(dest, src, type, NULL);
}

pmix_status_t pmix_bfrops_base_copy_dbuf(pmix_data_buffer_t **dest, pmix_data_buffer_t *src,
                                         pmix_data_type_t type)
{
    return pmix_bfrops_base_tma_copy_dbuf(dest, src, type, NULL);
}
