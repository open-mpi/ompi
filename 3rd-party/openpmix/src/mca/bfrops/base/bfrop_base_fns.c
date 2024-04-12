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
 * Copyright (c) 2015-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2019      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * Copyright (c) 2022      IBM Corporation.  All rights reserved.
 * Copyright (c) 2022      Triad National Security, LLC. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "src/include/pmix_config.h"

#include <stdio.h>
#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif

#include "pmix.h"

#include "src/include/pmix_globals.h"
#include "src/mca/pcompress/pcompress.h"
#include "src/mca/preg/preg.h"
#include "src/util/pmix_argv.h"
#include "src/util/pmix_error.h"
#include "src/hwloc/pmix_hwloc.h"

#include "src/mca/bfrops/base/base.h"
#include "src/mca/bfrops/base/bfrop_base_tma.h"

void pmix_bfrops_base_value_load(pmix_value_t *v,
                                 const void *data,
                                 pmix_data_type_t type)
{
    pmix_byte_object_t *bo;
    pmix_proc_info_t *pi;
    pmix_envar_t *envar;
    pmix_data_array_t *darray;
    pmix_status_t rc;
    pmix_coord_t *coord;
    pmix_regattr_t *regattr;
    pmix_topology_t *topo;
    pmix_cpuset_t *cpuset;
    pmix_geometry_t *geometry;
    pmix_endpoint_t *endpoint;
    pmix_device_distance_t *devdist;
    pmix_data_buffer_t *dbuf;
    pmix_nspace_t *nspace;
    pmix_proc_stats_t *pstats;
    pmix_disk_stats_t *dkstats;
    pmix_net_stats_t *netstats;
    pmix_node_stats_t *ndstats;

    v->type = type;
    if (NULL == data) {
        /* just set the fields to zero */
        memset(&v->data, 0, sizeof(v->data));
        if (PMIX_BOOL == type) {
            v->data.flag = true; // existence of the attribute indicates true unless specified different
        }
    } else {
        switch (type) {
        case PMIX_UNDEF:
            break;
        case PMIX_BOOL:
            memcpy(&(v->data.flag), data, 1);
            break;
        case PMIX_BYTE:
            memcpy(&(v->data.byte), data, 1);
            break;
        case PMIX_STRING:
            v->data.string = strdup(data);
            break;
        case PMIX_SIZE:
            memcpy(&(v->data.size), data, sizeof(size_t));
            break;
        case PMIX_PID:
            memcpy(&(v->data.pid), data, sizeof(pid_t));
            break;
        case PMIX_INT:
            memcpy(&(v->data.integer), data, sizeof(int));
            break;
        case PMIX_INT8:
            memcpy(&(v->data.int8), data, 1);
            break;
        case PMIX_INT16:
            memcpy(&(v->data.int16), data, 2);
            break;
        case PMIX_INT32:
            memcpy(&(v->data.int32), data, 4);
            break;
        case PMIX_INT64:
            memcpy(&(v->data.int64), data, 8);
            break;
        case PMIX_UINT:
            memcpy(&(v->data.uint), data, sizeof(int));
            break;
        case PMIX_UINT8:
            memcpy(&(v->data.uint8), data, 1);
            break;
        case PMIX_UINT16:
        case PMIX_STOR_ACCESS_TYPE:
            memcpy(&(v->data.uint16), data, 2);
            break;
        case PMIX_UINT32:
            memcpy(&(v->data.uint32), data, 4);
            break;
        case PMIX_UINT64:
        case PMIX_STOR_MEDIUM:
        case PMIX_STOR_ACCESS:
        case PMIX_STOR_PERSIST:
            memcpy(&(v->data.uint64), data, 8);
            break;
        case PMIX_FLOAT:
            memcpy(&(v->data.fval), data, sizeof(float));
            break;
        case PMIX_DOUBLE:
            memcpy(&(v->data.dval), data, sizeof(double));
            break;
        case PMIX_TIMEVAL:
            memcpy(&(v->data.tv), data, sizeof(struct timeval));
            break;
        case PMIX_TIME:
            memcpy(&(v->data.time), data, sizeof(time_t));
            break;
        case PMIX_STATUS:
            memcpy(&(v->data.status), data, sizeof(pmix_status_t));
            break;
        case PMIX_PROC_RANK:
            memcpy(&(v->data.rank), data, sizeof(pmix_rank_t));
            break;
        case PMIX_PROC_NSPACE:
            nspace = (pmix_nspace_t *) data;
            v->data.nspace = (pmix_nspace_t *) malloc(sizeof(pmix_nspace_t));
            PMIX_LOAD_NSPACE(*(v->data.nspace), *nspace);
            break;
        case PMIX_PROC:
            PMIX_PROC_CREATE(v->data.proc, 1);
            if (NULL == v->data.proc) {
                PMIX_ERROR_LOG(PMIX_ERR_NOMEM);
                return;
            }
            memcpy(v->data.proc, data, sizeof(pmix_proc_t));
            break;
        case PMIX_BYTE_OBJECT:
        case PMIX_COMPRESSED_STRING:
        case PMIX_COMPRESSED_BYTE_OBJECT:
            bo = (pmix_byte_object_t *) data;
            v->data.bo.bytes = (char *) malloc(bo->size);
            if (NULL == v->data.bo.bytes) {
                PMIX_ERROR_LOG(PMIX_ERR_NOMEM);
                return;
            }
            memcpy(v->data.bo.bytes, bo->bytes, bo->size);
            memcpy(&(v->data.bo.size), &bo->size, sizeof(size_t));
            break;
        case PMIX_PERSIST:
            memcpy(&(v->data.persist), data, sizeof(pmix_persistence_t));
            break;
        case PMIX_SCOPE:
            memcpy(&(v->data.scope), data, sizeof(pmix_scope_t));
            break;
        case PMIX_DATA_RANGE:
            memcpy(&(v->data.range), data, sizeof(pmix_data_range_t));
            break;
        case PMIX_PROC_STATE:
            memcpy(&(v->data.state), data, sizeof(pmix_proc_state_t));
            break;
        case PMIX_PROC_INFO:
            PMIX_PROC_INFO_CREATE(v->data.pinfo, 1);
            if (NULL == v->data.pinfo) {
                PMIX_ERROR_LOG(PMIX_ERR_NOMEM);
                return;
            }
            pi = (pmix_proc_info_t *) data;
            memcpy(&(v->data.pinfo->proc), &pi->proc, sizeof(pmix_proc_t));
            if (NULL != pi->hostname) {
                v->data.pinfo->hostname = strdup(pi->hostname);
            }
            if (NULL != pi->executable_name) {
                v->data.pinfo->executable_name = strdup(pi->executable_name);
            }
            memcpy(&(v->data.pinfo->pid), &pi->pid, sizeof(pid_t));
            memcpy(&(v->data.pinfo->exit_code), &pi->exit_code, sizeof(int));
            break;
        case PMIX_DATA_ARRAY:
            darray = (pmix_data_array_t *) data;
            rc = pmix_bfrops_base_copy_darray(&v->data.darray, darray, PMIX_DATA_ARRAY);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
            }
            break;
        case PMIX_POINTER:
            v->data.ptr = (void *) data;
            break;
        case PMIX_ALLOC_DIRECTIVE:
            memcpy(&(v->data.adir), data, sizeof(pmix_alloc_directive_t));
            break;
        case PMIX_ENVAR:
            envar = (pmix_envar_t *) data;
            if (NULL != envar->envar) {
                v->data.envar.envar = strdup(envar->envar);
            }
            if (NULL != envar->value) {
                v->data.envar.value = strdup(envar->value);
            }
            v->data.envar.separator = envar->separator;
            break;
        case PMIX_COORD:
            coord = (pmix_coord_t *) data;
            rc = pmix_bfrops_base_copy_coord(&v->data.coord, coord, PMIX_COORD);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
            }
            break;
        case PMIX_LINK_STATE:
            memcpy(&(v->data.linkstate), data, sizeof(pmix_link_state_t));
            break;
        case PMIX_JOB_STATE:
            memcpy(&(v->data.jstate), data, sizeof(pmix_job_state_t));
            break;
        case PMIX_TOPO:
            topo = (pmix_topology_t *) data;
            rc = pmix_bfrops_base_copy_topology(&v->data.topo, topo, PMIX_TOPO);
            if (PMIX_ERR_INIT == rc || PMIX_ERR_NOT_SUPPORTED == rc) {
                /* we are not initialized yet, so just copy the pointer */
                v->data.topo = topo;
                rc = PMIX_SUCCESS;
            }
            break;
        case PMIX_PROC_CPUSET:
            cpuset = (pmix_cpuset_t *) data;
            rc = pmix_bfrops_base_copy_cpuset(&v->data.cpuset, cpuset, PMIX_PROC_CPUSET);
            if (PMIX_ERR_INIT == rc || PMIX_ERR_NOT_SUPPORTED == rc) {
                /* we are not initialized yet, so just copy the pointer */
                v->data.cpuset = cpuset;
                rc = PMIX_SUCCESS;
            }
            break;
        case PMIX_LOCTYPE:
            memcpy(&(v->data.locality), data, sizeof(pmix_locality_t));
            break;
        case PMIX_GEOMETRY:
            geometry = (pmix_geometry_t *) data;
            rc = pmix_bfrops_base_copy_geometry(&v->data.geometry, geometry, PMIX_GEOMETRY);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
            }
            break;
        case PMIX_DEVTYPE:
            memcpy(&(v->data.devtype), data, sizeof(pmix_device_type_t));
            break;
        case PMIX_DEVICE_DIST:
            devdist = (pmix_device_distance_t *) data;
            rc = pmix_bfrops_base_copy_devdist(&v->data.devdist, devdist, PMIX_DEVICE_DIST);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
            }
            break;
        case PMIX_ENDPOINT:
            endpoint = (pmix_endpoint_t *) data;
            rc = pmix_bfrops_base_copy_endpoint(&v->data.endpoint, endpoint, PMIX_ENDPOINT);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
            }
            break;
        case PMIX_REGATTR:
            regattr = (pmix_regattr_t *) data;
            rc = pmix_bfrops_base_copy_regattr((pmix_regattr_t **) &v->data.ptr, regattr,
                                               PMIX_REGATTR);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
            }
            break;
        case PMIX_REGEX:
            /* load it into the byte object */
            rc = pmix_preg.copy(&v->data.bo.bytes, &v->data.bo.size, (char *) data);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
            }
            break;
        case PMIX_DATA_BUFFER:
            dbuf = (pmix_data_buffer_t *) data;
            PMIX_DATA_BUFFER_CREATE(v->data.dbuf);
            rc = PMIx_Data_copy_payload(v->data.dbuf, dbuf);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
            }
            break;
        case PMIX_PROC_STATS:
            pstats = (pmix_proc_stats_t *) data;
            rc = pmix_bfrops_base_copy_pstats(&v->data.pstats, pstats, PMIX_PROC_STATS);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
            }
            break;
        case PMIX_DISK_STATS:
            dkstats = (pmix_disk_stats_t *) data;
            rc = pmix_bfrops_base_copy_dkstats(&v->data.dkstats, dkstats, PMIX_DISK_STATS);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
            }
            break;
        case PMIX_NET_STATS:
            netstats = (pmix_net_stats_t *) data;
            rc = pmix_bfrops_base_copy_netstats(&v->data.netstats, netstats, PMIX_NET_STATS);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
            }
            break;
        case PMIX_NODE_STATS:
            ndstats = (pmix_node_stats_t *) data;
            rc = pmix_bfrops_base_copy_ndstats(&v->data.ndstats, ndstats, PMIX_NODE_STATS);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
            }
            break;

        default:
            /* silence warnings */
            break;
        }
    }
    return;
}

pmix_status_t pmix_bfrops_base_value_unload(pmix_value_t *kv, void **data, size_t *sz)
{
    pmix_status_t rc;
    pmix_envar_t *envar;
    pmix_data_array_t **darray;
    pmix_regattr_t *regattr, *r;

    rc = PMIX_SUCCESS;
    if (NULL == data ||
        (NULL == *data && PMIX_STRING != kv->type && PMIX_BYTE_OBJECT != kv->type)) {
        rc = PMIX_ERR_BAD_PARAM;
    } else {
        switch (kv->type) {
        case PMIX_UNDEF:
            rc = PMIX_ERR_UNKNOWN_DATA_TYPE;
            break;
        case PMIX_BOOL:
            memcpy(*data, &(kv->data.flag), sizeof(bool));
            *sz = sizeof(bool);
            break;
        case PMIX_BYTE:
            memcpy(*data, &(kv->data.byte), sizeof(uint8_t));
            *sz = sizeof(uint8_t);
            break;
        case PMIX_STRING:
            if (NULL != kv->data.string) {
                *data = strdup(kv->data.string);
                *sz = strlen(kv->data.string);
            }
            break;
        case PMIX_SIZE:
            memcpy(*data, &(kv->data.size), sizeof(size_t));
            *sz = sizeof(size_t);
            break;
        case PMIX_PID:
            memcpy(*data, &(kv->data.pid), sizeof(pid_t));
            *sz = sizeof(pid_t);
            break;
        case PMIX_UINT:
        case PMIX_INT:
            memcpy(*data, &(kv->data.integer), sizeof(int));
            *sz = sizeof(int);
            break;
        case PMIX_UINT8:
        case PMIX_INT8:
            memcpy(*data, &(kv->data.int8), sizeof(int8_t));
            *sz = sizeof(int8_t);
            break;
        case PMIX_UINT16:
        case PMIX_INT16:
        case PMIX_STOR_ACCESS_TYPE:
            memcpy(*data, &(kv->data.int16), sizeof(int16_t));
            *sz = sizeof(int16_t);
            break;
        case PMIX_UINT32:
        case PMIX_INT32:
            memcpy(*data, &(kv->data.int32), sizeof(int32_t));
            *sz = sizeof(int32_t);
            break;
        case PMIX_UINT64:
        case PMIX_INT64:
        case PMIX_STOR_MEDIUM:
        case PMIX_STOR_ACCESS:
        case PMIX_STOR_PERSIST:
            memcpy(*data, &(kv->data.int64), sizeof(int64_t));
            *sz = sizeof(int64_t);
            break;
        case PMIX_FLOAT:
            memcpy(*data, &(kv->data.fval), sizeof(float));
            *sz = sizeof(float);
            break;
        case PMIX_DOUBLE:
            memcpy(*data, &(kv->data.dval), sizeof(double));
            *sz = sizeof(double);
            break;
        case PMIX_TIMEVAL:
            memcpy(*data, &(kv->data.tv), sizeof(struct timeval));
            *sz = sizeof(struct timeval);
            break;
        case PMIX_TIME:
            memcpy(*data, &(kv->data.time), sizeof(time_t));
            *sz = sizeof(time_t);
            break;
        case PMIX_STATUS:
            memcpy(*data, &(kv->data.status), sizeof(pmix_status_t));
            *sz = sizeof(pmix_status_t);
            break;
        case PMIX_PROC_RANK:
            memcpy(*data, &(kv->data.rank), sizeof(pmix_rank_t));
            *sz = sizeof(pmix_rank_t);
            break;
        case PMIX_PROC_NSPACE:
            PMIX_LOAD_NSPACE(*data, *kv->data.nspace);
            *sz = strlen(*kv->data.nspace);
            break;
        case PMIX_PROC:
            PMIX_XFER_PROCID(*data, kv->data.proc);
            *sz = sizeof(pmix_proc_t);
            break;
        case PMIX_BYTE_OBJECT:
        case PMIX_COMPRESSED_STRING:
        case PMIX_COMPRESSED_BYTE_OBJECT:
            if (NULL != kv->data.bo.bytes && 0 < kv->data.bo.size) {
                *data = kv->data.bo.bytes;
                *sz = kv->data.bo.size;

            } else {
                *data = NULL;
                *sz = 0;
            }
            break;
        case PMIX_PERSIST:
            memcpy(*data, &(kv->data.persist), sizeof(pmix_persistence_t));
            *sz = sizeof(pmix_persistence_t);
            break;
        case PMIX_SCOPE:
            memcpy(*data, &(kv->data.scope), sizeof(pmix_scope_t));
            *sz = sizeof(pmix_scope_t);
            break;
        case PMIX_DATA_RANGE:
            memcpy(*data, &(kv->data.range), sizeof(pmix_data_range_t));
            *sz = sizeof(pmix_data_range_t);
            break;
        case PMIX_PROC_STATE:
            memcpy(*data, &(kv->data.state), sizeof(pmix_proc_state_t));
            *sz = sizeof(pmix_proc_state_t);
            break;
        case PMIX_PROC_INFO:
            rc = pmix_bfrops_base_copy_pinfo((pmix_proc_info_t **) data, kv->data.pinfo,
                                             PMIX_PROC_INFO);
            if (PMIX_SUCCESS == rc) {
                *sz = sizeof(pmix_proc_info_t);
            }
            break;
        case PMIX_DATA_ARRAY:
            darray = (pmix_data_array_t **) data;
            rc = pmix_bfrops_base_copy_darray(darray, kv->data.darray, PMIX_DATA_ARRAY);
            if (PMIX_SUCCESS == rc) {
                *sz = sizeof(pmix_data_array_t);
            }
            break;
        case PMIX_POINTER:
            *data = (void *) kv->data.ptr;
            *sz = sizeof(void *);
            break;
        case PMIX_ALLOC_DIRECTIVE:
            memcpy(*data, &(kv->data.adir), sizeof(pmix_alloc_directive_t));
            *sz = sizeof(pmix_alloc_directive_t);
            break;
        case PMIX_ENVAR:
            PMIX_ENVAR_CREATE(envar, 1);
            if (NULL == envar) {
                return PMIX_ERR_NOMEM;
            }
            if (NULL != kv->data.envar.envar) {
                envar->envar = strdup(kv->data.envar.envar);
            }
            if (NULL != kv->data.envar.value) {
                envar->value = strdup(kv->data.envar.value);
            }
            envar->separator = kv->data.envar.separator;
            *data = envar;
            *sz = sizeof(pmix_envar_t);
            break;
        case PMIX_COORD:
            rc = pmix_bfrops_base_copy_coord((pmix_coord_t **) data, kv->data.coord, PMIX_COORD);
            if (PMIX_SUCCESS == rc) {
                *sz = sizeof(pmix_coord_t);
            }
            break;
        case PMIX_LINK_STATE:
            memcpy(*data, &(kv->data.linkstate), sizeof(pmix_link_state_t));
            *sz = sizeof(pmix_link_state_t);
            break;
        case PMIX_JOB_STATE:
            memcpy(*data, &(kv->data.jstate), sizeof(pmix_job_state_t));
            *sz = sizeof(pmix_job_state_t);
            break;
        case PMIX_TOPO:
            rc = pmix_bfrops_base_copy_topology((pmix_topology_t **) data, kv->data.topo,
                                                PMIX_TOPO);
            if (PMIX_SUCCESS == rc) {
                *sz = sizeof(pmix_topology_t);
            } else if (PMIX_ERR_INIT == rc || PMIX_ERR_NOT_SUPPORTED == rc) {
                *data = malloc(sizeof(pmix_topology_t));
                memcpy(*data, kv->data.topo, sizeof(pmix_topology_t));
                *sz = sizeof(pmix_topology_t);
                rc = PMIX_SUCCESS;
            }
            break;
        case PMIX_PROC_CPUSET:
            rc = pmix_bfrops_base_copy_cpuset((pmix_cpuset_t **) data, kv->data.cpuset,
                                              PMIX_PROC_CPUSET);
            if (PMIX_SUCCESS == rc) {
                *sz = sizeof(pmix_cpuset_t);
            } else if (PMIX_ERR_INIT == rc || PMIX_ERR_NOT_SUPPORTED == rc) {
                *data = malloc(sizeof(pmix_cpuset_t));
                memcpy(*data, kv->data.cpuset, sizeof(pmix_cpuset_t));
                *sz = sizeof(pmix_cpuset_t);
                rc = PMIX_SUCCESS;
            }
            break;
        case PMIX_LOCTYPE:
            memcpy(*data, &(kv->data.locality), sizeof(pmix_locality_t));
            *sz = sizeof(pmix_locality_t);
            break;
        case PMIX_GEOMETRY:
            rc = pmix_bfrops_base_copy_geometry((pmix_geometry_t **) data, kv->data.geometry,
                                                PMIX_GEOMETRY);
            if (PMIX_SUCCESS == rc) {
                *sz = sizeof(pmix_geometry_t);
            }
            break;
        case PMIX_DEVTYPE:
            memcpy(*data, &(kv->data.devtype), sizeof(pmix_device_type_t));
            *sz = sizeof(pmix_device_type_t);
            break;
        case PMIX_DEVICE_DIST:
            rc = pmix_bfrops_base_copy_devdist((pmix_device_distance_t **) data, kv->data.devdist,
                                               PMIX_DEVICE_DIST);
            if (PMIX_SUCCESS == rc) {
                *sz = sizeof(pmix_device_distance_t);
            }
            break;
        case PMIX_ENDPOINT:
            rc = pmix_bfrops_base_copy_endpoint((pmix_endpoint_t **) data, kv->data.endpoint,
                                                PMIX_ENDPOINT);
            if (PMIX_SUCCESS == rc) {
                *sz = sizeof(pmix_endpoint_t);
            }
            break;
        case PMIX_REGATTR:
            PMIX_REGATTR_CREATE(regattr, 1);
            if (NULL == regattr) {
                return PMIX_ERR_NOMEM;
            }
            r = (pmix_regattr_t *) kv->data.ptr;
            if (NULL != r->name) {
                regattr->name = strdup(r->name);
            }
            PMIX_LOAD_KEY(regattr->string, r->string);
            regattr->type = r->type;
            regattr->description = PMIx_Argv_copy(r->description);
            *data = regattr;
            *sz = sizeof(pmix_regattr_t);
            break;
        case PMIX_REGEX:
            if (NULL != kv->data.bo.bytes && 0 < kv->data.bo.size) {
                *data = kv->data.bo.bytes;
                *sz = kv->data.bo.size;
            } else {
                *data = NULL;
                *sz = 0;
            }
            break;
        case PMIX_DATA_BUFFER:
            rc = pmix_bfrops_base_copy_dbuf((pmix_data_buffer_t **) data, kv->data.dbuf,
                                            PMIX_DATA_BUFFER);
            if (PMIX_SUCCESS == rc) {
                *sz = sizeof(pmix_data_buffer_t);
            }
            break;
        case PMIX_PROC_STATS:
            rc = pmix_bfrops_base_copy_pstats((pmix_proc_stats_t **) data, kv->data.pstats,
                                              PMIX_PROC_STATS);
            if (PMIX_SUCCESS == rc) {
                *sz = sizeof(pmix_proc_stats_t);
            }
            break;
        case PMIX_DISK_STATS:
            rc = pmix_bfrops_base_copy_dkstats((pmix_disk_stats_t **) data, kv->data.dkstats,
                                               PMIX_DISK_STATS);
            if (PMIX_SUCCESS == rc) {
                *sz = sizeof(pmix_disk_stats_t);
            }
            break;
        case PMIX_NET_STATS:
            rc = pmix_bfrops_base_copy_netstats((pmix_net_stats_t **) data, kv->data.netstats,
                                                PMIX_NET_STATS);
            if (PMIX_SUCCESS == rc) {
                *sz = sizeof(pmix_net_stats_t);
            }
            break;
        case PMIX_NODE_STATS:
            rc = pmix_bfrops_base_copy_ndstats((pmix_node_stats_t **) data, kv->data.ndstats,
                                               PMIX_NODE_STATS);
            if (PMIX_SUCCESS == rc) {
                *sz = sizeof(pmix_node_stats_t);
            }
            break;
        default:
            /* silence warnings */
            rc = PMIX_ERROR;
            break;
        }
    }
    return rc;
}

void pmix_bfrops_base_darray_destruct(pmix_data_array_t *d)
{
    pmix_bfrops_base_tma_data_array_destruct(d, NULL);
}

void pmix_bfrops_base_value_destruct(pmix_value_t *v)
{
    pmix_bfrops_base_tma_value_destruct(v, NULL);
}

/* Xfer FUNCTIONS FOR GENERIC PMIX TYPES */
pmix_status_t pmix_bfrops_base_value_xfer(pmix_value_t *p, const pmix_value_t *src)
{
    return pmix_bfrops_base_tma_value_xfer(p, src, NULL);
}

/**
 * Internal function that resizes (expands) an inuse buffer if
 * necessary.
 */
char *pmix_bfrop_buffer_extend(pmix_buffer_t *buffer, size_t bytes_to_add)
{
    return pmix_bfrops_base_tma_buffer_extend(buffer, bytes_to_add, NULL);
}

/*
 * Internal function that checks to see if the specified number of bytes
 * remain in the buffer for unpacking
 */
bool pmix_bfrop_too_small(pmix_buffer_t *buffer, size_t bytes_reqd)
{
    size_t bytes_remaining_packed;

    if (buffer->pack_ptr < buffer->unpack_ptr) {
        return true;
    }

    bytes_remaining_packed = buffer->pack_ptr - buffer->unpack_ptr;

    if (bytes_remaining_packed < bytes_reqd) {
        /* don't error log this - it could be that someone is trying to
         * simply read until the buffer is empty
         */
        return true;
    }

    return false;
}

pmix_status_t pmix_bfrop_store_data_type(pmix_pointer_array_t *regtypes, pmix_buffer_t *buffer,
                                         pmix_data_type_t type)
{
    pmix_status_t ret;

    PMIX_BFROPS_PACK_TYPE(ret, buffer, &type, 1, PMIX_UINT16, regtypes);
    return ret;
}

pmix_status_t pmix_bfrop_get_data_type(pmix_pointer_array_t *regtypes, pmix_buffer_t *buffer,
                                       pmix_data_type_t *type)
{
    pmix_status_t ret;
    int32_t m = 1;

    PMIX_BFROPS_UNPACK_TYPE(ret, buffer, type, &m, PMIX_UINT16, regtypes);
    return ret;
}

const char *pmix_bfrops_base_data_type_string(pmix_pointer_array_t *regtypes, pmix_data_type_t type)
{
    pmix_bfrop_type_info_t *info;

    /* Lookup the object for this type and call it */
    if (NULL == (info = (pmix_bfrop_type_info_t *) pmix_pointer_array_get_item(regtypes, type))) {
        return NULL;
    }

    return info->odti_name;
}

PMIX_EXPORT void *PMIx_Info_list_start(void)
{
    pmix_list_t *p;

    p = PMIX_NEW(pmix_list_t);
    return p;
}

PMIX_EXPORT pmix_status_t PMIx_Info_list_add(void *ptr,
                                             const char *key,
                                             const void *value,
                                             pmix_data_type_t type)
{
    pmix_list_t *p = (pmix_list_t *) ptr;
    pmix_infolist_t *iptr;

    iptr = PMIX_NEW(pmix_infolist_t);
    if (NULL == iptr) {
        return PMIX_ERR_NOMEM;
    }
    PMIX_INFO_LOAD(&iptr->info, key, value, type);
    pmix_list_append(p, &iptr->super);
    return PMIX_SUCCESS;
}

pmix_status_t PMIx_Info_list_prepend(void *ptr,
                                     const char *key,
                                     const void *value,
                                     pmix_data_type_t type)
{
    pmix_list_t *p = (pmix_list_t *) ptr;
    pmix_infolist_t *iptr;

    iptr = PMIX_NEW(pmix_infolist_t);
    if (NULL == iptr) {
        return PMIX_ERR_NOMEM;
    }
    PMIX_INFO_LOAD(&iptr->info, key, value, type);
    pmix_list_prepend(p, &iptr->super);
    return PMIX_SUCCESS;
}

PMIX_EXPORT pmix_status_t PMIx_Info_list_insert(void *ptr,
                                                pmix_info_t *info)
{
    pmix_list_t *p = (pmix_list_t *) ptr;
    pmix_infolist_t *iptr;

    iptr = PMIX_NEW(pmix_infolist_t);
    if (NULL == iptr) {
        return PMIX_ERR_NOMEM;
    }
    /* we want to preserve any pointers in the provided
     * info struct so the result points to the same
     * memory location */
    memcpy(&iptr->info, info, sizeof(pmix_info_t));
    // mark that this value should not be released
    PMIX_INFO_SET_PERSISTENT(&iptr->info);
    pmix_list_append(p, &iptr->super);
    return PMIX_SUCCESS;
}

PMIX_EXPORT pmix_status_t PMIx_Info_list_xfer(void *ptr, const pmix_info_t *info)
{
    pmix_list_t *p = (pmix_list_t *) ptr;
    pmix_infolist_t *iptr;

    iptr = PMIX_NEW(pmix_infolist_t);
    if (NULL == iptr) {
        return PMIX_ERR_NOMEM;
    }
    PMIx_Info_xfer(&iptr->info, info);
    pmix_list_append(p, &iptr->super);
    return PMIX_SUCCESS;
}

PMIX_EXPORT pmix_status_t PMIx_Info_list_convert(void *ptr, pmix_data_array_t *par)
{
    pmix_list_t *p = (pmix_list_t *) ptr;
    size_t n;
    pmix_infolist_t *iptr;
    pmix_info_t *array;

    if (NULL == par || NULL == ptr) {
        return PMIX_ERR_BAD_PARAM;
    }
    PMIX_DATA_ARRAY_INIT(par, PMIX_INFO);

    n = pmix_list_get_size(p);
    if (0 == n) {
        return PMIX_ERR_EMPTY;
    }
    PMIX_INFO_CREATE(par->array, n);
    if (NULL == par->array) {
        return PMIX_ERR_NOMEM;
    }
    par->type = PMIX_INFO;
    par->size = n;
    array = (pmix_info_t *) par->array;

    /* transfer the elements across */
    n = 0;
    PMIX_LIST_FOREACH (iptr, p, pmix_infolist_t) {
        PMIx_Info_xfer(&array[n], &iptr->info);
        ++n;
    }

    return PMIX_SUCCESS;
}

PMIX_EXPORT void PMIx_Info_list_release(void *ptr)
{
    pmix_list_t *p = (pmix_list_t *) ptr;
    PMIX_LIST_RELEASE(p);
}


pmix_info_t* PMIx_Info_list_get_info(void *ptr, void *prev, void **next)
{
    pmix_list_t *p = (pmix_list_t *) ptr;
    pmix_list_item_t *prv = (pmix_list_item_t*)prev;
    pmix_infolist_t *active;

    if (NULL == prev) {
        prv = pmix_list_get_first(p);
        active = (pmix_infolist_t*)prv;
    } else {
        active = (pmix_infolist_t*)prv;
    }
    if (prv == pmix_list_get_last(p)) {
        *next = NULL;
    } else {
        *next = (void*)pmix_list_get_next(prv);
    }
    return &active->info;
}

static pmix_status_t get_darray_size(pmix_data_array_t *array, 
                                     size_t *sz)
{
    pmix_status_t rc = PMIX_SUCCESS;
    size_t m, n, sz2;
    char **str;
    pmix_byte_object_t *bo;
    pmix_proc_info_t *pi;
    pmix_envar_t *en;
    pmix_coord_t *coord;
    pmix_topology_t *topo;
    pmix_cpuset_t *cset;
    pmix_geometry_t *geo;
    pmix_device_distance_t *dd;
    pmix_endpoint_t *endpt;
    pmix_regattr_t *rg;
    pmix_data_buffer_t *db;
    pmix_proc_stats_t *ps;
    pmix_disk_stats_t *ds;
    pmix_net_stats_t *nts;
    pmix_node_stats_t *nds;
    pmix_info_t *iptr;

    switch (array->type) {
        case PMIX_UNDEF:
            rc = PMIX_ERR_UNKNOWN_DATA_TYPE;
            break;
        case PMIX_BOOL:
        case PMIX_BYTE:
        case PMIX_INT8:
        case PMIX_UINT8:
            *sz = array->size;
            break;
        case PMIX_STRING:
            *sz = array->size * sizeof(void*);
            str = (char**)array->array;
            for (n=0; n < array->size; n++) {
                *sz += 1;
                if (NULL != str[n]) {
                    *sz += strlen(str[n]);
                }
            }
            break;
        case PMIX_SIZE:
            *sz = array->size * sizeof(size_t);
            break;
        case PMIX_PID:
            *sz = array->size * sizeof(pid_t);
            break;
        case PMIX_INT:
        case PMIX_UINT:
            *sz = array->size * sizeof(int);
            break;
        case PMIX_INT16:
        case PMIX_UINT16:
        case PMIX_STOR_ACCESS_TYPE:
            *sz = array->size * 2;
            break;
        case PMIX_INT32:
        case PMIX_UINT32:
            *sz = array->size * 4;
            break;
        case PMIX_INT64:
        case PMIX_UINT64:
        case PMIX_STOR_MEDIUM:
        case PMIX_STOR_ACCESS:
        case PMIX_STOR_PERSIST:
            *sz = array->size * 8;
            break;
        case PMIX_FLOAT:
            *sz = array->size * sizeof(float);
            break;
        case PMIX_DOUBLE:
            *sz = array->size * sizeof(double);
            break;
        case PMIX_TIMEVAL:
            *sz = array->size * sizeof(struct timeval);
            break;
        case PMIX_TIME:
            *sz = array->size * sizeof(time_t);
            break;
        case PMIX_STATUS:
            *sz = array->size * sizeof(pmix_status_t);
            break;
        case PMIX_PROC_RANK:
            *sz = array->size * sizeof(pmix_rank_t);
            break;
        case PMIX_PROC_NSPACE:
            *sz = array->size * PMIX_MAX_NSLEN;
            break;
        case PMIX_PROC:
            *sz = array->size * sizeof(pmix_proc_t);
            break;
        case PMIX_INFO:
            iptr = (pmix_info_t*)array->array;
            for (n=0; n < array->size; n++) {
                rc = PMIx_Info_get_size(&iptr[n], &sz2);
                if (PMIX_SUCCESS == rc) {
                    *sz += sz2;
                } else {
                    return rc;
                }
            }
            break;
        case PMIX_BYTE_OBJECT:
            *sz = array->size * sizeof(pmix_byte_object_t);
            bo = (pmix_byte_object_t*)array->array;
            for (n=0; n < array->size; n++) {
                *sz += bo[n].size;
            }
            break;
        case PMIX_COMPRESSED_STRING:
            *sz = array->size * sizeof(void*);
            bo = (pmix_byte_object_t*)array->array;
            for (n=0; n < array->size; n++) {
                *sz += pmix_compress.get_decompressed_strlen(&bo[n]);
            }
            break;
        case PMIX_COMPRESSED_BYTE_OBJECT:
            *sz = array->size * sizeof(void*);
            bo = (pmix_byte_object_t*)array->array;
            for (n=0; n < array->size; n++) {
                *sz += pmix_compress.get_decompressed_size(&bo[n]);
            }
            break;
        case PMIX_PERSIST:
            *sz = array->size * sizeof(pmix_persistence_t);
            break;
        case PMIX_SCOPE:
            *sz = array->size * sizeof(pmix_scope_t);
            break;
        case PMIX_DATA_RANGE:
            *sz = array->size * sizeof(pmix_data_range_t);
            break;
        case PMIX_PROC_STATE:
            *sz = array->size * sizeof(pmix_proc_state_t);
            break;
        case PMIX_PROC_INFO:
            *sz = array->size * sizeof(pmix_proc_info_t);
            pi = (pmix_proc_info_t*)array->array;
            for (n=0; n < array->size; n++) {
                *sz += 1;
                if (NULL != pi[n].hostname) {
                    *sz += strlen(pi[n].hostname);
                }
                *sz += 1;
                if (NULL != pi[n].executable_name) {
                    *sz += strlen(pi[n].executable_name);
                }
            }
            break;
        case PMIX_DATA_ARRAY:
            rc = PMIX_ERR_NOT_SUPPORTED;
            break;
        case PMIX_POINTER:
            *sz = array->size * sizeof(void *);
            break;
        case PMIX_ALLOC_DIRECTIVE:
            *sz = array->size * sizeof(pmix_alloc_directive_t);
            break;
        case PMIX_ENVAR:
            *sz = array->size * sizeof(pmix_envar_t);
            en = (pmix_envar_t*)array->array;
            for (n=0; n < array->size; n++) {
                *sz += 1;
                if (NULL != en[n].envar) {
                    *sz += strlen(en[n].envar);
                }
                *sz += 1;
                if (NULL != en[n].value) {
                    *sz += strlen(en[n].value);
                }
            }
            break;
        case PMIX_COORD:
            *sz = array->size * sizeof(pmix_coord_t);
            coord = (pmix_coord_t*)array->array;
            for (n=0; n < array->size; n++) {
                if (0 < coord[n].dims) {
                    *sz += coord[n].dims * sizeof(uint32_t);
                }
            }
            break;
        case PMIX_LINK_STATE:
            *sz = array->size * sizeof(pmix_link_state_t);
            break;
        case PMIX_JOB_STATE:
            *sz = array->size * sizeof(pmix_job_state_t);
            break;
        case PMIX_TOPO:
            *sz = array->size * sizeof(pmix_topology_t);
            topo = (pmix_topology_t*)array->array;
            for (n=0; n < array->size; n++) {
                rc = pmix_hwloc_get_topology_size(&topo[n], &sz2);
                if (PMIX_SUCCESS == rc) {
                    *sz += sz2;
                } else {
                    return rc;
                }
            }
            break;
        case PMIX_PROC_CPUSET:
            *sz = array->size * sizeof(pmix_cpuset_t);
            cset = (pmix_cpuset_t*)array->array;
            for (n=0; n < array->size; n++) {
                rc = pmix_hwloc_get_cpuset_size(&cset[n], &sz2);
                if (PMIX_SUCCESS == rc) {
                    *sz += sz2;
                } else {
                    return rc;
                }
            }
            break;
        case PMIX_LOCTYPE:
            *sz = array->size * sizeof(pmix_locality_t);
            break;
        case PMIX_GEOMETRY:
            *sz = array->size * sizeof(pmix_geometry_t);
            geo = (pmix_geometry_t*)array->array;
            for (m=0; m < array->size; m++) {
                *sz += 1;
                if (NULL != geo[m].uuid) {
                    *sz += strlen(geo[m].uuid);
                }
                *sz += 1;
                if (NULL != geo[m].osname) {
                    *sz += strlen(geo[m].osname);
                }
                for (n=0; n < geo[m].ncoords; n++) {
                    *sz += sizeof(pmix_coord_t);
                    if (0 < geo[m].coordinates[n].dims) {
                        *sz += geo[m].coordinates[n].dims * sizeof(uint32_t);
                    }
                }
            }
            break;
        case PMIX_DEVTYPE:
            *sz = array->size * sizeof(pmix_device_type_t);
            break;
        case PMIX_DEVICE_DIST:
            *sz = array->size * sizeof(pmix_device_distance_t);
            dd = (pmix_device_distance_t*)array->array;
            for (n=0; n < array->size; n++) {
                *sz += 1;
                if (NULL != dd[n].uuid) {
                    *sz += strlen(dd[n].uuid);
                }
                *sz += 1;
                if (NULL != dd[n].osname) {
                    *sz += strlen(dd[n].osname);
                }
            }
            break;
        case PMIX_ENDPOINT:
            *sz = array->size * sizeof(pmix_endpoint_t);
            endpt = (pmix_endpoint_t*)array->array;
            for (n=0; n < array->size; n++) {
                *sz += 1;
                if (NULL != endpt[n].uuid) {
                    *sz += strlen(endpt[n].uuid);
                }
                *sz += 1;
                if (NULL != endpt[n].osname) {
                    *sz += strlen(endpt[n].osname);
                }
                *sz += endpt[n].endpt.size;
            }
            break;
        case PMIX_REGATTR:
            *sz = array->size * sizeof(pmix_regattr_t);
            rg = (pmix_regattr_t*)array->array;
            for (n=0; n < array->size; n++) {
                *sz += 1;
                if (NULL != rg[n].name) {
                    *sz += strlen(rg[n].name);
                }
                if (NULL != rg[n].description) {
                    for (m=0; NULL != rg[n].description[m]; m++) {
                        *sz += 1;
                        *sz += strlen(rg[n].description[m]);
                    }
                }
            }
            break;
        case PMIX_REGEX:
            *sz = array->size * sizeof(pmix_byte_object_t);
            bo = (pmix_byte_object_t*)array->array;
            for (n=0; n < array->size; n++) {
                *sz += bo[n].size;
            }
            break;
        case PMIX_DATA_BUFFER:
            *sz = array->size * sizeof(pmix_data_buffer_t);
            db = (pmix_data_buffer_t*)array->array;
            for (n=0; n < array->size; n++) {
                *sz += db[n].bytes_used;
            }
            break;
        case PMIX_PROC_STATS:
            *sz = array->size * sizeof(pmix_proc_stats_t);
            ps = (pmix_proc_stats_t*)array->array;
            for (n=0; n < array->size; n++) {
                *sz += 1;
                if (NULL != ps[n].node) {
                    *sz += strlen(ps[n].node);
                }
                *sz += 1;
                if (NULL != ps[n].cmd) {
                    *sz += strlen(ps[n].cmd);
                }
            }
            break;
        case PMIX_DISK_STATS:
            *sz = array->size * sizeof(pmix_disk_stats_t);
            ds = (pmix_disk_stats_t*)array->array;
            for (n=0; n < array->size; n++) {
                *sz += 1;
                if (NULL != ds[n].disk) {
                    *sz += strlen(ds[n].disk);
                }
            }
            break;
        case PMIX_NET_STATS:
            *sz = array->size * sizeof(pmix_net_stats_t);
            nts = (pmix_net_stats_t*)array->array;
            for (n=0; n < array->size; n++) {
                *sz += 1;
                if (NULL != nts[n].net_interface) {
                    *sz += strlen(nts[n].net_interface);
                }
            }
            break;
        case PMIX_NODE_STATS:
            *sz = array->size * sizeof(pmix_node_stats_t);
            nds = (pmix_node_stats_t*)array->array;
            for (n=0; n < array->size; n++) {
                *sz += 1;
                if (NULL != nds[n].node) {
                    *sz += strlen(nds[n].node);
                }
                for (m=0; m < nds[n].ndiskstats; m++) {
                    *sz += sizeof(pmix_disk_stats_t);
                    *sz += 1;
                    if (NULL != nds[n].diskstats[m].disk) {
                        *sz += strlen(nds[n].diskstats[m].disk);
                    }
                }
                for (m=0; m < nds[n].nnetstats; m++) {
                    *sz += sizeof(pmix_net_stats_t);
                    *sz += 1;
                    if (NULL != nds[n].netstats[m].net_interface) {
                        *sz += strlen(nds[n].netstats[m].net_interface);
                    }
                }
            }
            break;

        default:
            /* silence warnings */
            break;
        }
    return PMIX_SUCCESS;
}

pmix_status_t PMIx_Value_get_size(const pmix_value_t *v,
                                  size_t *sz)
{
    pmix_status_t rc = PMIX_SUCCESS;
    size_t n;
    pmix_regattr_t *regattr;

    switch (v->type) {
        case PMIX_UNDEF:
            rc = PMIX_ERR_UNKNOWN_DATA_TYPE;
            break;
        case PMIX_BOOL:
        case PMIX_BYTE:
        case PMIX_INT8:
        case PMIX_UINT8:
            *sz = 1;
            break;
        case PMIX_STRING:
            *sz = 1;
            if (NULL != v->data.string) {
                *sz += strlen(v->data.string);
            }
            break;
        case PMIX_SIZE:
            *sz = sizeof(size_t);
            break;
        case PMIX_PID:
            *sz = sizeof(pid_t);
            break;
        case PMIX_INT:
        case PMIX_UINT:
            *sz = sizeof(int);
            break;
        case PMIX_INT16:
        case PMIX_UINT16:
        case PMIX_STOR_ACCESS_TYPE:
            *sz = 2;
            break;
        case PMIX_INT32:
        case PMIX_UINT32:
            *sz = 4;
            break;
        case PMIX_INT64:
        case PMIX_UINT64:
        case PMIX_STOR_MEDIUM:
        case PMIX_STOR_ACCESS:
        case PMIX_STOR_PERSIST:
            *sz = 8;
            break;
        case PMIX_FLOAT:
            *sz = sizeof(float);
            break;
        case PMIX_DOUBLE:
            *sz = sizeof(double);
            break;
        case PMIX_TIMEVAL:
            *sz = sizeof(struct timeval);
            break;
        case PMIX_TIME:
            *sz = sizeof(time_t);
            break;
        case PMIX_STATUS:
            *sz = sizeof(pmix_status_t);
            break;
        case PMIX_PROC_RANK:
            *sz = sizeof(pmix_rank_t);
            break;
        case PMIX_PROC_NSPACE:
            *sz = PMIX_MAX_NSLEN;
            break;
        case PMIX_PROC:
            *sz = sizeof(pmix_proc_t);
            break;
        case PMIX_BYTE_OBJECT:
            *sz = sizeof(pmix_byte_object_t);
            if (NULL != v->data.bo.bytes) {
                *sz += v->data.bo.size;
            }
            break;
        case PMIX_COMPRESSED_STRING:
            *sz = pmix_compress.get_decompressed_strlen(&v->data.bo);
            break;
        case PMIX_COMPRESSED_BYTE_OBJECT:
            *sz = pmix_compress.get_decompressed_size(&v->data.bo);
            break;
        case PMIX_PERSIST:
            *sz = sizeof(pmix_persistence_t);
            break;
        case PMIX_SCOPE:
            *sz = sizeof(pmix_scope_t);
            break;
        case PMIX_DATA_RANGE:
            *sz = sizeof(pmix_data_range_t);
            break;
        case PMIX_PROC_STATE:
            *sz = sizeof(pmix_proc_state_t);
            break;
        case PMIX_PROC_INFO:
            *sz = sizeof(pmix_proc_info_t);
            *sz += 1;
            if (NULL != v->data.pinfo->hostname) {
                *sz += strlen(v->data.pinfo->hostname);
            }
            *sz += 1;
            if (NULL != v->data.pinfo->executable_name) {
                *sz += strlen(v->data.pinfo->executable_name);
            }
            break;
        case PMIX_DATA_ARRAY:
            rc = get_darray_size(v->data.darray, sz);
            if (PMIX_SUCCESS == rc) {
                *sz += sizeof(pmix_data_array_t);
            }
            break;
        case PMIX_POINTER:
            *sz = sizeof(void *);
            break;
        case PMIX_ALLOC_DIRECTIVE:
            *sz = sizeof(pmix_alloc_directive_t);
            break;
        case PMIX_ENVAR:
            *sz = sizeof(pmix_envar_t);
            *sz += 1;
            if (NULL != v->data.envar.envar) {
                *sz += strlen(v->data.envar.envar);
            }
            *sz += 1;
            if (NULL != v->data.envar.value) {
                *sz += strlen(v->data.envar.value);
            }
            break;
        case PMIX_COORD:
            *sz = sizeof(pmix_coord_t);
            if (0 < v->data.coord->dims) {
                *sz += v->data.coord->dims * sizeof(uint32_t);
            }
            break;
        case PMIX_LINK_STATE:
            *sz = sizeof(pmix_link_state_t);
            break;
        case PMIX_JOB_STATE:
            *sz = sizeof(pmix_job_state_t);
            break;
        case PMIX_TOPO:
            rc = pmix_hwloc_get_topology_size(v->data.topo, sz);
            if (PMIX_SUCCESS == rc) {
                *sz += sizeof(pmix_topology_t);
            }
            break;
        case PMIX_PROC_CPUSET:
            rc = pmix_hwloc_get_cpuset_size(v->data.cpuset, sz);
            if (PMIX_SUCCESS == rc) {
                *sz += sizeof(pmix_cpuset_t);
            }
            break;
        case PMIX_LOCTYPE:
            *sz = sizeof(pmix_locality_t);
            break;
        case PMIX_GEOMETRY:
            *sz = sizeof(pmix_geometry_t);
            *sz += 1;
            if (NULL != v->data.geometry->uuid) {
                *sz += strlen(v->data.geometry->uuid);
            }
            *sz += 1;
            if (NULL != v->data.geometry->osname) {
                *sz += strlen(v->data.geometry->osname);
            }
            for (n=0; n < v->data.geometry->ncoords; n++) {
                *sz += sizeof(pmix_coord_t);
                if (0 < v->data.geometry->coordinates[n].dims) {
                    *sz += v->data.geometry->coordinates[n].dims * sizeof(uint32_t);
                }
            }
            break;
        case PMIX_DEVTYPE:
            *sz = sizeof(pmix_device_type_t);
            break;
        case PMIX_DEVICE_DIST:
            *sz = sizeof(pmix_device_distance_t);
            *sz += 1;
            if (NULL != v->data.devdist->uuid) {
                *sz += strlen(v->data.devdist->uuid);
            }
            *sz += 1;
            if (NULL != v->data.devdist->osname) {
                *sz += strlen(v->data.devdist->osname);
            }
            break;
        case PMIX_ENDPOINT:
            *sz = sizeof(pmix_endpoint_t);
            *sz += 1;
            if (NULL != v->data.endpoint->uuid) {
                *sz += strlen(v->data.endpoint->uuid);
            }
            *sz += 1;
            if (NULL != v->data.endpoint->osname) {
                *sz += strlen(v->data.endpoint->osname);
            }
            *sz += v->data.endpoint->endpt.size;
            break;
        case PMIX_REGATTR:
            *sz = sizeof(pmix_regattr_t);
            regattr = (pmix_regattr_t *)v->data.ptr;
            *sz += 1;
            if (NULL != regattr->name) {
                *sz += strlen(regattr->name);
            }
            if (NULL != regattr->description) {
                for (n=0; NULL != regattr->description[n]; n++) {
                    *sz += 1;
                    *sz += strlen(regattr->description[n]);
                }
            }
            break;
        case PMIX_REGEX:
            *sz = sizeof(pmix_byte_object_t);
            *sz += v->data.bo.size;
            break;
        case PMIX_DATA_BUFFER:
            *sz = sizeof(pmix_data_buffer_t);
            *sz += v->data.dbuf->bytes_used;
            break;
        case PMIX_PROC_STATS:
            *sz = sizeof(pmix_proc_stats_t);
            *sz += 1;
            if (NULL != v->data.pstats->node) {
                *sz += strlen(v->data.pstats->node);
            }
            *sz += 1;
            if (NULL != v->data.pstats->cmd) {
                *sz += strlen(v->data.pstats->cmd);
            }
            break;
        case PMIX_DISK_STATS:
            *sz = sizeof(pmix_disk_stats_t);
            *sz += 1;
            if (NULL != v->data.dkstats->disk) {
                *sz += strlen(v->data.dkstats->disk);
            }
            break;
        case PMIX_NET_STATS:
            *sz = sizeof(pmix_net_stats_t);
            *sz += 1;
            if (NULL != v->data.netstats->net_interface) {
                *sz += strlen(v->data.netstats->net_interface);
            }
            break;
        case PMIX_NODE_STATS:
            *sz = sizeof(pmix_node_stats_t);
            *sz += 1;
            if (NULL != v->data.ndstats->node) {
                *sz += strlen(v->data.ndstats->node);
            }
            for (n=0; n < v->data.ndstats->ndiskstats; n++) {
                *sz += sizeof(pmix_disk_stats_t);
                *sz += 1;
                if (NULL != v->data.ndstats->diskstats[n].disk) {
                    *sz += strlen(v->data.ndstats->diskstats[n].disk);
                }
            }
            for (n=0; n < v->data.ndstats->nnetstats; n++) {
                *sz += sizeof(pmix_net_stats_t);
                *sz += 1;
                if (NULL != v->data.ndstats->netstats[n].net_interface) {
                    *sz += strlen(v->data.ndstats->netstats[n].net_interface);
                }
            }
            break;

        default:
            /* silence warnings */
            break;
    }

    /* account for the size of the pmix_value_t itself */
    *sz += sizeof(pmix_value_t);

    return rc;
}

pmix_status_t PMIx_Info_get_size(const pmix_info_t *val,
                                 size_t *size)
{
    pmix_status_t rc;
    size_t len;

    rc = PMIx_Value_get_size(&val->value, size);
    if (PMIX_SUCCESS != rc) {
        return rc;
    }
    len = strnlen(val->key, PMIX_MAX_KEYLEN);
    if (PMIX_MAX_KEYLEN == len) {
        *size += PMIX_MAX_KEYLEN;
    } else {
        *size += len + 1;
    }
    *size += sizeof(pmix_info_t);
    return PMIX_SUCCESS;
}
