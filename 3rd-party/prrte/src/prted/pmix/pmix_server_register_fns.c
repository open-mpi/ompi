/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2009-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2011      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2013-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2014-2019 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2017-2020 IBM Corporation.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting.  All rights reserved.
 * Copyright (c) 2024      Triad National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include "prte_config.h"

#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif
#include <fcntl.h>
#include <pmix_server.h>

#include "prte_stdint.h"
#include "src/hwloc/hwloc-internal.h"
#include "src/include/hash_string.h"
#include "src/pmix/pmix-internal.h"
#include "src/util/pmix_argv.h"
#include "src/util/error.h"
#include "src/util/pmix_os_dirpath.h"
#include "src/util/pmix_output.h"
#include "src/util/pmix_printf.h"
#include "types.h"

#include "src/mca/errmgr/errmgr.h"
#include "src/mca/rmaps/base/base.h"
#include "src/runtime/prte_globals.h"
#include "src/runtime/prte_wait.h"
#include "src/util/name_fns.h"
#include "src/util/session_dir.h"

#include "src/prted/pmix/pmix_server.h"
#include "src/prted/pmix/pmix_server_internal.h"

static void opcbfunc(pmix_status_t status, void *cbdata);

/* stuff proc attributes for sending back to a proc */
int prte_pmix_server_register_nspace(prte_job_t *jdata)
{
    int rc;
    prte_proc_t *pptr;
    int i, k, n;
    void *info, *pmap, *iarray;
    prte_info_item_t *kv;
    prte_node_t *node;
    pmix_rank_t vpid;
    char **list, **procs, **micro, *tmp, *regex;
    prte_job_map_t *map;
    prte_app_context_t *app;
    uid_t uid;
    gid_t gid;
    pmix_list_t *cache;
    hwloc_obj_t machine;
    pmix_proc_t pproc, *parentproc, *procptr;
    pmix_status_t ret;
    pmix_info_t *pinfo, devinfo[2];
    size_t ninfo;
    prte_pmix_lock_t lock;
    pmix_list_t local_procs, members;
    prte_namelist_t *nm;
    size_t nmsize;
    pmix_server_pset_t *pset;
    pmix_cpuset_t cpuset;
    uint32_t ui32;
    prte_job_t *parent = NULL;
    pmix_device_distance_t *distances;
    size_t ndist;
    pmix_topology_t topo;
    pmix_data_array_t darray, lparray;
    bool flag, *fptr;
    size_t size, sz;
    pmix_info_t *iptr;
    void *next;

    pmix_output_verbose(2, prte_pmix_server_globals.output,
                        "%s register nspace for %s",
                        PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_JOBID_PRINT(jdata->nspace));

    /* setup the info list */
    PMIX_INFO_LIST_START(info);
    uid = geteuid();
    gid = getegid();
    topo.source = "hwloc";

    /* pass our nspace/rank */
    PMIX_INFO_LIST_ADD(ret, info, PMIX_SERVER_NSPACE, prte_process_info.myproc.nspace, PMIX_STRING);
    if (PMIX_SUCCESS != ret) {
        PMIX_ERROR_LOG(ret);
        PMIX_INFO_LIST_RELEASE(info);
        rc = prte_pmix_convert_status(ret);
        return rc;
    }
    PMIX_INFO_LIST_ADD(ret, info, PMIX_SERVER_RANK, &prte_process_info.myproc.rank, PMIX_PROC_RANK);
    if (PMIX_SUCCESS != ret) {
        PMIX_ERROR_LOG(ret);
        PMIX_INFO_LIST_RELEASE(info);
        rc = prte_pmix_convert_status(ret);
        return rc;
    }

    /* pass the session ID - just a 32-bit hash of our nspace for now */
    PRTE_HASH_STR(prte_process_info.myproc.nspace, ui32);
    PMIX_INFO_LIST_ADD(ret, info, PMIX_SESSION_ID, &ui32, PMIX_UINT32);
    if (PMIX_SUCCESS != ret) {
        PMIX_ERROR_LOG(ret);
        PMIX_INFO_LIST_RELEASE(info);
        rc = prte_pmix_convert_status(ret);
        return rc;
    }

    /* jobid */
    PMIX_INFO_LIST_ADD(ret, info, PMIX_JOBID, jdata->nspace, PMIX_STRING);
    if (PMIX_SUCCESS != ret) {
        PMIX_ERROR_LOG(ret);
        PMIX_INFO_LIST_RELEASE(info);
        rc = prte_pmix_convert_status(ret);
        return rc;
    }

    /* offset */
    PMIX_INFO_LIST_ADD(ret, info, PMIX_NPROC_OFFSET, &jdata->offset, PMIX_PROC_RANK);
    if (PMIX_SUCCESS != ret) {
        PMIX_ERROR_LOG(ret);
        PMIX_INFO_LIST_RELEASE(info);
        rc = prte_pmix_convert_status(ret);
        return rc;
    }

    /* check for cached values to add to the job info */
    cache = NULL;
    if (prte_get_attribute(&jdata->attributes, PRTE_JOB_INFO_CACHE, (void **) &cache, PMIX_POINTER)
        && NULL != cache) {
        while (NULL != (kv = (prte_info_item_t *) pmix_list_remove_first(cache))) {
            PMIX_INFO_LIST_XFER(ret, info, &kv->info);
            if (PMIX_SUCCESS != ret) {
                PMIX_ERROR_LOG(ret);
                PMIX_INFO_LIST_RELEASE(info);
                rc = prte_pmix_convert_status(ret);
                return rc;
            }
        }
        prte_remove_attribute(&jdata->attributes, PRTE_JOB_INFO_CACHE);
        PMIX_RELEASE(cache);
    }

    /* assemble the node and proc map info */
    list = NULL;
    procs = NULL;
    map = jdata->map;
    PMIX_LOAD_NSPACE(pproc.nspace, jdata->nspace);
    PMIX_CONSTRUCT(&local_procs, pmix_list_t);
    for (i = 0; i < map->nodes->size; i++) {
        if (NULL != (node = (prte_node_t *) pmix_pointer_array_get_item(map->nodes, i))) {
            micro = NULL;
            tmp = NULL;
            vpid = PMIX_RANK_VALID;
            ui32 = 0;
            PMIX_ARGV_APPEND_NOSIZE_COMPAT(&list, node->name);
            /* assemble all the ranks for this job that are on this node */
            for (k = 0; k < node->procs->size; k++) {
                if (NULL != (pptr = (prte_proc_t *) pmix_pointer_array_get_item(node->procs, k))) {
                    if (PMIX_CHECK_NSPACE(jdata->nspace, pptr->name.nspace)) {
                        PMIX_ARGV_APPEND_NOSIZE_COMPAT(&micro, PRTE_VPID_PRINT(pptr->name.rank));
                        if (pptr->name.rank < vpid) {
                            vpid = pptr->name.rank;
                        }
                        ++ui32;
                    }
                    if (PRTE_PROC_MY_NAME->rank == node->daemon->name.rank) {
                        /* track all procs on our node */
                        nm = PMIX_NEW(prte_namelist_t);
                        PMIX_LOAD_PROCID(&nm->name, pptr->name.nspace, pptr->name.rank);
                        pmix_list_append(&local_procs, &nm->super);
                        if (PMIX_CHECK_NSPACE(jdata->nspace, pptr->name.nspace)) {
                            /* go ahead and register this client - since we are going to wait
                             * for register_nspace to complete and the PMIx library serializes
                             * the registration requests, we don't need to wait here */
                            ret = PMIx_server_register_client(&pptr->name, uid, gid,
                                                              (void*)pptr, NULL, NULL);
                            if (PMIX_SUCCESS != ret && PMIX_OPERATION_SUCCEEDED != ret) {
                                PMIX_ERROR_LOG(ret);
                            }
                        }
                    }
                }
            }
            /* assemble the rank/node map */
            if (NULL != micro) {
                tmp = PMIX_ARGV_JOIN_COMPAT(micro, ',');
                PMIX_ARGV_FREE_COMPAT(micro);
                PMIX_ARGV_APPEND_NOSIZE_COMPAT(&procs, tmp);
            }
            /* construct the node info array */
            PMIX_INFO_LIST_START(iarray);
            /* start with the hostname */
            PMIX_INFO_LIST_ADD(ret, iarray, PMIX_HOSTNAME, node->name, PMIX_STRING);
            /* add any aliases */
            if (NULL != node->aliases) {
                regex = PMIX_ARGV_JOIN_COMPAT(node->aliases, ',');
                PMIX_INFO_LIST_ADD(ret, iarray, PMIX_HOSTNAME_ALIASES, regex, PMIX_STRING);
                free(regex);
            }
            /* pass the node ID */
            PMIX_INFO_LIST_ADD(ret, iarray, PMIX_NODEID, &node->index, PMIX_UINT32);
            /* add node size */
            PMIX_INFO_LIST_ADD(ret, iarray, PMIX_NODE_SIZE, &node->num_procs, PMIX_UINT32);
            /* add local size for this job */
            PMIX_INFO_LIST_ADD(ret, iarray, PMIX_LOCAL_SIZE, &ui32, PMIX_UINT32);
            /* pass the local ldr */
            PMIX_INFO_LIST_ADD(ret, iarray, PMIX_LOCALLDR, &vpid, PMIX_PROC_RANK);
            /* add the local peers */
            if (NULL != tmp) {
                PMIX_INFO_LIST_ADD(ret, iarray, PMIX_LOCAL_PEERS, tmp, PMIX_STRING);
                free(tmp);
            }
            /* if oversubscribed, mark it */
            if (PRTE_FLAG_TEST(node, PRTE_NODE_FLAG_OVERSUBSCRIBED)) {
                PMIX_INFO_LIST_ADD(ret, iarray, PMIX_NODE_OVERSUBSCRIBED, NULL, PMIX_BOOL);
            }
            /* add to the overall payload */
            PMIX_INFO_LIST_CONVERT(ret, iarray, &darray);
            PMIX_INFO_LIST_ADD(ret, info, PMIX_NODE_INFO_ARRAY, &darray, PMIX_DATA_ARRAY);
            PMIX_DATA_ARRAY_DESTRUCT(&darray);
            PMIX_INFO_LIST_RELEASE(iarray);
        }
    }
    /* let the PMIx server generate the nodemap regex */
    if (NULL != list) {
        tmp = PMIX_ARGV_JOIN_COMPAT(list, ',');
        PMIX_ARGV_FREE_COMPAT(list);
        list = NULL;
        if (PMIX_SUCCESS != (ret = PMIx_generate_regex(tmp, &regex))) {
            PMIX_ERROR_LOG(ret);
            free(tmp);
            PMIX_INFO_LIST_RELEASE(info);
            rc = prte_pmix_convert_status(ret);
            return rc;
        }
        free(tmp);
        PMIX_INFO_LIST_ADD(ret, info, PMIX_NODE_MAP, regex, PMIX_REGEX);
        free(regex);
    }

    /* let the PMIx server generate the procmap regex */
    if (NULL != procs) {
        tmp = PMIX_ARGV_JOIN_COMPAT(procs, ';');
        PMIX_ARGV_FREE_COMPAT(procs);
        procs = NULL;
        if (PMIX_SUCCESS != (ret = PMIx_generate_ppn(tmp, &regex))) {
            PMIX_ERROR_LOG(ret);
            free(tmp);
            PMIX_INFO_LIST_RELEASE(info);
            rc = prte_pmix_convert_status(ret);
            return rc;
        }
        free(tmp);
        PMIX_INFO_LIST_ADD(ret, info, PMIX_PROC_MAP, regex, PMIX_REGEX);
        free(regex);
    }

    /* pass the number of nodes in the job */
    PMIX_INFO_LIST_ADD(ret, info, PMIX_NUM_NODES, &map->num_nodes, PMIX_UINT32);

    /* univ size */
    PMIX_INFO_LIST_ADD(ret, info, PMIX_UNIV_SIZE, &jdata->total_slots_alloc, PMIX_UINT32);

    /* job size */
    PMIX_INFO_LIST_ADD(ret, info, PMIX_JOB_SIZE, &jdata->num_procs, PMIX_UINT32);

    /* number of apps in this job */
    PMIX_INFO_LIST_ADD(ret, info, PMIX_JOB_NUM_APPS, &jdata->num_apps, PMIX_UINT32);

    /* max procs */
    PMIX_INFO_LIST_ADD(ret, info, PMIX_MAX_PROCS, &jdata->total_slots_alloc, PMIX_UINT32);

    /* total available physical memory */
    machine = hwloc_get_next_obj_by_type(prte_hwloc_topology, HWLOC_OBJ_MACHINE, NULL);
    if (NULL != machine) {
#if HWLOC_API_VERSION < 0x20000
        PMIX_INFO_LIST_ADD(ret, info, PMIX_AVAIL_PHYS_MEMORY, &machine->memory.total_memory, PMIX_UINT64);
#else
        PMIX_INFO_LIST_ADD(ret, info, PMIX_AVAIL_PHYS_MEMORY, &machine->total_memory, PMIX_UINT64);
#endif
    }

    /* pass the mapping policy used for this job */
    PMIX_INFO_LIST_ADD(ret, info, PMIX_MAPBY, prte_rmaps_base_print_mapping(jdata->map->mapping), PMIX_STRING);

    /* pass the ranking policy used for this job */
    PMIX_INFO_LIST_ADD(ret, info, PMIX_RANKBY, prte_rmaps_base_print_ranking(jdata->map->ranking), PMIX_STRING);

    /* pass the binding policy used for this job */
    PMIX_INFO_LIST_ADD(ret, info, PMIX_BINDTO, prte_hwloc_base_print_binding(jdata->map->binding), PMIX_STRING);

    /* tell the user what we did with FQDN */
    PMIX_INFO_LIST_ADD(ret, info, PMIX_HOSTNAME_KEEP_FQDN, &prte_keep_fqdn_hostnames, PMIX_BOOL);

    /* pass the top-level session directory */
    PMIX_INFO_LIST_ADD(ret, info, PMIX_TMPDIR, prte_process_info.top_session_dir, PMIX_STRING);

    /* create and pass a job-level session directory */
    pproc.rank = PMIX_RANK_INVALID;
    rc = prte_session_dir(&pproc);
    if (PRTE_SUCCESS != rc) {
        PRTE_ERROR_LOG(rc);
        PMIX_INFO_LIST_RELEASE(info);
        rc = prte_pmix_convert_status(rc);
        return rc;
    }
    // job session dir will have been stored in the jdata object
    PMIX_INFO_LIST_ADD(ret, info, PMIX_NSDIR, jdata->session_dir, PMIX_STRING);

    /* check for output directives */
    fptr = &flag;
    if (prte_get_attribute(&jdata->attributes, PRTE_JOB_TAG_OUTPUT, (void**)&fptr, PMIX_BOOL)) {
        PMIX_INFO_LIST_ADD(ret, info, PMIX_IOF_TAG_OUTPUT, &flag, PMIX_BOOL);
    }
    if (prte_get_attribute(&jdata->attributes, PRTE_JOB_TAG_OUTPUT_DETAILED, (void**)&fptr, PMIX_BOOL)) {
        PMIX_INFO_LIST_ADD(ret, info, PMIX_IOF_TAG_DETAILED_OUTPUT, &flag, PMIX_BOOL);
    }
    if (prte_get_attribute(&jdata->attributes, PRTE_JOB_TAG_OUTPUT_FULLNAME, (void**)&fptr, PMIX_BOOL)) {
        PMIX_INFO_LIST_ADD(ret, info, PMIX_IOF_TAG_FULLNAME_OUTPUT, &flag, PMIX_BOOL);
    }
    if (prte_get_attribute(&jdata->attributes, PRTE_JOB_RANK_OUTPUT, (void**)&fptr, PMIX_BOOL)) {
        PMIX_INFO_LIST_ADD(ret, info, PMIX_IOF_RANK_OUTPUT, &flag, PMIX_BOOL);
    }
    if (prte_get_attribute(&jdata->attributes, PRTE_JOB_TIMESTAMP_OUTPUT, (void**)&fptr, PMIX_BOOL)) {
        PMIX_INFO_LIST_ADD(ret, info, PMIX_IOF_TIMESTAMP_OUTPUT, &flag, PMIX_BOOL);
    }
    if (prte_get_attribute(&jdata->attributes, PRTE_JOB_XML_OUTPUT, (void**)&fptr, PMIX_BOOL)) {
        PMIX_INFO_LIST_ADD(ret, info, PMIX_IOF_XML_OUTPUT, &flag, PMIX_BOOL);
    }
    tmp = NULL;
    if (prte_get_attribute(&jdata->attributes, PRTE_JOB_OUTPUT_TO_FILE, (void **) &tmp, PMIX_STRING)
        && NULL != tmp) {
        PMIX_INFO_LIST_ADD(ret, info, PMIX_OUTPUT_TO_FILE, tmp, PMIX_STRING);
        free(tmp);
    }
    tmp = NULL;
    if (prte_get_attribute(&jdata->attributes, PRTE_JOB_OUTPUT_TO_DIRECTORY, (void **) &tmp, PMIX_STRING)
        && NULL != tmp) {
        PMIX_INFO_LIST_ADD(ret, info, PMIX_OUTPUT_TO_DIRECTORY, tmp, PMIX_STRING);
        free(tmp);
    }
    if (prte_get_attribute(&jdata->attributes, PRTE_JOB_OUTPUT_NOCOPY, (void**)&fptr, PMIX_BOOL)) {
        PMIX_INFO_LIST_ADD(ret, info, PMIX_OUTPUT_NOCOPY, &flag, PMIX_BOOL);
    }
    if (prte_get_attribute(&jdata->attributes, PRTE_JOB_MERGE_STDERR_STDOUT, (void**)&fptr, PMIX_BOOL)) {
        PMIX_INFO_LIST_ADD(ret, info, PMIX_MERGE_STDERR_STDOUT, &flag, PMIX_BOOL);
    }
#ifdef PMIX_IOF_OUTPUT_RAW
    if (prte_get_attribute(&jdata->attributes, PRTE_JOB_RAW_OUTPUT, (void**)&fptr, PMIX_BOOL)) {
        PMIX_INFO_LIST_ADD(ret, info, PMIX_IOF_OUTPUT_RAW, &flag, PMIX_BOOL);
    }
#endif

    /* for each app in the job, create an app-array */
    for (n = 0; n < jdata->apps->size; n++) {
        if (NULL == (app = (prte_app_context_t *) pmix_pointer_array_get_item(jdata->apps, n))) {
            continue;
        }
        PMIX_INFO_LIST_START(iarray);
        /* start with the app number */
        PMIX_INFO_LIST_ADD(ret, iarray, PMIX_APPNUM, &app->idx, PMIX_UINT32);
        /* add the app size */
        PMIX_INFO_LIST_ADD(ret, iarray, PMIX_APP_SIZE, &app->num_procs, PMIX_UINT32);
        /* add the app leader */
        PMIX_INFO_LIST_ADD(ret, iarray, PMIX_APPLDR, &app->first_rank, PMIX_PROC_RANK);
        /* add the wdir */
        PMIX_INFO_LIST_ADD(ret, iarray, PMIX_WDIR, app->cwd, PMIX_STRING);
        /* add the argv */
        tmp = PMIX_ARGV_JOIN_COMPAT(app->argv, ' ');
        PMIX_INFO_LIST_ADD(ret, iarray, PMIX_APP_ARGV, tmp, PMIX_STRING);
        free(tmp);
        /* add the pset name */
        tmp = NULL;
        if (prte_get_attribute(&app->attributes, PRTE_APP_PSET_NAME, (void **) &tmp, PMIX_STRING)
            && NULL != tmp) {
            PMIX_INFO_LIST_ADD(ret, iarray, PMIX_PSET_NAME, tmp, PMIX_STRING);
            /* register it */
            pset = PMIX_NEW(pmix_server_pset_t);
            pset->name = strdup(tmp);
            pmix_list_append(&prte_pmix_server_globals.psets, &pset->super);
            free(tmp);
            /* and its membership */
            PMIX_CONSTRUCT(&members, pmix_list_t);
            for (k = 0; k < jdata->procs->size; k++) {
                pptr = (prte_proc_t*)pmix_pointer_array_get_item(jdata->procs, k);
                if (NULL == pptr) {
                    continue;
                }
                if (app->idx == pptr->app_idx) {
                    nm = PMIX_NEW(prte_namelist_t);
                    PMIX_LOAD_PROCID(&nm->name, pptr->name.nspace, pptr->name.rank);
                    pmix_list_append(&members, &nm->super);
                }
            }
            pset->num_members = pmix_list_get_size(&members);
            if (0 < (i = pmix_list_get_size(&members))) {
                PMIX_DATA_ARRAY_CONSTRUCT(&darray, i, PMIX_PROC);
                procptr = (pmix_proc_t*)darray.array;
                k = 0;
                pset->members = (pmix_proc_t *)malloc(i * sizeof (pmix_proc_t));
                PMIX_LIST_FOREACH(nm, &members, prte_namelist_t) {
                    PMIX_LOAD_PROCID(&procptr[k], nm->name.nspace, nm->name.rank);
                    PMIX_LOAD_PROCID(&pset->members[k], nm->name.nspace, nm->name.rank);
                    ++k;
                }
                PMIX_INFO_LIST_ADD(ret, iarray, PMIX_PSET_MEMBERS, &darray, PMIX_DATA_ARRAY);
                PMIX_DATA_ARRAY_DESTRUCT(&darray);
            }
            PMIX_LIST_DESTRUCT(&members);
        }
        /* add to the main payload */
        PMIX_INFO_LIST_CONVERT(ret, iarray, &darray);
        PMIX_INFO_LIST_ADD(ret, info, PMIX_APP_INFO_ARRAY, &darray, PMIX_DATA_ARRAY);
        PMIX_DATA_ARRAY_DESTRUCT(&darray);
        PMIX_INFO_LIST_RELEASE(iarray);
    }

    /* get the parent job that spawned this one */
    if (prte_get_attribute(&jdata->attributes, PRTE_JOB_LAUNCH_PROXY, (void **) &parentproc, PMIX_PROC)) {
        parent = prte_get_job_data_object(parentproc->nspace);
        if (NULL != parent && PMIX_CHECK_NSPACE(PRTE_PROC_MY_NAME->nspace, parent->nspace)) {
            PMIX_PROC_RELEASE(parentproc);
            parent = NULL;
        }
    }

    /* for each proc in this job, create an object that
     * includes the info describing the proc so the recipient has a complete
     * picture. This allows procs to connect to each other without
     * any further info exchange, assuming the underlying transports
     * support it. We also pass all the proc-specific data here so
     * that each proc can lookup info about every other proc in the job */
    if (0 != prte_pmix_server_globals.generate_dist) {
        PMIX_INFO_LOAD(&devinfo[0], PMIX_DEVICE_TYPE, &prte_pmix_server_globals.generate_dist, PMIX_DEVTYPE);
        PMIX_INFO_LOAD(&devinfo[1], PMIX_HOSTNAME, NULL, PMIX_STRING);
    }

    for (n = 0; n < map->nodes->size; n++) {
        if (NULL == (node = (prte_node_t *) pmix_pointer_array_get_item(map->nodes, n))) {
            continue;
        }
        /* cycle across each proc on this node, passing all data that
         * varies by proc */
        for (i = 0; i < node->procs->size; i++) {
            if (NULL == (pptr = (prte_proc_t *) pmix_pointer_array_get_item(node->procs, i))) {
                continue;
            }
            /* only consider procs from this job */
            if (!PMIX_CHECK_NSPACE(pptr->name.nspace, jdata->nspace)) {
                continue;
            }
            /* setup the proc map object */
            PMIX_INFO_LIST_START(pmap);

            /* must start with rank */
            PMIX_INFO_LIST_ADD(ret, pmap, PMIX_RANK, &pptr->name.rank, PMIX_PROC_RANK);

            /* location, for local procs */
            if (NULL != pptr->cpuset) {
                /* provide the cpuset string for this proc */
                PMIX_INFO_LIST_ADD(ret, pmap, PMIX_CPUSET, pptr->cpuset, PMIX_STRING);
                /* let PMIx generate the locality string */
                PMIX_CPUSET_CONSTRUCT(&cpuset);
                cpuset.source = "hwloc";
                cpuset.bitmap = hwloc_bitmap_alloc();
                hwloc_bitmap_list_sscanf(cpuset.bitmap, pptr->cpuset);
                ret = PMIx_server_generate_locality_string(&cpuset, &tmp);
                if (PMIX_SUCCESS != ret) {
                    PMIX_ERROR_LOG(ret);
                    hwloc_bitmap_free(cpuset.bitmap);
                    PMIX_INFO_LIST_RELEASE(info);
                    PMIX_INFO_LIST_RELEASE(pmap);
                    return prte_pmix_convert_status(ret);
                }
                PMIX_INFO_LIST_ADD(ret, pmap, PMIX_LOCALITY_STRING, tmp, PMIX_STRING);
                free(tmp);
                if (0 != prte_pmix_server_globals.generate_dist) {
                    /* compute the device distances for this proc */
                    topo.topology = node->topology->topo;
                    devinfo[1].value.data.string = node->name;
                    ret = PMIx_Compute_distances(&topo, &cpuset,
                                                 devinfo, 2, &distances, &ndist);
                    devinfo[1].value.data.string = NULL;
                    if (PMIX_SUCCESS == ret) {
                        if (4 < pmix_output_get_verbosity(prte_pmix_server_globals.output)) {
                            size_t f;
                            for (f=0; f < ndist; f++) {
                                pmix_output(0, "UUID: %s OSNAME: %s TYPE: %s MIND: %u MAXD: %u",
                                            distances[f].uuid, distances[f].osname,
                                            PMIx_Device_type_string(distances[f].type),
                                            distances[f].mindist, distances[f].maxdist);
                            }
                        }
                        darray.type = PMIX_DEVICE_DIST;
                        darray.array = distances;
                        darray.size = ndist;
                        PMIX_INFO_LIST_ADD(ret, pmap, PMIX_DEVICE_DISTANCES, &darray, PMIX_DATA_ARRAY);
                        PMIX_DEVICE_DIST_FREE(distances, ndist);
                    }
                }
                hwloc_bitmap_free(cpuset.bitmap);
            } else {
                /* the proc is not bound */
                PMIX_INFO_LIST_ADD(ret, pmap, PMIX_LOCALITY_STRING, NULL, PMIX_STRING);
            }
            if (PRTE_PROC_MY_NAME->rank == node->daemon->name.rank) {
                /* create and pass a proc-level session directory */
                rc = prte_session_dir(&pptr->name);
                if (PRTE_SUCCESS != rc) {
                    PMIX_ERROR_LOG(rc);
                    PMIX_INFO_LIST_RELEASE(info);
                    PMIX_INFO_LIST_RELEASE(pmap);
                    rc = prte_pmix_convert_status(rc);
                    return rc;
                }
                pmix_asprintf(&tmp, "%s/%s", jdata->session_dir,
                                          PMIX_RANK_PRINT(pptr->name.rank));
                PMIX_INFO_LIST_ADD(ret, pmap, PMIX_PROCDIR, tmp, PMIX_STRING);
                free(tmp);
            }

            /* global/univ rank */
            vpid = pptr->name.rank + jdata->offset;
            PMIX_INFO_LIST_ADD(ret, pmap, PMIX_GLOBAL_RANK, &vpid, PMIX_PROC_RANK);

            /* parent ID, if we were spawned by a non-tool */
            if (NULL != parent) {
                PMIX_INFO_LIST_ADD(ret, pmap, PMIX_PARENT_ID, parentproc, PMIX_PROC);
            }

            /* appnum */
            PMIX_INFO_LIST_ADD(ret, pmap, PMIX_APPNUM, &pptr->app_idx, PMIX_UINT32);

            /* app rank */
            PMIX_INFO_LIST_ADD(ret, pmap, PMIX_APP_RANK, &pptr->app_rank, PMIX_PROC_RANK);

            /* local rank */
            if (PRTE_LOCAL_RANK_INVALID != pptr->local_rank) {
                PMIX_INFO_LIST_ADD(ret, pmap, PMIX_LOCAL_RANK, &pptr->local_rank, PMIX_UINT16);
            }

            /* node rank */
            if (PRTE_NODE_RANK_INVALID != pptr->node_rank) {
                PMIX_INFO_LIST_ADD(ret, pmap, PMIX_NODE_RANK, &pptr->node_rank, PMIX_UINT16);
            }

            /* node ID */
            PMIX_INFO_LIST_ADD(ret, pmap, PMIX_NODEID, &pptr->node->index, PMIX_UINT32);

            /* reincarnation number */
            ui32 = 0; // we are starting this proc for the first time
            PMIX_INFO_LIST_ADD(ret, pmap, PMIX_REINCARNATION, &ui32, PMIX_UINT32);

            if (map->num_nodes < prte_hostname_cutoff) {
                PMIX_INFO_LIST_ADD(ret, pmap, PMIX_HOSTNAME, pptr->node->name, PMIX_STRING);
            }
            PMIX_INFO_LIST_CONVERT(ret, pmap, &darray);
            PMIX_INFO_LIST_ADD(ret, info, PMIX_PROC_DATA, &darray, PMIX_DATA_ARRAY);
            PMIX_DATA_ARRAY_DESTRUCT(&darray);
            PMIX_INFO_LIST_RELEASE(pmap);
        }
    }
    if (NULL != parent) {
        PMIX_PROC_RELEASE(parentproc);
    }
    if (0 != prte_pmix_server_globals.generate_dist) {
        PMIX_INFO_DESTRUCT(&devinfo[0]);
        PMIX_INFO_DESTRUCT(&devinfo[1]);
    }

    /* mark the job as registered */
    prte_set_attribute(&jdata->attributes, PRTE_JOB_NSPACE_REGISTERED, PRTE_ATTR_LOCAL, NULL,
                       PMIX_BOOL);

    /* add the local procs, if they are defined */
    if (0 < (nmsize = pmix_list_get_size(&local_procs))) {
        pmix_proc_t *procs_tmp;
        PMIX_DATA_ARRAY_CONSTRUCT(&lparray, nmsize, PMIX_PROC);
        procs_tmp = (pmix_proc_t *) lparray.array;
        n = 0;
        PMIX_LIST_FOREACH(nm, &local_procs, prte_namelist_t)
        {
            PMIX_LOAD_PROCID(&procs_tmp[n], nm->name.nspace, nm->name.rank);
            ++n;
        }
        PMIX_INFO_LIST_ADD(ret, info, PMIX_LOCAL_PROCS, &lparray, PMIX_DATA_ARRAY);
        PMIX_DATA_ARRAY_DESTRUCT(&lparray);
    }
    PMIX_LIST_DESTRUCT(&local_procs);

    /* register it */
    PMIX_INFO_LIST_CONVERT(ret, info, &darray);
    pinfo = (pmix_info_t*)darray.array;
    ninfo = darray.size;
    PMIX_INFO_LIST_RELEASE(info);
    PRTE_PMIX_CONSTRUCT_LOCK(&lock);
    ret = PMIx_server_register_nspace(pproc.nspace, jdata->num_local_procs, pinfo, ninfo, opcbfunc,
                                      &lock);
    if (PMIX_SUCCESS != ret) {
        PMIX_ERROR_LOG(ret);
        rc = prte_pmix_convert_status(ret);
        PMIX_INFO_FREE(pinfo, ninfo);
        PRTE_PMIX_DESTRUCT_LOCK(&lock);
        return rc;
    }
    PRTE_PMIX_WAIT_THREAD(&lock);
    rc = lock.status;
    PRTE_PMIX_DESTRUCT_LOCK(&lock);
    if (PRTE_SUCCESS != rc) {
        PMIX_INFO_FREE(pinfo, ninfo);
        return rc;
    }

    /* if the user has connected us to an external server, then we must
     * assume there is going to be some cross-mpirun exchange, and so
     * we protect against that situation by publishing the job info
     * for this job - this allows any subsequent "connect" to retrieve
     * the job info */
    if (NULL != prte_data_server_uri) {
        pmix_data_buffer_t pbkt;
        pmix_byte_object_t pbo;
        uid_t euid;
        pmix_data_range_t range = PMIX_RANGE_SESSION;
        pmix_persistence_t persist = PMIX_PERSIST_APP;

        PMIX_DATA_BUFFER_CONSTRUCT(&pbkt);
        ret = PMIx_Data_pack(NULL, &pbkt, &ninfo, 1, PMIX_SIZE);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            rc = prte_pmix_convert_status(ret);
            PMIX_INFO_FREE(pinfo, ninfo);
            return rc;
        }
        ret = PMIx_Data_pack(NULL, &pbkt, pinfo, ninfo, PMIX_INFO);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            rc = prte_pmix_convert_status(ret);
            PMIX_INFO_FREE(pinfo, ninfo);
            PMIX_DATA_BUFFER_DESTRUCT(&pbkt);
            return rc;
        }
        PMIX_INFO_FREE(pinfo, ninfo);
        ret = PMIx_Data_unload(&pbkt, &pbo);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            rc = prte_pmix_convert_status(ret);
            PMIX_DATA_BUFFER_DESTRUCT(&pbkt);
            return rc;
        }

        ninfo = 4;
        PMIX_INFO_CREATE(pinfo, ninfo);

        /* first pass the packed values with a key of the nspace */
        n = 0;
        PMIX_INFO_LOAD(&pinfo[n], prte_process_info.myproc.nspace, &pbo, PMIX_BYTE_OBJECT);
        PMIX_BYTE_OBJECT_DESTRUCT(&pbo);
        ++n;

        /* set the range to be session */
        PMIX_INFO_LOAD(&pinfo[n], PMIX_RANGE, &range, PMIX_DATA_RANGE);
        ++n;

        /* set the persistence to be app */
        PMIX_INFO_LOAD(&pinfo[n], PMIX_PERSISTENCE, &persist, PMIX_PERSIST);
        ++n;

        /* add our effective userid to the directives */
        euid = geteuid();
        PMIX_INFO_LOAD(&pinfo[n], PMIX_USERID, &euid, PMIX_UINT32);
        ++n;

        /* now publish it */
        PRTE_PMIX_CONSTRUCT_LOCK(&lock);
        if (PMIX_SUCCESS
            != (ret = pmix_server_publish_fn(&prte_process_info.myproc, pinfo, ninfo, opcbfunc,
                                             &lock))) {
            PMIX_ERROR_LOG(ret);
            rc = prte_pmix_convert_status(ret);
            PMIX_INFO_FREE(pinfo, ninfo);
            PMIX_LIST_RELEASE(info);
            PRTE_PMIX_DESTRUCT_LOCK(&lock);
            return rc;
        }
        PRTE_PMIX_WAIT_THREAD(&lock);
        rc = lock.status;
        PRTE_PMIX_DESTRUCT_LOCK(&lock);
    }
    PMIX_INFO_FREE(pinfo, ninfo);

    return rc;
}

static void opcbfunc(pmix_status_t status, void *cbdata)
{
    prte_pmix_lock_t *lock = (prte_pmix_lock_t *) cbdata;

    lock->status = prte_pmix_convert_status(status);
    PRTE_PMIX_WAKEUP_THREAD(lock);
}

/* add any info that the tool couldn't self-assign */
int prte_pmix_server_register_tool(pmix_nspace_t nspace)
{
    pmix_status_t ret;
    prte_pmix_lock_t lock;
    int rc;
    prte_pmix_tool_t *tl;

    /* record this tool */
    tl = PMIX_NEW(prte_pmix_tool_t);
    PMIX_LOAD_PROCID(&tl->name, nspace, 0);
    pmix_list_append(&prte_pmix_server_globals.tools, &tl->super);

    PRTE_PMIX_CONSTRUCT_LOCK(&lock);
    ret = PMIx_server_register_nspace(nspace, 1, NULL, 0,
                                      opcbfunc, &lock);
    if (PMIX_SUCCESS != ret) {
        PMIX_ERROR_LOG(ret);
        rc = prte_pmix_convert_status(ret);
        PRTE_PMIX_DESTRUCT_LOCK(&lock);
        return rc;
    }
    PRTE_PMIX_WAIT_THREAD(&lock);
    rc = lock.status;
    PRTE_PMIX_DESTRUCT_LOCK(&lock);
    return rc;
}
