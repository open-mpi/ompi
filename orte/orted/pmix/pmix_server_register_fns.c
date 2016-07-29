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
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2013-2016 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2014-2016 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include "orte_config.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <fcntl.h>

#include "opal_stdint.h"
#include "opal/types.h"
#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "opal/util/error.h"
#include "opal/mca/pmix/pmix.h"

#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/orte_wait.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rmaps/rmaps_types.h"

#include "pmix_server_internal.h"
#include "pmix_server.h"

/* stuff proc attributes for sending back to a proc */
int orte_pmix_server_register_nspace(orte_job_t *jdata)
{
    int rc;
    orte_proc_t *pptr;
    int i, k;
    opal_list_t *info, *pmap;
    opal_value_t *kv;
    orte_node_t *node, *n2;
    opal_vpid_t vpid;
    char **list, **procs, **micro, *tmp, *regex;
    orte_job_t *dmns;
    orte_job_map_t *map;
    orte_app_context_t *app;
    uid_t uid;
    gid_t gid;

    opal_output_verbose(2, orte_pmix_server_globals.output,
                        "%s register nspace for %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_JOBID_PRINT(jdata->jobid));

    /* setup the info list */
    info = OBJ_NEW(opal_list_t);
    uid = geteuid();
    gid = getegid();

    /* jobid */
    kv = OBJ_NEW(opal_value_t);
    kv->key = strdup(OPAL_PMIX_JOBID);
    kv->data.string = strdup(ORTE_JOBID_PRINT(jdata->jobid));
    kv->type = OPAL_STRING;
    opal_list_append(info, &kv->super);

    /* offset */
    kv = OBJ_NEW(opal_value_t);
    kv->key = strdup(OPAL_PMIX_NPROC_OFFSET);
    kv->data.uint32 = jdata->offset;
    kv->type = OPAL_UINT32;
    opal_list_append(info, &kv->super);

    /* assemble the node and proc map info */
    list = NULL;
    procs = NULL;
    map = jdata->map;
    for (i=0; i < map->nodes->size; i++) {
        micro = NULL;
        if (NULL != (node = (orte_node_t*)opal_pointer_array_get_item(jdata->map->nodes, i))) {
            opal_argv_append_nosize(&list, node->name);
            /* assemble all the ranks for this job that are on this node */
            for (k=0; k < node->procs->size; k++) {
                if (NULL != (pptr = (orte_proc_t*)opal_pointer_array_get_item(node->procs, k))) {
                    if (jdata->jobid == pptr->name.jobid) {
                        opal_argv_append_nosize(&micro, ORTE_VPID_PRINT(pptr->name.vpid));
                    }
                }
            }
            /* assemble the rank/node map */
            if (NULL != micro) {
                tmp = opal_argv_join(micro, ',');
                opal_argv_free(micro);
                opal_argv_append_nosize(&procs, tmp);
                free(tmp);
            }
        }
    }
    /* let the PMIx server generate the nodemap regex */
    if (NULL != list) {
        tmp = opal_argv_join(list, ',');
        opal_argv_free(list);
        list = NULL;
        if (OPAL_SUCCESS != (rc = opal_pmix.generate_regex(tmp, &regex))) {
            ORTE_ERROR_LOG(rc);
            free(tmp);
            OPAL_LIST_RELEASE(info);
            return rc;
        }
        free(tmp);
        kv = OBJ_NEW(opal_value_t);
        kv->key = strdup(OPAL_PMIX_NODE_MAP);
        kv->type = OPAL_STRING;
        kv->data.string = regex;
        opal_list_append(info, &kv->super);
    }

    /* let the PMIx server generate the procmap regex */
    if (NULL != procs) {
        tmp = opal_argv_join(procs, ';');
        opal_argv_free(procs);
        procs = NULL;
        if (OPAL_SUCCESS != (rc = opal_pmix.generate_ppn(tmp, &regex))) {
            ORTE_ERROR_LOG(rc);
            free(tmp);
            OPAL_LIST_RELEASE(info);
            return rc;
        }
        free(tmp);
        kv = OBJ_NEW(opal_value_t);
        kv->key = strdup(OPAL_PMIX_PROC_MAP);
        kv->type = OPAL_STRING;
        kv->data.string = regex;
        opal_list_append(info, &kv->super);
    }

    /* get our local node */
    if (NULL == (dmns = orte_get_job_data_object(ORTE_PROC_MY_NAME->jobid))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        OPAL_LIST_RELEASE(info);
        return ORTE_ERR_NOT_FOUND;
    }
    if (NULL == (pptr = (orte_proc_t*)opal_pointer_array_get_item(dmns->procs, ORTE_PROC_MY_NAME->vpid))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        OPAL_LIST_RELEASE(info);
        return ORTE_ERR_NOT_FOUND;
    }
    node = pptr->node;
    if (NULL == node) {
        /* cannot happen */
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        OPAL_LIST_RELEASE(info);
        return ORTE_ERR_NOT_FOUND;
    }
    /* pass our node ID */
    kv = OBJ_NEW(opal_value_t);
    kv->key = strdup(OPAL_PMIX_NODEID);
    kv->type = OPAL_UINT32;
    kv->data.uint32 = node->index;
    opal_list_append(info, &kv->super);

    /* identify our local node object within the map,
     * if we were included */
    node = NULL;
    map = (orte_job_map_t*)jdata->map;
    for (i=0; i < map->nodes->size; i++) {
        if (NULL == (n2 = (orte_node_t*)opal_pointer_array_get_item(map->nodes, i))) {
            continue;
        }
        if (n2 == pptr->node) {
            node = n2;
            break;
        }
    }
    if (NULL != node) {
        /* node size */
        kv = OBJ_NEW(opal_value_t);
        kv->key = strdup(OPAL_PMIX_NODE_SIZE);
        kv->type = OPAL_UINT32;
        kv->data.uint32 = node->num_procs;
        opal_list_append(info, &kv->super);
        /* construct the list of local peers, while adding
         * each proc's locality info */
        list = NULL;
        procs = NULL;
        vpid = ORTE_VPID_MAX;
        for (i=0; i < node->procs->size; i++) {
            if (NULL == (pptr = (orte_proc_t*)opal_pointer_array_get_item(node->procs, i))) {
                continue;
            }
            if (pptr->name.jobid == jdata->jobid) {
                opal_argv_append_nosize(&list, ORTE_VPID_PRINT(pptr->name.vpid));
                if (pptr->name.vpid < vpid) {
                    vpid = pptr->name.vpid;
                }
                /* note that we have to pass the cpuset for each local
                 * peer so locality can be computed */
                tmp = NULL;
                if (orte_get_attribute(&pptr->attributes, ORTE_PROC_CPU_BITMAP, (void**)&tmp, OPAL_STRING)) {
                    if (NULL != tmp) {
                        opal_argv_append_nosize(&procs, tmp);
                        free(tmp);
                    } else {
                        opal_argv_append_nosize(&procs, "UNBOUND");
                    }
                } else {
                    opal_argv_append_nosize(&procs, "UNBOUND");
                }
                /* go ahead and register this client */
                if (OPAL_SUCCESS != (rc = opal_pmix.server_register_client(&pptr->name, uid, gid,
                                                                           (void*)pptr, NULL, NULL))) {
                    ORTE_ERROR_LOG(rc);
                }
            }
        }

        /* construct the list of peers for transmission */
        if (NULL != list) {
            tmp = opal_argv_join(list, ',');
            opal_argv_free(list);
            list = NULL;
            /* pass the list of peers */
            kv = OBJ_NEW(opal_value_t);
            kv->key = strdup(OPAL_PMIX_LOCAL_PEERS);
            kv->type = OPAL_STRING;
            kv->data.string = tmp;
            opal_list_append(info, &kv->super);
        }
        /* construct the list of cpusets for transmission */
        if (NULL != procs) {
            tmp = opal_argv_join(procs, ':');
            opal_argv_free(procs);
            procs = NULL;
            /* pass the list of cpusets */
            kv = OBJ_NEW(opal_value_t);
            kv->key = strdup(OPAL_PMIX_LOCAL_CPUSETS);
            kv->type = OPAL_STRING;
            kv->data.string = tmp;
            opal_list_append(info, &kv->super);
        }
        /* pass the local ldr */
        kv = OBJ_NEW(opal_value_t);
        kv->key = strdup(OPAL_PMIX_LOCALLDR);
        kv->type = OPAL_UINT32;
        kv->data.uint32 = vpid;
        opal_list_append(info, &kv->super);
    }

    /* univ size */
    kv = OBJ_NEW(opal_value_t);
    kv->key = strdup(OPAL_PMIX_UNIV_SIZE);
    kv->type = OPAL_UINT32;
    kv->data.uint32 = jdata->total_slots_alloc;
    opal_list_append(info, &kv->super);

    /* job size */
    kv = OBJ_NEW(opal_value_t);
    kv->key = strdup(OPAL_PMIX_JOB_SIZE);
    kv->type = OPAL_UINT32;
    kv->data.uint32 = jdata->num_procs;
    opal_list_append(info, &kv->super);

    /* number of apps in this job */
    kv = OBJ_NEW(opal_value_t);
    kv->key = strdup(OPAL_PMIX_JOB_NUM_APPS);
    kv->type = OPAL_UINT32;
    kv->data.uint32 = jdata->num_apps;
    opal_list_append(info, &kv->super);

    /* local size */
    kv = OBJ_NEW(opal_value_t);
    kv->key = strdup(OPAL_PMIX_LOCAL_SIZE);
    kv->type = OPAL_UINT32;
    kv->data.uint32 = jdata->num_local_procs;
    opal_list_append(info, &kv->super);

    /* max procs */
    kv = OBJ_NEW(opal_value_t);
    kv->key = strdup(OPAL_PMIX_MAX_PROCS);
    kv->type = OPAL_UINT32;
    kv->data.uint32 = jdata->total_slots_alloc;
    opal_list_append(info, &kv->super);

    /* for each proc in this job, create an object that
     * includes the info describing the proc so the recipient has a complete
     * picture. This allows procs to connect to each other without
     * an further info exchange, assuming the underlying transports
     * support it */

    for (i=0; i < jdata->procs->size; i++) {
        if (NULL == (pptr = (orte_proc_t*)opal_pointer_array_get_item(jdata->procs, i))) {
            continue;
        }
        kv = OBJ_NEW(opal_value_t);
        kv->key = strdup(OPAL_PMIX_PROC_DATA);
        kv->type = OPAL_PTR;
        kv->data.ptr = OBJ_NEW(opal_list_t);
        opal_list_append(info, &kv->super);
        pmap = kv->data.ptr;

        /* rank */
        kv = OBJ_NEW(opal_value_t);
        kv->key = strdup(OPAL_PMIX_RANK);
        kv->type = OPAL_INT;
        kv->data.integer = pptr->name.vpid;
        opal_list_append(pmap, &kv->super);

        /* appnum */
        kv = OBJ_NEW(opal_value_t);
        kv->key = strdup(OPAL_PMIX_APPNUM);
        kv->type = OPAL_UINT32;
        kv->data.uint32 = pptr->app_idx;
        opal_list_append(pmap, &kv->super);

        /* app ldr */
        app = (orte_app_context_t*)opal_pointer_array_get_item(jdata->apps, pptr->app_idx);
        kv = OBJ_NEW(opal_value_t);
        kv->key = strdup(OPAL_PMIX_APPLDR);
        kv->type = OPAL_UINT32;
        kv->data.uint32 = app->first_rank;
        opal_list_append(pmap, &kv->super);

        /* global/univ rank */
        kv = OBJ_NEW(opal_value_t);
        kv->key = strdup(OPAL_PMIX_GLOBAL_RANK);
        kv->type = OPAL_UINT32;
        kv->data.uint32 = pptr->name.vpid + jdata->offset;
        opal_list_append(pmap, &kv->super);

        /* app rank */
        kv = OBJ_NEW(opal_value_t);
        kv->key = strdup(OPAL_PMIX_APP_RANK);
        kv->type = OPAL_UINT32;
        kv->data.uint32 = pptr->app_rank;
        opal_list_append(pmap, &kv->super);

        /* app size */
        kv = OBJ_NEW(opal_value_t);
        kv->key = strdup(OPAL_PMIX_APP_SIZE);
        kv->type = OPAL_UINT32;
        kv->data.uint32 = app->num_procs;
        opal_list_append(info, &kv->super);

        /* local rank */
        kv = OBJ_NEW(opal_value_t);
        kv->key = strdup(OPAL_PMIX_LOCAL_RANK);
        kv->type = OPAL_UINT16;
        kv->data.uint16 = pptr->local_rank;
        opal_list_append(pmap, &kv->super);

        /* node rank */
        kv = OBJ_NEW(opal_value_t);
        kv->key = strdup(OPAL_PMIX_NODE_RANK);
        kv->type = OPAL_UINT16;
        kv->data.uint32 = pptr->node_rank;
        opal_list_append(pmap, &kv->super);

        /* hostname */
        kv = OBJ_NEW(opal_value_t);
        kv->key = strdup(OPAL_PMIX_HOSTNAME);
        kv->type = OPAL_STRING;
        kv->data.string = strdup(pptr->node->name);
        opal_list_append(pmap, &kv->super);

        /* node ID */
        kv = OBJ_NEW(opal_value_t);
        kv->key = strdup(OPAL_PMIX_NODEID);
        kv->type = OPAL_UINT32;
        kv->data.uint32 = pptr->node->index;
        opal_list_append(pmap, &kv->super);
    }

    /* mark the job as registered */
    orte_set_attribute(&jdata->attributes, ORTE_JOB_NSPACE_REGISTERED, ORTE_ATTR_LOCAL, NULL, OPAL_BOOL);

    /* pass it down */
    /* we are in an event, so no need to callback */
    rc = opal_pmix.server_register_nspace(jdata->jobid,
                                          jdata->num_local_procs,
                                          info, NULL, NULL);
    OPAL_LIST_RELEASE(info);

    return rc;
}
