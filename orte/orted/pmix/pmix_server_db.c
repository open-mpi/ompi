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
 * Copyright (c) 2013-2015 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
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
#include "opal/mca/dstore/dstore.h"
#include "opal/mca/event/event.h"

#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/mca/errmgr/errmgr.h"

#include "pmix_server_internal.h"

/* stuff proc attributes for sending back to a proc */
int pmix_server_fetch_proc_map(opal_buffer_t *reply,
                               orte_job_t *jdata,
                               orte_proc_t *proc)
{
    char *tmp;
    opal_value_t kv, *kp;
    int rc;
    orte_node_t *node;
    orte_app_context_t *app;
    orte_proc_t *pptr;
    int i;
    char **list;
    orte_process_name_t name;
    opal_buffer_t buf, buf2;

    /* convenience def */
    node = proc->node;
    app = (orte_app_context_t*)opal_pointer_array_get_item(jdata->apps, proc->app_idx);
    kp = &kv;

#if OPAL_HAVE_HWLOC
    /* pass the local topology for the app so it doesn't
     * have to discover it for itself */
    if (NULL != opal_hwloc_topology) {
        OBJ_CONSTRUCT(&buf, opal_buffer_t);
        if (OPAL_SUCCESS != (rc = opal_dss.pack(&buf, &opal_hwloc_topology, 1, OPAL_HWLOC_TOPO))) {
            ORTE_ERROR_LOG(rc);
            OBJ_DESTRUCT(&buf);
            return rc;
        }
        OBJ_CONSTRUCT(&kv, opal_value_t);
        kv.key = strdup(PMIX_LOCAL_TOPO);
        kv.type = OPAL_BYTE_OBJECT;
        opal_dss.unload(&buf, (void**)&kv.data.bo.bytes, &kv.data.bo.size);
        OBJ_DESTRUCT(&buf);
        if (OPAL_SUCCESS != (rc = opal_dss.pack(reply, &kp, 1, OPAL_VALUE))) {
            ORTE_ERROR_LOG(rc);
            OBJ_DESTRUCT(&kv);
            return rc;
        }
        OBJ_DESTRUCT(&kv);
    }
#endif /* OPAL_HAVE_HWLOC */
    /* cpuset */
    tmp = NULL;
    if (orte_get_attribute(&proc->attributes, ORTE_PROC_CPU_BITMAP, (void**)&tmp, OPAL_STRING)) {
        OBJ_CONSTRUCT(&kv, opal_value_t);
        kv.key = strdup(PMIX_CPUSET);
        kv.type = OPAL_STRING;
        kv.data.string = tmp;
        if (OPAL_SUCCESS != (rc = opal_dss.pack(reply, &kp, 1, OPAL_VALUE))) {
            ORTE_ERROR_LOG(rc);
            OBJ_DESTRUCT(&kv);
            return rc;
        }
        OBJ_DESTRUCT(&kv);
    }
    /* jobid */
    OBJ_CONSTRUCT(&kv, opal_value_t);
    kv.key = strdup(PMIX_JOBID);
    kv.type = OPAL_UINT32;
    kv.data.uint32 = proc->name.jobid;
    if (OPAL_SUCCESS != (rc = opal_dss.pack(reply, &kp, 1, OPAL_VALUE))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&kv);
        return rc;
    }
    OBJ_DESTRUCT(&kv);
    /* rank */
    OBJ_CONSTRUCT(&kv, opal_value_t);
    kv.key = strdup(PMIX_RANK);
    kv.type = OPAL_UINT32;
    kv.data.uint32 = proc->name.vpid;
    if (OPAL_SUCCESS != (rc = opal_dss.pack(reply, &kp, 1, OPAL_VALUE))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&kv);
        return rc;
    }
    /* offset */
    OBJ_CONSTRUCT(&kv, opal_value_t);
    kv.key = strdup(PMIX_NPROC_OFFSET);
    kv.type = OPAL_UINT32;
    kv.data.uint32 = jdata->offset;
    if (OPAL_SUCCESS != (rc = opal_dss.pack(reply, &kp, 1, OPAL_VALUE))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&kv);
        return rc;
    }
    OBJ_DESTRUCT(&kv);
    /* pass a blob - for each proc in this job, include the info describing
     * it so the recipient has a complete picture */
    OBJ_CONSTRUCT(&buf, opal_buffer_t);
    /* jobid, for simplicity when unpacking */
    OBJ_CONSTRUCT(&kv, opal_value_t);
    kv.key = strdup(PMIX_JOBID);
    kv.type = OPAL_UINT32;
    kv.data.uint32 = proc->name.jobid;
    if (OPAL_SUCCESS != (rc = opal_dss.pack(&buf, &kp, 1, OPAL_VALUE))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&kv);
        return rc;
    }
    for (i=0; i < jdata->procs->size; i++) {
        if (NULL == (pptr = (orte_proc_t*)opal_pointer_array_get_item(jdata->procs, i))) {
            continue;
        }
        /* rank */
        OBJ_CONSTRUCT(&kv, opal_value_t);
        kv.key = strdup(PMIX_RANK);
        kv.type = OPAL_UINT32;
        kv.data.uint32 = pptr->name.vpid;
        if (OPAL_SUCCESS != (rc = opal_dss.pack(&buf, &kp, 1, OPAL_VALUE))) {
            ORTE_ERROR_LOG(rc);
            OBJ_DESTRUCT(&kv);
            return rc;
        }
        OBJ_DESTRUCT(&kv);
        /* create the buffer for this rank */
        OBJ_CONSTRUCT(&buf2, opal_buffer_t);
        /* appnum */
        OBJ_CONSTRUCT(&kv, opal_value_t);
        kv.key = strdup(PMIX_APPNUM);
        kv.type = OPAL_UINT32;
        kv.data.uint32 = pptr->app_idx;
        if (OPAL_SUCCESS != (rc = opal_dss.pack(&buf2, &kp, 1, OPAL_VALUE))) {
            ORTE_ERROR_LOG(rc);
            OBJ_DESTRUCT(&kv);
            return rc;
        }
        OBJ_DESTRUCT(&kv);
        /* global rank */
        OBJ_CONSTRUCT(&kv, opal_value_t);
        kv.key = strdup(PMIX_GLOBAL_RANK);
        kv.type = OPAL_UINT32;
        kv.data.uint32 = pptr->name.vpid + jdata->offset;
        if (OPAL_SUCCESS != (rc = opal_dss.pack(&buf2, &kp, 1, OPAL_VALUE))) {
            ORTE_ERROR_LOG(rc);
            OBJ_DESTRUCT(&kv);
            return rc;
        }
        OBJ_DESTRUCT(&kv);
        /* app rank */
        OBJ_CONSTRUCT(&kv, opal_value_t);
        kv.key = strdup(PMIX_APP_RANK);
        kv.type = OPAL_UINT32;
        kv.data.uint32 = pptr->app_rank;
        if (OPAL_SUCCESS != (rc = opal_dss.pack(&buf2, &kp, 1, OPAL_VALUE))) {
            ORTE_ERROR_LOG(rc);
            OBJ_DESTRUCT(&kv);
            return rc;
        }
        OBJ_DESTRUCT(&kv);
        /* local rank */
        OBJ_CONSTRUCT(&kv, opal_value_t);
        kv.key = strdup(PMIX_LOCAL_RANK);
        kv.type = OPAL_UINT16;
        kv.data.uint16 = pptr->local_rank;
        if (OPAL_SUCCESS != (rc = opal_dss.pack(&buf2, &kp, 1, OPAL_VALUE))) {
            ORTE_ERROR_LOG(rc);
            OBJ_DESTRUCT(&kv);
            return rc;
        }
        OBJ_DESTRUCT(&kv);
        /* node rank */
        OBJ_CONSTRUCT(&kv, opal_value_t);
        kv.key = strdup(PMIX_NODE_RANK);
        kv.type = OPAL_UINT16;
        kv.data.uint16 = pptr->node_rank;
        if (OPAL_SUCCESS != (rc = opal_dss.pack(&buf2, &kp, 1, OPAL_VALUE))) {
            ORTE_ERROR_LOG(rc);
            OBJ_DESTRUCT(&kv);
            return rc;
        }
        OBJ_DESTRUCT(&kv);
        /* node id */
        OBJ_CONSTRUCT(&kv, opal_value_t);
        kv.key = strdup(PMIX_NODE_ID);
        kv.type = OPAL_UINT32;
        kv.data.uint32 = pptr->node->daemon->name.vpid;
        if (OPAL_SUCCESS != (rc = opal_dss.pack(&buf2, &kp, 1, OPAL_VALUE))) {
            ORTE_ERROR_LOG(rc);
            OBJ_DESTRUCT(&kv);
            return rc;
        }
        OBJ_DESTRUCT(&kv);
        /* add the rank's blob */
        OBJ_CONSTRUCT(&kv, opal_value_t);
        kv.key = strdup(PMIX_PROC_MAP);
        kv.type = OPAL_BYTE_OBJECT;
        opal_dss.unload(&buf2, (void**)&kv.data.bo.bytes, &kv.data.bo.size);
        OBJ_DESTRUCT(&buf2);
        if (OPAL_SUCCESS != (rc = opal_dss.pack(&buf, &kp, 1, OPAL_VALUE))) {
            ORTE_ERROR_LOG(rc);
            OBJ_DESTRUCT(&kv);
            return rc;
        }
        OBJ_DESTRUCT(&kv);
    }
    /* now pass the blob as the proc-map key */
    OBJ_CONSTRUCT(&kv, opal_value_t);
    kv.key = strdup(PMIX_PROC_MAP);
    kv.type = OPAL_BYTE_OBJECT;
    opal_dss.unload(&buf, (void**)&kv.data.bo.bytes, &kv.data.bo.size);
    OBJ_DESTRUCT(&buf);
    if (OPAL_SUCCESS != (rc = opal_dss.pack(reply, &kp, 1, OPAL_VALUE))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&kv);
        return rc;
    }
    OBJ_DESTRUCT(&kv);
    /* construct the list of local peers */
    list = NULL;
    name.jobid = jdata->jobid;
    name.vpid = 0;
    OBJ_CONSTRUCT(&buf, opal_buffer_t);
    for (i=0; i < node->procs->size; i++) {
        if (NULL == (pptr = (orte_proc_t*)opal_pointer_array_get_item(node->procs, i))) {
            continue;
        }
        if (pptr->name.jobid == jdata->jobid) {
            opal_argv_append_nosize(&list, ORTE_VPID_PRINT(pptr->name.vpid));
            if (pptr->name.vpid < name.vpid) {
                name.vpid = pptr->name.vpid;
            }
            /* note that we have to pass the cpuset for each local
             * peer so locality can be computed */
            tmp = NULL;
            if (orte_get_attribute(&pptr->attributes, ORTE_PROC_CPU_BITMAP, (void**)&tmp, OPAL_STRING)) {
                /* add the name of the proc */
                if (OPAL_SUCCESS != (rc = opal_dss.pack(&buf, &pptr->name, 1, OPAL_NAME))) {
                    ORTE_ERROR_LOG(rc);
                    free(tmp);
                    opal_argv_free(list);
                    return rc;
                }
                /* add its cpuset */
                if (OPAL_SUCCESS != (rc = opal_dss.pack(&buf, &tmp, 1, OPAL_STRING))) {
                    ORTE_ERROR_LOG(rc);
                    free(tmp);
                    opal_argv_free(list);
                    return rc;
                }
                free(tmp);
            }
        }
    }
    /* pass the blob containing the cpusets for all local peers - note
     * that the cpuset of the proc we are responding to will be included,
     * so we don't need to send it separately */
    OBJ_CONSTRUCT(&kv, opal_value_t);
    kv.key = strdup(PMIX_LOCAL_CPUSETS);
    kv.type = OPAL_BYTE_OBJECT;
    opal_dss.unload(&buf, (void**)&kv.data.bo.bytes, &kv.data.bo.size);
    OBJ_DESTRUCT(&buf);
    if (OPAL_SUCCESS != (rc = opal_dss.pack(reply, &kp, 1, OPAL_VALUE))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&kv);
        opal_argv_free(list);
        return rc;
    }
    OBJ_DESTRUCT(&kv);
    /* construct the list of peers for transmission */
    tmp = opal_argv_join(list, ',');
    opal_argv_free(list);
    /* pass the local ldr */
    OBJ_CONSTRUCT(&kv, opal_value_t);
    kv.key = strdup(PMIX_LOCALLDR);
    kv.type = OPAL_NAME;
    kv.data.name = name;
    if (OPAL_SUCCESS != (rc = opal_dss.pack(reply, &kp, 1, OPAL_VALUE))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&kv);
        free(tmp);
        return rc;
    }
    OBJ_DESTRUCT(&kv);
    /* pass the list of peers */
    OBJ_CONSTRUCT(&kv, opal_value_t);
    kv.key = strdup(PMIX_LOCAL_PEERS);
    kv.type = OPAL_STRING;
    kv.data.string = tmp;
    if (OPAL_SUCCESS != (rc = opal_dss.pack(reply, &kp, 1, OPAL_VALUE))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&kv);
        return rc;
    }
    OBJ_DESTRUCT(&kv);
    /* app ldr */
    OBJ_CONSTRUCT(&kv, opal_value_t);
    kv.key = strdup(PMIX_APPLDR);
    kv.type = OPAL_UINT32;
    kv.data.uint32 = app->first_rank;
    if (OPAL_SUCCESS != (rc = opal_dss.pack(reply, &kp, 1, OPAL_VALUE))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&kv);
        return rc;
    }
    OBJ_DESTRUCT(&kv);
    /* univ size */
    OBJ_CONSTRUCT(&kv, opal_value_t);
    kv.key = strdup(PMIX_UNIV_SIZE);
    kv.type = OPAL_UINT32;
    kv.data.uint32 = jdata->num_procs;
    if (OPAL_SUCCESS != (rc = opal_dss.pack(reply, &kp, 1, OPAL_VALUE))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&kv);
        return rc;
    }
    OBJ_DESTRUCT(&kv);
    /* job size */
    OBJ_CONSTRUCT(&kv, opal_value_t);
    kv.key = strdup(PMIX_JOB_SIZE);
    kv.type = OPAL_UINT32;
    kv.data.uint32 = jdata->num_procs;
    if (OPAL_SUCCESS != (rc = opal_dss.pack(reply, &kp, 1, OPAL_VALUE))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&kv);
        return rc;
    }
    OBJ_DESTRUCT(&kv);
    /* local size */
    OBJ_CONSTRUCT(&kv, opal_value_t);
    kv.key = strdup(PMIX_LOCAL_SIZE);
    kv.type = OPAL_UINT32;
    kv.data.uint32 = jdata->num_local_procs;
    if (OPAL_SUCCESS != (rc = opal_dss.pack(reply, &kp, 1, OPAL_VALUE))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&kv);
        return rc;
    }
    OBJ_DESTRUCT(&kv);
    /* node size */
    OBJ_CONSTRUCT(&kv, opal_value_t);
    kv.key = strdup(PMIX_NODE_SIZE);
    kv.type = OPAL_UINT32;
    kv.data.uint32 = node->num_procs;
    if (OPAL_SUCCESS != (rc = opal_dss.pack(reply, &kp, 1, OPAL_VALUE))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&kv);
        return rc;
    }
    OBJ_DESTRUCT(&kv);
    /* max procs */
    OBJ_CONSTRUCT(&kv, opal_value_t);
    kv.key = strdup(PMIX_MAX_PROCS);
    kv.type = OPAL_UINT32;
    kv.data.uint16 = jdata->total_slots_alloc;
    if (OPAL_SUCCESS != (rc = opal_dss.pack(reply, &kp, 1, OPAL_VALUE))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&kv);
        return rc;
    }
    OBJ_DESTRUCT(&kv);
    /* local topology - we do this so the procs won't read the
     * topology themselves as this could overwhelm the local
     * system on large-scale SMPs */

    return ORTE_SUCCESS;
}
