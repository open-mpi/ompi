/* -*- C -*-
 *
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2011-2012 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/** @file:
 *
 */

/*
 * includes
 */
#include "orte_config.h"


#include "opal/dss/dss.h"

#include "orte/util/proc_info.h"
#include "orte/util/error_strings.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/odls/base/base.h"
#include "orte/mca/rmaps/rmaps_types.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/routed/routed.h"
#include "orte/mca/state/state.h"
#include "orte/util/name_fns.h"
#include "orte/util/nidmap.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/grpcomm/grpcomm.h"
#include "orte/mca/grpcomm/base/base.h"

static int pack_xcast(orte_process_name_t *procs,
                      size_t nprocs,
                      opal_buffer_t *buffer,
                      opal_buffer_t *message,
                      orte_rml_tag_t tag);
static orte_grpcomm_coll_t* get_tracker(orte_process_name_t *procs,
                                        size_t nprocs);

static int create_dmns(orte_process_name_t *procs, size_t nprocs,
                       orte_vpid_t **dmns, size_t *ndmns);

int orte_grpcomm_API_xcast(orte_process_name_t *procs,
                           size_t nprocs,
                           orte_rml_tag_t tag,
                           opal_buffer_t *msg)
{
    int rc = ORTE_ERROR;
    opal_buffer_t *buf;
    orte_grpcomm_base_active_t *active;
    orte_vpid_t *dmns;
    size_t ndmns;

    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_framework.framework_output,
                         "%s grpcomm:base:xcast sending to tag %ld",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), (long)tag));
    
    /* this function does not access any framework-global data, and
     * so it does not require us to push it into the event library */

    /* if there is no message to send, then just return ok */
    if (NULL == msg) {
        return ORTE_SUCCESS;
    }
    
    /* prep the output buffer */
    buf = OBJ_NEW(opal_buffer_t);
    
    /* create the array of participating daemons */
    if (ORTE_SUCCESS != (rc = create_dmns(procs, nprocs, &dmns, &ndmns))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(buf);
        return rc;
    }

    /* setup the payload */
    if (ORTE_SUCCESS != (rc = pack_xcast(procs, nprocs, buf, msg, tag))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(buf);
        if (NULL != dmns) {
            free(dmns);
        }
        return rc;
    }

    /* cycle thru the actives and see who can send it */
    OPAL_LIST_FOREACH(active, &orte_grpcomm_base.actives, orte_grpcomm_base_active_t) {
        if (NULL != active->module->xcast) {
            if (ORTE_SUCCESS == (rc = active->module->xcast(dmns, ndmns, buf))) {
                break;
            }
        }
    }
    OBJ_RELEASE(buf);  // if the module needs to keep the buf, it should OBJ_RETAIN it
    if (NULL != dmns) {
        free(dmns);
    }
    return rc;
}

int orte_grpcomm_API_allgather(orte_process_name_t *procs,
                               size_t nprocs,
                               opal_buffer_t *buf,
                               orte_grpcomm_cbfunc_t cbfunc,
                               void *cbdata)
{
    int rc;
    orte_grpcomm_base_active_t *active;
    orte_grpcomm_coll_t *coll;

    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_framework.framework_output,
                         "%s grpcomm:base:allgather",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* must push this into the event library to ensure we can
     * access framework-global data safely */

    /* retrieve an existing tracker, create it if not
     * already found. The allgather module is responsible
     * for releasing it upon completion of the collective */
    coll = get_tracker(procs, nprocs);

    /* cycle thru the actives and see who can process it */
    OPAL_LIST_FOREACH(active, &orte_grpcomm_base.actives, orte_grpcomm_base_active_t) {
        if (NULL != active->module->allgather) {
            if (ORTE_SUCCESS == (rc = active->module->allgather(coll, buf, cbfunc, cbdata))) {
                break;
            }
        }
    }
    return rc;
}

static orte_grpcomm_coll_t* get_tracker(orte_process_name_t *procs,
                                        size_t nprocs)
{
    orte_grpcomm_coll_t *coll;
    int rc;

    /* search the existing tracker list to see if this already exists */
    OPAL_LIST_FOREACH(coll, &orte_grpcomm_base.ongoing, orte_grpcomm_coll_t) {
        if (NULL == procs) {
            if (NULL == coll->signature) {
                /* only one collective can operate at a time
                 * across every process in the system */
                return coll;
            }
            /* if only one is NULL, then we can't possibly match */
            break;
        }
        /* if the size doesn't match, then they can't be the same */
        if (nprocs != coll->sz) {
            continue;
        }
        if (0 == memcmp(procs, coll->signature, coll->sz)) {
            return coll;
        }
    }
    /* if we get here, then this is a new collective - so create
     * the tracker for it */
    coll = OBJ_NEW(orte_grpcomm_coll_t);
    coll->signature = (orte_process_name_t*)malloc(nprocs * sizeof(orte_process_name_t));
    memcpy(coll->signature, procs, nprocs * sizeof(orte_process_name_t));
    coll->sz = nprocs;
    opal_list_append(&orte_grpcomm_base.ongoing, &coll->super);

    /* now get the daemons involved */
    if (ORTE_SUCCESS != (rc = create_dmns(procs, nprocs, &coll->dmns, &coll->ndmns))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(coll);
        return NULL;
    }
    return coll;
}

static int create_dmns(orte_process_name_t *procs, size_t nprocs,
                       orte_vpid_t **dmns, size_t *ndmns)
{
    size_t n;
    orte_job_t *jdata;
    orte_proc_t *proc;
    orte_node_t *node;
    int i;
    opal_list_t ds;
    orte_namelist_t *nm;
    orte_vpid_t vpid;
    bool found;
    size_t nds;
    orte_vpid_t *dns;

    /* if NULL == procs, then all daemons are participating */
    if (NULL == procs) {
        *ndmns = orte_process_info.num_procs;
        *dmns = NULL;
        return ORTE_SUCCESS;
    }

    if (ORTE_VPID_WILDCARD == procs[0].vpid) {
        /* all daemons hosting this jobid are participating */
        if (NULL == (jdata = orte_get_job_data_object(procs[0].jobid))) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            return ORTE_ERR_NOT_FOUND;
        }
        if (NULL == jdata->map) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            return ORTE_ERR_NOT_FOUND;
        }
        /* get the array */
        dns = (orte_vpid_t*)malloc(jdata->map->num_nodes * sizeof(vpid));
        nds = 0;
        for (i=0; i < jdata->map->nodes->size && (int)nds < jdata->map->num_nodes; i++) {
            if (NULL == (node = opal_pointer_array_get_item(jdata->map->nodes, i))) {
                continue;
            }
            if (NULL == node->daemon) {
                /* should never happen */
                ORTE_ERROR_LOG(ORTE_ERROR);
                free(dns);
                return ORTE_ERROR;
            }
            dns[nds++] = node->daemon->name.vpid;
        }
    } else {
        /* lookup the daemon for each proc and add it to the list, checking to
         * ensure any daemon only gets added once. Yes, this isn't a scalable
         * algo - someone can come up with something better! */
        OBJ_CONSTRUCT(&ds, opal_list_t);
        for (n=0; n < nprocs; n++) {
            if (NULL == (jdata = orte_get_job_data_object(procs[n].jobid))) {
                ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
                OPAL_LIST_DESTRUCT(&ds);
                return ORTE_ERR_NOT_FOUND;
            }
            if (NULL == (proc = (orte_proc_t*)opal_pointer_array_get_item(jdata->procs, procs[n].vpid))) {
                ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
                OPAL_LIST_DESTRUCT(&ds);
                return ORTE_ERR_NOT_FOUND;
            }
            if (NULL == proc->node || NULL == proc->node->daemon) {
                ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
                OPAL_LIST_DESTRUCT(&ds);
                return ORTE_ERR_NOT_FOUND;
            }
            vpid = proc->node->daemon->name.vpid;
            OPAL_LIST_FOREACH(nm, &ds, orte_namelist_t) {
                if (nm->name.vpid == vpid) {
                    found = true;
                    break;
                }
            }
            if (!found) {
                nm = OBJ_NEW(orte_namelist_t);
                nm->name.vpid = vpid;
                opal_list_append(&ds, &nm->super);
            }
        }
        if (0 == opal_list_get_size(&ds)) {
            ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
            OPAL_LIST_DESTRUCT(&ds);
            return ORTE_ERR_BAD_PARAM;
        }
        dns = (orte_vpid_t*)malloc(opal_list_get_size(&ds) * sizeof(orte_vpid_t));
        nds = 0;
        while (NULL != (nm = (orte_namelist_t*)opal_list_remove_first(&ds))) {
            dns[nds++] = nm->name.vpid;
            OBJ_RELEASE(nm);
        }
        OPAL_LIST_DESTRUCT(&ds);
    }
    *dmns = dns;
    *ndmns = nds;
    return ORTE_SUCCESS;
}

static int pack_xcast(orte_process_name_t *procs,
                      size_t nprocs,
                      opal_buffer_t *buffer,
                      opal_buffer_t *message,
                      orte_rml_tag_t tag)
{
    int rc;

    /* pass along the number of target procs */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(buffer, &nprocs, 1, OPAL_SIZE))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    /* pass along the names of the targets */
    if (0 < nprocs) {
        if (ORTE_SUCCESS != (rc = opal_dss.pack(buffer, procs, nprocs, ORTE_NAME))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
    }
    /* pass the final tag */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(buffer, &tag, 1, ORTE_RML_TAG))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    
    /* copy the payload into the new buffer - this is non-destructive, so our
     * caller is still responsible for releasing any memory in the buffer they
     * gave to us
     */
    if (ORTE_SUCCESS != (rc = opal_dss.copy_payload(buffer, message))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    
CLEANUP:
    return ORTE_SUCCESS;
}

