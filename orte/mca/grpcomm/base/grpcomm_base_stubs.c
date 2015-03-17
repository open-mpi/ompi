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

static int pack_xcast(orte_grpcomm_signature_t *sig,
                      opal_buffer_t *buffer,
                      opal_buffer_t *message,
                      orte_rml_tag_t tag);

static int create_dmns(orte_grpcomm_signature_t *sig,
                       orte_vpid_t **dmns, size_t *ndmns);

typedef struct {
    opal_object_t super;
    opal_event_t ev;
    orte_grpcomm_signature_t *sig;
    opal_buffer_t *buf;
    orte_grpcomm_cbfunc_t cbfunc;
    void *cbdata;
} orte_grpcomm_caddy_t;
static void gccon(orte_grpcomm_caddy_t *p)
{
    p->sig = NULL;
    p->buf = NULL;
    p->cbfunc = NULL;
    p->cbdata = NULL;
}
static OBJ_CLASS_INSTANCE(orte_grpcomm_caddy_t,
                          opal_object_t,
                          gccon, NULL);

int orte_grpcomm_API_xcast(orte_grpcomm_signature_t *sig,
                           orte_rml_tag_t tag,
                           opal_buffer_t *msg)
{
    int rc = ORTE_ERROR;
    opal_buffer_t *buf;
    orte_grpcomm_base_active_t *active;
    orte_vpid_t *dmns;
    size_t ndmns;

    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_framework.framework_output,
                         "%s grpcomm:base:xcast sending %u bytes to tag %ld",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         (NULL == msg) ? 0 : (unsigned int)msg->bytes_used, (long)tag));

    /* this function does not access any framework-global data, and
     * so it does not require us to push it into the event library */

    /* prep the output buffer */
    buf = OBJ_NEW(opal_buffer_t);

    /* create the array of participating daemons */
    if (ORTE_SUCCESS != (rc = create_dmns(sig, &dmns, &ndmns))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(buf);
        return rc;
    }

    /* setup the payload */
    if (ORTE_SUCCESS != (rc = pack_xcast(sig, buf, msg, tag))) {
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

static void allgather_stub(int fd, short args, void *cbdata)
{
    orte_grpcomm_caddy_t *cd = (orte_grpcomm_caddy_t*)cbdata;
    int ret = OPAL_SUCCESS;
    int rc;
    orte_grpcomm_base_active_t *active;
    orte_grpcomm_coll_t *coll;
    void *seq_number;

    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_framework.framework_output,
                         "%s grpcomm:base:allgather stub",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* retrieve an existing tracker, create it if not
     * already found. The allgather module is responsible
     * for releasing it upon completion of the collective */
    ret = opal_hash_table_get_value_ptr(&orte_grpcomm_base.sig_table, (void *)cd->sig->signature, cd->sig->sz * sizeof(orte_process_name_t), &seq_number);
    if (OPAL_ERR_NOT_FOUND == ret) {
        cd->sig->seq_num = 0;
    } else if (OPAL_SUCCESS == ret) {
        cd->sig->seq_num = *((uint32_t *)(seq_number)) + 1;
    } else {
        OPAL_OUTPUT((orte_grpcomm_base_framework.framework_output,
                     "%s rpcomm:base:allgather can't not get signature from hash table",
                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        ORTE_ERROR_LOG(ret);
        OBJ_RELEASE(cd);
        return;
    }
    ret = opal_hash_table_set_value_ptr(&orte_grpcomm_base.sig_table, (void *)cd->sig->signature, cd->sig->sz * sizeof(orte_process_name_t), (void *)&cd->sig->seq_num);
    if (OPAL_SUCCESS != ret) {
        OPAL_OUTPUT((orte_grpcomm_base_framework.framework_output,
                     "%s rpcomm:base:allgather can't not add new signature to hash table",
                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        ORTE_ERROR_LOG(ret);
        OBJ_RELEASE(cd);
        return;
    }
    coll = orte_grpcomm_base_get_tracker(cd->sig, true);
    coll->cbfunc = cd->cbfunc;
    coll->cbdata = cd->cbdata;

    /* cycle thru the actives and see who can process it */
    OPAL_LIST_FOREACH(active, &orte_grpcomm_base.actives, orte_grpcomm_base_active_t) {
        if (NULL != active->module->allgather) {
            if (ORTE_SUCCESS == (rc = active->module->allgather(coll, cd->buf))) {
                break;
            }
        }
    }
    OBJ_RELEASE(cd);
}

int orte_grpcomm_API_allgather(orte_grpcomm_signature_t *sig,
                               opal_buffer_t *buf,
                               orte_grpcomm_cbfunc_t cbfunc,
                               void *cbdata)
{
    orte_grpcomm_caddy_t *cd;

    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_framework.framework_output,
                         "%s grpcomm:base:allgather",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* must push this into the event library to ensure we can
     * access framework-global data safely */
    cd = OBJ_NEW(orte_grpcomm_caddy_t);
    /* ensure the data doesn't go away */
    OBJ_RETAIN(buf);
    opal_dss.copy((void **)&cd->sig, (void *)sig, ORTE_SIGNATURE);
    cd->buf = buf;
    cd->cbfunc = cbfunc;
    cd->cbdata = cbdata;
    opal_event_set(orte_event_base, &cd->ev, -1, OPAL_EV_WRITE, allgather_stub, cd);
    opal_event_set_priority(&cd->ev, ORTE_MSG_PRI);
    opal_event_active(&cd->ev, OPAL_EV_WRITE, 1);
    return ORTE_SUCCESS;
}

orte_grpcomm_coll_t* orte_grpcomm_base_get_tracker(orte_grpcomm_signature_t *sig, bool create)
{
    orte_grpcomm_coll_t *coll;
    int rc;

    /* search the existing tracker list to see if this already exists */
    OPAL_LIST_FOREACH(coll, &orte_grpcomm_base.ongoing, orte_grpcomm_coll_t) {
        if (NULL == sig->signature) {
            if (NULL == coll->sig->signature) {
                /* only one collective can operate at a time
                 * across every process in the system */
                return coll;
            }
            /* if only one is NULL, then we can't possibly match */
            break;
        }
        if (OPAL_EQUAL == (rc = opal_dss.compare(sig, coll->sig, ORTE_SIGNATURE))) {
            OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_framework.framework_output,
                                 "%s grpcomm:base:returning existing collective",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
            return coll;
        }
    }
    /* if we get here, then this is a new collective - so create
     * the tracker for it */
    if (!create) {
        OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_framework.framework_output,
                             "%s grpcomm:base: not creating new coll",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

        return NULL;
    }
    coll = OBJ_NEW(orte_grpcomm_coll_t);
    opal_dss.copy((void **)&coll->sig, (void *)sig, ORTE_SIGNATURE);

    if (1 < opal_output_get_verbosity(orte_grpcomm_base_framework.framework_output)) {
        char *tmp=NULL;
        (void)opal_dss.print(&tmp, NULL, coll->sig, ORTE_SIGNATURE);
        opal_output(0, "%s grpcomm:base: creating new coll for%s",
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), tmp);
        free(tmp);
    }

    opal_list_append(&orte_grpcomm_base.ongoing, &coll->super);

    /* now get the daemons involved */
    if (ORTE_SUCCESS != (rc = create_dmns(sig, &coll->dmns, &coll->ndmns))) {
        ORTE_ERROR_LOG(rc);
        return NULL;
    }
    return coll;
}

static int create_dmns(orte_grpcomm_signature_t *sig,
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

    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_framework.framework_output,
                         "%s grpcomm:base:create_dmns called with %s signature",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         (NULL == sig->signature) ? "NULL" : "NON-NULL"));

    /* if NULL == procs, then all daemons are participating */
    if (NULL == sig->signature) {
        *ndmns = orte_process_info.num_procs;
        *dmns = NULL;
        return ORTE_SUCCESS;
    }

    if (ORTE_VPID_WILDCARD == sig->signature[0].vpid) {
        OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_framework.framework_output,
                             "%s grpcomm:base:create_dmns called for all procs in job %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_JOBID_PRINT(sig->signature[0].jobid)));
        /* all daemons hosting this jobid are participating */
        if (NULL == (jdata = orte_get_job_data_object(sig->signature[0].jobid))) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            return ORTE_ERR_NOT_FOUND;
        }
        if (NULL == jdata->map) {
            /* we haven't generated a job map yet - if we are the HNP,
             * then we should only involve ourselves. Otherwise, we have
             * no choice but to abort to avoid hangs */
            if (ORTE_PROC_IS_HNP) {
                dns = (orte_vpid_t*)malloc(sizeof(vpid));
                dns[0] = ORTE_PROC_MY_NAME->vpid;
                *ndmns = 1;
                *dmns = dns;
                return ORTE_SUCCESS;
            }
            ORTE_FORCED_TERMINATE(ORTE_ERR_NOT_FOUND);
            *ndmns = 0;
            *dmns = NULL;
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
            OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base_framework.framework_output,
                                 "%s grpcomm:base:create_dmns adding daemon %s to array",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&node->daemon->name)));
            dns[nds++] = node->daemon->name.vpid;
        }
    } else {
        /* lookup the daemon for each proc and add it to the list, checking to
         * ensure any daemon only gets added once. Yes, this isn't a scalable
         * algo - someone can come up with something better! */
        OBJ_CONSTRUCT(&ds, opal_list_t);
        for (n=0; n < sig->sz; n++) {
            if (NULL == (jdata = orte_get_job_data_object(sig->signature[n].jobid))) {
                ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
                OPAL_LIST_DESTRUCT(&ds);
                return ORTE_ERR_NOT_FOUND;
            }
            opal_output_verbose(5, orte_grpcomm_base_framework.framework_output,
                                "%s sign: GETTING PROC OBJECT FOR %s",
                                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                ORTE_NAME_PRINT(&sig->signature[n]));
            if (NULL == (proc = (orte_proc_t*)opal_pointer_array_get_item(jdata->procs, sig->signature[n].vpid))) {
                ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
                OPAL_LIST_DESTRUCT(&ds);
                return ORTE_ERR_NOT_FOUND;
            }
            if (NULL == proc->node || NULL == proc->node->daemon) {
                ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
                OPAL_LIST_DESTRUCT(&ds);
                ORTE_FORCED_TERMINATE(ORTE_ERR_NOT_FOUND);
                return ORTE_ERR_NOT_FOUND;
            }
            vpid = proc->node->daemon->name.vpid;
            found = false;
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
            OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base_framework.framework_output,
                                 "%s grpcomm:base:create_dmns adding daemon %s to array",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&nm->name)));
            dns[nds++] = nm->name.vpid;
            OBJ_RELEASE(nm);
        }
        OPAL_LIST_DESTRUCT(&ds);
    }
    *dmns = dns;
    *ndmns = nds;
    return ORTE_SUCCESS;
}

static int pack_xcast(orte_grpcomm_signature_t *sig,
                      opal_buffer_t *buffer,
                      opal_buffer_t *message,
                      orte_rml_tag_t tag)
{
    int rc;

    /* pass along the signature */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(buffer, &sig, 1, ORTE_SIGNATURE))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
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

void orte_grpcomm_base_mark_distance_recv(orte_grpcomm_coll_t *coll,
                                   uint32_t distance) {
    uint32_t maskNumber = distance / 32;
    uint32_t bitNumber = distance % 32;

    coll->distance_mask_recv[maskNumber] |= (1 << bitNumber);

    return;
}

unsigned int orte_grpcomm_base_check_distance_recv(orte_grpcomm_coll_t *coll,
                                   uint32_t distance) {
    uint32_t maskNumber = distance / 32;
    uint32_t bitNumber = distance % 32;

    if (NULL == coll->distance_mask_recv) {
        return 0;
    } else {
        if (0 == distance) {
            return 1;
        }
        return (((coll->distance_mask_recv[maskNumber] & (1 << bitNumber)) != 0) ? 1 : 0);
    }
}
