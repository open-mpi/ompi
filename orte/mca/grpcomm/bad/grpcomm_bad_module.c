/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"
#include "orte/types.h"

#include <string.h>


#include "opal/dss/dss.h"
#include "opal/mca/hwloc/base/base.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/odls/base/base.h"
#include "orte/mca/odls/odls_types.h"
#include "orte/mca/ess/ess.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/routed/routed.h"
#include "orte/runtime/orte_globals.h"
#include "orte/util/name_fns.h"
#include "orte/util/proc_info.h"
#include "orte/orted/orted.h"
#include "orte/runtime/orte_wait.h"

#include "orte/mca/grpcomm/base/base.h"
#include "grpcomm_bad.h"


/* Static API's */
static int init(void);
static void finalize(void);
static int xcast(orte_jobid_t job,
                 opal_buffer_t *buffer,
                 orte_rml_tag_t tag);
static int bad_allgather(orte_grpcomm_collective_t *coll);
static int bad_barrier(orte_grpcomm_collective_t *coll);
static int bad_modex(orte_grpcomm_collective_t *modex);

/* Module def */
orte_grpcomm_base_module_t orte_grpcomm_bad_module = {
    init,
    finalize,
    xcast,
    bad_allgather,
    bad_barrier,
    bad_modex
};

/**
 * Initialize the module
 */
static int init(void)
{
    int rc;
    
    /* setup recvs */
    if (ORTE_SUCCESS != (rc = orte_grpcomm_base_comm_start())) {
        ORTE_ERROR_LOG(rc);
    }
    
    return rc;
}

/**
 * Finalize the module
 */
static void finalize(void)
{
    /* cancel recv */
    orte_grpcomm_base_comm_stop();
}

/**
 *  A "broadcast-like" function to a job's processes.
 *  @param  jobid   The job whose processes are to receive the message
 *  @param  buffer  The data to broadcast
 */

static int xcast(orte_jobid_t job,
                 opal_buffer_t *buffer,
                 orte_rml_tag_t tag)
{
    int rc = ORTE_SUCCESS;
    opal_buffer_t *buf;
    
    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_framework.framework_output,
                         "%s grpcomm:bad:xcast sent to job %s tag %ld",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(job), (long)tag));
    
    /* if there is no message to send, then just return ok */
    if (NULL == buffer) {
        return ORTE_SUCCESS;
    }
    
    /* prep the output buffer */
    buf = OBJ_NEW(opal_buffer_t);
    
    if (ORTE_SUCCESS != (rc = orte_grpcomm_base_pack_xcast(job, buf, buffer, tag))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }

    /* send it to the HNP (could be myself) for relay */
    if (0 > (rc = orte_rml.send_buffer_nb(ORTE_PROC_MY_HNP, buf, ORTE_RML_TAG_XCAST,
                                          orte_rml_send_callback, NULL))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(buf);
        goto CLEANUP;
    }
    rc = ORTE_SUCCESS;
    
CLEANUP:
    return rc;
}

static void process_barrier(int fd, short args, void *cbdata)
{
    orte_grpcomm_caddy_t *caddy = (orte_grpcomm_caddy_t*)cbdata;
    orte_grpcomm_collective_t *coll = caddy->op;
    int rc;
    opal_buffer_t *buf;
    orte_namelist_t *nm;

    OBJ_RELEASE(caddy);

    /* if we are a singleton and routing isn't enabled,
     * then we have nobody with which to communicate, so
     * we can just declare success
     */
    if ((orte_process_info.proc_type & ORTE_PROC_SINGLETON) &&
        !orte_routing_is_enabled) {
        if (NULL != coll->cbfunc) {
            OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base_framework.framework_output,
                                 "%s CALLING BARRIER RELEASE",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
            coll->cbfunc(NULL, coll->cbdata);
        }
        /* flag the collective as complete */
        coll->active = false;
        return;
    }

    /* setup the collective */
    opal_list_append(&orte_grpcomm_base.active_colls, &coll->super);

    if (0 == opal_list_get_size(&coll->participants)) {
        /* add a wildcard name to the participants so the daemon knows
         * that everyone in my job must participate
         */
        nm = OBJ_NEW(orte_namelist_t);
        nm->name.jobid = ORTE_PROC_MY_NAME->jobid;
        nm->name.vpid = ORTE_VPID_WILDCARD;
        opal_list_append(&coll->participants, &nm->super);
    }

    /* pack the collective - no data should be involved, but we need
     * to ensure we get the header info correct so it can be
     * unpacked without error
     */
    buf = OBJ_NEW(opal_buffer_t);
    orte_grpcomm_base_pack_collective(buf, ORTE_PROC_MY_NAME->jobid,
                                      coll, ORTE_GRPCOMM_INTERNAL_STG_APP);

    /* send the buffer to my daemon */
    if (0 > (rc = orte_rml.send_buffer_nb(ORTE_PROC_MY_DAEMON, buf, ORTE_RML_TAG_COLLECTIVE,
                                          orte_rml_send_callback, NULL))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(buf);
        opal_list_remove_item(&orte_grpcomm_base.active_colls, &coll->super);
        return;
    }
    
    OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base_framework.framework_output,
                         "%s grpcomm:bad barrier underway",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
}

static int bad_barrier(orte_grpcomm_collective_t *coll)
{
    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_framework.framework_output,
                         "%s grpcomm:bad entering barrier",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* push it into the event library for processing as
     * we will be accessing global lists
     */
    ORTE_GRPCOMM_ACTIVATE(coll, process_barrier);
    return ORTE_SUCCESS;
}

static void process_allgather(int fd, short args, void *cbdata)
{
    orte_grpcomm_caddy_t *caddy = (orte_grpcomm_caddy_t*)cbdata;
    orte_grpcomm_collective_t *gather = caddy->op;
    int rc;
    opal_buffer_t *buf;
    orte_namelist_t *nm;
    opal_list_item_t *item;

    OBJ_RELEASE(caddy);

    /* if we are a singleton and routing isn't enabled,
     * then we have nobody with which to communicate, so
     * we can just declare success
     */
    if ((orte_process_info.proc_type & ORTE_PROC_SINGLETON) &&
        !orte_routing_is_enabled) {
        if (NULL != gather->cbfunc) {
            OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base_framework.framework_output,
                                 "%s CALLING GATHER RELEASE",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
            gather->cbfunc(NULL, gather->cbdata);
        }
        /* flag the collective as complete */
        gather->active = false;
        return;
    }

    /* if this is an original request, then record the collective */
    if (NULL == gather->next_cb) {
        opal_list_append(&orte_grpcomm_base.active_colls, &gather->super);
    }

    /* if the participants are not a WILDCARD, then we know that
     * this is a collective operation between a limited subset
     * of processes. In that scenario, we cannot use the daemon-based
     * collective system as the daemons won't know anything about
     * this collective
     */
    nm = (orte_namelist_t*)opal_list_get_first(&gather->participants);
    if (NULL == nm || ORTE_VPID_WILDCARD == nm->name.vpid) {
        /* start the allgather op by sending the data to our daemon - the
         * user will have put the data in the "buffer" field
         */
        buf = OBJ_NEW(opal_buffer_t);
        orte_grpcomm_base_pack_collective(buf, ORTE_PROC_MY_NAME->jobid,
                                          gather, ORTE_GRPCOMM_INTERNAL_STG_APP);

        OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_framework.framework_output,
                             "%s grpcomm:bad sending collective %d to our daemon",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             (int)gather->id));
        /* send to our daemon */
        if (0 > (rc = orte_rml.send_buffer_nb(ORTE_PROC_MY_DAEMON, buf,
                                              ORTE_RML_TAG_COLLECTIVE,
                                              orte_rml_send_callback, NULL))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(buf);
            opal_list_remove_item(&orte_grpcomm_base.active_colls, &gather->super);
            return;
        }
    } else {
        /* send directly to each participant - note that this will
         * include ourselves, which is fine as it will aid in
         * determining the collective is complete
         */
        for (item = opal_list_get_first(&gather->participants);
             item != opal_list_get_end(&gather->participants);
             item = opal_list_get_next(item)) {
            nm = (orte_namelist_t*)item;
            buf = OBJ_NEW(opal_buffer_t);
            opal_dss.copy_payload(buf, &gather->buffer);
            OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_framework.framework_output,
                                 "%s grpcomm:bad sending collective %d to %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 (int)gather->id,
                                 ORTE_NAME_PRINT(&nm->name)));
            if (0 > (rc = orte_rml.send_buffer_nb(&nm->name, buf,
                                                  ORTE_RML_TAG_COLLECTIVE,
                                                  orte_rml_send_callback, NULL))) {
                ORTE_ERROR_LOG(rc);
                OBJ_RELEASE(buf);
                opal_list_remove_item(&orte_grpcomm_base.active_colls, &gather->super);
                return;
            }
        }
        return;
    }
    
    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_framework.framework_output,
                         "%s grpcomm:bad allgather underway",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
}

static int bad_allgather(orte_grpcomm_collective_t *gather)
{
    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_framework.framework_output,
                         "%s grpcomm:bad entering allgather",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* push it into the event library for processing as
     * we will be accessing global lists
     */
    ORTE_GRPCOMM_ACTIVATE(gather, process_allgather);
    return ORTE_SUCCESS;
}

static int bad_modex(orte_grpcomm_collective_t *modex)
{
    /* we need to get this into the event library
     * to avoid race conditions with modex data arriving
     * from other sources via the RML
     */
    ORTE_GRPCOMM_ACTIVATE(modex, orte_grpcomm_base_modex);
    return ORTE_SUCCESS;
}
