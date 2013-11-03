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
#include "orte/mca/rml/rml.h"
#include "orte/mca/routed/routed.h"
#include "orte/mca/state/state.h"
#include "orte/util/name_fns.h"
#include "orte/util/nidmap.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/grpcomm/grpcomm_types.h"
#include "orte/mca/grpcomm/grpcomm.h"
#include "orte/mca/grpcomm/base/base.h"

void orte_grpcomm_base_xcast_recv(int status, orte_process_name_t* sender,
                                  opal_buffer_t* buffer, orte_rml_tag_t tag,
                                  void* cbdata)
{
    opal_list_item_t *item;
    orte_namelist_t *nm;
    int ret, cnt;
    opal_buffer_t *relay, *rly;
    orte_daemon_cmd_flag_t command;
    opal_buffer_t wireup;
    opal_byte_object_t *bo;
    int8_t flag;
    orte_grpcomm_collective_t coll;
    orte_job_t *jdata;
    orte_proc_t *rec;

    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_framework.framework_output,
                         "%s grpcomm:xcast:recv: with %d bytes",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         (int)buffer->bytes_used));

    /* setup the relay message */
    relay = OBJ_NEW(opal_buffer_t);
    opal_dss.copy_payload(relay, buffer);

    /* peek at the command */
    cnt=1;
    if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &command, &cnt, ORTE_DAEMON_CMD))) {
        ORTE_ERROR_LOG(ret);
        ORTE_FORCED_TERMINATE(ret);
        return;
    }

    /* if it is add_procs, then... */
    if (ORTE_DAEMON_ADD_LOCAL_PROCS == command) {
        /* extract the byte object holding the daemonmap */
        cnt=1;
        if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &bo, &cnt, OPAL_BYTE_OBJECT))) {
            ORTE_ERROR_LOG(ret);
            goto relay;
        }
    
        /* update our local nidmap, if required - the decode function
         * knows what to do - it will also free the bytes in the bo. Decode
         * also updates our global nidmap object for sending to our local procs
         */
        if (ORTE_PROC_IS_HNP) {
            /* no need - already have the info */
            if (NULL != bo->bytes) {
                free(bo->bytes);
            }
        } else {
            OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base_framework.framework_output,
                                 "%s grpcomm:base:xcast updating daemon nidmap",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
            if (ORTE_SUCCESS != (ret = orte_util_decode_daemon_nodemap(bo))) {
                ORTE_ERROR_LOG(ret);
                goto relay;
            }
        }

        /* update the routing plan */
        orte_routed.update_routing_plan();
    
        /* see if we have wiring info as well */
        cnt=1;
        if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &flag, &cnt, OPAL_INT8))) {
            ORTE_ERROR_LOG(ret);
            goto relay;
        }
        if (0 == flag) {
            /* no - just return */
            goto relay;
        }

        /* unpack the byte object */
        cnt=1;
        if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &bo, &cnt, OPAL_BYTE_OBJECT))) {
            ORTE_ERROR_LOG(ret);
            goto relay;
        }
        if (0 < bo->size) {
            /* load it into a buffer */
            OBJ_CONSTRUCT(&wireup, opal_buffer_t);
            opal_dss.load(&wireup, bo->bytes, bo->size);
            /* pass it for processing */
            if (ORTE_SUCCESS != (ret = orte_routed.init_routes(ORTE_PROC_MY_NAME->jobid, &wireup))) {
                ORTE_ERROR_LOG(ret);
                OBJ_DESTRUCT(&wireup);
                goto relay;
            }
            /* done with the wireup buffer - dump it */
            OBJ_DESTRUCT(&wireup);
        }
    }

 relay:
    /* setup the relay list */
    OBJ_CONSTRUCT(&coll, orte_grpcomm_collective_t);

    /* get the list of next recipients from the routed module */
    orte_routed.get_routing_list(ORTE_GRPCOMM_XCAST, &coll);

    /* if list is empty, no relay is required */
    if (opal_list_is_empty(&coll.targets)) {
        OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base_framework.framework_output,
                             "%s orte:daemon:send_relay - recipient list is empty!",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        goto CLEANUP;
    }
    
    /* send the message to each recipient on list, deconstructing it as we go */
    while (NULL != (item = opal_list_remove_first(&coll.targets))) {
        nm = (orte_namelist_t*)item;

        OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base_framework.framework_output,
                             "%s orte:daemon:send_relay sending relay msg to %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&nm->name)));
        rly = OBJ_NEW(opal_buffer_t);
        opal_dss.copy_payload(rly, relay);
        /* check the state of the recipient - no point
         * sending to someone not alive
         */
        jdata = orte_get_job_data_object(nm->name.jobid);
        if (NULL == (rec = (orte_proc_t*)opal_pointer_array_get_item(jdata->procs, nm->name.vpid))) {
            OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base_framework.framework_output,
                                 "%s orte:daemon:send_relay proc %s not found - cannot relay",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&nm->name)));
            OBJ_RELEASE(rly);
            continue;
        }
        if (ORTE_PROC_STATE_RUNNING < rec->state) {
            OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base_framework.framework_output,
                                 "%s orte:daemon:send_relay proc %s not running - cannot relay",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&nm->name)));
            OBJ_RELEASE(rly);
            continue;
        }
        if (0 > (ret = orte_rml.send_buffer_nb(&nm->name, rly, ORTE_RML_TAG_XCAST,
                                               orte_rml_send_callback, NULL))) {
            ORTE_ERROR_LOG(ret);
            OBJ_RELEASE(rly);
            continue;
        }
    }
    
 CLEANUP:
    /* cleanup */
    OBJ_DESTRUCT(&coll);

    /* now send it to myself for processing */
    if (0 > (ret = orte_rml.send_buffer_nb(ORTE_PROC_MY_NAME, relay,
                                           ORTE_RML_TAG_DAEMON,
                                           orte_rml_send_callback, NULL))) {
        ORTE_ERROR_LOG(ret);
        OBJ_RELEASE(relay);
    }
}

int orte_grpcomm_base_pack_xcast(orte_jobid_t job,
                                 opal_buffer_t *buffer,
                                 opal_buffer_t *message,
                                 orte_rml_tag_t tag)
{
    orte_daemon_cmd_flag_t command;
    int rc;

    /* if this isn't intended for the daemon command tag, then we better
     * tell the daemon to deliver it to the procs, and what job is supposed
     * to get it - this occurs when a caller just wants to send something
     * to all the procs in a job. In that use-case, the caller doesn't know
     * anything about inserting daemon commands or what routing algo might
     * be used, so we have to help them out a little. Functions that are
     * sending commands to the daemons themselves are smart enough to know
     * what they need to do.
     */
    if (ORTE_RML_TAG_DAEMON != tag) {
        command = ORTE_DAEMON_MESSAGE_LOCAL_PROCS;
        if (ORTE_SUCCESS != (rc = opal_dss.pack(buffer, &command, 1, ORTE_DAEMON_CMD))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
        if (ORTE_SUCCESS != (rc = opal_dss.pack(buffer, &job, 1, ORTE_JOBID))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
        if (ORTE_SUCCESS != (rc = opal_dss.pack(buffer, &tag, 1, ORTE_RML_TAG))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
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

