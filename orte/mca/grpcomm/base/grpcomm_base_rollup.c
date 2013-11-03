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
#include "orte/mca/ess/ess.h"
#include "orte/mca/odls/base/base.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/routed/routed.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/grpcomm/grpcomm_types.h"
#include "orte/mca/grpcomm/grpcomm.h"
#include "orte/mca/grpcomm/base/base.h"

/* function to cleanup collective once completed */
static void rml_send_callback(int status, orte_process_name_t* sender,
                              opal_buffer_t* buffer, orte_rml_tag_t tag,
                              void* cbdata)
{
    orte_grpcomm_collective_t *coll;

    /* remove this collective from our list */
    coll = (orte_grpcomm_collective_t*)opal_list_remove_first(&orte_grpcomm_base.active_colls);

    /* release it */
    OBJ_RELEASE(coll);

    /* release our buffer */
    OBJ_RELEASE(buffer);
}

void orte_grpcomm_base_rollup_recv(int status, orte_process_name_t* sender,
                                  opal_buffer_t* buffer, orte_rml_tag_t tag,
                                  void* cbdata)
{
    int ret;
    orte_grpcomm_collective_t *coll;
    bool done = false;
    opal_buffer_t *relay;

    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_framework.framework_output,
                         "%s grpcomm:rollup:recv from sender %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(sender)));

    /* lookup the collective - can only be one on the list at this time */
    coll = (orte_grpcomm_collective_t*)opal_list_get_first(&orte_grpcomm_base.active_colls);

    /* flag that we received a bucket */
    if (sender->vpid != ORTE_PROC_MY_NAME->vpid) {
        coll->num_peer_buckets++;
    }

    /* transfer the data */
    opal_dss.copy_payload(&coll->buffer, buffer);

    /* if list is empty, then we can just send our data along */
    if (opal_list_is_empty(&coll->targets)) {
        done = true;
    } else if (coll->num_peer_buckets == opal_list_get_size(&coll->targets)) {
        done = true;
    } else {
        /* check for a wildcard */
        orte_namelist_t *nm;
        nm = (orte_namelist_t*)opal_list_get_first(&coll->targets);
        if (ORTE_VPID_WILDCARD == nm->name.vpid &&
            coll->num_peer_buckets == orte_process_info.num_procs) {
            done = true;
        }
    }

    if (done) {
        /* send the message to my parent */
        OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base_framework.framework_output,
                             "%s grpcomm:rollup: sending rollup msg to %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(ORTE_PROC_MY_PARENT)));
        relay = OBJ_NEW(opal_buffer_t);
        opal_dss.copy_payload(relay, &coll->buffer);
        /* if my parent is the HNP, send it to the final destination */
        if (ORTE_PROC_MY_PARENT->vpid == ORTE_PROC_MY_HNP->vpid) {
            if (0 > (ret = orte_rml.send_buffer_nb(ORTE_PROC_MY_HNP, relay,
                                                   ORTE_RML_TAG_ORTED_CALLBACK,
                                                   rml_send_callback, NULL))) {
                ORTE_ERROR_LOG(ret);
            }
        } else {
            if (0 > (ret = orte_rml.send_buffer_nb(ORTE_PROC_MY_PARENT, relay,
                                                   ORTE_RML_TAG_ROLLUP,
                                                   rml_send_callback, NULL))) {
                ORTE_ERROR_LOG(ret);
            }
        }
    }
}
