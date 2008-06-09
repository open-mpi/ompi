/* -*- C -*-
 *
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
#include "orte/constants.h"
#include "orte/types.h"

#if HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#if HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#if HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "orte/util/show_help.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/mca_base_param.h"

#include "opal/dss/dss.h"
#include "orte/util/proc_info.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rml/rml.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/orte_wait.h"

#include "orte/mca/routed/base/base.h"

static bool recv_issued=false;

int orte_routed_base_comm_start(void)
{
    int rc;

    if (recv_issued || !orte_process_info.hnp) {
        return ORTE_SUCCESS;
    }
    
    OPAL_OUTPUT_VERBOSE((5, orte_routed_base_output,
                         "%s routed:base: Receive: Start command recv",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    if (ORTE_SUCCESS != (rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                                                      ORTE_RML_TAG_INIT_ROUTES,
                                                      ORTE_RML_NON_PERSISTENT,
                                                      orte_routed_base_recv,
                                                      NULL))) {
        ORTE_ERROR_LOG(rc);
    }

    recv_issued = true;
    
    return rc;
}


int orte_routed_base_comm_stop(void)
{
    int rc;
    
    if (!recv_issued || !orte_process_info.hnp) {
        return ORTE_SUCCESS;
    }
    
    OPAL_OUTPUT_VERBOSE((5, orte_routed_base_output,
                         "%s routed:base:receive stop comm",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    if (ORTE_SUCCESS != (rc = orte_rml.recv_cancel(ORTE_NAME_WILDCARD, ORTE_RML_TAG_INIT_ROUTES))) {
        ORTE_ERROR_LOG(rc);
    }
    recv_issued = false;
    
    return rc;
}

void orte_routed_base_process_msg(int fd, short event, void *data)
{
    orte_message_event_t *mev = (orte_message_event_t*)data;
    orte_jobid_t job;
    int rc;
    orte_std_cntr_t cnt;

    /* unpack the jobid this is for */
    cnt=1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(mev->buffer, &job, &cnt, ORTE_JOBID))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(mev);
        return;
    }
    
    /* pass the remainder of the buffer to the active module's
     * init_routes API
     */
    if (ORTE_SUCCESS != (rc = orte_routed.init_routes(job, mev->buffer))) {
        ORTE_ERROR_LOG(rc);
    }
    OBJ_RELEASE(mev);
    return;
}


/*
 * handle init routes requests from non-HNP-local procs
 * NOTE: The incoming buffer "buffer" is OBJ_RELEASED by the calling program.
 * DO NOT RELEASE THIS BUFFER IN THIS CODE
 */
void orte_routed_base_recv(int status, orte_process_name_t* sender,
                           opal_buffer_t* buffer, orte_rml_tag_t tag,
                           void* cbdata)
{
    int rc;
    
    OPAL_OUTPUT_VERBOSE((5, orte_routed_base_output,
                         "%s routed:base:receive got message from %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(sender)));
    
    /* don't process this right away - we need to get out of the recv before
     * we process the message as it may ask us to do something that involves
     * more messaging! Instead, setup an event so that the message gets processed
     * as soon as we leave the recv.
     *
     * The macro makes a copy of the buffer, which we release above - the incoming
     * buffer, however, is NOT released here, although its payload IS transferred
     * to the message buffer for later processing
     */
    ORTE_MESSAGE_EVENT(sender, buffer, tag, orte_routed_base_process_msg);
    
    /* reissue the recv */
    if (ORTE_SUCCESS != (rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                                                      ORTE_RML_TAG_INIT_ROUTES,
                                                      ORTE_RML_NON_PERSISTENT,
                                                      orte_routed_base_recv,
                                                      NULL))) {
        ORTE_ERROR_LOG(rc);
    }
    return;
}
