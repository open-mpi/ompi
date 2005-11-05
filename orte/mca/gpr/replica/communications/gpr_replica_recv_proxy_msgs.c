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
 * The Open MPI General Purpose Registry - Replica component
 *
 */

/*
 * includes
 */
#include "orte_config.h"

#include "opal/util/output.h"
#include "opal/util/trace.h"
#include "orte/util/proc_info.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ns/ns_types.h"
#include "orte/mca/rml/rml.h"

#include "orte/mca/gpr/replica/communications/gpr_replica_comm.h"


static void orte_gpr_replica_send_cb(
    int status,
    orte_process_name_t* peer,
    orte_buffer_t* buffer,
    orte_rml_tag_t tag,
    void *cbdata)
{
    if(0 > status) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
    }
	OBJ_RELEASE(buffer);
}

/* 
 * handle message from proxies
 */

void orte_gpr_replica_recv(int status, orte_process_name_t* sender,
			  orte_buffer_t *buffer, orte_rml_tag_t tag, void* cbdata)
{
    orte_buffer_t *answer;
    int rc;

    OPAL_TRACE(3);
    
    if (orte_gpr_replica_globals.debug) {
	   opal_output(0, "[%lu,%lu,%lu] gpr replica: received message from [%lu,%lu,%lu]",
			    ORTE_NAME_ARGS(orte_process_info.my_name), ORTE_NAME_ARGS(sender));
    }

    OPAL_THREAD_LOCK(&orte_gpr_replica_globals.mutex);

    if (ORTE_SUCCESS == orte_gpr_replica_process_command_buffer(buffer, sender, &answer)) {
        if (0 > orte_rml.send_buffer_nb(sender, answer, tag, 0, orte_gpr_replica_send_cb, NULL)) {
		   ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
	    }
	}

	if (orte_gpr_replica_globals.debug) {
	    opal_output(0, "gpr replica: msg processing complete - processing callbacks");
	}

    /* be sure to process callbacks */
    if (!orte_gpr_replica.processing_callbacks) {
        if (ORTE_SUCCESS != (rc = orte_gpr_replica_process_callbacks())) {
            ORTE_ERROR_LOG(rc);
        }
    }

    OPAL_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
    return;
}
