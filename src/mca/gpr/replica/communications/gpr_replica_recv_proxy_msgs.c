/* -*- C -*-
 *
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
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

#include "util/output.h"
#include "util/proc_info.h"

#include "mca/errmgr/errmgr.h"
#include "mca/ns/ns_types.h"
#include "mca/rml/rml.h"

#include "gpr_replica_comm.h"

/* 
 * handle message from proxies
 */

void orte_gpr_replica_recv(int status, orte_process_name_t* sender,
			  orte_buffer_t *buffer, orte_rml_tag_t tag, void* cbdata)
{
    orte_buffer_t *answer;
    int rc;

    if (orte_gpr_replica_globals.debug) {
	   ompi_output(0, "[%d,%d,%d] gpr replica: received message from [%d,%d,%d]",
			    ORTE_NAME_ARGS(orte_process_info.my_name), ORTE_NAME_ARGS(sender));
    }

    if (ORTE_SUCCESS == orte_gpr_replica_process_command_buffer(buffer, sender, &answer)) {
        if (0 > orte_rml.send_buffer(sender, answer, tag, 0)) {
		   ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
	    }
	}

	OBJ_RELEASE(answer);
	if (orte_gpr_replica_globals.debug) {
	    ompi_output(0, "gpr replica: msg processing complete - processing callbacks");
	}

    /* be sure to process callbacks before returning */
    if (ORTE_SUCCESS != (rc = orte_gpr_replica_process_callbacks())) {
        ORTE_ERROR_LOG(rc);
    }

    /* reissue the non-blocking receive */
    orte_rml.recv_buffer_nb(ORTE_RML_NAME_ANY, ORTE_RML_TAG_GPR, 0, orte_gpr_replica_recv, NULL);

    return;
}
