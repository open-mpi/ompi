/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/** @file:
 *
 * 
 *
 */

/*
 * includes
 */

#include "ompi_config.h"

#include "mca/oob/oob.h"
#include "mca/oob/base/base.h"
#include "mca/ns/base/base.h"

#include "runtime/runtime.h"


int ompi_rte_job_startup(mca_ns_base_jobid_t jobid)
{
    ompi_list_t *recipients;
    ompi_buffer_t startup_msg;
    ompi_name_server_namelist_t *ptr;
    ompi_rte_process_status_t proc_status;
    int num_procs;

    if (ompi_rte_debug_flag) {
	ompi_output(0, "[%d,%d,%d] entered rte_job_startup",
		    OMPI_NAME_ARGS(*ompi_rte_get_self()));
    }

    recipients = OBJ_NEW(ompi_list_t);

    startup_msg = ompi_registry.get_startup_msg(jobid, recipients);
    ompi_registry.triggers_active(jobid);

    if (ompi_rte_debug_flag) {
	ompi_output(0, "[%d,%d,%d] rte_job_startup: sending startup message to %d recipients",
		    OMPI_NAME_ARGS(*ompi_rte_get_self()),
		    ompi_list_get_size(recipients));
    }

    /* check to ensure there are recipients on list - don't send if not */
    if (0 < (num_procs = (int)ompi_list_get_size(recipients))) {
	mca_oob_xcast(ompi_rte_get_self(), recipients, startup_msg, NULL);

        /* for each recipient, set process status to "running" */
	proc_status.status_key = OMPI_PROC_RUNNING;
	proc_status.exit_code = 0;
	while (NULL != (ptr = (ompi_name_server_namelist_t*)ompi_list_remove_first(recipients))) {
	    ompi_rte_set_process_status(&proc_status, ptr->name);
	}
    }

   OBJ_RELEASE(recipients);

    /* return number of processes started = number of recipients */
    return num_procs;

}
