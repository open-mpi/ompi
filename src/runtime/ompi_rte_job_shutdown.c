/*
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
 * The Open MPI general purpose registry - support functions.
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

int ompi_rte_job_shutdown(mca_ns_base_jobid_t jobid)
{
    ompi_list_t *recipients;
    ompi_buffer_t shutdown_msg;
    int return_code;

    recipients = OBJ_NEW(ompi_list_t);

    shutdown_msg = ompi_registry.get_shutdown_msg(jobid, recipients);
    ompi_registry.triggers_inactive(jobid);

    /* check to ensure there are recipients on list - error if not */
    if (0 < ompi_list_get_size(recipients)) {
	mca_oob_xcast(ompi_rte_get_self(), recipients, shutdown_msg, NULL);
	return_code = OMPI_SUCCESS;
    } else {
	return_code = OMPI_ERROR;
    }

    ompi_registry.cleanup_job(jobid);
    OBJ_RELEASE(recipients);

    return return_code;
}
