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
 * Shutdown a job and cleanup the registry
 *
 */

/*
 * includes
 */

#include "ompi_config.h"

#include "mca/ns/base/base.h"
#include "mca/gpr/base/base.h"

#include "runtime/runtime.h"

int ompi_rte_job_shutdown(mca_ns_base_jobid_t jobid)
{
    ompi_registry.triggers_inactive(jobid);
    ompi_registry.cleanup_job(jobid);

    return OMPI_SUCCESS;
}
