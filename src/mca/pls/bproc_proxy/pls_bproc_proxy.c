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
 *
 */

#include "ompi_config.h"


#include "mca/ns/base/base.h"
#include "mca/pls/base/base.h"
#include "pls_bproc_proxy.h"


int orte_pls_bproc_proxy_launch(orte_jobid_t jobid)
{
    return ORTE_ERROR;
}

int orte_pls_bproc_proxy_terminate_job(orte_jobid_t jobid)
{
    return ORTE_ERROR;
}

int orte_pls_bproc_proxy_terminate_proc(const orte_process_name_t* proc_name)
{
    return ORTE_ERROR;
}

int orte_pls_bproc_proxy_finalize(void)
{
    return ORTE_SUCCESS;
}


orte_pls_base_module_t orte_pls_bproc_proxy_module = {
    orte_pls_bproc_proxy_launch,
    orte_pls_bproc_proxy_terminate_job,
    orte_pls_bproc_proxy_terminate_proc,
    orte_pls_bproc_proxy_finalize
};

