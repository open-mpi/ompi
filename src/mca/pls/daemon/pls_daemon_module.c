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
 *
 * These symbols are in a file by themselves to provide nice linker
 * semantics.  Since linkers generally pull in symbols by object
 * files, keeping these symbols as the only symbols in this file
 * prevents utility programs such as "ompi_info" from having to import
 * entire components just to query their version and parameters.
 */

#include "ompi_config.h"

#include "include/orte_constants.h"
#include "mca/pls/pls.h"
#include "pls_daemon.h"


/*
 * Local functions
 */
static int pls_daemon_launch(orte_jobid_t jobid);
static int pls_daemon_terminate_job(orte_jobid_t jobid);
static int pls_daemon_terminate_proc(const orte_process_name_t *name);
static int pls_daemon_finalize(void);


orte_pls_base_module_1_0_0_t orte_pls_daemon_module = {
    pls_daemon_launch,
    pls_daemon_terminate_job,
    pls_daemon_terminate_proc,
    pls_daemon_finalize
};


static int pls_daemon_launch(orte_jobid_t jobid)
{
    return ORTE_ERR_NOT_IMPLEMENTED;
}


static int pls_daemon_terminate_job(orte_jobid_t jobid)
{
    return ORTE_ERR_NOT_IMPLEMENTED;
}


static int pls_daemon_terminate_proc(const orte_process_name_t *name)
{
    return ORTE_ERR_NOT_IMPLEMENTED;
}


static int pls_daemon_finalize(void)
{
    return ORTE_ERR_NOT_IMPLEMENTED;
}
