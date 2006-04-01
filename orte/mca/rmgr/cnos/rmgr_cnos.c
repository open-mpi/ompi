/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include "orte_config.h"

#include <stdlib.h>
#include <errno.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#ifdef HAVE_STRING_H
#include <string.h>
#endif  /* HAVE_STRING_H */

#include "orte/orte_constants.h"
#include "orte/mca/rmgr/base/base.h"
#include "rmgr_cnos.h"


static int orte_rmgr_cnos_query(void);

static int orte_rmgr_cnos_create(
    orte_app_context_t** app_context,
    size_t num_context,
    orte_jobid_t* jobid);

static int orte_rmgr_cnos_allocate(
    orte_jobid_t jobid);

static int orte_rmgr_cnos_deallocate(
    orte_jobid_t jobid);

static int orte_rmgr_cnos_map(
    orte_jobid_t jobid);

static int orte_rmgr_cnos_launch(
    orte_jobid_t jobid);

static int orte_rmgr_cnos_terminate_job(
    orte_jobid_t jobid);

static int orte_rmgr_cnos_terminate_proc(
    const orte_process_name_t* proc_name);

static int orte_rmgr_cnos_spawn(
    orte_app_context_t** app_context,
    size_t num_context,
    orte_jobid_t* jobid,
    orte_rmgr_cb_fn_t cbfn,
    orte_proc_state_t cb_conditions);

orte_rmgr_base_module_t orte_rmgr_cnos_module = {
    orte_rmgr_cnos_query,
    orte_rmgr_cnos_create,
    orte_rmgr_cnos_allocate,
    orte_rmgr_cnos_deallocate,
    orte_rmgr_cnos_map,
    orte_rmgr_cnos_launch,
    orte_rmgr_cnos_terminate_job,
    orte_rmgr_cnos_terminate_proc,
    orte_rmgr_cnos_spawn,
    orte_rmgr_base_proc_stage_gate_init,
    orte_rmgr_base_proc_stage_gate_mgr,
    NULL, /* finalize */
};


/*
 *  Create the job segment and initialize the application context.
 */
static int orte_rmgr_cnos_create(
    orte_app_context_t** app_context,
    size_t num_context,
    orte_jobid_t* jobid)
{
    return ORTE_ERR_NOT_SUPPORTED;
}

static int orte_rmgr_cnos_query(void)
{
    return ORTE_ERR_NOT_SUPPORTED;

}

static int orte_rmgr_cnos_allocate(orte_jobid_t jobid)
{
    return ORTE_ERR_NOT_SUPPORTED;
}

static int orte_rmgr_cnos_deallocate(orte_jobid_t jobid)
{
    return ORTE_ERR_NOT_SUPPORTED;
}

static int orte_rmgr_cnos_map(orte_jobid_t jobid)
{
    return ORTE_ERR_NOT_SUPPORTED;
}

static int orte_rmgr_cnos_launch(orte_jobid_t jobid)
{
    return ORTE_ERR_NOT_SUPPORTED;
}

static int orte_rmgr_cnos_terminate_job(orte_jobid_t jobid)
{
    abort();
    return ORTE_SUCCESS;
}

static int orte_rmgr_cnos_terminate_proc(const orte_process_name_t* proc_name)
{
    abort();
    return ORTE_SUCCESS;
}


static int orte_rmgr_cnos_spawn(
    orte_app_context_t** app_context,
    size_t num_context,
    orte_jobid_t* jobid,
    orte_rmgr_cb_fn_t cbfunc,
    orte_proc_state_t cb_conditions)
{
    return ORTE_ERR_NOT_SUPPORTED;
}



