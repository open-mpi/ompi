/*
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

#include "orte_config.h"

#include "orte/orte_constants.h"
#include "opal/mca/mca.h"
#include "orte/mca/rmgr/base/rmgr_private.h"


/*
 * "not available" functions
 */
int
orte_rmgr_base_setup_job_not_available(
    orte_app_context_t** app_context,
    orte_std_cntr_t num_context,
    orte_jobid_t* jobid, opal_list_t *attrs)
{
    return ORTE_ERR_UNREACH;
}

int
orte_rmgr_base_spawn_not_available(
    orte_app_context_t** app_context,
    orte_std_cntr_t num_context,
    orte_jobid_t* jobid,
    orte_std_cntr_t num_connect,
    orte_process_name_t *connect,
    orte_rmgr_cb_fn_t cbfn,
    orte_proc_state_t cb_conditions,
    opal_list_t *attributes)
{
    return ORTE_ERR_UNREACH;
}

int
orte_rmgr_base_finalize_not_available(void)
{
    return ORTE_ERR_UNREACH;
}
