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
 */

#include "orte_config.h"

#include "include/orte_constants.h"
#include "mca/mca.h"
#include "mca/rmgr/base/base.h"


/*
 * "not available" functions
 */
int
orte_rmgr_base_create_not_available(
    orte_app_context_t** app_context, 
    size_t num_context, 
    orte_jobid_t* jobid)
{
    return ORTE_ERR_UNREACH;
}

int
orte_rmgr_base_query_not_available(void)
{
    return ORTE_ERR_UNREACH;
}

int
orte_rmgr_base_allocate_not_available(orte_jobid_t jobid)
{
    return ORTE_ERR_UNREACH;
}

int
orte_rmgr_base_deallocate_not_available(orte_jobid_t jobid)
{
    return ORTE_ERR_UNREACH;
}

int
orte_rmgr_base_map_not_available(orte_jobid_t jobid)
{
    return ORTE_ERR_UNREACH;
}

int
orte_rmgr_base_launch_not_available(orte_jobid_t jobid)
{
    return ORTE_ERR_UNREACH;
}

int
orte_rmgr_base_terminate_job_not_available(orte_jobid_t jobid)
{
    return ORTE_ERR_UNREACH;
}

int
orte_rmgr_base_terminate_proc_not_available(const orte_process_name_t* proc_name)
{
    return ORTE_ERR_UNREACH;
}

int
orte_rmgr_base_spawn_not_available(
    orte_app_context_t** app_context, 
    size_t num_context, 
    orte_jobid_t* jobid,
    orte_rmgr_cb_fn_t cbfn)
{
    return ORTE_ERR_UNREACH;
}

int
orte_rmgr_base_finalize_not_available(void)
{
    return ORTE_ERR_UNREACH;
}
