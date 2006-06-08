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
#ifdef HAVE_SIGNAL_H
#include <signal.h>
#endif
#ifdef HAVE_CNOS_PM_BARRIER
#include <catamount/cnos_mpi_os.h>
#endif

#include "orte/orte_constants.h"
#include "orte/mca/rmgr/base/base.h"
#include "orte/mca/ns/ns.h"
#include "orte/util/proc_info.h"
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

static int orte_rmgr_cnos_signal_job(
        orte_jobid_t jobid);

static int orte_rmgr_cnos_signal_proc(
        const orte_process_name_t* proc_name);

static int orte_rmgr_cnos_spawn(
    orte_app_context_t** app_context,
    size_t num_context,
    orte_jobid_t* jobid,
    orte_rmgr_cb_fn_t cbfn,
    orte_proc_state_t cb_conditions);

static int orte_rmgr_cnos_finalize(void);

orte_rmgr_base_module_t orte_rmgr_cnos_module = {
    orte_rmgr_cnos_query,
    orte_rmgr_cnos_create,
    orte_rmgr_cnos_allocate,
    orte_rmgr_cnos_deallocate,
    orte_rmgr_cnos_map,
    orte_rmgr_cnos_launch,
    orte_rmgr_cnos_terminate_job,
    orte_rmgr_cnos_terminate_proc,
    orte_rmgr_cnos_signal_job,
    orte_rmgr_cnos_signal_proc,
    orte_rmgr_cnos_spawn,
    orte_rmgr_base_proc_stage_gate_init,
    orte_rmgr_base_proc_stage_gate_mgr,
    orte_rmgr_cnos_finalize
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

#ifdef HAVE_KILLRANK
#include "catamount/types.h"
/* secret sauce on the Cray machine */
extern int killrank(rank_t RANK, int SIG);
#endif

static int orte_rmgr_cnos_terminate_job(orte_jobid_t jobid)
{
#ifdef HAVE_KILLRANK
    orte_jobid_t my_jobid;

    orte_ns.get_jobid(&my_jobid, orte_process_info.my_name);

    /* make sure it's my job */
    if (jobid == my_jobid) {
        killrank(-1, SIGKILL);
    } else {
        return ORTE_ERR_NOT_SUPPORTED;
    }
#else
    exit(0);
#endif

    return ORTE_SUCCESS;
}

static int orte_rmgr_cnos_terminate_proc(const orte_process_name_t* proc_name)
{
#ifdef HAVE_KILLRANK
    orte_jobid_t my_jobid;
    orte_jobid_t his_jobid;
    orte_vpid_t his_vpid;

    orte_ns.get_jobid(&my_jobid, orte_process_info.my_name);
    orte_ns.get_jobid(&his_jobid, proc_name);

    orte_ns.get_vpid(&his_vpid, proc_name);

    /* make sure it's my job.  This may end up killing me, but what
       the heck. */
    if (his_jobid == my_jobid) {
        killrank((int) his_vpid, SIGKILL);
    } else {
        return ORTE_ERR_NOT_SUPPORTED;
    }
#else
    exit(0);
#endif

    return ORTE_SUCCESS;
}


static int orte_rmgr_cnos_signal_job(orte_jobid_t jobid)
{
    return ORTE_ERR_NOT_SUPPORTED;
}

static int orte_rmgr_cnos_signal_proc(const orte_process_name_t* proc_name)
{
    return ORTE_ERR_NOT_SUPPORTED;
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


static int orte_rmgr_cnos_finalize(void)
{
#ifdef HAVE_CNOS_PM_BARRIER
    /* register with the process manager so that everyone aborts if
       any one process aborts.  This is a bit slower than it needs to
       be, but useful. */
    cnos_pm_barrier(1);
#endif

    return ORTE_SUCCESS;
}
