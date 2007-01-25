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
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

#ifdef HAVE_CNOS_PM_BARRIER
#include <catamount/cnos_mpi_os.h>
#endif

#include "orte/orte_constants.h"
#include "orte/mca/rmgr/base/base.h"
#include "orte/mca/ns/ns.h"
#include "orte/util/proc_info.h"
#include "pls_cnos.h"


static int orte_pls_cnos_launch_job(orte_jobid_t jobid);
static int orte_pls_cnos_terminate_job(orte_jobid_t jobid, struct timeval *timeout, opal_list_t *attrs);
static int orte_pls_cnos_terminate_orteds(orte_jobid_t jobid, struct timeval *timeout, opal_list_t *attrs);
static int orte_pls_cnos_terminate_proc(const orte_process_name_t* proc_name);
static int orte_pls_cnos_signal_job(orte_jobid_t jobid, int32_t signal, opal_list_t *attrs);
static int orte_pls_cnos_signal_proc(const orte_process_name_t* proc_name, int32_t signal);
static int orte_pls_cnos_finalize(void);
static int orte_pls_cnos_cancel_operation(void);


orte_pls_base_module_t orte_pls_cnos_module = {
    orte_pls_cnos_launch_job,
    orte_pls_cnos_terminate_job,
    orte_pls_cnos_terminate_orteds,
    orte_pls_cnos_terminate_proc,
    orte_pls_cnos_signal_job,
    orte_pls_cnos_signal_proc,
    orte_pls_cnos_cancel_operation,
    orte_pls_cnos_finalize
};


static int orte_pls_cnos_launch_job(orte_jobid_t jobid)
{
    return ORTE_ERR_NOT_SUPPORTED;
}

#ifdef HAVE_KILLRANK
#include "catamount/types.h"
/* secret sauce on the Cray machine */
extern int killrank(rank_t RANK, int SIG);
#endif

static int orte_pls_cnos_terminate_job(orte_jobid_t jobid, struct timeval *timeout, opal_list_t *attrs)
{
    orte_jobid_t my_jobid = ORTE_PROC_MY_NAME->jobid;

    /* make sure it's my job */
    if (jobid == my_jobid) {
#ifdef HAVE_KILLRANK
        killrank(-1, SIGKILL);
#else
        exit(0);
#endif
    }

    return ORTE_ERR_NOT_SUPPORTED;
}


static int orte_pls_cnos_terminate_orteds(orte_jobid_t jobid, struct timeval *timeout, opal_list_t *attrs)
{
    orte_jobid_t my_jobid = ORTE_PROC_MY_NAME->jobid;
    
    /* make sure it's my job */
    if (jobid == my_jobid) {
#ifdef HAVE_KILLRANK
        killrank(-1, SIGKILL);
#else
        exit(0);
#endif
    }

    return ORTE_ERR_NOT_SUPPORTED;
}

static int orte_pls_cnos_terminate_proc(const orte_process_name_t* proc_name)
{
    orte_jobid_t my_jobid = ORTE_PROC_MY_NAME->jobid;
    orte_jobid_t his_jobid = proc_name->jobid;
    orte_vpid_t his_vpid = proc_name->vpid;

    /* make sure it's my job.  This may end up killing me, but what
       the heck. */
    if (his_jobid == my_jobid) {
#ifdef HAVE_KILLRANK
        killrank((int) his_vpid, SIGKILL);
#else
        exit(0);
#endif
    }

    return ORTE_ERR_NOT_SUPPORTED;
}


static int orte_pls_cnos_signal_job(orte_jobid_t jobid, int32_t signal, opal_list_t *attrs)
{
    return ORTE_ERR_NOT_SUPPORTED;
}

static int orte_pls_cnos_signal_proc(const orte_process_name_t* proc_name, int32_t signal)
{
    return ORTE_ERR_NOT_SUPPORTED;
}

int orte_pls_cnos_cancel_operation(void)
{
    return ORTE_ERR_NOT_SUPPORTED;
}

static int orte_pls_cnos_finalize(void)
{
    return ORTE_SUCCESS;
}
