/*
 * Copyright (c) 2012      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#include "oshmem_config.h"

#include "oshmem/include/shmem.h"
#include "oshmem/runtime/runtime.h"

#include "orte/mca/odls/odls_types.h"
#include "orte/mca/errmgr/errmgr.h"
#include "opal/dss/dss.h"
#include "orte/mca/rml/rml.h"

#if WANT_PMI_SUPPORT
#include <pmi.h>
#endif

extern int inGlobalExit;

void globalexit(int status)
{
    inGlobalExit++;

    if ((ORTE_JOBID_INVALID != ORTE_PROC_MY_DAEMON->jobid) &&
        (0 != ORTE_PROC_MY_DAEMON->jobid) )
    {
        orte_errmgr.abort(status, NULL);
    }
    else
    {
#if WANT_PMI_SUPPORT
        PMI_Abort(status, NULL);
#endif /* WANT_PMI_SUPPORT */
        _exit(status);
    }
    
    oshmem_shmem_aborted = true;
    exit(status);
}
