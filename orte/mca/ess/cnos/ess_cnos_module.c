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
 *
 */

#include "orte_config.h"
#include "orte/constants.h"

#include <catamount/cnos_mpi_os.h>

#include "orte/util/show_help.h"

#include "orte/mca/errmgr/base/base.h"
#include "orte/util/name_fns.h"
#include "orte/util/proc_info.h"
#include "orte/runtime/orte_globals.h"
#include "orte/mca/grpcomm/base/base.h"
#include "orte/runtime/runtime_internals.h"

#include "orte/mca/ess/ess.h"
#include "orte/mca/ess/base/base.h"
#include "orte/mca/ess/cnos/ess_cnos.h"

static int rte_init(char flags);
static int rte_finalize(void);
static void rte_abort(int status, bool report) __opal_attribute_noreturn__;
static bool proc_is_local(orte_process_name_t *proc);
static char* proc_get_hostname(orte_process_name_t *proc);
static uint32_t proc_get_arch(orte_process_name_t *proc);
static orte_local_rank_t proc_get_local_rank(orte_process_name_t *proc);
static orte_node_rank_t proc_get_node_rank(orte_process_name_t *proc);
static int update_arch(orte_process_name_t *proc, uint32_t arch);

orte_ess_base_module_t orte_ess_cnos_module = {
    rte_init,
    rte_finalize,
    rte_abort,
    proc_is_local,
    proc_get_hostname,
    proc_get_arch,
    proc_get_local_rank,
    proc_get_node_rank,
    update_arch,
    NULL /* ft_event */
};

static cnos_nidpid_map_t *map;

static int rte_init(char flags)
{
    int rc;
    int nprocs;

    orte_dt_init();
    
    /* Get our process information */
    
    /* Procs in this environment are directly launched. Hence, there
     * was no mpirun to create a jobid for us, and each app proc is
     * going to have to fend for itself. For now, we assume that the
     * jobid is some arbitrary number (say, 1).
     */
    ORTE_PROC_MY_NAME->jobid = 1;
    
    /*  find our vpid from cnos */
    ORTE_PROC_MY_NAME->vpid = (orte_vpid_t) cnos_get_rank();
    
    /* Get the number of procs in the job from cnos */
    orte_process_info.num_procs = (orte_std_cntr_t) cnos_get_size();
    
    /* Get the nid map */
    nprocs = cnos_get_nidpid_map(&map);
    if (nprocs <= 0) {
        opal_output(0, "%5d: cnos_get_nidpid_map() returned %d", 
                    cnos_get_rank(), nprocs);
        return ORTE_ERR_FATAL;
    }

    /* MPI_Init needs the grpcomm framework, so we have to init it */
    if (ORTE_SUCCESS != (rc = orte_grpcomm_base_open())) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    if (ORTE_SUCCESS != (rc = orte_grpcomm_base_select())) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* that's all we need here */
    return ORTE_SUCCESS;
}


static int rte_finalize(void)
{
    /* just cleanup the things we used */
    orte_grpcomm_base_close();
    
    /* clean out the global structures */
    orte_proc_info_finalize();
    
    return ORTE_SUCCESS;
}

static void rte_abort(int status, bool report)
{
    exit(status);
}

static bool proc_is_local(orte_process_name_t *proc)
{
    if (map[ORTE_PROC_MY_NAME->vpid].nid ==
        map[proc->vpid].nid) {
        return true;
    }
    
    return false;
}

static char* proc_get_hostname(orte_process_name_t *proc)
{
    static char hostname[128];
    snprintf(hostname, 128, "n%d", map[proc->vpid].nid);
    return hostname;
}

static uint32_t proc_get_arch(orte_process_name_t *proc)
{
    /* always homogeneous, so other side is always same as us */
    return orte_process_info.arch;
}

static int update_arch(orte_process_name_t *proc, uint32_t arch)
{
    return ORTE_SUCCESS;
}

static orte_local_rank_t proc_get_local_rank(orte_process_name_t *proc)
{
    /* RHC: someone more familiar with CNOS needs to
     * fix this to return the correct value
     */
    return 0;
}

static orte_node_rank_t proc_get_node_rank(orte_process_name_t *proc)
{
    /* RHC: someone more familiar with CNOS needs to
     * fix this to return the correct value
     */
    return 0;
}
