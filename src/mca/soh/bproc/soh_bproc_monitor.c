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

#include <errno.h>
#include <unistd.h>

#include "pcm_bproc.h"
#include "mca/pcm/pcm.h"
#include "mca/pcm/base/base.h"
#include "class/ompi_list.h"
#include "runtime/runtime.h"
#include "runtime/runtime_types.h"
#include "runtime/ompi_rte_wait.h"
#include "util/show_help.h"
#include "mca/pcm/base/base_kill_track.h"
#include "mca/pcm/base/base_job_track.h"

void
mca_pcm_bproc_monitor_cb(pid_t pid, int status, void *data)
{
    mca_ns_base_jobid_t jobid = 0;
    mca_ns_base_vpid_t upper = 0;
    mca_ns_base_vpid_t lower = 0;
    mca_ns_base_vpid_t i = 0;
    int ret;
    ompi_process_name_t *proc_name;
    mca_pcm_bproc_module_t *me = (mca_pcm_bproc_module_t*) data;
    ompi_rte_process_status_t proc_status;
                                                                                
    printf("pcm: bproc: process %d exited with status %d\n", pid, status);

    ret = mca_pcm_base_job_list_get_job_info(me->jobs, pid, &jobid,
                                             &lower, &upper, true);
    if (ret != OMPI_SUCCESS) {
        ompi_show_help("help-mca-pcm-bproc.txt",
                       "spawn:no-process-record", true, pid, status);
        return;
    }
                                                                                
    /* unregister all the procs */
    proc_status.status_key = OMPI_PROC_KILLED;
    proc_status.exit_code = (ompi_exit_code_t)status;
    for (i = lower ; i <= upper ; ++i) {
        proc_name = mca_ns_base_create_process_name(0, jobid, i);
        ompi_rte_set_process_status(&proc_status, proc_name);
        free(proc_name);
    }
                                                                                
    mca_pcm_base_kill_unregister((mca_pcm_base_module_t*)me, jobid, lower, upper);
}
