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
#include "mca/pcm/base/base_data_store.h"
#include "mca/pcm/base/base_kill_track.h"
#include "class/ompi_list.h"
#include "runtime/runtime.h"
#include "runtime/runtime_types.h"
#include "runtime/ompi_rte_wait.h"
#include "util/show_help.h"

void
mca_pcm_bproc_monitor_cb(pid_t pid, int status, void *data)
{
    mca_pcm_bproc_module_t *me = (mca_pcm_bproc_module_t*) data;
    ompi_process_name_t **procs;
    size_t procs_len, i;
    ompi_rte_process_status_t *proc_status;
    int ret;

    ompi_output_verbose(10, mca_pcm_base_output,
                        "process %d exited with status %d", pid, status);

    ret = mca_pcm_base_data_store_get_procs(me->data_store, pid, &procs,
                                            &procs_len, true);
    if (ret != OMPI_SUCCESS) {
        ompi_show_help("help-mca-pcm-rsh.txt",
                       "spawn:no-process-record", true, pid, status);
        return;
    }

    /* unregister all the procs */
    for (i = 0 ; i < procs_len ; ++i) {
        proc_status = ompi_rte_get_process_status(procs[i]);
        if (NULL != proc_status) {
            proc_status->status_key = OMPI_PROC_KILLED;
            proc_status->exit_code = (ompi_exit_code_t)status;
            ompi_rte_set_process_status(proc_status, procs[i]);
        }
        free(procs[i]);
    }

    free(procs);
}
