/* -*- C -*-
 * 
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 */

#include "ompi_config.h"

#include <sys/types.h>
#include <signal.h>
#include <errno.h>

#include "pcm_rsh.h"
#include "include/constants.h"
#include "runtime/runtime.h"
#include "mca/pcm/pcm.h"
#include "mca/pcm/base/base_job_track.h"


int
mca_pcm_rsh_kill_proc(struct mca_pcm_base_module_1_0_0_t* me_super,
                      ompi_process_name_t *name, int flags)
{
    pid_t pid;
    mca_pcm_rsh_module_t *me = (mca_pcm_rsh_module_t*) me_super;

    if (0 != (OMPI_RTE_SPAWN_HIGH_QOS & me->constraints)) {
        pid = mca_pcm_base_job_list_get_starter(me->jobs,
                                                mca_ns_base_get_jobid(name),
                                                mca_ns_base_get_vpid(name),
                                                false);
        if (pid <= 0) return errno;

        kill(pid, SIGTERM);
    } else {
        return OMPI_ERR_NOT_IMPLEMENTED;
    }

    return OMPI_SUCCESS;
}


int
mca_pcm_rsh_kill_job(struct mca_pcm_base_module_1_0_0_t* me_super, 
                     mca_ns_base_jobid_t jobid, int flags)
{
    pid_t *pids;
    size_t pids_len, i;
    int ret;
    mca_pcm_rsh_module_t *me = (mca_pcm_rsh_module_t*) me_super;

    if (0 != (OMPI_RTE_SPAWN_HIGH_QOS &me->constraints)) {
        ret = mca_pcm_base_job_list_get_starters(me->jobs,
                                                 jobid, 
                                                 &pids, &pids_len, 
                                                 false);
        if (ret != OMPI_SUCCESS) return ret;

        for (i = 0 ; i < pids_len ; ++i) {
            kill(pids[i], SIGTERM);
        }

    } else {
        return OMPI_ERR_NOT_IMPLEMENTED;
    }

    return OMPI_SUCCESS;
}
