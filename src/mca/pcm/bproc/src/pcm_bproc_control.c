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
#include <signal.h>

#include "pcm_bproc.h"
#include "mca/pcm/pcm.h"
#include "mca/pcm/base/base.h"
#include "class/ompi_list.h"
#include "mca/pcm/base/base_job_track.h"
#include "mca/ns/ns.h"
#include "mca/ns/base/base.h"


int
mca_pcm_bproc_kill_proc(struct mca_pcm_base_module_1_0_0_t* me_super,
                      ompi_process_name_t *name, int flags)
{
  mca_pcm_bproc_module_t *me = (mca_pcm_bproc_module_t*) me_super;
  pid_t doomed;

  if (NULL == me) return OMPI_ERR_BAD_PARAM;
  if (NULL == name) return OMPI_ERR_BAD_PARAM;

    doomed = mca_pcm_base_job_list_get_starter(me->jobs, 
                                               mca_ns_base_get_jobid(name), 
                                               mca_ns_base_get_vpid(name),
					       true);
    if (doomed > 0) {
        kill(doomed, SIGTERM);
    } else {
        return OMPI_ERR_NOT_FOUND;
    }

    return OMPI_SUCCESS;
}


int
mca_pcm_bproc_kill_job(struct mca_pcm_base_module_1_0_0_t* me_super,
                     mca_ns_base_jobid_t jobid, int flags)
{
    mca_pcm_bproc_module_t *me = (mca_pcm_bproc_module_t*) me_super;
    pid_t *doomed;
    size_t doomed_len, i;
    int ret;

    if (NULL == me) return OMPI_ERR_BAD_PARAM;
    /* check for invalid jobid */
    
    ret = mca_pcm_base_job_list_get_starters(me->jobs,
                                             jobid, &doomed, &doomed_len, 
                                             true);
    if (OMPI_SUCCESS != ret) return ret;

    for (i = 0 ; i < doomed_len ; ++i) {
        kill(doomed[i], SIGTERM);
    }

    if (NULL != doomed) {
        free(doomed);
    }

    return OMPI_SUCCESS;
}
