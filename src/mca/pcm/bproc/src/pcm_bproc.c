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

#include "pcm_bproc.h"
#include "mca/pcm/pcm.h"
#include "mca/pcm/base/base.h"
#include "class/ompi_list.h"


ompi_list_t *
mca_pcm_bproc_allocate_resources(struct mca_pcm_base_module_1_0_0_t* me,
                               mca_ns_base_jobid_t jobid,
                               int nodes, int procs)
{
    return NULL;
}


int
mca_pcm_bproc_spawn_procs(struct mca_pcm_base_module_1_0_0_t* me,
                        mca_ns_base_jobid_t jobid, ompi_list_t *schedlist)
{
    return OMPI_SUCCESS;
}


int
mca_pcm_bproc_kill_proc(struct mca_pcm_base_module_1_0_0_t* me,
                      ompi_process_name_t *name, int flags)
{
    return OMPI_SUCCESS;
}


int
mca_pcm_bproc_kill_job(struct mca_pcm_base_module_1_0_0_t* me,
                     mca_ns_base_jobid_t jobid, int flags)
{
    return OMPI_SUCCESS;
}


int
mca_pcm_bproc_deallocate_resources(struct mca_pcm_base_module_1_0_0_t* me,
                                 mca_ns_base_jobid_t jobid,
                                 ompi_list_t *nodelist)
{
    return OMPI_SUCCESS;
}
