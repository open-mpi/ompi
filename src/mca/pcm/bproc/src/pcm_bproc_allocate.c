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

#include "pcm_bproc.h"
#include "mca/pcm/pcm.h"
#include "mca/pcm/base/base.h"
#include "class/ompi_list.h"


ompi_list_t *
mca_pcm_bproc_allocate_resources(struct mca_pcm_base_module_1_0_0_t* me_super,
                               mca_ns_base_jobid_t jobid,
                               int nodes, int procs)
{
  mca_pcm_bproc_module_t *me = (mca_pcm_bproc_module_t*) me_super;

  if (NULL == me) {
    errno = OMPI_ERR_BAD_PARAM;
    return NULL;
  }
  /* since we are using llm, don't worry about the other params */

    return me->llm->llm_allocate_resources(me->llm, jobid, nodes, procs);;
}


int
mca_pcm_bproc_deallocate_resources(struct mca_pcm_base_module_1_0_0_t* me_super,
                                 mca_ns_base_jobid_t jobid,
                                 ompi_list_t *nodelist)
{
  mca_pcm_bproc_module_t *me = (mca_pcm_bproc_module_t*) me_super;

  if (NULL == me) return OMPI_ERR_BAD_PARAM;
  /* since we are using llm, don't worry about the other params */

  return me->llm->llm_deallocate_resources(me->llm, jobid, nodelist);
}
