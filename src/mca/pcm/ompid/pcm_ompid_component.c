/* -*- C -*-
 * 
 * $HEADER$
 *
 */

#include "ompi_config.h"

#include "include/constants.h"
#include "include/types.h"
#include "util/output.h"
#include "mca/mca.h"
#include "mca/pcm/pcm.h"
#include "mca/pcm/base/base.h"
#include "mca/base/mca_base_param.h"
#include "pcm_ompid.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

/*
 * Struct of function pointers and all that to let us be initialized
 */
mca_pcm_base_component_1_0_0_t mca_pcm_ompid_component = {
  {
    MCA_PCM_BASE_VERSION_1_0_0,

    "ompid", /* MCA component name */
    1,  /* MCA component major version */
    0,  /* MCA component minor version */
    0,  /* MCA component release version */
    mca_pcm_ompid_open,  /* component open */
    mca_pcm_ompid_close /* component close */
  },
  {
    false /* checkpoint / restart */
  },
  mca_pcm_ompid_init,    /* component init */
  NULL                   /* unique name */
};


struct mca_pcm_base_module_1_0_0_t mca_pcm_ompid_1_0_0 = {
    mca_pcm_ompid_allocate_resources, /* allocate_resources */
    mca_pcm_ompid_spawn_procs, /* spawn_procs */
    NULL, /* kill_proc */
    NULL, /* kill_job */
    NULL,  /* deallocate_resources */
    mca_pcm_ompid_finalize
};


int mca_pcm_ompid_open(void)
{
   return OMPI_SUCCESS;
}


int mca_pcm_ompid_close(void)
{
  return OMPI_SUCCESS;
}


struct mca_pcm_base_module_1_0_0_t *
mca_pcm_ompid_init(
    int *priority, 
    bool have_threads,
    int constraints)
{
    return NULL;
}


int mca_pcm_ompid_finalize(struct mca_pcm_base_module_1_0_0_t* me)
{
  return OMPI_SUCCESS;
}

