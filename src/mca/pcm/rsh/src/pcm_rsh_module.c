/* -*- C -*-
 * 
 * $HEADER$
 *
 */

#include "ompi_config.h"

#include "include/constants.h"
#include "include/types.h"
#include "util/malloc.h"
#include "class/ompi_list.h"
#include "mca/mca.h"
#include "mca/base/mca_base_param.h"
#include "mca/pcm/pcm.h"
#include "mca/pcm/rsh/src/pcm_rsh.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

/*
 * Struct of function pointers and all that to let us be initialized
 */
mca_pcm_base_component_1_0_0_t mca_pcm_rsh_component = {
  {
    MCA_PCM_BASE_VERSION_1_0_0,

    "rsh", /* MCA component name */
    1,  /* MCA component major version */
    0,  /* MCA component minor version */
    0,  /* MCA component release version */
    mca_pcm_rsh_open,  /* component open */
    mca_pcm_rsh_close /* component close */
  },
  {
    false /* checkpoint / restart */
  },
  mca_pcm_rsh_init,    /* component init */
  mca_pcm_rsh_finalize
};


int
mca_pcm_rsh_open(void)
{
  return OMPI_SUCCESS;
}


int
mca_pcm_rsh_close(void)
{
  return OMPI_SUCCESS;
}


struct mca_pcm_base_module_1_0_0_t*
mca_pcm_rsh_init(int *priority, bool *allow_multi_user_threads, 
                  bool *have_hidden_threads)
{
  return NULL;
}


int
mca_pcm_rsh_finalize(void)
{
  return OMPI_SUCCESS;
}

