/*
 * $HEADER$
 */


#include "ompi_config.h"

#include "include/constants.h"
#include "include/types.h"
#include "mca/mca.h"
#include "mca/pcm/pcm.h"
#include "mca/pcm/seed/pcm_seed.h"
#include "util/proc_info.h"
#include "mca/base/mca_base_param.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

/*
 * Struct of function pointers and all that to let us be initialized
 */
mca_pcm_base_component_1_0_0_t mca_pcm_seed_component = {
  {
    MCA_PCM_BASE_VERSION_1_0_0,

    "seed", /* MCA component name */
    1,  /* MCA component major version */
    0,  /* MCA component minor version */
    0,  /* MCA component release version */
    mca_pcm_seed_open,  /* component open */
    mca_pcm_seed_close /* component close */
  },
  {
    false /* checkpoint / restart */
  },
  mca_pcm_seed_init,    /* component init */
  mca_pcm_seed_finalize
};


struct mca_pcm_base_module_1_0_0_t mca_pcm_seed_1_0_0 = {
    mca_pcm_seed_get_unique_name,
    NULL, /* allocate_resources */
    NULL, /* register_monitor */
    mca_pcm_seed_can_spawn,
    NULL, /* spawn_procs */
    mca_pcm_seed_get_peers,
    mca_pcm_seed_get_self,
    NULL, /* kill_proc */
    NULL, /* kill_job */
    NULL  /* deallocate_resources */
};

char *mca_pcm_seed_unique_name;
static int mca_pcm_seed_unique_name_param;


int
mca_pcm_seed_open(void)
{
    mca_base_param_register_string("pcm", "seed", "unique", NULL, "0");
    return OMPI_SUCCESS;
}


int
mca_pcm_seed_close(void)
{
  return OMPI_SUCCESS;
}


struct mca_pcm_base_module_1_0_0_t *
mca_pcm_seed_init(int *priority, bool *allow_multiple_user_threads, 
                  bool *have_hidden_threads)
{
    if (!ompi_process_info.seed) {
        *priority = 0;
        return NULL;
    }

    mca_base_param_lookup_string(mca_pcm_seed_unique_name_param,
                                 &mca_pcm_seed_unique_name);

    *priority = 100;
    *allow_multiple_user_threads = true;
    *have_hidden_threads = false;
    
    return &mca_pcm_seed_1_0_0;
}


int
mca_pcm_seed_finalize(void)
{
  return OMPI_SUCCESS;
}


