/* -*- C -*-
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "llm_hostfile.h"

#include "mca/llm/llm.h"
#include "mca/llm/base/base.h"


struct mca_llm_base_module_1_0_0_t mca_llm_hostfile_module = {
  mca_llm_hostfile_allocate_resources,
  mca_llm_hostfile_deallocate_resources
};

char *mca_llm_hostfile_filename = NULL;

struct mca_llm_module_1_0_0_t* 
mca_llm_hostfile_component_init(const char *active_pcm,
                                int *priority, 
                                bool *allow_multiple_user_threads,
                                bool *have_hidden_threads)
{
  *allow_multiple_user_threads = true;
  *have_hidden_threads = false;
  *priority = 1;

  return &mca_llm_hostfile_module;
}


int
mca_llm_hostfile_component_finalize(void)
{
  return OMPI_SUCCESS;
}
