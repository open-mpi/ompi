/* -*- C -*-
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "llm_hostfile.h"

#include "include/constants.h"
#include "mca/llm/llm.h"
#include "mca/llm/base/base.h"


struct mca_llm_base_module_1_0_0_t mca_llm_hostfile_module = {
  mca_llm_hostfile_allocate_resources,
  mca_llm_hostfile_deallocate_resources
};

char *mca_llm_hostfile_filename = NULL;

struct mca_llm_base_module_1_0_0_t* 
mca_llm_hostfile_component_init(const char *active_pcm,
                                bool have_threads,
                                int *priority)
{
  *priority = 1;

  return &mca_llm_hostfile_module;
}


int
mca_llm_hostfile_component_finalize(void)
{
  return OMPI_SUCCESS;
}
