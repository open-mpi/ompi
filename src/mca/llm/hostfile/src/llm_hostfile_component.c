/* -*- C -*-
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "llm_hostfile.h"
#include "util/os_path.h"
#include "mca/llm/llm.h"
#include "mca/llm/base/base.h"
#include "mca/base/mca_base_param.h"


/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */
const mca_llm_base_component_1_0_0_t mca_llm_hostfile_component = {

  /* First, the mca_module_t struct containing meta information
     about the module itself */
  {
    /* Indicate that we are a llm v1.0.0 module (which also implies a
       specific MCA version) */
    MCA_LLM_BASE_VERSION_1_0_0,

    /* Module name and version */
    "hostfile",
    MCA_llm_hostfile_MAJOR_VERSION,
    MCA_llm_hostfile_MINOR_VERSION,
    MCA_llm_hostfile_RELEASE_VERSION,

    /* Module open and close functions */
    mca_llm_hostfile_component_open,
    mca_llm_hostfile_component_close
  },

  /* Next the MCA v1.0.0 module meta data */
  {
   /* Whether the module is checkpointable or not */
    false
  },

  /* Initialization / shutdown functions */
  mca_llm_hostfile_component_init,
  mca_llm_hostfile_component_finalize
};


int
mca_llm_hostfile_component_open(void)
{
    char *default_path = ompi_os_path(false, OMPI_SYSCONFDIR, 
                                      "llm_hostfile", NULL);

    int id = mca_base_param_register_string("llm",
                                            "hostfile",
                                            "hostfile",
                                            NULL,
                                            default_path);
    mca_base_param_lookup_string(id, &mca_llm_hostfile_filename);

    if (NULL != default_path) free(default_path);

    return OMPI_SUCCESS;
}


int
mca_llm_hostfile_component_close(void)
{
    return OMPI_SUCCESS;
}

