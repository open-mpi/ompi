/* -*- C -*-
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "mca/llm/hostfile/llm-hostfile-version.h"
#include "llm_hostfile.h"
#include "include/constants.h"
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
};

/*
 * component variables handles
 */
static int param_filename;
static int param_filename_deprecated;
static int param_priority;

int
mca_llm_hostfile_component_open(void)
{
    char *default_path = ompi_os_path(false, OMPI_SYSCONFDIR, 
                                      "openmpi-default-hostfile", NULL);
    /* accept either OMPI_MCA_llm_hostfile_hostfile or
       OMPI_MCA_hostfile */
    param_filename = mca_base_param_register_string("llm",
                                                    "hostfile",
                                                    "hostfile",
                                                    "hostfile",
                                                    default_path);
    if (NULL != default_path) free(default_path);

    param_priority = mca_base_param_register_int("llm",
                                                 "hostfile",
                                                 "priority",
                                                 NULL,
                                                 1);

    return OMPI_SUCCESS;
}


int
mca_llm_hostfile_component_close(void)
{
    return OMPI_SUCCESS;
}


struct mca_llm_base_module_1_0_0_t* 
mca_llm_hostfile_component_init(const char *active_pcm,
                                bool have_threads,
                                int *priority)
{
    mca_llm_hostfile_module_t *me;
    

    mca_base_param_lookup_int(param_priority, priority);

    if (0 == *priority) return NULL;

    me = malloc(sizeof(mca_llm_hostfile_module_t));
    if (NULL == me) return NULL;

    /* fill in the struct */
    mca_base_param_lookup_string(param_filename,
                                 &(me->hostfile_filename));

    me->super.llm_allocate_resources = mca_llm_hostfile_allocate_resources;
    me->super.llm_deallocate_resources = mca_llm_hostfile_deallocate_resources;
    me->super.llm_finalize = mca_llm_hostfile_finalize;

    return (mca_llm_base_module_t*) me;
}


int
mca_llm_hostfile_finalize(mca_llm_base_module_t *me_base)
{
    mca_llm_hostfile_module_t *me = (mca_llm_hostfile_module_t*) me_base;

    if (NULL == me) return OMPI_ERR_BAD_PARAM;

    if (NULL != me->hostfile_filename) free(me->hostfile_filename);
    free(me);
    
    return OMPI_SUCCESS;
}
