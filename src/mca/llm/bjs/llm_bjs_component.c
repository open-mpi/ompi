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
 */

#include "ompi_config.h"

#include "mca/llm/bjs/llm-bjs-version.h"
#include "llm_bjs.h"
#include "include/constants.h"
#include "util/os_path.h"
#include "mca/llm/llm.h"
#include "mca/llm/base/base.h"
#include "mca/base/mca_base_param.h"
#include "util/output.h"

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */
const mca_llm_base_component_1_0_0_t mca_llm_bjs_component = {

  /* First, the mca_module_t struct containing meta information
     about the module itself */
  {
    /* Indicate that we are a llm v1.0.0 module (which also implies a
       specific MCA version) */
    MCA_LLM_BASE_VERSION_1_0_0,

    /* Module name and version */
    "bjs",
    MCA_llm_bjs_MAJOR_VERSION,
    MCA_llm_bjs_MINOR_VERSION,
    MCA_llm_bjs_RELEASE_VERSION,

    /* Module open and close functions */
    mca_llm_bjs_component_open,
    mca_llm_bjs_component_close
  },

  /* Next the MCA v1.0.0 module meta data */
  {
   /* Whether the module is checkpointable or not */
    false
  },

  /* Initialization / shutdown functions */
  mca_llm_bjs_component_init,
};

/*
 * component variables handles
 */
static int param_priority;

int
mca_llm_bjs_component_open(void)
{
    param_priority = mca_base_param_register_int("llm",
                                                 "bjs",
                                                 "priority",
                                                 NULL,
                                                 0);

    return OMPI_SUCCESS;
}


int
mca_llm_bjs_component_close(void)
{
    return OMPI_SUCCESS;
}


struct mca_llm_base_module_1_0_0_t* 
mca_llm_bjs_component_init(const char *active_pcm,
                                bool have_threads,
                                int *priority)
{
    mca_llm_base_module_t *me;
    char *nodes;
    int nodes_len, i;

    mca_base_param_lookup_int(param_priority, priority);

    if (0 == *priority) return NULL;

    /* make sure we are running in BJS or a BJS-like system */
    nodes = getenv("NODES");
    if (NULL == nodes) return NULL;

    nodes_len = strlen(nodes);
    for (i = 0 ; i < nodes_len ; ++i) {
        /* make sure all we have are negative sign, number, or comma */
        if (! (nodes[i] == '-' || nodes[i] == ',' ||
               (nodes[i] >= '0' && nodes[i] <= '9'))) {
            ompi_output_verbose(10, 0, "found non-BJS friendly character %c", nodes[i]);
            return NULL;
        }
    }

    me = malloc(sizeof(mca_llm_base_module_t));
    if (NULL == me) return NULL;

    me->llm_allocate_resources = mca_llm_bjs_allocate_resources;
    me->llm_deallocate_resources = mca_llm_bjs_deallocate_resources;
    me->llm_finalize = mca_llm_bjs_finalize;

    return me;
}


int
mca_llm_bjs_finalize(mca_llm_base_module_t *me)
{
    if (NULL == me) return OMPI_ERR_BAD_PARAM;

    free(me);
    
    return OMPI_SUCCESS;
}
