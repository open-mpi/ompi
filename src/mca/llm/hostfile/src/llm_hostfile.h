/* -*- C -*-
 *
 * $HEADER$
 */

#ifndef LLM_HOSTFILE_H
#define LLM_HOSTFILE_H

#include "ompi_config.h"

#include "mca/mca.h"
#include "mca/llm/llm.h"
#include "mca/ns/ns.h"

/*
 * Globally exported variable
 */
extern const mca_llm_base_component_1_0_0_t mca_llm_hostfile_component;


extern char *mca_llm_hostfile_filename;

/*
 * llm API functions
 */
#ifdef __cplusplus
extern "C" {
#endif

    int mca_llm_hostfile_component_open(void);
    int mca_llm_hostfile_component_close(void);

    struct mca_llm_base_module_1_0_0_t* 
    mca_llm_hostfile_component_init(const char *active_pcm,
                                    bool have_threads,
                                    int *priority);

    int mca_llm_hostfile_component_finalize(void);

    ompi_list_t* mca_llm_hostfile_allocate_resources(mca_ns_base_jobid_t jobid,
                                                     int nodes,
                                                     int procs);

    int mca_llm_hostfile_deallocate_resources(mca_ns_base_jobid_t jobid,
                                              ompi_list_t *nodelist);

#ifdef __cplusplus
}
#endif

#endif
