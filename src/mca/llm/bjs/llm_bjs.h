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

#ifndef LLM_BJS_H
#define LLM_BJS_H

#include "ompi_config.h"

#include "mca/mca.h"
#include "mca/llm/llm.h"
#include "mca/ns/ns.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
/*
 * Globally exported variable
 */
OMPI_COMP_EXPORT extern const mca_llm_base_component_1_0_0_t mca_llm_bjs_component;

/*
 * llm API functions
 */

    int mca_llm_bjs_component_open(void);
    int mca_llm_bjs_component_close(void);

    struct mca_llm_base_module_1_0_0_t* 
    mca_llm_bjs_component_init(const char *active_pcm,
                                    bool have_threads,
                                    int *priority);

    int mca_llm_bjs_finalize(mca_llm_base_module_t *me);

    ompi_list_t*
    mca_llm_bjs_allocate_resources(mca_llm_base_module_t *me,
                                        mca_ns_base_jobid_t jobid,
                                        int nodes,
                                        int procs);

    int mca_llm_bjs_deallocate_resources(mca_llm_base_module_t *me,
                                              mca_ns_base_jobid_t jobid,
                                              ompi_list_t *nodelist);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif
