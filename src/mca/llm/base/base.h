/* -*- C -*-
 *
 * $HEADER$
 */

#ifndef MCA_LLM_BASE_H_
#define MCA_LLM_BASE_H_

#include "ompi_config.h"

#include "mca/mca.h"
#include "mca/llm/llm.h"


/*
 * Forward define 
 */

/*
 * Global functions for the LLM
 */

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
  int mca_llm_base_open(void);
  int mca_llm_base_select(const char *active_pcm,
                          mca_llm_base_module_t **selected, 
                          bool have_threads);
  int mca_llm_base_close(void);

/*
 * Globals
 */
extern int mca_llm_base_output;
extern ompi_list_t mca_llm_base_components_available;
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* MCA_LLM_BASE_H */
