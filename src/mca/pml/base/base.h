/*
 * $HEADER$
 */

#ifndef MCA_PML_BASE_H
#define MCA_PML_BASE_H

#include "lam_config.h"

#include "mca/mca.h"
#include "mca/pml/pml.h"


/*
 * Global functions for the PML
 */

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
  int mca_pml_base_open(void);
  int mca_pml_base_select(mca_pml_t *selected, 
                          bool *allow_multi_user_threads, 
                          bool *have_hidden_threads);
  int mca_pml_base_close(void);
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif


/*
 * Globals
 */
extern int mca_pml_base_output;
extern lam_list_t mca_pml_base_modules_available;
extern mca_pml_base_module_t mca_pml_base_selected_module;
extern mca_pml_t mca_pml;

#endif /* MCA_PML_BASE_H */
