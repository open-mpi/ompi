/*
 * $HEADER$
 */

#ifndef MCA_PML_BASE_H
#define MCA_PML_BASE_H

#include "ompi_config.h"

#include "mca/mca.h"
#include "mca/pml/pml.h"


/*
 * Global functions for the PML
 */

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
OMPI_DECLSPEC  int mca_pml_base_open(void);
OMPI_DECLSPEC  int mca_pml_base_select(mca_pml_base_module_t *selected, 
                          bool *allow_multi_user_threads, 
                          bool *have_hidden_threads);
OMPI_DECLSPEC int mca_pml_base_close(void);


/*
 * Globals
 */
OMPI_DECLSPEC extern int mca_pml_base_output;
OMPI_DECLSPEC extern ompi_list_t mca_pml_base_components_available;
OMPI_DECLSPEC extern mca_pml_base_component_t mca_pml_base_selected_component;
OMPI_DECLSPEC extern mca_pml_base_module_t mca_pml;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif /* MCA_PML_BASE_H */
