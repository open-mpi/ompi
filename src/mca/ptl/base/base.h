/*
 * $HEADER$
 */
/**
 * @file
 */
#ifndef MCA_PTL_BASE_H
#define MCA_PTL_BASE_H

#include "ompi_config.h"

#include "class/ompi_list.h"
#include "mca/mca.h"
#include "mca/ptl/ptl.h"


struct mca_ptl_base_selected_module_t {
  ompi_list_item_t super;

  mca_ptl_base_component_t *pbsm_component;
  mca_ptl_base_module_t *pbsm_module;
};
typedef struct mca_ptl_base_selected_module_t mca_ptl_base_selected_module_t;


/*
 * Global functions for MCA: overall PTL open and close
 */

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
  int mca_ptl_base_open(void);
  int mca_ptl_base_select(bool *allow_multi_user_threads, 
                          bool *have_hidden_threads);
  int mca_ptl_base_close(void);
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif


/*
 * Globals
 */
extern int mca_ptl_base_output;
extern ompi_list_t mca_ptl_base_components_available;
extern ompi_list_t mca_ptl_base_components_initialized;

#endif /* MCA_PTL_BASE_H */
