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

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

struct mca_ptl_base_selected_module_t {
  ompi_list_item_t super;

  mca_ptl_base_component_t *pbsm_component;
  mca_ptl_base_module_t *pbsm_module;
};
typedef struct mca_ptl_base_selected_module_t mca_ptl_base_selected_module_t;


/*
 * Global functions for MCA: overall PTL open and close
 */

OMPI_DECLSPEC  int mca_ptl_base_open(void);
OMPI_DECLSPEC  int mca_ptl_base_select(bool *allow_multi_user_threads, 
                          bool *have_hidden_threads);
OMPI_DECLSPEC  int mca_ptl_base_close(void);


/*
 * Globals
 */
OMPI_DECLSPEC extern int mca_ptl_base_output;
OMPI_DECLSPEC extern char* mca_ptl_base_include;
OMPI_DECLSPEC extern char* mca_ptl_base_exclude;
OMPI_DECLSPEC extern ompi_list_t mca_ptl_base_components_opened;
OMPI_DECLSPEC extern ompi_list_t mca_ptl_base_modules_initialized;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif /* MCA_PTL_BASE_H */
