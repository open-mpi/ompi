/*
 * $HEADER$
 */

#ifndef MCA_PTL_BASE_H
#define MCA_PTL_BASE_H

#include "lam_config.h"

#include "lam/lfc/list.h"
#include "mca/mca.h"
#include "mca/mpi/ptl/ptl.h"


struct mca_ptl_base_selected_module_t {
  lam_list_item_t super;

  mca_ptl_base_module_t *pbsm_module;
  mca_ptl_t *pbsm_actions;
};
typedef struct mca_ptl_base_selected_module_t mca_ptl_base_selected_module_t;


/*
 * Global functions for MCA: overall PTL open and close
 */

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
  int mca_ptl_base_open(void);
  int mca_ptl_base_select(void);
  int mca_ptl_base_close(void);
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif


/*
 * Globals
 */
extern int mca_ptl_base_output;
extern lam_list_t mca_ptl_base_modules_available;
extern lam_list_t mca_ptl_base_modules_initialized;

#endif /* MCA_PTL_BASE_H */
