/*
 * $HEADER$
 */
/**
 * @file
 */
#ifndef MCA_MEM_BASE_H
#define MCA_MEM_BASE_H

#include "ompi_config.h"

#include "class/ompi_list.h"
#include "mca/mca.h"
#include "mca/mem/mem.h"


struct mca_mem_base_selected_module_t {
  ompi_list_item_t super;
  mca_mem_base_module_t *pbsm_module;
  mca_mem_t *pbsm_actions;
};
typedef struct mca_mem_base_selected_module_t mca_mem_base_selected_module_t;


/*
 * Global functions for MCA: overall PTL open and close
 */

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
  int mca_mem_base_open(void);
  int mca_mem_base_select(bool *allow_multi_user_threads, 
                          bool *have_hidden_threads); 
  int mca_mem_base_close(void);
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif


/*
 * Globals
 */
extern int mca_mem_base_output;
extern ompi_list_t mca_mem_base_modules_available;
extern ompi_list_t mca_mem_base_modules_initialized;

#endif /* MCA_MEM_BASE_H */
