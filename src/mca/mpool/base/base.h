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
#include "mca/mpool/mpool.h"


struct mca_mpool_base_selected_module_t {
  ompi_list_item_t super;
  mca_mpool_base_component_t *mpool_component;
  mca_mpool_t *mpool_module;
};
typedef struct mca_mpool_base_selected_module_t mca_mpool_base_selected_module_t;

OBJ_CLASS_DECLARATION(mca_mpool_base_selected_module_t);

/*
 * Global functions for MCA: overall PTL open and close
 */

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
  int mca_mpool_base_open(void);
  int mca_mpool_base_init(bool *allow_multi_user_threads);
  int mca_mpool_base_close(void);
  mca_mpool_t* mca_mpool_lookup(const char* name);
  void* mca_mpool_base_is_registered(void* addr, size_t size);
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif


/*
 * Globals
 */
extern int mca_mpool_base_output;
extern ompi_list_t mca_mpool_base_modules_available;
extern ompi_list_t mca_mpool_base_modules_initialized;

#endif /* MCA_MEM_BASE_H */
