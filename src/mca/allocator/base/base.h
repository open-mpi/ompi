/*
 * $HEADER$
 */
/**
 * @file
 */
#ifndef MCA_ALLOCATOR_BASE_H
#define MCA_ALLOCATOR_BASE_H

#include "ompi_config.h"

#include "class/ompi_list.h"
#include "mca/mca.h"
#include "mca/allocator/allocator.h"


struct mca_allocator_base_selected_module_t {
  ompi_list_item_t super;
  mca_allocator_base_module_t *allocator_component;
  mca_allocator_t *allocator_module;
};
typedef struct mca_allocator_base_selected_module_t mca_allocator_base_selected_module_t;
                                                                                                     
OBJ_CLASS_DECLARATION(mca_mpool_base_selected_module_t);
                                                                                                     

/*
 * Global functions for MCA: overall PTL open and close
 */

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
  int mca_allocator_base_open(void);
  int mca_allocator_base_close(void);
  mca_allocator_base_module_t* mca_allocator_component_lookup(const char* name);
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif


/*
 * Globals
 */

extern ompi_list_t mca_allocator_base_components;

#endif /* MCA_ALLOCATOR_BASE_H */
