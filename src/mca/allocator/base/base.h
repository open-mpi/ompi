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

/**
 * Structure which describes a selected module.
 */
struct mca_allocator_base_selected_module_t {
  ompi_list_item_t super;    /**< Makes this an object of type ompi_list_item */
  mca_allocator_base_module_t *allocator_component; /**< Info about the module */
  mca_allocator_t *allocator_module; /**< The function pointers for all the module's functions. */
};
/**
 * Convenience typedef.
 */
typedef struct mca_allocator_base_selected_module_t mca_allocator_base_selected_module_t;

/**
 * Declaces mca_mpool_base_selected_module_t as a class.
 */
OBJ_CLASS_DECLARATION(mca_mpool_base_selected_module_t);
                                                                                                     

/*
 * Global functions for MCA: overall allocator open and close
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
/**
 * The list of all the selected components.
 */
extern ompi_list_t mca_allocator_base_components;

#endif /* MCA_ALLOCATOR_BASE_H */
