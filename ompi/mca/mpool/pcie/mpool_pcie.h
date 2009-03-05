/*
 * Copyright (c) 2007      Los Alamos National Security, LLC.
 *                         All righs reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef MCA_MPOOL_PCIE_H
#define MCA_MPOOL_PCIE_H

#include "opal/class/opal_list.h"
#include "opal/event/event.h"
#include "ompi/class/ompi_free_list.h"
#include "ompi/mca/allocator/allocator.h"
#include "ompi/mca/mpool/mpool.h"

BEGIN_C_DECLS

struct mca_mpool_pcie_component_t {
  mca_mpool_base_component_t super;
  int    verbose;
};
typedef struct mca_mpool_pcie_component_t mca_mpool_pcie_component_t;

OMPI_MODULE_DECLSPEC extern mca_mpool_pcie_component_t mca_mpool_pcie_component;

struct mca_mpool_pcie_module_t {
  mca_mpool_base_module_t super;
  void* base;
  size_t offset;
  size_t len;
  
}; typedef struct mca_mpool_pcie_module_t mca_mpool_pcie_module_t; 


struct mca_mpool_base_resources_t {
  void *base;
  size_t len;
};
typedef struct mca_mpool_base_resources_t mca_mpool_base_resources_t;


/* 
 *  Initializes the mpool module. 
 */ 
void mca_mpool_pcie_module_init(mca_mpool_pcie_module_t* mpool); 


/*
 *  Returns base address of shared memory mapping.
 */
void* mca_mpool_pcie_base(mca_mpool_base_module_t*);


/**
  *  Allocate block of shared memory.
  */
void* mca_mpool_pcie_alloc(mca_mpool_base_module_t* mpool, 
                           size_t size, 
                           size_t align, 
                           uint32_t flags,
                           mca_mpool_base_registration_t** registration);


/**
  * realloc function typedef
  */
void* mca_mpool_pcie_realloc(mca_mpool_base_module_t* mpool, 
                             void* addr, 
                             size_t size, 
                             mca_mpool_base_registration_t** registration);


/**
  * free function typedef
  */
void mca_mpool_pcie_free(mca_mpool_base_module_t* mpool, 
                         void * addr, 
                         mca_mpool_base_registration_t* registration);


END_C_DECLS

#endif
