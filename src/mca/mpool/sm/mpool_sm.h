/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/**
 * @file
 */
#ifndef MCA_MPOOL_SM_H
#define MCA_MPOOL_SM_H

#include "class/ompi_list.h"
#include "class/ompi_free_list.h"
#include "event/event.h"
#include "mca/mpool/mpool.h"
#include "mca/allocator/allocator.h"
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

struct mca_mpool_sm_component_t {
  mca_mpool_base_component_t super;
  /*      mca_allocator_base_module_t* sm_allocator; */
  char*  sm_allocator_name;
  size_t sm_size;
  /*      struct mca_mpool_sm_mmap_t *sm_mmap; */
};
typedef struct mca_mpool_sm_component_t mca_mpool_sm_component_t;

OMPI_COMP_EXPORT extern mca_mpool_sm_component_t mca_mpool_sm_component;
/*  OMPI_COMP_EXPORT extern mca_mpool_base_module_t mca_mpool_sm_module; */



struct mca_mpool_sm_module_t {
  mca_mpool_base_module_t super;
  mca_allocator_base_module_t * sm_allocator; 
  struct mca_mpool_sm_mmap_t *sm_mmap; 
}; typedef struct mca_mpool_sm_module_t mca_mpool_sm_module_t; 

/* 
 *  Initializes the mpool module. 
 */ 
void mca_mpool_sm_module_init(mca_mpool_sm_module_t* mpool); 


/*
 *  Returns base address of shared memory mapping.
 */
void* mca_mpool_sm_base(mca_mpool_base_module_t*);

/**
  *  Allocate block of shared memory.
  */
void* mca_mpool_sm_alloc( 
    mca_mpool_base_module_t* mpool, 
    size_t size, 
    size_t align, 
    struct mca_bmi_base_registration_t** registration);

/**
  * realloc function typedef
  */
void* mca_mpool_sm_realloc(
    mca_mpool_base_module_t* mpool, 
    void* addr, 
    size_t size, 
    struct mca_bmi_base_registration_t** registration);

/**
  * free function typedef
  */
void mca_mpool_sm_free(
    mca_mpool_base_module_t* mpool, 
    void * addr, 
    struct mca_bmi_base_registration_t* registration);


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif















