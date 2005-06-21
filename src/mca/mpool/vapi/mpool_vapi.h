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
#ifndef MCA_MPOOL_VAPI_H
#define MCA_MPOOL_VAPI_H

#include "class/ompi_list.h"
#include "class/ompi_free_list.h"
#include "event/event.h"
#include "mca/mpool/mpool.h"
#include "mca/allocator/allocator.h"
#include "mca/common/vapi/vapi_mem_reg.h" 

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif


static inline  void * DOWN_ALIGN_ADDR(void * addr, uint32_t cnt) { 
    return (void*)((MT_virt_addr_t)(addr) & (~((MT_virt_addr_t)0) << (cnt))); 
}

static inline void*  ALIGN_ADDR(void* addr, uint32_t cnt ) { 
    DOWN_ALIGN_ADDR(((addr) +  ~(~((MT_virt_addr_t)0) << (cnt))), (cnt)); 
    return addr;
} 


struct mca_mpool_vapi_component_t {
    mca_mpool_base_component_t super;
    char*  vapi_allocator_name;
    long page_size; 
    long page_size_log; 
};

typedef struct mca_mpool_vapi_component_t mca_mpool_vapi_component_t;

OMPI_COMP_EXPORT extern mca_mpool_vapi_component_t mca_mpool_vapi_component;

struct mca_mpool_vapi_module_t {
  mca_mpool_base_module_t super;
  mca_allocator_base_module_t * vapi_allocator; 
  mca_bmi_base_resources_t  hca_pd;
  mca_bmi_base_registration_t mem_hndl; 
}; typedef struct mca_mpool_vapi_module_t mca_mpool_vapi_module_t; 

/* 
 *  Initializes the mpool module. 
 */ 
void mca_mpool_vapi_module_init(mca_mpool_vapi_module_t* mpool); 


/*
 *  Returns base address of shared memory mapping.
 */
void* mca_mpool_vapi_base(mca_mpool_base_module_t*);

/**
  *  Allocate block of shared memory.
  */
void* mca_mpool_vapi_alloc( 
    mca_mpool_base_module_t* mpool, 
    size_t size, 
    size_t align, 
    struct mca_bmi_base_registration_t** registration);

/**
  * realloc function typedef
  */
void* mca_mpool_vapi_realloc(
    mca_mpool_base_module_t* mpool, 
    void* addr, 
    size_t size, 
    struct mca_bmi_base_registration_t** registration);

/**
  * register function typedef
  */
int mca_mpool_vapi_register(
    mca_mpool_base_module_t* mpool, 
    void *addr, 
    size_t size, 
    struct mca_bmi_base_registration_t** registration);

int mca_mpool_vapi_deregister(
    mca_mpool_base_module_t* mpool, 
    void *addr, 
    size_t size);


/**
  * free function typedef
  */
void mca_mpool_vapi_free(mca_mpool_base_module_t* mpool, void *);

void* mca_common_vapi_segment_alloc(
    struct mca_mpool_base_module_t* module, 
    size_t* size, 
    struct mca_bmi_base_registration_t** registration);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif















