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

#include "opal/class/opal_list.h"
#include "class/ompi_free_list.h"
#include "opal/event/event.h"
#include "mca/mpool/mpool.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif


static inline  void * DOWN_ALIGN_ADDR(void * addr, uint32_t cnt) { 
    return (void*)((uintptr_t)((unsigned char*) addr) & (~((uintptr_t)0) << (cnt))); 
}

static inline void*  ALIGN_ADDR(void* addr, uint32_t cnt ) { 
    DOWN_ALIGN_ADDR((((unsigned char*) addr) +  ~(~((uintptr_t)0) << (cnt))), (cnt)); 
    return addr;
} 


struct mca_mpool_openib_component_t {
    mca_mpool_base_component_t super;
    char* rcache_name;
};

typedef struct mca_mpool_openib_component_t mca_mpool_openib_component_t;

OMPI_COMP_EXPORT extern mca_mpool_openib_component_t mca_mpool_openib_component;





struct mca_mpool_base_resources_t {
    struct ibv_pd* ib_pd; 
}; 
typedef struct mca_mpool_base_resources_t mca_mpool_base_resources_t;  

struct mca_mpool_openib_module_t {
    mca_mpool_base_module_t super;
    struct mca_mpool_base_resources_t  resources;
}; typedef struct mca_mpool_openib_module_t mca_mpool_openib_module_t; 
    

struct mca_mpool_openib_registration_t {
    mca_mpool_base_registration_t base_reg; 
    struct ibv_mr *mr;
    
};
typedef struct mca_mpool_openib_registration_t mca_mpool_openib_registration_t;
OMPI_DECLSPEC OBJ_CLASS_DECLARATION(mca_mpool_openib_registration_t); 




/* 
 *  Initializes the mpool module. 
 */ 
void mca_mpool_openib_module_init(mca_mpool_openib_module_t* mpool); 


/*
 *  Returns base address of shared memory mapping.
 */
void* mca_mpool_openib_base(mca_mpool_base_module_t*);

/**
  *  Allocate block of shared memory.
  */
void* mca_mpool_openib_alloc( 
    mca_mpool_base_module_t* mpool, 
    size_t size, 
    size_t align, 
    uint32_t flags, 
    mca_mpool_base_registration_t** registration);

/**
  * realloc function typedef
  */
void* mca_mpool_openib_realloc(
    mca_mpool_base_module_t* mpool, 
    void* addr, 
    size_t size, 
    mca_mpool_base_registration_t** registration);

/**
  * register function typedef
  */
int mca_mpool_openib_register(
    mca_mpool_base_module_t* mpool, 
    void *addr, 
    size_t size, 
    uint32_t flags, 
    mca_mpool_base_registration_t** registration);

int mca_mpool_openib_deregister(
    mca_mpool_base_module_t* mpool, 
    mca_mpool_base_registration_t* );


/**
  * free function typedef
  */
void mca_mpool_openib_free(mca_mpool_base_module_t* mpool, 
                         void * addr, 
                         mca_mpool_base_registration_t* registration);


int mca_mpool_openib_find(
                         struct mca_mpool_base_module_t* mpool, 
                         void* addr, 
                         size_t size, 
                         ompi_pointer_array_t *regs,
                         uint32_t *cnt
                         );
 
int mca_mpool_openib_release(
                            struct mca_mpool_base_module_t* mpool, 
                            mca_mpool_base_registration_t* registraion
                            ); 

int mca_mpool_openib_retain(
                            struct mca_mpool_base_module_t* mpool, 
                            mca_mpool_base_registration_t* registraion
                            ); 



#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif















