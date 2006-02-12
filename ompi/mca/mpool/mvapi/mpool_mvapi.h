/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
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
#include "ompi/class/ompi_free_list.h"
#include "opal/event/event.h"
#include "ompi/mca/mpool/mpool.h"
#include <vapi.h> 
#include <vapi_types.h>
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif


 
struct mca_mpool_mvapi_component_t {
    mca_mpool_base_component_t super;
    char* rcache_name;
};

typedef struct mca_mpool_mvapi_component_t mca_mpool_mvapi_component_t;

OMPI_COMP_EXPORT extern mca_mpool_mvapi_component_t mca_mpool_mvapi_component;





struct mca_mpool_base_resources_t {
  VAPI_hca_hndl_t hca;   /* the hca (nic) */ 
  VAPI_pd_hndl_t pd_tag; /* the protection domain */ 
}; 
typedef struct mca_mpool_base_resources_t mca_mpool_base_resources_t;  

struct mca_mpool_mvapi_module_t {
    mca_mpool_base_module_t super;
    struct mca_mpool_base_resources_t  hca_pd;
    ompi_free_list_t reg_list;
}; typedef struct mca_mpool_mvapi_module_t mca_mpool_mvapi_module_t; 


struct mca_mpool_mvapi_registration_t {
    mca_mpool_base_registration_t base_reg; 
    VAPI_mr_hndl_t                  hndl;
    /* Memory region handle */
    
    VAPI_lkey_t                     l_key;
    /* Local key to registered memory, needed for
     * posting send/recv requests */
    
    VAPI_rkey_t                     r_key;
    /* Remote key to registered memory, need to send this
     * to remote processes for incoming RDMA ops */
  
};
typedef struct mca_mpool_mvapi_registration_t mca_mpool_mvapi_registration_t;
OMPI_DECLSPEC OBJ_CLASS_DECLARATION(mca_mpool_mvapi_registration_t); 





/* 
 *  Initializes the mpool module. 
 */ 
void mca_mpool_mvapi_module_init(mca_mpool_mvapi_module_t* mpool); 


/*
 *  Returns base address of shared memory mapping.
 */
void* mca_mpool_mvapi_base(mca_mpool_base_module_t*);

/**
  *  Allocate block of shared memory.
  */
void* mca_mpool_mvapi_alloc( 
    mca_mpool_base_module_t* mpool, 
    size_t size, 
    size_t align, 
    uint32_t flags,
    mca_mpool_base_registration_t** registration);

/**
  * realloc function typedef
  */
void* mca_mpool_mvapi_realloc(
    mca_mpool_base_module_t* mpool, 
    void* addr, 
    size_t size, 
    mca_mpool_base_registration_t** registration);

/**
  * register function typedef
  */
int mca_mpool_mvapi_register(
    mca_mpool_base_module_t* mpool, 
    void *addr, 
    size_t size, 
    uint32_t flags, 
    mca_mpool_base_registration_t** registration);

int mca_mpool_mvapi_deregister(
    mca_mpool_base_module_t* mpool, 
    mca_mpool_base_registration_t* );


/**
  * free function typedef
  */
void mca_mpool_mvapi_free(mca_mpool_base_module_t* mpool, 
                         void * addr, 
                         mca_mpool_base_registration_t* registration);


int mca_mpool_mvapi_find(
                         struct mca_mpool_base_module_t* mpool, 
                         void* addr, 
                         size_t size, 
                         ompi_pointer_array_t *regs,
                         uint32_t *cnt
                         );
 
int mca_mpool_mvapi_release(
                            struct mca_mpool_base_module_t* mpool, 
                            mca_mpool_base_registration_t* registraion
                            ); 

int mca_mpool_mvapi_retain(
                            struct mca_mpool_base_module_t* mpool, 
                            mca_mpool_base_registration_t* registraion
                            ); 


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif















