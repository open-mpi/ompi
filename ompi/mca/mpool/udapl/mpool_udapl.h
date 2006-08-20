/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
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
#ifndef MCA_MPOOL_UDAPL_H
#define MCA_MPOOL_UDAPL_H

#include "ompi_config.h"
#include <dat/udat.h>
#include "opal/class/opal_list.h"
#include "ompi/class/ompi_free_list.h"
#include "opal/event/event.h"
#include "ompi/mca/mpool/mpool.h"
#include "ompi/mca/allocator/allocator.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif


struct mca_mpool_udapl_component_t {
    mca_mpool_base_component_t super;
    char* rcache_name; 
};
typedef struct mca_mpool_udapl_component_t mca_mpool_udapl_component_t;
OMPI_DECLSPEC extern mca_mpool_udapl_component_t mca_mpool_udapl_component;


struct mca_mpool_base_resources_t {
    DAT_IA_HANDLE udapl_ia; /* interface adapter */
    DAT_PZ_HANDLE udapl_pz; /* protection zone */
}; 
typedef struct mca_mpool_base_resources_t mca_mpool_base_resources_t;  

struct mca_mpool_udapl_module_t {
    mca_mpool_base_module_t super;
    struct mca_mpool_base_resources_t udapl_res;
}; 
typedef struct mca_mpool_udapl_module_t mca_mpool_udapl_module_t; 


struct mca_mpool_udapl_registration_t {
    mca_mpool_base_registration_t base_reg;
    DAT_LMR_HANDLE lmr;          /* local memory region (LMR) */
    DAT_LMR_TRIPLET lmr_triplet; /* LMR triplet - context, address, length */
    DAT_RMR_CONTEXT rmr_context; /* remote memory region context handle */
};
typedef struct mca_mpool_udapl_registration_t mca_mpool_udapl_registration_t;
OMPI_DECLSPEC OBJ_CLASS_DECLARATION(mca_mpool_udapl_registration_t);


/**
  * Report a uDAPL error - for debugging
  */

#if OMPI_ENABLE_DEBUG
extern void mca_mpool_udapl_error(DAT_RETURN ret, char* str);

#define MCA_MPOOL_UDAPL_ERROR(ret, str) \
    mca_mpool_udapl_error((ret), (str));

#else
#define MCA_MPOOL_UDAPL_ERROR(ret, str)
#endif


/* 
 *  Initializes the mpool module. 
 */ 
void mca_mpool_udapl_module_init(mca_mpool_udapl_module_t* mpool); 


/*
 *  Returns base address of shared memory mapping.
 */
void* mca_mpool_udapl_base(mca_mpool_base_module_t*);

/**
  *  Allocate block of shared memory.
  */
void* mca_mpool_udapl_alloc( 
    mca_mpool_base_module_t* mpool, 
    size_t size, 
    size_t align, 
    uint32_t flags, 
    mca_mpool_base_registration_t** registration);

/**
  * realloc function typedef
  */
void* mca_mpool_udapl_realloc(
    mca_mpool_base_module_t* mpool, 
    void* addr, 
    size_t size, 
    mca_mpool_base_registration_t** registration);

/**
  * register function typedef
  */
int mca_mpool_udapl_register(
    mca_mpool_base_module_t* mpool, 
    void *addr, 
    size_t size, 
    uint32_t flags, 
    mca_mpool_base_registration_t** registration);

int mca_mpool_udapl_deregister(
    mca_mpool_base_module_t* mpool, 
    mca_mpool_base_registration_t*);


/**
  * free function typedef
  */
void mca_mpool_udapl_free(
    mca_mpool_base_module_t* mpool, 
    void * addr, 
    mca_mpool_base_registration_t* registration);

int mca_mpool_udapl_find(
                      struct mca_mpool_base_module_t* mpool, 
                      void* addr, 
                      size_t size, 
                      ompi_pointer_array_t *regs,
                      uint32_t *cnt
                      );
    
int mca_mpool_udapl_release(
                            struct mca_mpool_base_module_t* mpool, 
                            mca_mpool_base_registration_t* registration
                            ); 

int mca_mpool_udapl_retain(
                            struct mca_mpool_base_module_t* mpool, 
                            mca_mpool_base_registration_t* registration
                            ); 



void* mca_common_udapl_segment_alloc(
    struct mca_mpool_base_module_t* module, 
    size_t* size, 
    mca_mpool_base_registration_t** registration);


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

