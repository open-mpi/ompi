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
#ifndef MCA_MPOOL_GM_H
#define MCA_MPOOL_GM_H

#include "ompi_config.h"
#include <gm.h>
#include "opal/class/opal_list.h"
#include "class/ompi_free_list.h"
#include "opal/event/event.h"
#include "mca/mpool/mpool.h"
#include "mca/allocator/allocator.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif


struct mca_mpool_gm_component_t {
    mca_mpool_base_component_t super;
    char*  gm_allocator_name;
    long page_size; 
    long page_size_log; 
};

typedef struct mca_mpool_gm_component_t mca_mpool_gm_component_t;

OMPI_COMP_EXPORT extern mca_mpool_gm_component_t mca_mpool_gm_component;



struct mca_mpool_base_resources_t {
  struct gm_port *gm_port;   /* the gm NIC handle */
}; 
typedef struct mca_mpool_base_resources_t mca_mpool_base_resources_t;  

struct mca_mpool_gm_module_t {
    mca_mpool_base_module_t super;
    mca_allocator_base_module_t *gm_allocator; 
    struct gm_port* gm_port;
}; 
typedef struct mca_mpool_gm_module_t mca_mpool_gm_module_t; 



/* 
 *  Initializes the mpool module. 
 */ 
void mca_mpool_gm_module_init(mca_mpool_gm_module_t* mpool); 


/*
 *  Returns base address of shared memory mapping.
 */
void* mca_mpool_gm_base(mca_mpool_base_module_t*);

/**
  *  Allocate block of shared memory.
  */
void* mca_mpool_gm_alloc( 
    mca_mpool_base_module_t* mpool, 
    size_t size, 
    size_t align, 
    mca_mpool_base_registration_t** registration);

/**
  * realloc function typedef
  */
void* mca_mpool_gm_realloc(
    mca_mpool_base_module_t* mpool, 
    void* addr, 
    size_t size, 
    mca_mpool_base_registration_t** registration);

/**
  * register function typedef
  */
int mca_mpool_gm_register(
    mca_mpool_base_module_t* mpool, 
    void *addr, 
    size_t size, 
    mca_mpool_base_registration_t** registration);

int mca_mpool_gm_deregister(
    mca_mpool_base_module_t* mpool, 
    void *addr, 
    size_t size, 
    mca_mpool_base_registration_t*);


/**
  * free function typedef
  */
void mca_mpool_gm_free(
    mca_mpool_base_module_t* mpool, 
    void * addr, 
    mca_mpool_base_registration_t* registration);

void* mca_common_gm_segment_alloc(
    struct mca_mpool_base_module_t* module, 
    size_t* size, 
    mca_mpool_base_registration_t** registration);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif















