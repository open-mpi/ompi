/*
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


struct mca_mpool_sm_component_t {
    mca_mpool_base_component_t super;
    mca_allocator_t* sm_allocator;
    char*  sm_allocator_name;
    size_t sm_min_size;
    size_t sm_max_size;
    size_t sm_size;
    size_t sm_segment;
    ompi_list_t sm_mmaps;
    struct mca_mpool_sm_mmap_t *sm_mmap;
};
typedef struct mca_mpool_sm_component_t mca_mpool_sm_component_t;

extern mca_mpool_sm_component_t mca_mpool_sm_module;

/**
  * allocate function typedef
  */
void* mca_mpool_sm_alloc(size_t size, size_t align);
                                                                                                                         
/**
  * realloc function typedef
  */
void* mca_mpool_sm_realloc(void* addr, size_t size);
                                                                                                                         
/**
  * free function typedef
  */
void mca_mpool_sm_free(void *);
                                                                                                                         
/**
  * register memory
  */
void mca_mpool_sm_register(void * addr, size_t size, void* user);
                                                                                                                         
/**
  * deregister memory
  */
void mca_mpool_sm_deregister(void * addr);

/**
  * component open/close/init function 
  */
int mca_mpool_sm_open(void);
int mca_mpool_sm_close(void);
mca_mpool_t* mca_mpool_sm_init(bool *allow_multi_user_threads);


#endif
