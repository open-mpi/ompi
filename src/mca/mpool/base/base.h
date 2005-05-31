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
#ifndef MCA_MEM_BASE_H
#define MCA_MEM_BASE_H

#include "ompi_config.h"

#include "class/ompi_list.h"
#include "class/ompi_rb_tree.h"
#include "mca/mca.h"
#include "mca/mpool/mpool.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

struct mca_mpool_base_selected_module_t {
  ompi_list_item_t super;
  mca_mpool_base_component_t *mpool_component;
  mca_mpool_base_module_t *mpool_module;
};
typedef struct mca_mpool_base_selected_module_t mca_mpool_base_selected_module_t;

OBJ_CLASS_DECLARATION(mca_mpool_base_selected_module_t);

/*
 * Data structures for the tree of allocated memory
 */

/**
 * The maximum number of mpools a chunk of memorry can be registered with
 */
#define MCA_MPOOL_BASE_MAX_REG 10

/**
 * Holds the key for the tree
 */
struct mca_mpool_base_key_t
{
    void * bottom;          /**< the bottom of the memory range */
    void * top;             /**< the top of the memory range */
};
typedef struct mca_mpool_base_key_t mca_mpool_base_key_t;

/**
 * Holds a pointer to the mpool the memory is registered with and 
 * a user pointer for mpool specific information
 */
struct mca_mpool_base_reg_mpool_t
{
    mca_mpool_base_module_t * mpool; /**< the registered memory pool */
    void * user;                     /**< user specific information */
};
typedef struct mca_mpool_base_reg_mpool_t mca_mpool_base_reg_mpool_t;

/**
 * Holds all the information about a chunk of registered memory. The whole
 * structure serves as a value in the tree
 */
struct mca_mpool_base_chunk_t
{
    ompi_list_item_t super;   /**< the parent class */
    mca_mpool_base_key_t key; /**< the key which holds the memory pointers */
    mca_mpool_base_reg_mpool_t mpools[MCA_MPOOL_BASE_MAX_REG]; 
                              /**< the mpools the memory is registered with */
};
typedef struct mca_mpool_base_chunk_t mca_mpool_base_chunk_t;

/*
 * Global functions for MCA: overall mpool open and close
 */

OMPI_DECLSPEC int mca_mpool_base_open(void);
OMPI_DECLSPEC int mca_mpool_base_init(bool enable_progress_threads,
                                      bool enable_mpi_threads);
OMPI_DECLSPEC int mca_mpool_base_close(void);
OMPI_DECLSPEC mca_mpool_base_component_t* mca_mpool_base_component_lookup(const char* name);
OMPI_DECLSPEC mca_mpool_base_module_t* mca_mpool_base_module_lookup(const char* name);
OMPI_DECLSPEC mca_mpool_base_module_t* mca_mpool_base_module_init(const char* name);


/*
 * Globals
 */
OMPI_DECLSPEC extern int mca_mpool_base_output;
OMPI_DECLSPEC extern ompi_list_t mca_mpool_base_components;
OMPI_DECLSPEC extern ompi_list_t mca_mpool_base_modules;
OMPI_DECLSPEC extern ompi_free_list_t mca_mpool_base_mem_list;
OMPI_DECLSPEC extern ompi_rb_tree_t mca_mpool_base_tree;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* MCA_MEM_BASE_H */
