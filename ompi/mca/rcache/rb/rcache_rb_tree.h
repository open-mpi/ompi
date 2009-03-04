
/**
  * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
  *                         University Research and Technology
  *                         Corporation.  All rights reserved.
  * Copyright (c) 2004-2006 The University of Tennessee and The University
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
  * Description of the Registration Cache framework
  */
#ifndef MCA_RCACHE_RB_TREE_H
#define MCA_RCACHE_RB_TREE_H
#include "ompi_config.h"
#include "opal/mca/mca.h"
#include "ompi/class/ompi_free_list.h"
#include "ompi/mca/mpool/mpool.h"
#include "rcache_rb.h"
/*
 * Data structures for the tree of allocated memory
 */

/**
 * Holds the key for the tree
 */
struct mca_rcache_rb_tree_key_t
{
    void * base;          /**< the base of the memory range */
    void * bound;             /**< the bound of the memory range */
};
typedef struct mca_rcache_rb_tree_key_t mca_rcache_rb_tree_key_t;

/**
 * The item in the rb_tree itself
 */
struct mca_rcache_rb_tree_item_t
{
    ompi_free_list_item_t super;   /**< the parent class */
    mca_rcache_rb_tree_key_t key; /**< the key which holds the memory pointers */
    mca_mpool_base_registration_t* reg; /**< the registration */  
};
typedef struct mca_rcache_rb_tree_item_t mca_rcache_rb_tree_item_t;

OMPI_DECLSPEC OBJ_CLASS_DECLARATION(mca_rcache_rb_tree_item_t);

/*
 * initialize the rb tree
 */
int mca_rcache_rb_tree_init(mca_rcache_rb_module_t* rcache); 

/**
 *  Returns the item in the rb tree  
 */
mca_rcache_rb_tree_item_t* mca_rcache_rb_tree_find(
                                                   mca_rcache_rb_module_t* rcache, 
                                                   void* base
                                                   );

/* 
 * insert an item in the rb tree 
 */ 
int mca_rcache_rb_tree_insert(
                              mca_rcache_rb_module_t* rcache, 
                              mca_mpool_base_registration_t* reg
                              ); 

/* 
 * remove an item from the rb tree 
 */
int mca_rcache_rb_tree_delete( 
                              mca_rcache_rb_module_t* rcache, 
                              mca_mpool_base_registration_t* reg
                              ); 


#endif /* MCA_RCACHE_RB_TREE_H */

