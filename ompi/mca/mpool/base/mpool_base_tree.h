/**
  * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
  *                         University Research and Technology
  *                         Corporation.  All rights reserved.
  * Copyright (c) 2004-2006 The University of Tennessee and The University
  *                         of Tennessee Research Foundation.  All rights
  *                         reserved.
  * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
  *                         University of Stuttgart.  All rights reserved.
  * Copyright (c) 2004-2006 The Regents of the University of California.
  *                         All rights reserved.
  * $COPYRIGHT$
  * 
  * Additional copyrights may follow
  * 
  * $HEADER$
  */

#ifndef MCA_MPOOL_BASE_TREE_H
#define MCA_MPOOL_BASE_TREE_H
#define MCA_MPOOL_BASE_TREE_MAX 8
#include "opal/mca/mca.h"
#include "ompi/info/info.h"
#include "ompi/class/ompi_free_list.h" 
#include "ompi/class/ompi_rb_tree.h"
#include "ompi/mca/mpool/mpool.h"

/*
 * Data structures for the tree of allocated memory
 * used for MPI_Alloc_mem and MPI_Free_mem 
 */

/**
 * The item in the tree itself
 */
struct mca_mpool_base_tree_item_t
{
    ompi_free_list_item_t super;   /**< the parent class */
    void* key; /* the address this was alloc'd on */
    mca_mpool_base_module_t* mpools[MCA_MPOOL_BASE_TREE_MAX]; /**< the mpools */
    mca_mpool_base_registration_t* regs[MCA_MPOOL_BASE_TREE_MAX]; /**< the registrations */
    uint8_t count;
};
typedef struct mca_mpool_base_tree_item_t mca_mpool_base_tree_item_t;

OMPI_DECLSPEC OBJ_CLASS_DECLARATION(mca_mpool_base_tree_item_t);

/*
 * initialize the rb tree
 */
int mca_mpool_base_tree_init(void); 

/* 
 * insert an item in the rb tree 
 */ 
int mca_mpool_base_tree_insert(mca_mpool_base_tree_item_t* item); 

/* 
 * remove an item from the rb tree 
 */
int mca_mpool_base_tree_delete(mca_mpool_base_tree_item_t* item); 


/**
 *  find the item in the rb tree  
 */
mca_mpool_base_tree_item_t* mca_mpool_base_tree_find(void* base);

/* 
 * get a tree item from the free list 
 */
mca_mpool_base_tree_item_t* mca_mpool_base_tree_item_get(void); 

/* 
 * put tree item back into the free list 
 */
void mca_mpool_base_tree_item_put(mca_mpool_base_tree_item_t* item); 



#endif /* MCA_MPOOL_BASE_TREE_H */
