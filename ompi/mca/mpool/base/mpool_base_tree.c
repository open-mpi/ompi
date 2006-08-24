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
  *                         All rights reserved.5A
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
#include "ompi_config.h"
#include "opal/mca/mca.h"
#include "mpool_base_tree.h"


OBJ_CLASS_INSTANCE(mca_mpool_base_tree_item_t, ompi_free_list_item_t, NULL, NULL); 

/* 
 * use globals for the tree and the tree_item free list.. 
 */
ompi_rb_tree_t mca_mpool_base_tree; 
ompi_free_list_t mca_mpool_base_tree_item_free_list;

/*
 *  simple minded compare function... 
 */
int mca_mpool_base_tree_node_compare(void * key1, void * key2)
{
    if(key1 < key2)
    {
        return -1;
    }
    else if(key1 > key2)
    {
        return 1;
    }
    else
    {
        return 0;
    }
}

/*
 * initialize the rb tree
 */
int mca_mpool_base_tree_init(void) { 
    int rc;
    OBJ_CONSTRUCT(&mca_mpool_base_tree, ompi_rb_tree_t); 
    OBJ_CONSTRUCT(&mca_mpool_base_tree_item_free_list, ompi_free_list_t); 
    rc = ompi_free_list_init(&mca_mpool_base_tree_item_free_list, sizeof(mca_mpool_base_tree_item_t), 
                             OBJ_CLASS(mca_mpool_base_tree_item_t), 0, -1 , 4, NULL);
    if(OMPI_SUCCESS == rc) { 
        rc = ompi_rb_tree_init(&mca_mpool_base_tree, mca_mpool_base_tree_node_compare);
    }
    return rc;
}

/* 
 * insert an item in the rb tree 
 */ 
int mca_mpool_base_tree_insert(mca_mpool_base_tree_item_t* item) { 
    return ompi_rb_tree_insert(&mca_mpool_base_tree, item->key, item); 
}

/* 
 * remove an item from the rb tree 
 */
int mca_mpool_base_tree_delete(mca_mpool_base_tree_item_t* item) { 
    int rc; 
    rc = ompi_rb_tree_delete(&mca_mpool_base_tree, item->key); 
    if(OMPI_SUCCESS == rc) { 
        mca_mpool_base_tree_item_put(item); 
    }
    return rc;
}

/**
 *  find the item in the rb tree  
 */
mca_mpool_base_tree_item_t* mca_mpool_base_tree_find(void* base) { 
    return (mca_mpool_base_tree_item_t*)ompi_rb_tree_find(&mca_mpool_base_tree, base);
}
    
/* 
 * get a tree item from the free list 
 */
mca_mpool_base_tree_item_t* mca_mpool_base_tree_item_get(void) { 
    ompi_free_list_item_t* item = NULL;
    int rc;
    OMPI_FREE_LIST_GET(&mca_mpool_base_tree_item_free_list, 
                       item, 
                       rc); 
    if(OMPI_SUCCESS == rc) { 
        return (mca_mpool_base_tree_item_t*) item; 
    } else { 
        return NULL;
    }
}

/*
 * put an item back into the free list
 */
void mca_mpool_base_tree_item_put(mca_mpool_base_tree_item_t* item) { 
    OMPI_FREE_LIST_RETURN(&mca_mpool_base_tree_item_free_list,
                          &(item->super));
}
