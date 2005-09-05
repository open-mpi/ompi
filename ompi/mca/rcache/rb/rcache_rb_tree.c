/**
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
  * Description of the Registration Cache framework
  */

#include "mca/mca.h"
#include "rcache_rb_tree.h"


/**
 * Searches the rcache to see if it has allocated the memory that is passed in.
 * If so it returns an array of rcaches the memory is registered with.
 *
 * @param base pointer to the memory to lookup
 *
 * @retval NULL if the memory is not in any rcache
 * @retval pointer to an array of type mca_rcache_base_reg_rcache_t
 */
static inline struct mca_rcache_rb_tree_item_t * 
  mca_rcache_rb_tree_find_nl(
                             mca_rcache_rb_module_t* rcache, 
                             void* base
                             )
{
    mca_rcache_rb_tree_key_t key;
    key.base = base;
    key.bound = base;
    return (mca_rcache_rb_tree_item_t *)
        ompi_rb_tree_find(&rcache->rb_tree, &key);
}

/**
 * Searches the mpool to see if it has allocated the memory that is passed in.
 * If so it returns an array of mpools the memory is registered with.
 *
 * @param base pointer to the memory to lookup
 *
 * @retval NULL if the memory is not in any mpool
 * @retval pointer to an array of type mca_mpool_base_reg_mpool_t
 */
struct mca_rcache_rb_tree_item_t * mca_rcache_rb_tree_find(
                                                           mca_rcache_rb_module_t* rb_module, 
                                                           void * base
                                                           )
{
    mca_rcache_rb_tree_item_t* found, *copy;
        
    OPAL_THREAD_LOCK(&rb_module->rb_lock);
    found = mca_rcache_rb_tree_find_nl(rb_module, base);
    if(NULL == found) { 
        copy = NULL; 
    } else { 
        copy = OBJ_NEW(mca_rcache_rb_tree_item_t); 
        *copy = *found; 
    }
    OPAL_THREAD_UNLOCK(&rb_module->rb_lock);
    return copy;
}

/**
 * Memory Pool Registration
 */


/**
 * Function for the red black tree to compare 2 keys
 *
 * @param key1 a pointer to the 1st key
 * @param key2 a pointer to the second key
 *
 * @retval -1 if key1 is below key2
 * @retval 1 if key 1 is above key2
 * @retval 0 if the keys are the same
 */
int mca_rcache_rb_tree_node_compare(void * key1, void * key2)
{
    if(((mca_rcache_rb_tree_key_t *) key1)->base <
       ((mca_rcache_rb_tree_key_t *) key2)->base)
    {
        return -1;
    }
    else if(((mca_rcache_rb_tree_key_t *) key1)->base >
            ((mca_rcache_rb_tree_key_t *) key2)->bound)
    {
        return 1;
    }
    else
    {
        return 0;
    }
}

int mca_rcache_rb_tree_insert(
                          mca_rcache_rb_module_t* rb_module, 
                          mca_mpool_base_registration_t* reg
                          )
{
    opal_list_item_t *item; 
    int rc; 
    mca_rcache_rb_tree_item_t* rb_tree_item; 

    OMPI_FREE_LIST_GET(&rb_module->rb_tree_item_list, item, rc);
    if(rc != OMPI_SUCCESS) 
        return rc; 
    rb_tree_item = (mca_rcache_rb_tree_item_t*) item; 
    
    rb_tree_item->key.base = reg->base;
    rb_tree_item->key.bound = reg->bound; 
    rb_tree_item->reg = reg;
    
    
    OPAL_THREAD_LOCK(&rb_module->rb_lock); 
    rc = ompi_rb_tree_insert(&rb_module->rb_tree, 
                             (void*) &rb_tree_item->key, item);
    OPAL_THREAD_UNLOCK(&rb_module->rb_lock); 
    
    if(OMPI_SUCCESS != rc) {
        OMPI_FREE_LIST_RETURN(&rb_module->rb_tree_item_list, item);
        return rc; 
    }
    return OMPI_SUCCESS; 
}

/**
 * Function to remove previously memory from the tree without freeing it
 *
 * @param base pointer to the memory to free
 *
 * @retval OMPI_SUCCESS
 * @retval OMPI_ERR_BAD_PARAM if the passed base pointer was invalid
 */
int mca_rcache_rb_tree_delete(mca_rcache_rb_module_t* rb_module, 
                          mca_mpool_base_registration_t* reg)
{
    int rc; 
    mca_rcache_rb_tree_item_t* tree_item; 
    tree_item = mca_rcache_rb_tree_find(rb_module, 
                                        reg->base); 
    if(NULL == tree_item) {
        return OMPI_ERROR; 
    }
    OPAL_THREAD_LOCK(&rb_module->rb_lock); 
    rc =  ompi_rb_tree_delete(&rb_module->rb_tree, &tree_item->key); 
    OPAL_THREAD_UNLOCK(&rb_module->rb_lock);
    return rc;
}


