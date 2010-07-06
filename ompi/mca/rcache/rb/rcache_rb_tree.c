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
#include "rcache_rb_tree.h"


OBJ_CLASS_INSTANCE(mca_rcache_rb_tree_item_t, ompi_free_list_item_t, NULL, NULL); 


int mca_rcache_rb_tree_node_compare(void * key1, void * key2); 

int mca_rcache_rb_tree_init(mca_rcache_rb_module_t* rcache) { 
    OBJ_CONSTRUCT(&rcache->rb_tree, ompi_rb_tree_t);
    OBJ_CONSTRUCT(&rcache->rb_tree_item_list, ompi_free_list_t);
    ompi_free_list_init_new(&rcache->rb_tree_item_list, 
            sizeof(mca_rcache_rb_tree_item_t), 
            opal_cache_line_size,
            OBJ_CLASS(mca_rcache_rb_tree_item_t), 
            0,opal_cache_line_size,
            0, -1, 32, NULL); 
    
    return ompi_rb_tree_init(&rcache->rb_tree, 
                             mca_rcache_rb_tree_node_compare);
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
                                                           mca_rcache_rb_module_t* rcache, 
                                                           void * base
                                                           )
{
    mca_rcache_rb_tree_item_t* found = NULL; 
    mca_rcache_rb_tree_key_t key;
    
    
    key.base = base;
    key.bound = base;
    found =  (mca_rcache_rb_tree_item_t *)
        ompi_rb_tree_find(&rcache->rb_tree, &key);
    if(found) { 
        assert((void*)found->reg->bound >= base);
    }
    return found;
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
    ompi_free_list_item_t *item; 
    int rc; 
    mca_rcache_rb_tree_item_t* rb_tree_item; 
    
    OMPI_FREE_LIST_GET(&rb_module->rb_tree_item_list, item, rc);
    if(OMPI_SUCCESS != rc) { 
        return rc; 
    }
    rb_tree_item = (mca_rcache_rb_tree_item_t*) item; 
    
    rb_tree_item->key.base = reg->base;
    rb_tree_item->key.bound = reg->bound; 
    rb_tree_item->reg = reg;
    
    rc = ompi_rb_tree_insert(&rb_module->rb_tree, 
                             (void*) &rb_tree_item->key, item);
        
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
    mca_rcache_rb_tree_item_t *tree_item; 
    tree_item = mca_rcache_rb_tree_find(rb_module, 
                                        reg->base); 
    if(NULL == tree_item) {
        return OMPI_ERROR; 
    }
    assert(reg == tree_item->reg);
    rc =  ompi_rb_tree_delete(&rb_module->rb_tree, &tree_item->key); 
   
    OMPI_FREE_LIST_RETURN(&rb_module->rb_tree_item_list, 
                          (ompi_free_list_item_t*) tree_item);

    return rc;
}


