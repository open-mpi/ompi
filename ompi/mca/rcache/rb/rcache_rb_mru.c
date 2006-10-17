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

#include "ompi_config.h"

#include "opal/mca/mca.h"
#include "rcache_rb_mru.h"
#include "ompi/mca/mpool/mpool.h"

/*
 * initialize the rb mru
 */
int mca_rcache_rb_mru_init(mca_rcache_rb_module_t* rcache){
    OBJ_CONSTRUCT(&rcache->mru_list, opal_list_t); 
    rcache->reg_cur_mru_size = 0;
    return OMPI_SUCCESS; 
}

/* 
 * insert an item in the rb mru 
 */ 
int mca_rcache_rb_mru_insert(
                              mca_rcache_rb_module_t* rcache, 
                              mca_mpool_base_registration_t* reg
        
                              ) {
    mca_mpool_base_registration_t* old_reg; 
    size_t reg_size = reg->bound - reg->base + 1;
    
    if(reg_size > rcache->reg_max_mru_size) { 
        return OMPI_ERR_TEMP_OUT_OF_RESOURCE; 
    }
    
    rcache->reg_cur_mru_size += reg_size; 

    if(rcache->reg_mru_len <= rcache->mru_list.opal_list_length) {
        /* call deregister - which removes the registration from
         * the tree and mru list. memory will be deregistered when
         * the reference count goes to zero.
         */
        old_reg = (mca_mpool_base_registration_t*)
            opal_list_get_first(&rcache->mru_list);
        /* we need to retain first, because we only want the registration 
         removed from the tree and the mru */
        old_reg->mpool->mpool_retain(old_reg->mpool, old_reg);
        old_reg->mpool->mpool_deregister(old_reg->mpool, old_reg);
        
    }
    
    while(rcache->reg_max_mru_size <= rcache->reg_cur_mru_size) {
        old_reg = (mca_mpool_base_registration_t*)
            opal_list_get_first(&rcache->mru_list);
        /* we need to retain first, because we only want the registration 
         removed from the tree and the mru we didn't get this registration 
        using mpool_find so the refernce count wasn't bumped up.*/
        old_reg->mpool->mpool_retain(old_reg->mpool, old_reg);
        old_reg->mpool->mpool_deregister(old_reg->mpool, old_reg);
        
    }

    opal_list_append(&rcache->mru_list,(opal_list_item_t*) reg); 
    
    return OMPI_SUCCESS; 
}

/* 
 * remove an item from 
the rb mru 
 */
int mca_rcache_rb_mru_delete( 
                               mca_rcache_rb_module_t* rcache, 
                               mca_mpool_base_registration_t *reg
                               ){
    int rc; 
    if(NULL == opal_list_remove_item(
                                     &rcache->mru_list,
                                     (opal_list_item_t*) reg
                                     )) {
        rc =  OMPI_ERROR;
    } else { 
        rcache->reg_cur_mru_size -= (reg->bound - reg->base + 1);
        rc = OMPI_SUCCESS; 
    }
    return rc; 
}

/* 
 * touch an item in the mru list 
 */ 
int mca_rcache_rb_mru_touch( 
                              mca_rcache_rb_module_t* rcache, 
                              mca_mpool_base_registration_t* reg
                              ){
    int rc; 
    if(NULL == opal_list_remove_item(
                                     &rcache->mru_list,
                                     (opal_list_item_t*) reg
                                     )) {
       rc =  OMPI_ERROR;
    } else { 
        opal_list_append(&rcache->mru_list, (opal_list_item_t*) reg); 
        rc = OMPI_SUCCESS; 
    }
    return rc; 
}




