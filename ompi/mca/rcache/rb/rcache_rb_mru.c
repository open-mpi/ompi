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
#include "rcache_rb_mru.h"
#include "mca/mpool/mpool.h"

/*
 * initialize the rb mru
 */
int mca_rcache_rb_mru_init(mca_rcache_rb_module_t* rcache){
    OBJ_CONSTRUCT(&rcache->mru_list, opal_list_t); 
    rcache->reg_mru_len = mca_rcache_rb_component.reg_mru_len;
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
    if(rcache->reg_mru_len <= rcache->mru_list.opal_list_length) { 
        old_reg = (mca_mpool_base_registration_t*) 
            opal_list_remove_last(&rcache->mru_list); 
        /* need to pull out of rb tree here */ 
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




