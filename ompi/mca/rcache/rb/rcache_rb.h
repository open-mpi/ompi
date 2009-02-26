/* -*- Mode: C; c-basic-offset:4 ; -*- */
/**
  * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
  *                         University Research and Technology
  *                         Corporation.  All rights reserved.
  * Copyright (c) 2004-2007 The University of Tennessee and The University
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
#ifndef MCA_RCACHE_RB_H
#define MCA_RCACHE_RB_H

#include "opal/mca/mca.h"
#include "opal/class/opal_list.h" 
#include "ompi/class/ompi_rb_tree.h"
#include "ompi/mca/rcache/rcache.h"

struct mca_rcache_rb_module_t { 
    mca_rcache_base_module_t base; 
    ompi_rb_tree_t rb_tree;
    ompi_free_list_t rb_tree_item_list; 
    opal_list_t mru_list; 
    size_t reg_mru_len; 
    size_t reg_max_mru_size;
    size_t reg_cur_mru_size;

};
typedef struct mca_rcache_rb_module_t mca_rcache_rb_module_t; 


struct mca_rcache_rb_component_t { 
    mca_rcache_base_component_t super; 
};
typedef struct mca_rcache_rb_component_t mca_rcache_rb_component_t; 

OMPI_MODULE_DECLSPEC extern mca_rcache_rb_component_t mca_rcache_rb_component;

void mca_rcache_rb_module_init( mca_rcache_rb_module_t* rcache );

int mca_rcache_rb_find( mca_rcache_base_module_t* rcache, 
                        void* addr, 
                        size_t size, 
                        opal_pointer_array_t* regs, 
                        uint32_t *cnt );

int mca_rcache_rb_insert( struct mca_rcache_base_module_t* rcache, 
                          mca_mpool_base_registration_t* registration, 
                          uint32_t flags ); 

int mca_rcache_rb_delete( struct mca_rcache_base_module_t* rcache, 
                          mca_mpool_base_registration_t* registration, 
                          uint32_t flags ); 


/**
  * init/finalize
  */

void mca_rcache_rb_module_init( mca_rcache_rb_module_t* rcache );

void mca_rcache_rb_finalize( struct mca_rcache_base_module_t* );

#endif /* MCA_RCACHE_RB_H */

