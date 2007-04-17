
/**
  * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
  *                         University Research and Technology
  *                         Corporation.  All rights reserved.
  * Copyright (c) 2004-2005 The University of Tennessee and The University
  *                         of Tennessee Research Foundation.  All rights
  *                         reserved.
  * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
  *                         University of Stuttgart.  All rights reserved.
  * Copyright (c) 2004-2005 The Regents of the University of California.
  *                         All rights reserved.
  *
  * Copyright (c) 2006      Voltaire. All rights reserved.
  *
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
#ifndef MCA_RCACHE_VMA_MRU_H
#define MCA_RCACHE_VMA_MRU_H
#include "opal/mca/mca.h"
#include "ompi/info/info.h"
#include "opal/class/opal_list.h" 
#include "rcache_vma.h"



/*
 * initialize the rb mru
 */
int mca_rcache_vma_mru_init(mca_rcache_vma_module_t* rcache);

/* 
 * insert an item in the rb mru 
 */ 
int mca_rcache_vma_mru_insert(
                             mca_rcache_vma_module_t* rcache, 
                             mca_mpool_base_registration_t* reg
                             ); 

/* 
 * remove an item from the rb mru 
 */
int mca_rcache_vma_mru_delete( 
                             mca_rcache_vma_module_t* rcache, 
                             mca_mpool_base_registration_t* reg
                             ); 

int mca_rcache_vma_mru_touch( 
                              mca_rcache_vma_module_t* rcache, 
                              mca_mpool_base_registration_t* reg
                              ); 

#endif /* MCA_RCACHE_VMA_MRU_H */

