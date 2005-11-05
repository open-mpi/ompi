/*
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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef MCA_BTL_MVAPI_RDMA_BUF_H
#define MCA_BTL_MVAPI_RDMA_BUF_H

#include "ompi_config.h"
#include "btl_mvapi.h" 


#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
#if 0
    struct mca_btl_mvapi_rdma_buf_t { 
        void* base; 
        size_t entry_size; 
        uint32_t entry_cnt; 
        void* current; 
        opal_mutex_t lock; 
        mca_mpool_base_registration_t* reg;
        uint32_t tokens; 
        void* rem_addr; 
        size_t rem_size; 
        uint32_t rem_cnt; 
        void* rem_current; 
        VAPI_rkey_t r_key;
        
    }; 
    typedef struct mca_btl_mvapi_rdma_buf_t mca_btl_mvapi_rdma_buf_t; 
#endif
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif
