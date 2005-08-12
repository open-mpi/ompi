/*
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

#ifndef _MCA_PML_BASE_PTL_
#define _MCA_PML_BASE_PTL_

#include "mca/pml/pml.h"
#include "mca/ptl/ptl.h"
#include "opal/threads/condition.h"
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif


struct mca_pml_base_ptl_t {
    opal_list_t       ptl_cache;       /**< cache of send requests */
    size_t            ptl_cache_size;  /**< maximum size of cache */
    size_t            ptl_cache_alloc; /**< current number of allocated items */
    opal_mutex_t      ptl_cache_lock;  /**< lock for queue access */
    struct mca_ptl_base_module_t* ptl; /**< back pointer to ptl */
};
typedef struct mca_pml_base_ptl_t mca_pml_base_ptl_t;

OBJ_CLASS_DECLARATION(mca_pml_base_ptl_t);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif

