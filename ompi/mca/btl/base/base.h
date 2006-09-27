/*
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
 * Copyright (c) 2006      Sun Microsystems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/**
 * @file
 */
#ifndef MCA_BTL_BASE_H
#define MCA_BTL_BASE_H

#include "ompi_config.h"
#include "opal/class/opal_list.h"
#include "opal/mca/mca.h"
#include "ompi/mca/btl/btl.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

struct mca_btl_base_selected_module_t {
  opal_list_item_t super;
  mca_btl_base_component_t *btl_component;
  mca_btl_base_module_t *btl_module;
};
typedef struct mca_btl_base_selected_module_t mca_btl_base_selected_module_t;


/* holds the recv call back function to be called by the btl on 
 * a receive. 
 */ 
struct mca_btl_base_recv_reg_t {  
    mca_btl_base_module_recv_cb_fn_t cbfunc; 
    void* cbdata; 
}; 
typedef struct mca_btl_base_recv_reg_t mca_btl_base_recv_reg_t; 


OMPI_DECLSPEC OBJ_CLASS_DECLARATION(mca_btl_base_selected_module_t); 

/*
 * Global functions for MCA: overall BTL open and close
 */

OMPI_DECLSPEC  int mca_btl_base_open(void);
OMPI_DECLSPEC  int mca_btl_base_select(bool enable_progress_threads, bool enable_mpi_threads);
OMPI_DECLSPEC  int mca_btl_base_close(void);
OMPI_DECLSPEC  void mca_btl_base_dump(
    struct mca_btl_base_module_t*,
    struct mca_btl_base_endpoint_t*,
    int verbose);



/*
 * Globals
 */
OMPI_DECLSPEC extern int mca_btl_base_output;
OMPI_DECLSPEC extern char* mca_btl_base_include;
OMPI_DECLSPEC extern char* mca_btl_base_exclude;
OMPI_DECLSPEC extern int mca_btl_base_warn_component_unused;
OMPI_DECLSPEC extern opal_list_t mca_btl_base_components_opened;
OMPI_DECLSPEC extern opal_list_t mca_btl_base_modules_initialized;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif /* MCA_BTL_BASE_H */
