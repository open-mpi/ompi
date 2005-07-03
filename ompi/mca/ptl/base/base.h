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
/**
 * @file
 */
#ifndef MCA_PTL_BASE_H
#define MCA_PTL_BASE_H

#include "ompi_config.h"

#include "opal/class/opal_list.h"
#include "mca/mca.h"
#include "mca/ptl/ptl.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

struct mca_ptl_base_selected_module_t {
  opal_list_item_t super;

  mca_ptl_base_component_t *pbsm_component;
  mca_ptl_base_module_t *pbsm_module;
};
typedef struct mca_ptl_base_selected_module_t mca_ptl_base_selected_module_t;


/*
 * Global functions for MCA: overall PTL open and close
 */

OMPI_DECLSPEC  int mca_ptl_base_open(void);
OMPI_DECLSPEC  int mca_ptl_base_select(bool enable_progress_threads,
                                       bool enable_mpi_threads);
OMPI_DECLSPEC  int mca_ptl_base_close(void);


/*
 * Globals
 */
OMPI_DECLSPEC extern int mca_ptl_base_output;
OMPI_DECLSPEC extern char* mca_ptl_base_include;
OMPI_DECLSPEC extern char* mca_ptl_base_exclude;
OMPI_DECLSPEC extern opal_list_t mca_ptl_base_components_opened;
OMPI_DECLSPEC extern opal_list_t mca_ptl_base_modules_initialized;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif /* MCA_PTL_BASE_H */
