/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/** @file **/

#ifndef MCA_SVC_BASE_H
#define MCA_SVC_BASE_H

#include "ompi_config.h"

#include "mca/mca.h"
#include "mca/svc/svc.h"
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif


struct mca_svc_base_module_item_t {
  ompi_list_item_t super;
  mca_svc_base_component_t *svc_component;
  mca_svc_base_module_t *svc_module;
};
typedef struct mca_svc_base_module_item_t mca_svc_base_module_item_t;

OBJ_CLASS_DECLARATION(mca_svc_base_module_item_t);
                                                                                                                                

/*
 * Global functions for the SVC
 */

  int mca_svc_base_open(void);
  int mca_svc_base_init(void);
  int mca_svc_base_close(void);


/*
 * Globals
 */
extern int mca_svc_base_output;
extern ompi_list_t mca_svc_base_components;
extern ompi_list_t mca_svc_base_modules;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif /* MCA_SVC_BASE_H */
