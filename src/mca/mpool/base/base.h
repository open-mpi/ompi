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
#ifndef MCA_MEM_BASE_H
#define MCA_MEM_BASE_H

#include "ompi_config.h"

#include "class/ompi_list.h"
#include "mca/mca.h"
#include "mca/mpool/mpool.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

struct mca_mpool_base_selected_module_t {
  ompi_list_item_t super;
  mca_mpool_base_component_t *mpool_component;
  mca_mpool_base_module_t *mpool_module;
};
typedef struct mca_mpool_base_selected_module_t mca_mpool_base_selected_module_t;

OBJ_CLASS_DECLARATION(mca_mpool_base_selected_module_t);

/*
 * Global functions for MCA: overall mpool open and close
 */

OMPI_DECLSPEC int mca_mpool_base_open(void);
OMPI_DECLSPEC int mca_mpool_base_init(bool *allow_multi_user_threads);
OMPI_DECLSPEC int mca_mpool_base_close(void);
OMPI_DECLSPEC mca_mpool_base_component_t* mca_mpool_base_component_lookup(const char* name);
OMPI_DECLSPEC mca_mpool_base_module_t* mca_mpool_base_module_lookup(const char* name);


/*
 * Globals
 */
OMPI_DECLSPEC extern int mca_mpool_base_output;
OMPI_DECLSPEC extern ompi_list_t mca_mpool_base_components;
OMPI_DECLSPEC extern ompi_list_t mca_mpool_base_modules;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* MCA_MEM_BASE_H */
