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
/** @file:
 *
 * The Open RTE Error Manager
 *
 */

#ifndef OMPI_MCA_ERRMGR_H
#define OMPI_MCA_ERRMGR_H

/*
 * includes
 */

#include "ompi_config.h"
#include "include/constants.h"



/*
 * Component functions - all MUST be provided!
 */

/**
 * Log an error
 * Log an error that occurred in the runtime environment.
 * 
 * @code
 * ompi_errmgr.log("this is an error", __FILE__, __LINE__);
 * @endcode
 */
typedef void (*mca_errmgr_base_module_log_fn_t)(char *msg, char *filename, int line);

/*
 * Ver 1.0.0
 */
struct mca_errmgr_base_module_1_0_0_t {
    mca_errmgr_base_module_log_fn_t log;
};

typedef struct mca_errmgr_base_module_1_0_0_t mca_errmgr_base_module_1_0_0_t;
typedef mca_errmgr_base_module_1_0_0_t mca_errmgr_base_module_t;

/*
 * ERRMGR Component
 */

typedef mca_errmgr_base_module_t* (*mca_errmgr_base_component_init_fn_t)(
    bool *allow_multi_user_threads,
    bool *have_hidden_threads,
    int *priority);

typedef int (*mca_errmgr_base_component_finalize_fn_t)(void);
 
/*
 * the standard component data structure
 */

struct mca_errmgr_base_component_1_0_0_t {
    mca_base_component_t errmgr_version;
    mca_base_component_data_1_0_0_t errmgr_data;

    mca_errmgr_base_component_init_fn_t errmgr_init;
    mca_errmgr_base_component_finalize_fn_t errmgr_finalize;
};
typedef struct mca_errmgr_base_component_1_0_0_t mca_errmgr_base_component_1_0_0_t;
typedef mca_errmgr_base_component_1_0_0_t mca_errmgr_base_component_t;



/*
 * Macro for use in components that are of type errmgr v1.0.0
 */
#define OMPI_MCA_ERRMGR_BASE_VERSION_1_0_0 \
  /* mca v1.0 is chained to MCA v1.0 */ \
  MCA_BASE_VERSION_1_0_0, \
  /* errmgr v1.0 */ \
  "errmgr", 1, 0, 0

/* Global structure for accessing error manager functions
 */
OMPI_DECLSPEC extern mca_errmgr_base_module_t orte_errmgr;  /* holds selected module's function pointers */

#endif
