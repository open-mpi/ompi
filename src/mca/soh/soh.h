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
 * The Open MPI State-of-Health Monitoring Subsystem
 *
 */

#ifndef MCA_SOH_H
#define MCA_SOH_H

/*
 * includes
 */

#include "ompi_config.h"

#include "mca/mca.h"
#include "mca/ns/base/base.h"

/*
 * Component functions - all MUST be provided!
 */

/* Update the state-of-health of a cell
 */
typedef int (*mca_soh_base_module_update_cell_soh_fn_t)(mca_ns_base_cellid_t cellid);


/*
 * Ver 1.0.0
 */
struct mca_soh_base_module_1_0_0_t {
    mca_soh_base_module_update_cell_soh_fn_t update_cell_soh;
};

typedef struct mca_soh_base_module_1_0_0_t mca_soh_base_module_1_0_0_t;
typedef mca_soh_base_module_1_0_0_t mca_soh_base_module_t;

/*
 * SOH Component
 */

typedef mca_soh_base_module_t* (*mca_soh_base_component_init_fn_t)(
    bool *allow_multi_user_threads,
    bool *have_hidden_threads,
    int *priority);

typedef int (*mca_soh_base_component_finalize_fn_t)(void);
 
/*
 * the standard component data structure
 */

struct mca_soh_base_component_1_0_0_t {
    mca_base_component_t soh_version;
    mca_base_component_data_1_0_0_t soh_data;

    mca_soh_base_component_init_fn_t soh_init;
    mca_soh_base_component_finalize_fn_t soh_finalize;
};
typedef struct mca_soh_base_component_1_0_0_t mca_soh_base_component_1_0_0_t;
typedef mca_soh_base_component_1_0_0_t mca_soh_base_component_t;



/*
 * Macro for use in components that are of type ns v1.0.0
 */
#define MCA_SOH_BASE_VERSION_1_0_0 \
  /* soh v1.0 is chained to MCA v1.0 */ \
  MCA_BASE_VERSION_1_0_0, \
  /* soh v1.0 */ \
  "soh", 1, 0, 0

#endif
