/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2011-2012 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/** @file:
 *
 * The OpenRTE Group Communications
 *
 * The OpenRTE Group Comm framework provides communication services that
 * span entire jobs or collections of processes. It is not intended to be
 * used for point-to-point communications (the RML does that), nor should
 * it be viewed as a high-performance communication channel for large-scale
 * data transfers.
 */

#ifndef MCA_GRPCOMM_H
#define MCA_GRPCOMM_H

/*
 * includes
 */

#include "orte_config.h"
#include "orte/constants.h"
#include "orte/types.h"

#include "opal/mca/mca.h"
#include "opal/class/opal_list.h"
#include "opal/dss/dss_types.h"

#include "orte/mca/rml/rml_types.h"

#include "orte/mca/grpcomm/grpcomm_types.h"

BEGIN_C_DECLS

/*
 * Component functions - all MUST be provided!
 */

/* initialize the selected module */
typedef int (*orte_grpcomm_base_module_init_fn_t)(void);

/* finalize the selected module */
typedef void (*orte_grpcomm_base_module_finalize_fn_t)(void);

/* Send a message to all members of a job - blocking */
typedef int (*orte_grpcomm_base_module_xcast_fn_t)(orte_jobid_t job,
                                                   opal_buffer_t *buffer,
                                                   orte_rml_tag_t tag);

/* allgather - gather data from all procs */
typedef int (*orte_grpcomm_base_module_allgather_fn_t)(orte_grpcomm_collective_t *coll);

/* barrier function */
typedef int (*orte_grpcomm_base_module_barrier_fn_t)(orte_grpcomm_collective_t *coll);

/** DATA EXCHANGE FUNCTIONS - SEE ompi/runtime/ompi_module_exchange.h FOR A DESCRIPTION
 *  OF HOW THIS ALL WORKS
 */

/* perform a modex operation */
typedef int (*orte_grpcomm_base_module_modex_fn_t)(orte_grpcomm_collective_t *coll);

/*
 * Ver 2.0
 */
struct orte_grpcomm_base_module_2_0_0_t {
    orte_grpcomm_base_module_init_fn_t                  init;
    orte_grpcomm_base_module_finalize_fn_t              finalize;
    /* collective operations */
    orte_grpcomm_base_module_xcast_fn_t                 xcast;
    orte_grpcomm_base_module_allgather_fn_t             allgather;
    orte_grpcomm_base_module_barrier_fn_t               barrier;
    orte_grpcomm_base_module_modex_fn_t                 modex;
};

typedef struct orte_grpcomm_base_module_2_0_0_t orte_grpcomm_base_module_2_0_0_t;
typedef orte_grpcomm_base_module_2_0_0_t orte_grpcomm_base_module_t;

/*
 * the standard component data structure
 */
struct orte_grpcomm_base_component_2_0_0_t {
    mca_base_component_t base_version;
    mca_base_component_data_t base_data;
};
typedef struct orte_grpcomm_base_component_2_0_0_t orte_grpcomm_base_component_2_0_0_t;
typedef orte_grpcomm_base_component_2_0_0_t orte_grpcomm_base_component_t;



/*
 * Macro for use in components that are of type grpcomm v2.0.0
 */
#define ORTE_GRPCOMM_BASE_VERSION_2_0_0 \
  /* grpcomm v2.0 is chained to MCA v2.0 */ \
  MCA_BASE_VERSION_2_0_0, \
  /* grpcomm v2.0 */ \
  "grpcomm", 2, 0, 0

/* Global structure for accessing name server functions
 */
ORTE_DECLSPEC extern orte_grpcomm_base_module_t orte_grpcomm;  /* holds selected module's function pointers */

END_C_DECLS

#endif
