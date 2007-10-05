/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
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
#include "orte/orte_constants.h"
#include "orte/orte_types.h"

#include "opal/mca/mca.h"
#include "opal/class/opal_list.h"

#include "orte/dss/dss_types.h"
#include "orte/mca/gpr/gpr_types.h"
#include "orte/mca/ns/ns_types.h"
#include "orte/mca/rml/rml_types.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif


/*
 * Component functions - all MUST be provided!
 */

/* Send a message to all members of a job - blocking */
typedef int (*orte_grpcomm_base_module_xcast_fn_t)(orte_jobid_t job,
                                                   orte_buffer_t *buffer,
                                                   orte_rml_tag_t tag);

/* Send a message to all members of a job - non-blocking */
typedef int (*orte_grpcomm_base_module_xcast_nb_fn_t)(orte_jobid_t job,
                                                      orte_buffer_t *buffer,
                                                      orte_rml_tag_t tag);

/* allgather - gather data from all procs */
typedef int (*orte_grpcomm_base_module_allgather_fn_t)(orte_buffer_t *sbuf, orte_buffer_t *rbuf);

typedef int (*orte_grpcomm_base_module_allgather_list_fn_t)(opal_list_t *names,
                                                            orte_buffer_t *sbuf, orte_buffer_t *rbuf);

/* barrier function */
typedef int (*orte_grpcomm_base_module_barrier_fn_t)(void);

/*
 * Ver 2.0
 */
struct orte_grpcomm_base_module_2_0_0_t {
    orte_grpcomm_base_module_xcast_fn_t             xcast;
    orte_grpcomm_base_module_xcast_nb_fn_t          xcast_nb;
    orte_grpcomm_base_module_allgather_fn_t         allgather;
    orte_grpcomm_base_module_allgather_list_fn_t    allgather_list;
    orte_grpcomm_base_module_barrier_fn_t           barrier;
};

typedef struct orte_grpcomm_base_module_2_0_0_t orte_grpcomm_base_module_2_0_0_t;
typedef orte_grpcomm_base_module_2_0_0_t orte_grpcomm_base_module_t;

/*
 * NS Component
 */
/**
 * Initialize the selected component.
 */
typedef orte_grpcomm_base_module_t* (*orte_grpcomm_base_component_init_fn_t)(int *priority);

/**
 * Finalize the selected module
 */
typedef int (*orte_grpcomm_base_component_finalize_fn_t)(void);


/*
 * the standard component data structure
 */

struct orte_grpcomm_base_component_2_0_0_t {
    mca_base_component_t grpcomm_version;
    mca_base_component_data_1_0_0_t grpcomm_data;

    orte_grpcomm_base_component_init_fn_t grpcomm_init;
    orte_grpcomm_base_component_finalize_fn_t grpcomm_finalize;
};
typedef struct orte_grpcomm_base_component_2_0_0_t orte_grpcomm_base_component_2_0_0_t;
typedef orte_grpcomm_base_component_2_0_0_t orte_grpcomm_base_component_t;



/*
 * Macro for use in components that are of type grpcomm v2.0.0
 */
#define ORTE_GRPCOMM_BASE_VERSION_2_0_0 \
  /* grpcomm v2.0 is chained to MCA v1.0 */ \
  MCA_BASE_VERSION_1_0_0, \
  /* grpcomm v2.0 */ \
  "grpcomm", 2, 0, 0

/* Global structure for accessing name server functions
 */
ORTE_DECLSPEC extern orte_grpcomm_base_module_t orte_grpcomm;  /* holds selected module's function pointers */

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif
