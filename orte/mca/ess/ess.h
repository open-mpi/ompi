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
/** @file:
 *
 * The OpenRTE Environment-Specific Services
 *
 */

#ifndef ORTE_ESS_H
#define ORTE_ESS_H

#include "opal/mca/mca.h"

BEGIN_C_DECLS

/*
 * Module and component structures
 */
struct orte_ess_base_module_1_0_0_t;
typedef struct orte_ess_base_module_1_0_0_t orte_ess_base_module_1_0_0_t;
typedef orte_ess_base_module_1_0_0_t orte_ess_base_module_t;

struct orte_ess_base_component_1_0_0_t;
typedef struct orte_ess_base_component_1_0_0_t orte_ess_base_component_1_0_0_t;
typedef orte_ess_base_component_1_0_0_t orte_ess_base_component_t;

/**
 * Selection function
 */
typedef orte_ess_base_module_t* 
(*orte_ess_base_component_init_fn_t)(int *priority);

/*
 * API functions
 */

/*
 * Initialize the RTE for this environment
 */
typedef int (*orte_ess_base_module_init_fn_t)(char flags);

/*
 * Finalize the RTE for this environment
 */
typedef int (*orte_ess_base_module_finalize_fn_t)(void);

/**
 * Abort the current application
 *
 * Aborts currently running application, NOTE: We do NOT call the
 * regular C-library "abort" function, even
 * though that would have alerted us to the fact that this is
 * an abnormal termination, because it would automatically cause
 * a core file to be generated. The "report" flag indicates if the
 * function should create an appropriate file to alert the local
 * orted that termination was abnormal.
 */
typedef void (*orte_ess_base_module_abort_fn_t)(int status, bool report);

/**
 * Handle fault tolerance updates
 *
 * Handle fault tolerance updates
 *
 * @param[in] state Fault tolerance state update
 *
 * @retval ORTE_SUCCESS The operation completed successfully
 * @retval ORTE_ERROR   An unspecifed error occurred
 */
typedef int  (*orte_ess_base_module_ft_event_fn_t)(int state);

/*
 * the standard module data structure
 */
struct orte_ess_base_module_1_0_0_t {
    orte_ess_base_module_init_fn_t          init;
    orte_ess_base_module_finalize_fn_t      finalize;
    orte_ess_base_module_abort_fn_t         abort;
    orte_ess_base_module_ft_event_fn_t      ft_event;
};

 
/*
 * the standard component data structure
 */
struct orte_ess_base_component_1_0_0_t {
    mca_base_component_t ess_version;
    mca_base_component_data_1_0_0_t ess_data;
    orte_ess_base_component_init_fn_t  ess_init;
};

/*
 * Macro for use in components that are of type ess v1.0.0
 */
#define ORTE_ESS_BASE_VERSION_1_0_0 \
  /* ess v1.0 is chained to MCA v1.0 */ \
  MCA_BASE_VERSION_1_0_0, \
  /* ess v1.0 */ \
  "ess", 1, 0, 0

/* Global structure for accessing ESS functions */
ORTE_DECLSPEC extern orte_ess_base_module_t orte_ess;  /* holds selected module's function pointers */

END_C_DECLS

#endif
