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

#include "orte_config.h"
#include "orte/types.h"

#include "opal/mca/mca.h"

BEGIN_C_DECLS

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
 * Determine if a process is local to me
 *
 * MPI procs need to know if a process is "local" or not - i.e.,
 * if they share the same node. Different environments are capable
 * of making that determination in different ways - e.g., they may
 * provide a callable utility to return the answer, or download
 * a map of information into each process. This API provides a
 * means for each environment to do the "right thing".
 */
typedef bool (*orte_ess_base_module_proc_is_local_fn_t)(orte_process_name_t *proc);

/**
 * Get the hostname where a proc resides
 *
 * MPI procs need to know the hostname where a specified proc resides.
 * Different environments provide that info in different ways - e.g., they may
 * provide a callable utility to return the answer, or download
 * a map of information into each process. This API provides a
 * means for each environment to do the "right thing".
 *
 * NOTE: To avoid memory waste, this function returns a pointer
 * to a static storage. IT MUST NOT BE FREED!
 */
typedef char* (*orte_ess_base_module_proc_get_hostname_fn_t)(orte_process_name_t *proc);

/**
 * Determine the arch of the node where a specified proc resides
 *
 * MPI procs need to know the arch being used by a specified proc.
 * Different environments provide that info in different ways - e.g., they may
 * provide a callable utility to return the answer, or download
 * a map of information into each process. This API provides a
 * means for each environment to do the "right thing".
 */
typedef uint32_t (*orte_ess_base_module_proc_get_arch_fn_t)(orte_process_name_t *proc);

/**
 * Get the local rank of a remote process
 */
typedef orte_local_rank_t (*orte_ess_base_module_proc_get_local_rank_fn_t)(orte_process_name_t *proc);

/**
 * Get the node rank of a remote process
 */
typedef orte_node_rank_t (*orte_ess_base_module_proc_get_node_rank_fn_t)(orte_process_name_t *proc);

/**
 * Update the arch of a remote process
 */
typedef int (*orte_ess_base_module_update_arch_fn_t)(orte_process_name_t *proc, uint32_t arch);


/**
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
    orte_ess_base_module_init_fn_t                  init;
    orte_ess_base_module_finalize_fn_t              finalize;
    orte_ess_base_module_abort_fn_t                 abort;
    orte_ess_base_module_proc_is_local_fn_t         proc_is_local;
    orte_ess_base_module_proc_get_hostname_fn_t     proc_get_hostname;
    orte_ess_base_module_proc_get_arch_fn_t         proc_get_arch;
    orte_ess_base_module_proc_get_local_rank_fn_t   get_local_rank;
    orte_ess_base_module_proc_get_node_rank_fn_t    get_node_rank;
    orte_ess_base_module_update_arch_fn_t           update_arch;
    orte_ess_base_module_ft_event_fn_t              ft_event;
};
typedef struct orte_ess_base_module_1_0_0_t orte_ess_base_module_1_0_0_t;
typedef struct orte_ess_base_module_1_0_0_t orte_ess_base_module_t;

/*
 * the standard component data structure
 */
struct orte_ess_base_component_2_0_0_t {
    mca_base_component_t base_version;
    mca_base_component_data_t base_data;
};
typedef struct orte_ess_base_component_2_0_0_t orte_ess_base_component_2_0_0_t;
typedef struct orte_ess_base_component_2_0_0_t orte_ess_base_component_t;

/*
 * Macro for use in components that are of type ess
 */
#define ORTE_ESS_BASE_VERSION_2_0_0 \
  MCA_BASE_VERSION_2_0_0, \
  "ess", 2, 0, 0

/* Global structure for accessing ESS functions */
ORTE_DECLSPEC extern orte_ess_base_module_t orte_ess;  /* holds selected module's function pointers */

END_C_DECLS

#endif
