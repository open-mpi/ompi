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
 * The Open MPI Start-up Discovery Service
 *
 */

#ifndef ORTE_SDS_H
#define ORTE_SDS_H

#include "opal/mca/mca.h"

/*
 * Module and component structures
 */
struct orte_sds_base_module_1_0_0_t;
typedef struct orte_sds_base_module_1_0_0_t orte_sds_base_module_1_0_0_t;
typedef orte_sds_base_module_1_0_0_t orte_sds_base_module_t;

struct orte_sds_base_component_1_0_0_t;
typedef struct orte_sds_base_component_1_0_0_t orte_sds_base_component_1_0_0_t;
typedef orte_sds_base_component_1_0_0_t orte_sds_base_component_t;

/**
 * Selection function
 *
 * Priority list:
 *
 *  -  0 singleton process (not seed)
 *  - 20 process has name provided by starter
 *  - 40 seed process
 *  - 60 process has name provided by starter and needs to
 *       override seed.  This should be used with care, and
 *       is mainly intended for environments where orte is
 *       not the starter (such as cnos)
 */
typedef orte_sds_base_module_t* 
(*orte_sds_base_component_init_fn_t)(int *priority);

/*
 * Module functions
 */

/**
 * Contact universe and set contact information
 *
 * Attempt to contact the universe to get replica contact information.
 */
typedef int (*orte_sds_base_contact_universe_fn_t)(void);

/**
 * Set Name and Job information for current process
 *
 * Set name and job information for current process.  This information
 * includes:
 *
 *   - orte_process_info.my_name
 *   - orte_process_info.vpid_start
 *   - orte_process_info.num_procs
 *
 * From this, the ns is able to develop a map of all processes started
 * in the curent job.
 *
 * Upon startup, each process must discover its official ORTE process
 * name. There are several ways this name could be passed to the
 * process. This typicall involves an environmental parameter of some
 * appropriate name, possibly followed by some computation of the vpid
 * based on process rank. This function checks the different
 * environmental parameters to find the one that has been set with the
 * appropriate value, determines (based on that) the name of this
 * process, and then sets that value in the orte_system_info global
 * structure.
 */
typedef int (*orte_sds_base_set_name_fn_t)(void);

typedef int (*orte_sds_base_module_finalize_fn_t)(void);


/*
 * the standard module data structure
 */
struct orte_sds_base_module_1_0_0_t {
    orte_sds_base_contact_universe_fn_t contact_universe;
    orte_sds_base_set_name_fn_t set_name;
    orte_sds_base_module_finalize_fn_t finalize;
};

 
/*
 * the standard component data structure
 */
struct orte_sds_base_component_1_0_0_t {
    mca_base_component_t sds_version;
    mca_base_component_data_1_0_0_t sds_data;
    orte_sds_base_component_init_fn_t  sds_init;
};

/*
 * Macro for use in components that are of type ns v1.0.0
 */
#define ORTE_SDS_BASE_VERSION_1_0_0 \
  /* sds v1.0 is chained to MCA v1.0 */ \
  MCA_BASE_VERSION_1_0_0, \
  /* sds v1.0 */ \
  "sds", 1, 0, 0

#endif
