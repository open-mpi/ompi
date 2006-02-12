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
/**
 * @file
 *
 * paffinity (processor affinity) framework component interface
 * definitions.
 *
 * Intent
 *
 * This is an extremely simple framework that is used to support the
 * OS-specific API for placement of processes on processors.  It does
 * *not* decide scheduling issues -- it is simply for assigning the
 * current process it to a specific processor.  As such, the
 * components are likely to be extremely short/simple -- there will
 * likely be one component for each OS/API that we support (e.g.,
 * Linux, IRIX, etc.).  As a direct consequence, there will likely
 * only be one component that is useable on a given platform (making
 * selection easy).
 *
 * It is *not* an error if there is no paffinity component available;
 * processor affinity services are simply not available.  Hence,
 * paffinity component functions are invoked through short wrapper
 * functions in paffinity/base (that check to see if there is a
 * selected component before invoking function pointers).  If there is
 * no selected component, they return an appropriate error code.
 *
 * General scheme
 *
 * The component has one function: query().  It simply returns a
 * priority (for the unlikely event where there are multiple
 * components available on a given platform).
 *
 * The module has four functions:
 *
 * - module_init: initialze the module
 * - get_num_processors: return the maximum number N that can be used
 *   in the set() module function (i.e., the number of available
 *   processors)
 * - set: set this process's affinity to a specific processor
 * - get: get this process's processor affinity
 */

#ifndef OPAL_PAFFINITY_H
#define OPAL_PAFFINITY_H

#include "opal_config.h"

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"


/**
 * Query function for paffinity components.  Simply returns a priority
 * to rank it against other available paffinity components (assumedly,
 * only one component will be available per platform, but it's
 * possible that there could be more than one available).
 */
typedef const struct opal_paffinity_base_module_1_0_0_t *
    (*opal_paffinity_base_component_query_1_0_0_fn_t)
    (int *priority);


/**
 * Module initialization function.  Should return OPAL_SUCCESS.
 */
typedef int (*opal_paffinity_base_module_init_1_0_0_fn_t)(void);


/**
 * Module function to query the physical number of processors (which
 * may be different than what schedulers have allocated to us).
 */
typedef int (*opal_paffinity_base_module_get_num_processors_t)(int *nprocs);


/**
 * Module function to set this process' affinity to a specific
 * [virtual] CPU.
 */
typedef int (*opal_paffinity_base_module_set_fn_t)(int cpuid);


/**
 * Module function to get this process' affinity to a specific
 * [virtual] CPU.  Returns OPAL_ERR_NOT_FOUND if
 * opal_paffinity_base_module_set_fn_t() was not previously invoked in
 * this process.
 */
typedef int (*opal_paffinity_base_module_get_fn_t)(int *cpuid);


/**
 * Structure for paffinity v1.0.0 components.
 * Chained to MCA v1.0.0
 */
struct opal_paffinity_base_component_1_0_0_t {
    /** MCA base component */
    mca_base_component_t paffinityc_version;
    /** MCA base data */
    mca_base_component_data_1_0_0_t paffinityc_data;

    /** Component query function */
    opal_paffinity_base_component_query_1_0_0_fn_t paffinityc_query;
};
/**
 * Convenience typedef
 */
typedef struct opal_paffinity_base_component_1_0_0_t opal_paffinity_base_component_1_0_0_t;


/**
 * Structure for paffinity v1.0.0 modules
 */
struct opal_paffinity_base_module_1_0_0_t {

    /** Module initialization function */
    opal_paffinity_base_module_init_1_0_0_fn_t paff_module_init;

    /** Query the number of physical processors */
    opal_paffinity_base_module_get_num_processors_t 
        paff_module_get_num_processors;

    /** Set this process' affinity */
    opal_paffinity_base_module_set_fn_t paff_module_set;

    /** Get this process' affinity */
    opal_paffinity_base_module_get_fn_t paff_module_get;
};
/**
 * Convenience typedef
 */
typedef struct opal_paffinity_base_module_1_0_0_t opal_paffinity_base_module_1_0_0_t;


/*
 * Macro for use in components that are of type paffinity v1.0.0
 */
#define OPAL_PAFFINITY_BASE_VERSION_1_0_0 \
    /* paffinity v1.0 is chained to MCA v1.0 */ \
    MCA_BASE_VERSION_1_0_0, \
    /* paffinity v1.0 */ \
    "paffinity", 1, 0, 0

#endif /* OPAL_PAFFINITY_H */
