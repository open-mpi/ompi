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
 * maffinity (memory affinity) framework component interface
 * definitions.
 *
 * Intent
 *
 * Simple component to set memory affinity for pages.  Note that this
 * framework assumes that processor affinity is being used (it doesn't
 * make much sense to use memory affinity unless processes are bound
 * to specific processors, otherwise the processes may move around and
 * the memory may end up being remote).
 *
 * maffinity components are typically used with shared memory
 * operations, but can be used elsewhere as well.  The idea is to get
 * memory physically located with the process that is going to use it.
 * For memory allocated to a processor-bound process, functions such
 * as malloc() do this naturally.  However, when working with memory
 * shared by multiple processes on a NUMA machine, it can be extremely
 * advantageous to ensure that pages containing the data structures
 * for a given process are physically local to the processor where
 * that process is bound.
 *
 * One process will allocate a large shared memory block and all will
 * need to participate to make pages local to specific processors.
 *
 * There is one main module function
 * (opal_maffinity_base_module_set_fn_t) that takes an array of
 * segment descriptions within the block.  Each process will get a
 * different set of segment descriptions (i.e., the segments belonging
 * to that process).  Components then do whatever is necessary to make
 * pages local to their respective processes (i.e., the processors
 * where the processes are running).
 */

#ifndef OPAL_MAFFINITY_H
#define OPAL_MAFFINITY_H

#include "opal_config.h"

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/maffinity/maffinity_types.h"


/**
 * Query function for maffinity components.  Simply returns a priority
 * to rank it against other available maffinity components (assumedly,
 * only one component will be available per platform, but it's
 * possible that there could be more than one available).
 */
typedef const struct opal_maffinity_base_module_1_0_0_t *
    (*opal_maffinity_base_component_query_1_0_0_fn_t)
    (int *priority);


/**
 * Module initialization function.  Should return OPAL_SUCCESS.
 */
typedef int (*opal_maffinity_base_module_init_1_0_0_fn_t)(void);


/**
 * Module function to set memory affinity.  Take an array of
 * maffinity_base_segment_t instances to describe which memory should
 * physically reside with which process.
 *
 * This function is intended to be invoked by each process immediately
 * after they mmap / attach the shared memory.  In some cases, it will
 * be a no-op for some processes (i.e., machines where a single
 * function call in the creating process sets the memory affinity for
 * the entire region), but in other cases all processes will need to
 * participate (e.g., the first_use component, each each process will
 * "touch" the pages that are supposed to be local to them).
 */
typedef int (*opal_maffinity_base_module_set_fn_t)
    (opal_maffinity_base_segment_t *segments, size_t num_segments);


/**
 * Structure for maffinity v1.0.0 components.
 * Chained to MCA v1.0.0
 */
struct opal_maffinity_base_component_1_0_0_t {
    /** MCA base component */
    mca_base_component_t maffinityc_version;
    /** MCA base data */
    mca_base_component_data_1_0_0_t maffinityc_data;

    /** Component query function */
    opal_maffinity_base_component_query_1_0_0_fn_t maffinityc_query;
};
/**
 * Convenience typedef
 */
typedef struct opal_maffinity_base_component_1_0_0_t opal_maffinity_base_component_1_0_0_t;


/**
 * Structure for maffinity v1.0.0 modules
 */
struct opal_maffinity_base_module_1_0_0_t {

    /** Module initialization function */
    opal_maffinity_base_module_init_1_0_0_fn_t maff_module_init;

    /** Set memory affinity */
    opal_maffinity_base_module_set_fn_t maff_module_set;
};
/**
 * Convenience typedef
 */
typedef struct opal_maffinity_base_module_1_0_0_t opal_maffinity_base_module_1_0_0_t;


/*
 * Macro for use in components that are of type maffinity v1.0.0
 */
#define OPAL_MAFFINITY_BASE_VERSION_1_0_0 \
    /* maffinity v1.0 is chained to MCA v1.0 */ \
    MCA_BASE_VERSION_1_0_0, \
    /* maffinity v1.0 */ \
    "maffinity", 1, 0, 0

#endif /* OPAL_MAFFINITY_H */
