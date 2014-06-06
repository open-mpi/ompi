/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
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
 * Atomic Operations Interface
 *
 */

#ifndef OSHMEM_MCA_ATOMIC_H
#define OSHMEM_MCA_ATOMIC_H

#include "oshmem_config.h"
#include "oshmem/types.h"
#include "oshmem/constants.h"

#include "opal/util/output.h"
#include "mpi.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "oshmem/mca/atomic/base/base.h"

BEGIN_C_DECLS

/* ******************************************************************** */

struct oshmem_op_t;

/* ******************************************************************** */

typedef int (*mca_atomic_base_component_init_fn_t)(bool enable_progress_threads,
                                                   bool enable_threads);

typedef int (*mca_atomic_base_component_finalize_fn_t)(void);

typedef struct mca_atomic_base_module_1_0_0_t* (*mca_atomic_base_component_query_fn_t)(int *priority);

/* ******************************************************************** */

/**
 * Atomic component interface
 *
 * Component interface for the atomic framework.  A public
 * instance of this structure, called
 * mca_atomic_[component_name]_component, must exist in any atomic
 * component.
 */
struct mca_atomic_base_component_1_0_0_t {
    /** Base component description */
    mca_base_component_t atomic_version;
    /** Base component data block */
    mca_base_component_data_t atomic_data;

    /** Component initialization function */
    mca_atomic_base_component_init_fn_t atomic_init;
    mca_atomic_base_component_finalize_fn_t atomic_finalize;
    mca_atomic_base_component_query_fn_t atomic_query;

    /* priority for component */
    int priority;
};
typedef struct mca_atomic_base_component_1_0_0_t mca_atomic_base_component_1_0_0_t;

/** Per guidence in mca.h, use the unversioned struct name if you just
 want to always keep up with the most recent version of the
 interace. */
typedef struct mca_atomic_base_component_1_0_0_t mca_atomic_base_component_t;

/**
 * Atomic module interface
 *
 */
struct mca_atomic_base_module_1_0_0_t {
    /** Collective modules all inherit from opal_object */
    opal_object_t super;

    /* Collective function pointers */
    int (*atomic_fadd)(void *target,
                       void *prev,
                       const void *value,
                       size_t nlong,
                       int pe,
                       struct oshmem_op_t *op);
    int (*atomic_cswap)(void *target,
                        void *prev,
                        const void *cond,
                        const void *value,
                        size_t nlong,
                        int pe);
};
typedef struct mca_atomic_base_module_1_0_0_t mca_atomic_base_module_1_0_0_t;

/** Per guidence in mca.h, use the unversioned struct name if you just
 want to always keep up with the most recent version of the
 interace. */
typedef struct mca_atomic_base_module_1_0_0_t mca_atomic_base_module_t;
OSHMEM_DECLSPEC OBJ_CLASS_DECLARATION(mca_atomic_base_module_t);

/* ******************************************************************** */

/*
 * Macro for use in components
 */
#define MCA_ATOMIC_BASE_VERSION_2_0_0 \
  MCA_BASE_VERSION_2_0_0, \
  "atomic", 1, 0, 0

/* ******************************************************************** */

OSHMEM_DECLSPEC extern mca_atomic_base_component_t mca_atomic_base_selected_component;
OSHMEM_DECLSPEC extern mca_atomic_base_module_t mca_atomic;
#define MCA_ATOMIC_CALL(a) mca_atomic.atomic_ ## a

END_C_DECLS

#endif /* OSHMEM_MCA_ATOMIC_H */
