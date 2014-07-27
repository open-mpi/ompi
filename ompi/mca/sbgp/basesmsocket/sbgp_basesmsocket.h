/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * Copyright (c) 2014      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_BCOL_basesmsocket_EXPORT_H
#define MCA_BCOL_basesmsocket_EXPORT_H

#include "ompi_config.h"

#include "mpi.h"
#include "opal/mca/mca.h"
#include "ompi/mca/sbgp/sbgp.h"
#include "ompi/mca/sbgp/base/base.h"
#include "opal/mca/mpool/mpool.h"
#include "ompi/request/request.h"
#include "ompi/proc/proc.h"
#include "opal/util/output.h"

BEGIN_C_DECLS

#ifdef HAVE_SCHED_YIELD
#  include <sched.h>
#  define SPIN sched_yield()
#else  /* no switch available */
#  define SPIN
#endif

#define BASESMSOCKET_VERBOSE(level, ...)                                \
    do {								\
        OPAL_OUTPUT_VERBOSE((ompi_sbgp_base_framework.framework_output, level, \
                             __VA_ARGS__));                             \
    } while(0);

/**
 * Structure to hold the basic shared memory coll component.  First it holds the
 * base coll component, and then holds a bunch of
 * sm-coll-component-specific stuff (e.g., current MCA param
 * values).
 */
struct mca_sbgp_basesmsocket_component_t {
    /** Base coll component */
    mca_sbgp_base_component_2_0_0_t super;
};

/**
 * Convenience typedef
 */
typedef struct mca_sbgp_basesmsocket_component_t
    mca_sbgp_basesmsocket_component_t;


/*
** Base sub-group module
**/

struct mca_sbgp_basesmsocket_module_t {
    /** Collective modules all inherit from opal_object */
    mca_sbgp_base_module_t super;

};
typedef struct mca_sbgp_basesmsocket_module_t mca_sbgp_basesmsocket_module_t;
OBJ_CLASS_DECLARATION(mca_sbgp_basesmsocket_module_t);

/**
* Global component instance
*/
OMPI_MODULE_DECLSPEC extern mca_sbgp_basesmsocket_component_t mca_sbgp_basesmsocket_component;


END_C_DECLS

#endif /* MCA_BCOL_basesmsocket_EXPORT_H */
