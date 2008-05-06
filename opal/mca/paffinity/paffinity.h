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
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
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
 * current process it to a specific processor set.  As such, the
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
 * The module has three functions:
 *
 * - module_init: initialze the module
 * - set: set this process's affinity to a specific processor set
 * - get: get this process's processor affinity set
 */

#ifndef OPAL_PAFFINITY_H
#define OPAL_PAFFINITY_H

#include "opal_config.h"

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"

/**
 * Buffer type for paffinity processor masks.
 * Copied almost directly from PLPA.
 */

/**
 * \internal
 * Internal type used for the underlying bitmask unit
 */
typedef unsigned long int opal_paffinity_base_bitmask_t;

/**
 * \internal
 * Number of bits in opal_paffinity_base_bitmask_t
 */
#define OPAL_PAFFINITY_BITMASK_T_NUM_BITS (sizeof(opal_paffinity_base_bitmask_t) * 8)
/**
 * \internal
 * How many bits we want
 */
#define OPAL_PAFFINITY_BITMASK_CPU_MAX 1024
/**
 * \internal
 * How many opal_paffinity_base_bitmask_t's we need
 */
#define OPAL_PAFFINITY_BITMASK_NUM_ELEMENTS (OPAL_PAFFINITY_BITMASK_CPU_MAX / OPAL_PAFFINITY_BITMASK_T_NUM_BITS)

/**
 * Public processor bitmask type
 */
typedef struct opal_paffinity_base_cpu_set_t { 
    opal_paffinity_base_bitmask_t bitmask[OPAL_PAFFINITY_BITMASK_NUM_ELEMENTS];
} opal_paffinity_base_cpu_set_t;

/***************************************************************************/

/**
 * \internal
 * Internal macro for identifying the byte in a bitmask array 
 */
#define OPAL_PAFFINITY_CPU_BYTE(num) ((num) / OPAL_PAFFINITY_BITMASK_T_NUM_BITS)

/**
 * \internal
 * Internal macro for identifying the bit in a bitmask array 
 */
#define OPAL_PAFFINITY_CPU_BIT(num) ((num) % OPAL_PAFFINITY_BITMASK_T_NUM_BITS)

/***************************************************************************/

/**
 * Public macro to zero out a OPAL_PAFFINITY cpu set 
 */
#define OPAL_PAFFINITY_CPU_ZERO(cpuset) \
    memset(&(cpuset), 0, sizeof(opal_paffinity_base_cpu_set_t))

/**
 * Public macro to set a bit in a OPAL_PAFFINITY cpu set 
 */
#define OPAL_PAFFINITY_CPU_SET(num, cpuset) \
    (cpuset).bitmask[OPAL_PAFFINITY_CPU_BYTE(num)] |= ((opal_paffinity_base_bitmask_t) 1 << OPAL_PAFFINITY_CPU_BIT(num))

/**
 * Public macro to clear a bit in a OPAL_PAFFINITY cpu set 
 */
#define OPAL_PAFFINITY_CPU_CLR(num, cpuset) \
    (cpuset).bitmask[OPAL_PAFFINITY_CPU_BYTE(num)] &= ~((opal_paffinity_base_bitmask_t) 1 << OPAL_PAFFINITY_CPU_BIT(num))

/**
 * Public macro to test if a bit is set in a OPAL_PAFFINITY cpu set 
 */
#define OPAL_PAFFINITY_CPU_ISSET(num, cpuset) \
    (0 != (((cpuset).bitmask[OPAL_PAFFINITY_CPU_BYTE(num)]) & ((opal_paffinity_base_bitmask_t) 1 << OPAL_PAFFINITY_CPU_BIT(num))))

/***************************************************************************/

/**
 * Module initialization function.  Should return OPAL_SUCCESS.
 */
typedef int (*opal_paffinity_base_module_init_1_1_0_fn_t)(void);

/**
 * Module function to set this process' affinity to a specific set of
 * [virtual] CPUs.
 */
typedef int (*opal_paffinity_base_module_set_fn_t)(opal_paffinity_base_cpu_set_t cpumask);


/**
 * Module function to get this process' affinity to a specific set of
 * [virtual] CPUs.  Returns OPAL_ERR_NOT_FOUND if
 * opal_paffinity_base_module_set_fn_t() was not previously invoked in
 * this process.
 */
typedef int (*opal_paffinity_base_module_get_fn_t)(opal_paffinity_base_cpu_set_t *cpumask);

/**
 * Provides mapping socket:core -> processor id. currently
 * supported only in Linux hosts
 * 
 * return OPAL_SUCCESS or OPAL_ERR_NOT_SUPPORTED if not
 * supporeted (solaris, windows, etc...)
 */
typedef int (*opal_paffinity_base_module_map_to_processor_id_fn_t)(int socket, int core, int *processor_id);

/**
 * Provides mapping processor id -> socket:core. currently
 * supported only in Linux hosts
 * 
 * return OPAL_SUCCESS or OPAL_ERR_NOT_SUPPORTED if not
 * supporeted (solaris, windows, etc...)
 */
typedef int (*opal_paffinity_base_module_map_to_socket_core_fn_t)(int processor_id, int *socket, int *core);

/**
 * Provides highest processor id number in a host. currently
 * supported only in Linux hosts
 * 
 * return OPAL_SUCCESS or OPAL_ERR_NOT_SUPPORTED if not
 * supporeted (solaris, windows, etc...)
 */
typedef int (*opal_paffinity_base_module_get_processor_info_fn_t)(int *num_processors, int *max_processor_id);

/**
 * Provides the number of sockets in a host. currently supported
 * only in Linux hosts
 * 
 * return OPAL_SUCCESS or OPAL_ERR_NOT_SUPPORTED if not
 * supporeted (solaris, windows, etc...)
 */
typedef int (*opal_paffinity_base_module_get_socket_info_fn_t)(int *num_sockets, int *max_socket_num);

/**
 * Provides the number of cores in a socket. currently supported
 * only in Linux hosts
 * 
 * return OPAL_SUCCESS or OPAL_ERR_NOT_SUPPORTED if not
 * supporeted (solaris, windows, etc...)
 */
typedef int (*opal_paffinity_base_module_get_core_info_fn_t)(int socket, int *num_cores, int *max_core_num);


/**
 * Module finalize function.  Invoked by the base on the selected
 * module when the paffinity framework is being shut down.
 */
typedef int (*opal_paffinity_base_module_finalize_fn_t)(void);


/**
 * Structure for paffinity v1.1.0 components.
 * Chained to MCA v1.0.0
 */
struct opal_paffinity_base_component_1_1_0_t {
    /** MCA base component */
    mca_base_component_t base_version;
    /** MCA base data */
    mca_base_component_data_1_0_0_t base_data;
};
/**
 * Convenience typedef
 */
typedef struct opal_paffinity_base_component_1_1_0_t opal_paffinity_base_component_1_1_0_t;
typedef struct opal_paffinity_base_component_1_1_0_t opal_paffinity_base_component_t;


/**
 * Structure for paffinity v1.0.0 modules
 */
struct opal_paffinity_base_module_1_1_0_t {
    /** Module initialization function */
    opal_paffinity_base_module_init_1_1_0_fn_t paff_module_init;

    /** Set this process' affinity */
    opal_paffinity_base_module_set_fn_t paff_module_set;

    /** Get this process' affinity */
    opal_paffinity_base_module_get_fn_t paff_module_get;

    /** Map socket:core to processor ID */
    opal_paffinity_base_module_map_to_processor_id_fn_t paff_map_to_processor_id;

    /** Map processor ID to socket:core */
    opal_paffinity_base_module_map_to_socket_core_fn_t  paff_map_to_socket_core;

    /** Return the max processor ID */
    opal_paffinity_base_module_get_processor_info_fn_t paff_get_processor_info;

    /** Return the max socket number */
    opal_paffinity_base_module_get_socket_info_fn_t paff_get_socket_info;

    /** Return the max core number */
    opal_paffinity_base_module_get_core_info_fn_t paff_get_core_info;

    /** Shut down this module */
    opal_paffinity_base_module_finalize_fn_t paff_module_finalize;
};
/**
 * Convenience typedef
 */
typedef struct opal_paffinity_base_module_1_1_0_t opal_paffinity_base_module_1_1_0_t;
typedef struct opal_paffinity_base_module_1_1_0_t opal_paffinity_base_module_t;


/*
 * Macro for use in components that are of type paffinity v1.1.0
 */
#define OPAL_PAFFINITY_BASE_VERSION_1_1_0 \
    /* paffinity v1.1 is chained to MCA v1.0 */ \
    MCA_BASE_VERSION_1_0_0, \
    /* paffinity v1.1 */ \
    "paffinity", 1, 1, 0

#endif /* OPAL_PAFFINITY_H */
