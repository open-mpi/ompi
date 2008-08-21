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
 * Copyright (c) 2008      Sun Microsystems, Inc.  All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"

/* This component will only be compiled on Solaris, where we are
   guaranteed to have these headers */
#include <sys/types.h>
#include <unistd.h>
#include <errno.h>

#include "opal/constants.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/mca/paffinity/paffinity.h"
#include "opal/mca/paffinity/base/base.h"
#include "opal/util/output.h"

#include "paffinity_posix.h"

/*
 * Local functions
 */
static int posix_module_init(void);
static int posix_module_set(opal_paffinity_base_cpu_set_t cpumask);
static int posix_module_get(opal_paffinity_base_cpu_set_t *cpumask);
static int posix_module_finalize(void);
static int posix_module_map_to_processor_id(int socket, int core, int *processor_id);
static int posix_module_map_to_socket_core(int processor_id, int *socket, int *core);
static int posix_module_get_processor_info(int *num_processors);
static int posix_module_get_socket_info(int *num_sockets);
static int posix_module_get_core_info(int socket, int *num_cores);
static int get_physical_processor_id(int logical_processor_id);
static int get_physical_socket_id(int logical_socket_id);
static int get_physical_core_id(int physical_socket_id, int logical_core_id);

/*
 * Solaris paffinity module
 */
static const opal_paffinity_base_module_1_1_0_t loc_module = {
    /* Initialization function */
    posix_module_init,

    /* Module function pointers */
    posix_module_set,
    posix_module_get,
    posix_module_map_to_processor_id,
    posix_module_map_to_socket_core,
    posix_module_get_processor_info,
    posix_module_get_socket_info,
    posix_module_get_core_info,
    get_physical_processor_id,
    get_physical_socket_id,
    get_physical_core_id,
    posix_module_finalize
};

int opal_paffinity_posix_component_query(mca_base_module_t **module, int *priority)
{
    /* set this priority really low so we will be overridden by any
     * other component such as linux or solaris if they can build
     */
    *priority = 2;
    *module = (mca_base_module_t *)&loc_module;
    
    return OPAL_SUCCESS;
}

/* do nothing here. both mpirun and processes would run init(), but
 * only processes would run the posix_module_set function */
static int posix_module_init(void)
{
    return OPAL_SUCCESS;
}

/* this gives us a cpumask which tells which CPU to bind */
static int posix_module_set(opal_paffinity_base_cpu_set_t cpumask)
{
    return OPAL_ERR_NOT_SUPPORTED;
}

/* This get function returns the CPU id that's currently binded,
 * and then sets the cpumask. */
static int posix_module_get(opal_paffinity_base_cpu_set_t *cpumask) 
{
    return OPAL_ERR_NOT_SUPPORTED;
}

static int posix_module_map_to_processor_id(int socket, int core, int *processor_id)
{
    return OPAL_ERR_NOT_SUPPORTED;
}

static int posix_module_map_to_socket_core(int processor_id, int *socket, int *core)
{
    return OPAL_ERR_NOT_SUPPORTED;
}

static int posix_module_get_processor_info(int *num_processors)
{
    int rc;
    
    /* get the number of active processors */
    if (0 > (rc = sysconf(_SC_NPROCESSORS_ONLN))) {
        /* system was unable to provide a number, so return
         * an error and set the values to something negative
         */
        *num_processors = -1;
        return OPAL_ERR_NOT_FOUND;
    }
    /* rc will contain the number of processors
     * that are currently online - return that value
     */
    *num_processors = rc;

    return OPAL_SUCCESS;
}

static int posix_module_get_socket_info(int *num_sockets)
{
    return OPAL_ERR_NOT_SUPPORTED;
}

static int posix_module_get_core_info(int socket, int *num_cores)
{
    return OPAL_ERR_NOT_SUPPORTED;
}

static int get_physical_processor_id(int logical_processor_id)
{
    return OPAL_ERR_NOT_SUPPORTED;
}

static int get_physical_socket_id(int logical_socket_id)
{
    return OPAL_ERR_NOT_SUPPORTED;
}

static int get_physical_core_id(int physical_socket_id, int logical_core_id)
{
    return OPAL_ERR_NOT_SUPPORTED;
}


static int posix_module_finalize(void)
{
    return OPAL_SUCCESS;
}

