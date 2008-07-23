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

#include "paffinity_osx.h"

/*
 * Local functions
 */
static int osx_module_init(void);
static int osx_module_set(opal_paffinity_base_cpu_set_t cpumask);
static int osx_module_get(opal_paffinity_base_cpu_set_t *cpumask);
static int osx_module_finalize(void);
static int osx_module_map_to_processor_id(int socket, int core, int *processor_id);
static int osx_module_map_to_socket_core(int processor_id, int *socket, int *core);
static int osx_module_get_processor_info(int *num_processors, int *max_processor_id);
static int osx_module_get_socket_info(int *num_sockets, int *max_socket_num);
static int osx_module_get_core_info(int socket, int *num_cores, int *max_core_num);

/*
 * Solaris paffinity module
 */
static const opal_paffinity_base_module_1_1_0_t loc_module = {
    /* Initialization function */
    osx_module_init,

    /* Module function pointers */
    osx_module_set,
    osx_module_get,
    osx_module_map_to_processor_id,
    osx_module_map_to_socket_core,
    osx_module_get_processor_info,
    osx_module_get_socket_info,
    osx_module_get_core_info,
    osx_module_finalize
};

int opal_paffinity_osx_component_query(mca_base_module_t **module, int *priority)
{
    *priority = 2;
    *module = (mca_base_module_t *)&loc_module;
    
    return OPAL_SUCCESS;
}

/* do nothing here. both mpirun and processes would run init(), but
 * only processes would run the osx_module_set function */
static int osx_module_init(void)
{
    return OPAL_SUCCESS;
}

/* this gives us a cpumask which tells which CPU to bind */
static int osx_module_set(opal_paffinity_base_cpu_set_t cpumask)
{
    return OPAL_ERR_NOT_SUPPORTED;
}

/* This get function returns the CPU id that's currently binded,
 * and then sets the cpumask. */
static int osx_module_get(opal_paffinity_base_cpu_set_t *cpumask) 
{
    return OPAL_ERR_NOT_SUPPORTED;
}

static int osx_module_map_to_processor_id(int socket, int core, int *processor_id)
{
    return OPAL_ERR_NOT_SUPPORTED;
}

static int osx_module_map_to_socket_core(int processor_id, int *socket, int *core)
{
    return OPAL_ERR_NOT_SUPPORTED;
}

static int osx_module_get_processor_info(int *num_processors, int *max_processor_id)
{
    int rc;
    
    /* get the number of active processors */
    if (0 > (rc = sysconf(_SC_NPROCESSORS_ONLN))) {
        /* system was unable to provide a number, so return
         * an error and set the values to something negative
         */
        *num_processors = *max_processor_id = -1;
        return OPAL_ERR_NOT_FOUND;
    }
    /* rc will contain the number of processors
     * that are currently online - return that value
     */
    *num_processors = *max_processor_id = rc;

    return OPAL_SUCCESS;
}

static int osx_module_get_socket_info(int *num_sockets, int *max_socket_num)
{
    return OPAL_ERR_NOT_SUPPORTED;
}

static int osx_module_get_core_info(int socket, int *num_cores, int *max_core_num)
{
    return OPAL_ERR_NOT_SUPPORTED;
}


static int osx_module_finalize(void)
{
    return OPAL_SUCCESS;
}

