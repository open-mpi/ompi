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

#include <mach/mach_host.h>
#include <mach/host_info.h>

#include "opal/constants.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/mca/paffinity/paffinity.h"
#include "opal/mca/paffinity/base/base.h"
#include "opal/util/output.h"

#include "paffinity_darwin.h"

/*
 * Local functions
 */
static int darwin_module_init(void);
static int darwin_module_set(opal_paffinity_base_cpu_set_t cpumask);
static int darwin_module_get(opal_paffinity_base_cpu_set_t *cpumask);
static int darwin_module_finalize(void);
static int darwin_module_map_to_processor_id(int socket, int core, int *processor_id);
static int darwin_module_map_to_socket_core(int processor_id, int *socket, int *core);
static int darwin_module_get_processor_info(int *num_processors, int *max_processor_id);
static int darwin_module_get_socket_info(int *num_sockets, int *max_socket_num);
static int darwin_module_get_core_info(int socket, int *num_cores, int *max_core_num);

/*
 * Solaris paffinity module
 */
static const opal_paffinity_base_module_1_1_0_t loc_module = {
    /* Initialization function */
    darwin_module_init,

    /* Module function pointers */
    darwin_module_set,
    darwin_module_get,
    darwin_module_map_to_processor_id,
    darwin_module_map_to_socket_core,
    darwin_module_get_processor_info,
    darwin_module_get_socket_info,
    darwin_module_get_core_info,
    darwin_module_finalize
};

int opal_paffinity_darwin_component_query(mca_base_module_t **module, int *priority)
{
    /* set this priority high enough to override posix */
    *priority = 30;
    *module = (mca_base_module_t *)&loc_module;
    
    return OPAL_SUCCESS;
}

/* do nothing here. both mpirun and processes would run init(), but
 * only processes would run the darwin_module_set function */
static int darwin_module_init(void)
{
    return OPAL_SUCCESS;
}

/* this gives us a cpumask which tells which CPU to bind */
static int darwin_module_set(opal_paffinity_base_cpu_set_t cpumask)
{
    return OPAL_ERR_NOT_SUPPORTED;
}

/* This get function returns the CPU id that's currently binded,
 * and then sets the cpumask. */
static int darwin_module_get(opal_paffinity_base_cpu_set_t *cpumask) 
{
    return OPAL_ERR_NOT_SUPPORTED;
}

static int darwin_module_map_to_processor_id(int socket, int core, int *processor_id)
{
    return OPAL_ERR_NOT_SUPPORTED;
}

static int darwin_module_map_to_socket_core(int processor_id, int *socket, int *core)
{
    return OPAL_ERR_NOT_SUPPORTED;
}

static int darwin_module_get_processor_info(int *num_processors, int *max_processor_id)
{
    int rc=OPAL_SUCCESS, num_cores;
#if !OPAL_HAVE__SC_NPROCESSORS_ONLN
    host_basic_info_data_t hostInfo;
    mach_msg_type_number_t infoCount;
#endif

#if OPAL_HAVE__SC_NPROCESSORS_ONLN
    /* this must be Leopard - get the number of active processors */
    if (0 > (num_cores = sysconf(_SC_NPROCESSORS_ONLN))) {
        /* system was unable to provide a number, so return
         * an error and set the values to something negative
         */
        num_cores = -1;
        rc = OPAL_ERR_NOT_FOUND;
    }
#else
    /* this must be Tiger - unlike Leopard, Tiger doesn't allow
     * users to turn processors on/off on-the-fly. So we have
     * to query the #processors in a different way
     */
    infoCount = HOST_BASIC_INFO_COUNT;
    if (KERN_SUCCESS != (host_info(mach_host_self(), HOST_BASIC_INFO,
                                   (host_info_t)&hostInfo, &infoCount))) {
        num_cores = -1;
        rc = OPAL_ERR_NOT_FOUND;
    } else {
        num_cores = hostInfo.max_cpus;
    }
#endif

    /* rc will contain the number of processors
     * that are currently online - return that value
     */
    *num_processors = *max_processor_id = num_cores;
    
    return rc;
}

static int darwin_module_get_socket_info(int *num_sockets, int *max_socket_num)
{
    return OPAL_ERR_NOT_SUPPORTED;
}

static int darwin_module_get_core_info(int socket, int *num_cores, int *max_core_num)
{
    return OPAL_ERR_NOT_SUPPORTED;
}


static int darwin_module_finalize(void)
{
    return OPAL_SUCCESS;
}

