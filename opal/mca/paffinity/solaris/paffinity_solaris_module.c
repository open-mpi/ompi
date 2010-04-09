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
 * Copyright (c) 2008-2010 Oracle and/or its affiliates.  All rights reserved.
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
#include <sys/processor.h>
#include <sys/procset.h>
#include <unistd.h>
#include <errno.h>

#include "opal/constants.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/mca/paffinity/paffinity.h"
#include "opal/mca/paffinity/base/base.h"
#include "paffinity_solaris.h"
#include "opal/util/output.h"

/*
 * Local functions
 */
static int solaris_module_init(void);
static int solaris_module_set(opal_paffinity_base_cpu_set_t cpumask);
static int solaris_module_get(opal_paffinity_base_cpu_set_t *cpumask);
static int solaris_module_finalize(void);
static int cpumask_to_id(opal_paffinity_base_cpu_set_t cpumask);
static int solaris_module_map_to_processor_id(int socket, int core, int *processor_id);
static int solaris_module_map_to_socket_core(int processor_id, int *socket, int *core);
static int solaris_module_get_processor_info(int *num_processors);
static int solaris_module_get_socket_info(int *num_sockets);
static int solaris_module_get_core_info(int socket, int *num_cores);
static int solaris_module_get_physical_processor_id(int logical_processor_id);
static int solaris_module_get_physical_socket_id(int logical_socket_id);
static int solaris_module_get_physical_core_id(int physical_socket_id, int logical_core_id);

/*
 * Solaris paffinity module
 */
static const opal_paffinity_base_module_1_1_0_t loc_module = {
    /* Initialization function */
    solaris_module_init,

    /* Module function pointers */
    solaris_module_set,
    solaris_module_get,
    solaris_module_map_to_processor_id,
    solaris_module_map_to_socket_core,
    solaris_module_get_processor_info,
    solaris_module_get_socket_info,
    solaris_module_get_core_info,
    solaris_module_get_physical_processor_id,
    solaris_module_get_physical_socket_id,
    solaris_module_get_physical_core_id,
    solaris_module_finalize
};

int opal_paffinity_solaris_component_query(mca_base_module_t **module, int *priority)
{
    int param;

    param = mca_base_param_find("paffinity", "solaris", "priority");
    mca_base_param_lookup_int(param, priority);

    *module = (mca_base_module_t *)&loc_module;

    return OPAL_SUCCESS;
}

/* do nothing here. both mpirun and processes would run init(), but
 * only processes would run the solaris_module_set function */
static int solaris_module_init(void)
{
    return OPAL_SUCCESS;
}

/* this gives us a cpumask which tells which CPU to bind */
static int solaris_module_set(opal_paffinity_base_cpu_set_t cpumask)
{
    processorid_t cpuid;


    /* Find out where in the cpumask is the location of the current CPU. */
    cpuid = cpumask_to_id(cpumask);
    if (-1 == cpuid) {
        opal_output(0, "paffinity:solaris: Error when coverting cpumask to id");
        return OPAL_ERR_IN_ERRNO;
    }

    if (0 != processor_bind(P_PID, P_MYID, cpuid, NULL)) {
        opal_output(0, "paffinity:solaris: Error when binding to CPU #%d: %s",
            cpuid, strerror(errno));
        return OPAL_ERR_IN_ERRNO;
    }

    opal_output_verbose(5, opal_paffinity_base_output,
        "paffinity:solaris: Successfully bind to CPU #%d", cpuid);
    return OPAL_SUCCESS;
}

/* this takes a cpumask and converts it to CPU id on a node */
static int cpumask_to_id(opal_paffinity_base_cpu_set_t cpumask)
{
    int currid;

    for(currid=0; currid<OPAL_PAFFINITY_BITMASK_CPU_MAX; currid++) {
        if (OPAL_PAFFINITY_CPU_ISSET(currid, cpumask))
            return currid;
    }
    return -1;
}

/* This get function returns the CPU id that's currently binded,
 * and then sets the cpumask. */
static int solaris_module_get(opal_paffinity_base_cpu_set_t *cpumask) 
{
    processorid_t obind;
    if (0 != processor_bind(P_PID, P_MYID, PBIND_QUERY, &obind)) {
        return OPAL_ERR_IN_ERRNO;
    }

    opal_output_verbose(5, opal_paffinity_base_output,
        "paffinity:solaris: obind=%d", obind);

    /* if there isn't any processor binded, just zero out and return */
    OPAL_PAFFINITY_CPU_ZERO(*cpumask);
    if (PBIND_NONE != obind) {
        OPAL_PAFFINITY_CPU_SET(obind, *cpumask);
    }
    return OPAL_SUCCESS;
}

static int solaris_module_map_to_processor_id(int socket, int core, int *processor_id)
{
    return OPAL_ERR_NOT_SUPPORTED;
}

static int solaris_module_map_to_socket_core(int processor_id, int *socket, int *core)
{
    return OPAL_ERR_NOT_SUPPORTED;
}

static int solaris_module_get_processor_info(int *num_processors)
{
    processorid_t currid, cpuid_max;
    processor_info_t pinfo;

    /* cpuid_max is the max number available for a system arch. It is
     * an inclusive list. e.g. If cpuid_max=31, cpuid would be 0-31 */
    cpuid_max = sysconf(_SC_CPUID_MAX);

    /* clear out num_processors to make sure we get the correct count */
    *num_processors = 0;

    /* Because not all CPU ID in cpuid_max are actually valid,
     * and CPU ID may also not be contiguous. Therefore we
     * need to run through processor_info to ensure the validity.
     * Then find out which are actually online */
    for (currid=0; currid<=cpuid_max; currid++) {
        if (0 == processor_info(currid, &pinfo)) {
            if (P_ONLINE == pinfo.pi_state || P_NOINTR == pinfo.pi_state) {
                 (*num_processors)++;
            }
        }
    }
    return OPAL_SUCCESS;
}

static int solaris_module_get_socket_info(int *num_sockets)
{
    return OPAL_ERR_NOT_SUPPORTED;
}

static int solaris_module_get_core_info(int socket, int *num_cores)
{
    return OPAL_ERR_NOT_SUPPORTED;
}

static int solaris_module_get_physical_processor_id(int logical_processor_id)
{
    processorid_t currid, retid, cpuid_max, cpuid_log=0;
    processor_info_t pinfo;

    /* cpuid_max is the max number available for a system arch. It is
     * an inclusive list. e.g. If cpuid_max=31, cpuid would be 0-31 */
    if ( 0 > (cpuid_max = sysconf(_SC_CPUID_MAX))) {
	return OPAL_ERR_NOT_SUPPORTED;
    }

    /* set retid to OPAL_ERROR to reflect no processor found to match logical proc */
    retid = OPAL_ERROR;
    /* Because not all CPU ID in cpuid_max are actually valid,
     * and CPU ID may also not be contiguous. Therefore we
     * need to run through processor_info to ensure the validity.
     * Then find out which are actually online */
    for (currid=0; currid<=cpuid_max; currid++) {
        if (0 == processor_info(currid, &pinfo)) {
            if (P_ONLINE == pinfo.pi_state || P_NOINTR == pinfo.pi_state) {
                 if (cpuid_log == logical_processor_id) {
		     retid = currid;
                     break;
                 } 
                 cpuid_log++;
            }
        }
    }
    
    return retid;
}

static int solaris_module_get_physical_socket_id(int logical_socket_id)
{
    return OPAL_ERR_NOT_SUPPORTED;
}

static int solaris_module_get_physical_core_id(int physical_socket_id, int logical_core_id)
{
    return OPAL_ERR_NOT_SUPPORTED;
}


static int solaris_module_finalize(void)
{
    return OPAL_SUCCESS;
}

