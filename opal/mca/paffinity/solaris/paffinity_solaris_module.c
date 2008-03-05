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
#include <sys/processor.h>
#include <sys/procset.h>
#include <unistd.h>
#include <errno.h>

#include "opal/constants.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/mca/paffinity/paffinity.h"
#include "opal/mca/paffinity/base/base.h"
#include "paffinity_solaris.h"

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
static int solaris_module_get_processor_info(int *num_processors, int *max_processor_id);
static int solaris_module_get_socket_info(int *num_sockets, int *max_socket_num);        
static int solaris_module_get_core_info(int socket, int *num_cores, int *max_core_num);  

/*
 * Solaris paffinity module
 */
static const opal_paffinity_base_module_1_1_0_t module = {

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
    solaris_module_finalize
};


const opal_paffinity_base_module_1_1_0_t *
opal_paffinity_solaris_component_query(int *query)
{
    int param;

    param = mca_base_param_find("paffinity", "solaris", "priority");
    mca_base_param_lookup_int(param, query);

    return &module;
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
    int index, *cpuid_list, cpuid_loc=0;
    processorid_t currid, cpuid_max;
    processor_info_t pinfo;

    /* cpuid_max is the max number available for a system arch. It is
     * an inclusive list. e.g. If cpuid_max=31, cpuid would be 0-31 */
    cpuid_max = sysconf(_SC_CPUID_MAX);
    cpuid_list = (int*)malloc((cpuid_max+1)*sizeof(int));

    /* Because not all CPU ID in cpuid_max are actually valid,
     * and CPU ID may also not be contiguous. Therefore we
     * need to run through processor_info to ensure the validity.
     * Then find out which are actually online */
    for (currid=0; currid<=cpuid_max; currid++) {
        if (0 == processor_info(currid, &pinfo)) {
            if (P_ONLINE == pinfo.pi_state) {
                 cpuid_list[cpuid_loc++] = currid;
            } 
        }
    }

    /* Find out where in the cpumask is the location of the current CPU.
     * Once the index position of the CPU is located, then the set index
     * We will use the same index in cpuid_list to locate the CPU ID */
    index = cpumask_to_id(cpumask);
    if (-1 == index) {
        opal_output(0, "paffinity:solaris: Error when coverting cpumask to id");
        return OPAL_ERR_IN_ERRNO;
    }

    if (0 != processor_bind(P_PID, P_MYID, cpuid_list[index], NULL)) {
        opal_output(0, "paffinity:solaris: Error when binding to CPU %d: %s",
            cpuid_list[index], strerror(errno));
        free(cpuid_list);
        return OPAL_ERR_IN_ERRNO;
    }
    opal_output_verbose(100, opal_paffinity_base_output,
        "paffinity:solaris: Successfully bind to CPU %d", cpuid_list[index]);
    free(cpuid_list);
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
 * and then sets the cpumask. But I don't see this function or the
 * base paffinity get called from anywhere */
static int solaris_module_get(opal_paffinity_base_cpu_set_t *cpumask) 
{
    processorid_t obind;
    if (0 != processor_bind(P_PID, P_MYID, PBIND_QUERY, &obind)) {
        return OPAL_ERR_IN_ERRNO;
    }
    opal_output_verbose(100, opal_paffinity_base_output,
        "paffinity:solaris: obind=%d", obind);

    OPAL_PAFFINITY_CPU_ZERO(*cpumask);
    OPAL_PAFFINITY_CPU_SET(obind, *cpumask);
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

static int solaris_module_get_processor_info(int *num_processors, int *max_processor_id);
{
    return OPAL_ERR_NOT_SUPPORTED;
}

static int solaris_module_get_socket_info(int *num_sockets, int *max_socket_num);        
{
    return OPAL_ERR_NOT_SUPPORTED;
}

static int solaris_module_get_core_info(int socket, int *num_cores, int *max_core_num);  
{
    return OPAL_ERR_NOT_SUPPORTED;
}


static int solaris_module_finalize(void)
{
    return OPAL_SUCCESS;
}

