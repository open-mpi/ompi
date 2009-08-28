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
#include "opal/constants.h"

#include "opal/mca/base/mca_base_param.h"
#include "opal/mca/paffinity/paffinity.h"
#include "opal/mca/paffinity/base/base.h"

#include "paffinity_test.h"

/* Fake an arch */
#define NUM_SOCKETS 4
#define NUM_CORES   4

/*
 * Local functions
 */
static int init(void);
static int set(opal_paffinity_base_cpu_set_t cpumask);
static int get(opal_paffinity_base_cpu_set_t *cpumask);
static int finalize(void);
static int map_to_processor_id(int socket, int core, int *processor_id);
static int map_to_socket_core(int processor_id, int *socket, int *core);
static int get_processor_info(int *num_processors);
static int get_socket_info(int *num_sockets);
static int get_core_info(int socket, int *num_cores);
static int get_physical_processor_id(int logical_processor_id);
static int get_physical_socket_id(int logical_socket_id);
static int get_physical_core_id(int physical_socket_id, int logical_core_id);

/*
 * Test paffinity module
 */
static const opal_paffinity_base_module_1_1_0_t loc_module = {
    /* Initialization function */
    init,

    /* Module function pointers */
    set,
    get,
    map_to_processor_id,
    map_to_socket_core,
    get_processor_info,
    get_socket_info,
    get_core_info,
    get_physical_processor_id,
    get_physical_socket_id,
    get_physical_core_id,
    finalize
};

int opal_paffinity_test_component_query(mca_base_module_t **module, int *priority)
{
    /* set this priority so I can only be selected if directed */
    *priority = 00;
    *module = (mca_base_module_t *)&loc_module;
    
    return OPAL_SUCCESS;
}

/* do nothing here. both mpirun and processes would run init(), but
 * only processes would run the set function */
static int init(void)
{
    return OPAL_SUCCESS;
}

/* this gives us a cpumask which tells which CPU to bind */
static int set(opal_paffinity_base_cpu_set_t cpumask)
{
    return OPAL_SUCCESS;
}

/* This get function returns the CPU id that's currently binded,
 * and then sets the cpumask. */
static int get(opal_paffinity_base_cpu_set_t *cpumask) 
{
    int i;
    
    OPAL_PAFFINITY_CPU_ZERO(*cpumask);
    if (opal_paffinity_test_bound) {
        for (i=0; i < NUM_SOCKETS*NUM_CORES; i+=2) {
            OPAL_PAFFINITY_CPU_SET(i, *cpumask);
        }
        /* assign all cores in the 2nd socket */
        for (i=NUM_CORES; i < 2*NUM_CORES; i++) {
            OPAL_PAFFINITY_CPU_SET(i, *cpumask);
        }
    } else {
        for (i=0; i < NUM_SOCKETS*NUM_CORES; i++) {
            OPAL_PAFFINITY_CPU_SET(i, *cpumask);
        }
    }
    return OPAL_SUCCESS;
}

static int map_to_processor_id(int socket, int core, int *processor_id)
{
    *processor_id = socket*NUM_CORES + core;
    return OPAL_SUCCESS;
}

static int map_to_socket_core(int processor_id, int *socket, int *core)
{
    *socket = processor_id / NUM_CORES;
    *core = processor_id % NUM_CORES;
    return OPAL_SUCCESS;
}

static int get_processor_info(int *num_processors)
{
    *num_processors = NUM_SOCKETS * NUM_CORES;
    return OPAL_SUCCESS;
}

static int get_socket_info(int *num_sockets)
{
    *num_sockets = NUM_SOCKETS;
    return OPAL_SUCCESS;
}

static int get_core_info(int socket, int *num_cores)
{
    *num_cores = NUM_CORES;
    return OPAL_SUCCESS;
}

static int get_physical_processor_id(int logical_processor_id)
{
    return logical_processor_id;
}

static int get_physical_socket_id(int logical_socket_id)
{
    return logical_socket_id;
}

static int get_physical_core_id(int physical_socket_id, int logical_core_id)
{
    if (NUM_CORES < logical_core_id) {
        return OPAL_ERROR;
    }
    return logical_core_id;
}

static int finalize(void)
{
    return OPAL_SUCCESS;
}

