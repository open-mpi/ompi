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
 * Copyright (c) 2006-2007 Cisco Systems, Inc.  All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"

/* This component will only be compiled on Linux, where we are
   guaranteed to have <unistd.h> and friends */
#include <stdio.h>

#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#include "opal/constants.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/mca/paffinity/paffinity.h"
#include "opal/mca/paffinity/base/base.h"
#include "paffinity_linux.h"
#include "plpa/src/libplpa/plpa.h"


/*
 * Local functions
 */
static int linux_module_init(void);
static int linux_module_set(opal_paffinity_base_cpu_set_t cpumask);
static int linux_module_get(opal_paffinity_base_cpu_set_t *cpumask);
static int linux_module_map_to_processor_id(int socket, int core, int *processor_id);
static int linux_module_map_to_socket_core(int processor_id, int *socket, int *core);
static int linux_module_get_processor_info(int *num_processors, int *max_processor_id);
static int linux_module_get_socket_info(int *num_sockets, int *max_socket_num);
static int linux_module_get_core_info(int socket, int *num_cores, int *max_core_num);

/*
 * Linux paffinity module
 */
static const opal_paffinity_base_module_1_1_0_t loc_module = {
    /* Initialization function */
    linux_module_init,

    /* Module function pointers */
    linux_module_set,
    linux_module_get,
    linux_module_map_to_processor_id,
    linux_module_map_to_socket_core,
    linux_module_get_processor_info,
    linux_module_get_socket_info,
    linux_module_get_core_info,
    NULL
};

int opal_paffinity_linux_component_query(mca_base_module_t **module, int *priority)
{
    int param;

    param = mca_base_param_find("paffinity", "linux", "priority");
    mca_base_param_lookup_int(param, priority);

    *module = (mca_base_module_t *)&loc_module;

    return OPAL_SUCCESS;
}


static int linux_module_init(void)
{
    /* Nothing to do */

    return OPAL_SUCCESS;
}



/************************************************************************
   See the note in paffinity_linux.h -- there are at least 3 different
   ways that Linux's sched_setaffinity()/sched_getaffinity() are
   implemented.  Thankfully there is the Portable Linux Processor
   Affinity project which determines the flavor of affinity at runtime
   and takes care of of the problem.

   Using get/set affinity functions from plpa - configured with an
   opal prefix.

   User needs to set a mask with the bit number of the cpu set. We provide
   macros to do this.

 ************************************************************************/

static int linux_module_set(opal_paffinity_base_cpu_set_t mask)
{

    opal_paffinity_linux_plpa_cpu_set_t plpa_mask;
    unsigned int i;

    if (sizeof(mask) > sizeof(plpa_mask)) {
        return OPAL_ERR_BAD_PARAM;
    } else {
        PLPA_CPU_ZERO(&plpa_mask);
	for (i = 0; i < sizeof(plpa_mask) ; i++) {
	    if (PLPA_CPU_ISSET(i,&mask)) {
		PLPA_CPU_SET(i,&plpa_mask);
	    }
	}
    }

    if (0 != opal_paffinity_linux_plpa_sched_setaffinity(getpid(), 
                                                         sizeof(plpa_mask), 
                                                         &plpa_mask)) {
        return OPAL_ERR_IN_ERRNO;
    }
    return OPAL_SUCCESS;
}


static int linux_module_get(opal_paffinity_base_cpu_set_t *mask)
{
    opal_paffinity_linux_plpa_cpu_set_t plpa_mask;
    unsigned int i;

    if (NULL == mask) {
        return OPAL_ERR_BAD_PARAM;
    }

    if (sizeof(*mask) > sizeof(plpa_mask)) {
        return OPAL_ERR_BAD_PARAM; /* look up in header file */
    }

    if (0 != opal_paffinity_linux_plpa_sched_getaffinity(getpid(), sizeof(plpa_mask), &plpa_mask)) {
        return OPAL_ERR_IN_ERRNO;
    }
    for (i = 0; i < sizeof(mask); i++) {
	if (PLPA_CPU_ISSET(i,&plpa_mask)) {
	    PLPA_CPU_SET(i,mask);
	}
    }

    return OPAL_SUCCESS;
}

static int linux_module_map_to_processor_id(int socket, int core, int *processor_id)
{
   return opal_paffinity_linux_plpa_map_to_processor_id(socket, core, processor_id);
}

static int linux_module_map_to_socket_core(int processor_id, int *socket, int *core)
{
   return opal_paffinity_linux_plpa_map_to_socket_core(processor_id, socket, core);
}

static int linux_module_get_processor_info(int *num_processors, int *max_processor_id)
{
   return opal_paffinity_linux_plpa_get_processor_info(num_processors, max_processor_id);
}

static int linux_module_get_socket_info(int *num_sockets, int *max_socket_num)
{
   return opal_paffinity_linux_plpa_get_socket_info(num_sockets, max_socket_num);
}

static int linux_module_get_core_info(int socket, int *num_cores, int *max_core_num)
{
   return opal_paffinity_linux_plpa_get_core_info(socket, num_cores, max_core_num);
}

