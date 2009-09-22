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
 * Local static variables
 */
opal_paffinity_linux_plpa_cpu_set_t global_paff_mask;

/*
 * Local functions
 */
static int linux_module_init(void);
static int linux_module_set(opal_paffinity_base_cpu_set_t cpumask);
static int linux_module_get(opal_paffinity_base_cpu_set_t *cpumask);
static int linux_module_map_to_processor_id(int socket, int core, int *processor_id);
static int linux_module_map_to_socket_core(int processor_id, int *socket, int *core);
static int linux_module_get_processor_info(int *num_processors);
static int linux_module_get_socket_info(int *num_sockets);
static int linux_module_get_core_info(int socket, int *num_cores);
static int get_physical_processor_id(int logical_processor_id);
static int get_physical_socket_id(int logical_socket_id);
static int get_physical_core_id(int physical_socket_id, int logical_core_id);

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
    get_physical_processor_id,
    get_physical_socket_id,
    get_physical_core_id,
    NULL
};

/* Trivial helper function to convert system error codes to OPAL_ERR_*
   codes */
static int convert(int ret) 
{
    switch(ret) {
    case 0:
        return OPAL_SUCCESS;
    case ENOSYS:
        return OPAL_ERR_NOT_SUPPORTED;
    case EINVAL:
        return OPAL_ERR_BAD_PARAM;
    default:
        return OPAL_ERROR;
    }
}

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
    int supported;
    opal_paffinity_linux_plpa_cpu_set_t tmp;
    int i;
    
    /* ensure the global mask is clean */
    OPAL_PAFFINITY_CPU_ZERO(global_paff_mask);

    /* check if PLPA supports topology */
    opal_paffinity_linux_plpa_have_topology_information(&supported);
    
    if (!supported) {
        /* do a little dance to give us some info we can
         * use to support at least binding processors
         */
        OPAL_PAFFINITY_CPU_ZERO(tmp);  /* ensure this is clean */
        /* get our current affinity so we can return to it later */
        opal_paffinity_linux_plpa_sched_getaffinity(getpid(), sizeof(tmp), &tmp);
        /* set all the bits in the global mask */
        for (i=0; i < OPAL_PAFFINITY_BITMASK_CPU_MAX; i++) {
            OPAL_PAFFINITY_CPU_SET(i, global_paff_mask);
        }
        /* set the affinity, but don't check the return code as
         * it may return an error. This is a simple method
         * for probing which processors actually exist
         */
        opal_paffinity_linux_plpa_sched_setaffinity(getpid(), 
                                                    sizeof(global_paff_mask), 
                                                    &global_paff_mask);
        /* now do a get and find out where we actually are bound */
        opal_paffinity_linux_plpa_sched_getaffinity(getpid(),
                                                    sizeof(global_paff_mask),
                                                    &global_paff_mask);
        /* the mask now contains a map of the actual physical processors
         * Set ourselves back to our original affinity
         */
        opal_paffinity_linux_plpa_sched_setaffinity(getpid(), 
                                                    sizeof(tmp), 
                                                    &tmp);
    }
    
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
	for (i = 0; i < 8 * sizeof(mask) ; i++) {
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
    for (i = 0; i < 8 * sizeof(*mask); i++) {
	if (PLPA_CPU_ISSET(i,&plpa_mask)) {
	    PLPA_CPU_SET(i,mask);
	}
    }

    return OPAL_SUCCESS;
}

static int linux_module_map_to_processor_id(int socket, int core, int *processor_id)
{
    int ret = opal_paffinity_linux_plpa_map_to_processor_id(socket, core, 
                                                            processor_id);
    return convert(ret);
}

static int linux_module_map_to_socket_core(int processor_id, int *socket, int *core)
{
    int ret = opal_paffinity_linux_plpa_map_to_socket_core(processor_id, 
                                                           socket, core);
    return convert(ret);
}

static int linux_module_get_processor_info(int *num_processors)
{
    int max_processor_id;
    
    int ret = opal_paffinity_linux_plpa_get_processor_data(OPAL_PAFFINITY_LINUX_PLPA_COUNT_ONLINE,
                                                           num_processors, 
                                                           &max_processor_id);

    /* If we're on a kernel that does not support the topology
       functionality, PLPA will return ENOSYS and not try to calculate
       the number of processors or max processor.  In this case, just
       call sysconf() and assume that the max_processor_id equals the
       number of processors. */
    if (ENOSYS == ret) {
        ret = sysconf(_SC_NPROCESSORS_ONLN);
        if (ret > 0) {
            *num_processors = ret;
            return OPAL_SUCCESS;
        } else {
            return OPAL_ERR_IN_ERRNO;
        }
    } else {
        return convert(ret);
    }
}

static int linux_module_get_socket_info(int *num_sockets)
{
    int max_socket_num;
    
    int ret = opal_paffinity_linux_plpa_get_socket_info(num_sockets, 
                                                        &max_socket_num);
    return convert(ret);
}

static int linux_module_get_core_info(int socket, int *num_cores)
{
    int max_core_num;
    
    int ret = opal_paffinity_linux_plpa_get_core_info(socket, num_cores, 
                                                      &max_core_num);
    return convert(ret);
}

static int get_physical_processor_id(int logical_processor_id)
{
    int ret, phys_id;
    int i, count;
    
    ret = opal_paffinity_linux_plpa_get_processor_id(logical_processor_id,
                                                     OPAL_PAFFINITY_LINUX_PLPA_COUNT_ONLINE,
                                                     &phys_id);
    if (0 == ret) {
        /* PLPA was able to return a value, so pass it along */
        return phys_id;
    }

    ret = convert(ret);
    if (OPAL_ERR_NOT_SUPPORTED == ret) {
        /* if it isn't supported, then we may be able
         * to use our global_paff_mask to compute the
         * mapping
         */
        count = 0;
        for (i=0; i < OPAL_PAFFINITY_BITMASK_CPU_MAX; i++) {
            if (OPAL_PAFFINITY_CPU_ISSET(i, global_paff_mask)) {
                if (count == logical_processor_id) {
                    ret = i;
                    break;
                } 
                count++;
            }
        }
    }
    /* if we executed the above loop and didn't find anything,
     * this will still be set to OPAL_ERR_NOT_SUPPORTED, which
     * is what we want in that case
     */
    return ret;
}


static int get_physical_socket_id(int logical_socket_id)
{
    int ret, phys_id;

    ret = opal_paffinity_linux_plpa_get_socket_id(logical_socket_id,
                                                  &phys_id);
    if (0 == ret) {
        return phys_id;
    } else {
        return convert(ret);
    }
}

static int get_physical_core_id(int physical_socket_id, int logical_core_id)
{
    int ret, phys_id;

    ret = opal_paffinity_linux_plpa_get_core_id(physical_socket_id,
                                                logical_core_id, &phys_id);
    if (0 == ret) {
        return phys_id;
    } else {
        return convert(ret);
    }
}

