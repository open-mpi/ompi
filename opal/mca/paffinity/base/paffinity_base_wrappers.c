/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "opal_config.h"

#if HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "opal/constants.h"
#include "opal/mca/paffinity/paffinity.h"
#include "opal/mca/paffinity/base/base.h"

int opal_paffinity_base_set(opal_paffinity_base_cpu_set_t cpumask)
{
    if (!opal_paffinity_base_selected) {
        return OPAL_ERR_NOT_FOUND;
    }
    return opal_paffinity_base_module->paff_module_set(cpumask);
}

int opal_paffinity_base_get(opal_paffinity_base_cpu_set_t *cpumask)
{
    if (!opal_paffinity_base_selected) {
        if(NULL != cpumask) {
            OPAL_PAFFINITY_CPU_ZERO(*cpumask);
        }
        return OPAL_ERR_NOT_FOUND;
    }
    if(NULL == cpumask) {
        return OPAL_ERR_BAD_PARAM;
    }
    return opal_paffinity_base_module->paff_module_get(cpumask);
}

int opal_paffinity_base_get_map_to_processor_id(int socket, int core, int *processor_id)
{
    if (!opal_paffinity_base_selected) {
        return OPAL_ERR_NOT_FOUND;
    }
    return opal_paffinity_base_module->paff_get_map_to_processor_id(socket, core, processor_id);
}

int opal_paffinity_base_get_map_to_socket_core(int processor_id, int *socket, int *core)
{
    if (!opal_paffinity_base_selected) {
        return OPAL_ERR_NOT_FOUND;
    }
    return opal_paffinity_base_module->paff_get_map_to_socket_core(processor_id, socket, core);
}


int opal_paffinity_base_get_processor_info(int *num_processors)
{
    if (!opal_paffinity_base_selected) {
        return OPAL_ERR_NOT_FOUND;
    }
    return opal_paffinity_base_module->paff_get_processor_info(num_processors);
}

int opal_paffinity_base_get_socket_info(int *num_sockets)
{
    if (!opal_paffinity_base_selected) {
        return OPAL_ERR_NOT_FOUND;
    }
    return opal_paffinity_base_module->paff_get_socket_info(num_sockets);
}

int opal_paffinity_base_get_core_info(int socket, int *num_cores)
{
    if (!opal_paffinity_base_selected) {
        return OPAL_ERR_NOT_FOUND;
    }
    return opal_paffinity_base_module->paff_get_core_info(socket, num_cores);
}

int opal_paffinity_base_get_physical_processor_id(int logical_processor_id)
{
    if (!opal_paffinity_base_selected) {
        return OPAL_ERR_NOT_FOUND;
    }
    return opal_paffinity_base_module->paff_get_physical_processor_id(logical_processor_id);
}

int opal_paffinity_base_get_physical_socket_id(int logical_socket_id)
{
    if (!opal_paffinity_base_selected) {
        return OPAL_ERR_NOT_FOUND;
    }
    return opal_paffinity_base_module->paff_get_physical_socket_id(logical_socket_id);
}

int opal_paffinity_base_get_physical_core_id(int physical_socket_id, int logical_core_id)
{
    if (!opal_paffinity_base_selected) {
        return OPAL_ERR_NOT_FOUND;
    }
    return opal_paffinity_base_module->paff_get_physical_core_id(physical_socket_id, logical_core_id);
}

