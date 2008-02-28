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
 * Copyright (c) 2006-2007 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "ompi/constants.h"

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"

#include "orte/mca/grpcomm/grpcomm.h"

#include "ompi/proc/proc.h"
#include "ompi/runtime/ompi_module_exchange.h"


int
ompi_modex_send(mca_base_component_t * source_component,
                const void *data, size_t size)
{
    int rc;
    char * name = mca_base_component_to_string(source_component);
    
    if(NULL == name) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    
    rc = orte_grpcomm.set_proc_attr(name, data, size);
    free(name);
    return rc;
}


int
ompi_modex_recv(mca_base_component_t * component,
                ompi_proc_t * proc,
                void **buffer,
                size_t * size)
{
    int rc;
    char * name = mca_base_component_to_string(component);
    
    if(NULL == name) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    
    rc = orte_grpcomm.get_proc_attr(proc->proc_name, name, buffer, size);
    free(name);
    return rc;
}

int
ompi_modex_send_string(const char* key,
                       const void *buffer, size_t size)
{
    return orte_grpcomm.set_proc_attr(key, buffer, size);
}


int
ompi_modex_recv_string(const char* key,
                       struct ompi_proc_t *source_proc,
                       void **buffer, size_t *size)
{
    return orte_grpcomm.get_proc_attr(source_proc->proc_name, key, buffer, size);
}
