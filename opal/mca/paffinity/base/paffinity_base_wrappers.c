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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "opal_config.h"

#include "opal/constants.h"
#include "opal/mca/paffinity/paffinity.h"
#include "opal/mca/paffinity/base/base.h"

int opal_paffinity_base_get_num_processors(int *num_procs)
{
    if (!opal_paffinity_base_selected) {
        *num_procs = -1;
        return OPAL_ERR_NOT_FOUND;
    }
    return opal_paffinity_base_module->paff_module_get_num_processors(num_procs);
}


int opal_paffinity_base_set(int id)
{
    if (!opal_paffinity_base_selected) {
        return OPAL_ERR_NOT_FOUND;
    }
    return opal_paffinity_base_module->paff_module_set(id);
}


int opal_paffinity_base_get(int *id)
{
    if (!opal_paffinity_base_selected) {
        *id = -1;
        return OPAL_ERR_NOT_FOUND;
    }
    return opal_paffinity_base_module->paff_module_get(id);
}
