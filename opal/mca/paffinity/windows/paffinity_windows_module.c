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
#include "opal/mca/base/mca_base_param.h"
#include "opal/mca/paffinity/paffinity.h"
#include "opal/mca/paffinity/base/base.h"
#include "paffinity_windows.h"


/*
 * Local functions
 */
static int windows_module_init(void);
static int windows_module_get_num_procs(int *num_procs);
static int windows_module_set(int id);
static int windows_module_get(int *id);

/*
 * Linux paffinity module
 */
static const opal_paffinity_base_module_1_0_0_t module = {

    /* Initialization function */

    windows_module_init,

    /* Module function pointers */

    windows_module_get_num_procs,
    windows_module_set,
    windows_module_get
};


const opal_paffinity_base_module_1_0_0_t *
opal_paffinity_windows_component_query(int *query)
{
    int param;

    param = mca_base_param_find("paffinity", "windows", "priority");
    mca_base_param_lookup_int(param, query);

    return &module;
}


static int windows_module_init(void)
{
    /* Nothing to do */

    return OPAL_SUCCESS;
}


static int windows_module_get_num_procs(int *num_procs)
{
    *num_procs = 1;
    return OPAL_SUCCESS;
}

static int windows_module_set(int id)
{
    int num_procs;

    windows_module_get_num_procs(&num_procs);
    if (id >= num_procs || id < 0) {
        return OPAL_ERR_BAD_PARAM;
    }

    return OPAL_SUCCESS;
}


static int windows_module_get(int *id)
{
    return OPAL_SUCCESS;
}

