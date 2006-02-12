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

#include "opal/constants.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/mca/paffinity/paffinity.h"
#include "opal/mca/paffinity/base/base.h"
#include "paffinity_solaris.h"


/*
 * Local functions
 */
static int solaris_module_init(void);
static int solaris_module_get_num_procs(int *num_procs);
static int solaris_module_set(int id);
static int solaris_module_get(int *id);

/*
 * Solaris paffinity module
 */
static const opal_paffinity_base_module_1_0_0_t module = {

    /* Initialization function */

    solaris_module_init,

    /* Module function pointers */

    solaris_module_get_num_procs,
    solaris_module_set,
    solaris_module_get
};


const opal_paffinity_base_module_1_0_0_t *
opal_paffinity_solaris_component_query(int *query)
{
    int param;

    param = mca_base_param_find("paffinity", "solaris", "priority");
    mca_base_param_lookup_int(param, query);

    return &module;
}


static int solaris_module_init(void)
{
    /* Nothing to do */

    return OPAL_SUCCESS;
}


static int solaris_module_get_num_procs(int *num_procs)
{
    *num_procs = sysconf(_SC_NPROCESSORS_ONLN);
    return OPAL_SUCCESS;
}


static int solaris_module_set(int id)
{
    if (0 != processor_bind(P_PID, P_MYID, (processorid_t) id, NULL)) {
        return OPAL_ERR_IN_ERRNO;
    }
    return OPAL_SUCCESS;
}


static int solaris_module_get(int *id)
{
    processorid_t obind;
    if (0 != processor_bind(P_PID, P_MYID, PBIND_QUERY, &obind)) {
        return OPAL_ERR_IN_ERRNO;
    }
    *id = (int) obind;
    return OPAL_SUCCESS;
}
