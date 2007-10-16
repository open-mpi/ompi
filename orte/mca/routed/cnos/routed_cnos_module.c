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
 * Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/orte_constants.h"

#include "orte/mca/ns/ns_types.h"
#include "orte/mca/gpr/gpr_types.h"

#include "orte/mca/routed/base/base.h"
#include "routed_cnos.h"

#if OMPI_ROUTED_CNOS_HAVE_BARRIER
#include <catamount/cnos_mpi_os.h>
#endif

/* API functions */
static int orte_routed_cnos_module_init(void);

static int orte_routed_cnos_finalize(void);

static int orte_routed_cnos_update_route(orte_process_name_t *target,
                                   orte_process_name_t *route);

static orte_process_name_t orte_routed_cnos_get_route(orte_process_name_t *target);

static int orte_routed_cnos_init_routes(orte_jobid_t job, orte_gpr_notify_data_t *ndat);

static int orte_routed_cnos_warmup_routes(void);

orte_routed_module_t orte_routed_cnos_module = {
    orte_routed_cnos_module_init,
    orte_routed_cnos_finalize,
    orte_routed_cnos_update_route,
    orte_routed_cnos_get_route,
    orte_routed_cnos_init_routes,
    orte_routed_cnos_warmup_routes
};

static int
orte_routed_cnos_module_init(void)
{
    return ORTE_SUCCESS;
}

static int
orte_routed_cnos_finalize(void)
{
    return ORTE_SUCCESS;
}


static int
orte_routed_cnos_update_route(orte_process_name_t *target,
                               orte_process_name_t *route)
{
    OPAL_OUTPUT_VERBOSE((1, orte_routed_base_output,
                         "routed_cnos_update: %s --> %s",
                         ORTE_NAME_PRINT(target), 
                         ORTE_NAME_PRINT(route)));
    return ORTE_SUCCESS;
}


static orte_process_name_t
orte_routed_cnos_get_route(orte_process_name_t *target)
{
    OPAL_OUTPUT_VERBOSE((1, orte_routed_base_output,
                         "routed_cnos_get(%s) --> %s",
                         ORTE_NAME_PRINT(target), 
                         ORTE_NAME_PRINT(target)));
    return *target;
}

static int orte_routed_cnos_init_routes(orte_jobid_t job, orte_gpr_notify_data_t *ndat)
{
    return ORTE_SUCCESS;
}

static int orte_routed_cnos_warmup_routes(void)
{
    return ORTE_SUCCESS;
}

