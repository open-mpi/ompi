/* -*- C -*-
 *
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

#include "orte_config.h"

#include "orte/mca/gpr/gpr.h"
#include "orte/orte_constants.h"
#include "orte/mca/gpr/null/gpr_null.h"

extern orte_gpr_base_module_t orte_gpr_null_module;

static int
orte_gpr_null_open(void)
{
    return ORTE_SUCCESS;
}


static int
orte_gpr_null_close(void)
{
    return ORTE_SUCCESS;
}

static orte_gpr_base_module_t *
orte_gpr_null_init(bool *allow_multi_user_threads, 
                   bool *have_hidden_threads, 
                   int *priority)
{
    *priority = 0;
    return &orte_gpr_null_module;
}

static int
orte_gpr_null_finalize(void)
{
    return ORTE_SUCCESS;
}

mca_gpr_base_component_t mca_gpr_null_component = {
    {
	MCA_GPR_BASE_VERSION_1_0_0,

	"null", /* MCA module name */
	ORTE_MAJOR_VERSION,  /* MCA module major version */
	ORTE_MINOR_VERSION,  /* MCA module minor version */
	ORTE_RELEASE_VERSION,  /* MCA module release version */
	orte_gpr_null_open,  /* module open */
	orte_gpr_null_close /* module close */
    },
    {
	false /* checkpoint / restart */
    },
    orte_gpr_null_init,    /* module init */
    orte_gpr_null_finalize /* module shutdown */
};
