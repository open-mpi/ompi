/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 */

#include "orte_config.h"

#include "include/orte_constants.h"
#include "mca/sds/sds.h"
#include "mca/sds/base/base.h"
#include "mca/sds/cnos/sds_cnos.h"

orte_sds_base_module_t sds_cnos_module = {
    orte_sds_base_contact_universe,
    orte_sds_cnos_set_name,
    orte_sds_cnos_finalize,
};

int
orte_sds_cnos_contact_universe(void)
{
    return ORTE_ERR_NOT_IMPLEMENTED;
}

int
orte_sds_cnos_set_name(void)
{
    return ORTE_ERR_NOT_IMPLEMENTED;
}


int 
orte_sds_cnos_finalize(void)
{
    return ORTE_ERR_NOT_IMPLEMENTED;
}
