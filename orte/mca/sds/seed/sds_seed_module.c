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
 *
 */

#include "orte_config.h"

#include "orte/orte_constants.h"
#include "orte/mca/sds/sds.h"
#include "orte/mca/sds/base/base.h"
#include "orte/mca/sds/seed/sds_seed.h"

orte_sds_base_module_t orte_sds_seed_module = {
    orte_sds_base_basic_contact_universe,
    orte_sds_base_seed_set_name,
    orte_sds_seed_finalize,
};


/* seed_set_name is part of base because it's possible that the
   base_set_name will decide that the process has been "promoted" to
   seed after the selection occured.  Therefore, need a reasonable way
   to do the seed setname without actually having the seed
   component. */


int 
orte_sds_seed_finalize(void)
{
    return ORTE_SUCCESS;
}
