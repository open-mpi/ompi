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
#include "mca/sds/seed/sds_seed.h"

orte_sds_base_module_t sds_seed_module = {
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
