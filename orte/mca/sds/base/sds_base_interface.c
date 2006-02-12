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

#include "orte_config.h"

#include "orte/orte_constants.h"
#include "opal/util/output.h"
#include "orte/mca/sds/base/base.h"
#include "orte/util/proc_info.h"

extern orte_sds_base_module_t *orte_sds_base_module;

int
orte_sds_base_contact_universe(void)
{
    return orte_sds_base_module->contact_universe();
}


int
orte_sds_base_set_name(void)
{
    /* if we got "promoted" to the seed between selection and
       set_name, it's probably the seed component isn't the one
       running.  Go behind the selected component's back and do the
       seed set name, since that's what we really want to have
       happen */
    if (orte_process_info.seed) {
        return orte_sds_base_seed_set_name();
    } else {
        return orte_sds_base_module->set_name();
    }
}
