/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"

#include "include/orte_constants.h"
#include "class/ompi_list.h"
#include "util/output.h"
#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/pls/base/base.h"


/*
 * Local functions
 */
static orte_pls_base_module_t *select_preferred(char *name);
static orte_pls_base_module_t *select_any(void);


/*
 * Function for selecting one component from all those that are
 * available.
 */
orte_pls_base_module_t* orte_pls_base_select(char *preferred)
{
    if (NULL != preferred) {
        return select_preferred(preferred);
    } else {
        return select_any();
    }
}


static orte_pls_base_module_t *select_preferred(char *name)
{
    ompi_list_item_t *item;
    orte_pls_base_cmp_t *cmp;

    /* Look for a matching selected name */

    ompi_output(orte_pls_base.pls_output,
                "orte:base:select: looking for component %s", name);
    for (item = ompi_list_get_first(&orte_pls_base.pls_available);
         item != ompi_list_get_end(&orte_pls_base.pls_available);
         item = ompi_list_get_next(item)) {
        cmp = (orte_pls_base_cmp_t *) item;

        if (0 == strcmp(name, 
                        cmp->component->pls_version.mca_component_name)) {
            ompi_output(orte_pls_base.pls_output,
                        "orte:base:select: found module for compoent %s", name);
            return cmp->module;
        }
    }

    /* Didn't find a matching name */

    ompi_output(orte_pls_base.pls_output,
                "orte:base:select: did not find module for compoent %s", name);
    return NULL;
}


static orte_pls_base_module_t *select_any(void)
{
    ompi_list_item_t *item;
    orte_pls_base_cmp_t *cmp;

    /* If the list is empty, return NULL */

    if (ompi_list_is_empty(&orte_pls_base.pls_available)) {
        ompi_output(orte_pls_base.pls_output,
                    "orte:base:select: no components available!");
        return NULL;
    }

    /* Otherwise, return the first item (it's already sorted in
       priority order) */

    item = ompi_list_get_first(&orte_pls_base.pls_available);
    cmp = (orte_pls_base_cmp_t *) item;
    ompi_output(orte_pls_base.pls_output,
                "orte:base:select: highest priority component: %s",
                cmp->component->pls_version.mca_component_name);
    return cmp->module;
}
