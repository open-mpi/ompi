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
 */
 
#include "orte_config.h"

#include <sys/types.h>
#if HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif

#include "opal/class/opal_list.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/dss/dss.h"

#include "orte/mca/rmgr/base/rmgr_private.h"

/*
 * APP CONTEXT
 */
int orte_rmgr_base_compare_app_context(orte_app_context_t *value1, orte_app_context_t *value2, orte_data_type_t type)
{
    if (value1->idx > value2->idx) return ORTE_VALUE1_GREATER;
    if (value2->idx > value1->idx) return ORTE_VALUE2_GREATER;
    
    return ORTE_EQUAL;
}


/*
 * APP CONTEXT MAP
 */
int orte_rmgr_base_compare_app_context_map(orte_app_context_map_t *value1, orte_app_context_map_t *value2, orte_data_type_t type)
{
    if (value1->map_type > value2->map_type) return ORTE_VALUE1_GREATER;
    if (value2->map_type > value1->map_type) return ORTE_VALUE2_GREATER;
    
    return ORTE_EQUAL;
}


/*
 * ATTRIBUTE
 */
int orte_rmgr_base_compare_attribute(orte_attribute_t *value1, orte_attribute_t *value2, orte_data_type_t type)
{
    return orte_dss.compare(value1, value2, ORTE_GPR_KEYVAL);
}


/*
 * ATTRIBUTE LIST
 */
int orte_rmgr_base_compare_attr_list(opal_list_t *value1, opal_list_t *value2, orte_data_type_t type)
{
    if (opal_list_get_size(value1) > opal_list_get_size(value2)) return ORTE_VALUE1_GREATER;
    if (opal_list_get_size(value2) > opal_list_get_size(value1)) return ORTE_VALUE2_GREATER;
    
    return ORTE_EQUAL;
}

