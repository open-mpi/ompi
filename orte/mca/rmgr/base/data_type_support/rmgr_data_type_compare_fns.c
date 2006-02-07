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

#include "mca/errmgr/errmgr.h"
#include "dss/dss_internal.h"

#include "mca/rmgr/base/base.h"

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
