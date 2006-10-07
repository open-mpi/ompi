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

#include "opal/util/argv.h"
#include "opal/class/opal_list.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/dss/dss.h"

#include "orte/mca/rmaps/base/rmaps_private.h"

/*
 * JOB_MAP
 */
int orte_rmaps_base_size_map(size_t *size, orte_job_map_t *src, orte_data_type_t type)
{
    /* account for the object itself */
    *size = sizeof(orte_job_map_t);

    /* if src is NULL, then that's all we wanted */
    if (NULL == src) return ORTE_SUCCESS;

    return ORTE_SUCCESS;
}

/*
 * MAPPED_PROC
 */
int orte_rmaps_base_size_mapped_proc(size_t *size, orte_mapped_proc_t *src, orte_data_type_t type)
{
    /* account for the object itself */
    *size = sizeof(orte_mapped_proc_t);
    
    /* if src is NULL, then that's all we wanted */
    if (NULL == src) return ORTE_SUCCESS;
    
    return ORTE_SUCCESS;
}

/*
 * MAPPED_NODE
 */
int orte_rmaps_base_size_mapped_node(size_t *size, orte_mapped_node_t *src, orte_data_type_t type)
{
    /* account for the object itself */
    *size = sizeof(orte_mapped_node_t);
    
    /* if src is NULL, then that's all we wanted */
    if (NULL == src) return ORTE_SUCCESS;
    
    return ORTE_SUCCESS;
}
