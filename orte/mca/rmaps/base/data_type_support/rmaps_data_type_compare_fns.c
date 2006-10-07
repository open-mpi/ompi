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

#include "orte/mca/errmgr/errmgr.h"
#include "orte/dss/dss_internal.h"

#include "orte/mca/rmaps/base/rmaps_private.h"

/*
 * JOB_MAP
 */
int orte_rmaps_base_compare_map(orte_job_map_t *value1, orte_job_map_t *value2, orte_data_type_t type)
{
    return ORTE_EQUAL;
}


/* MAPPED_PROC */
int orte_rmaps_base_compare_mapped_proc(orte_mapped_proc_t *value1, orte_mapped_proc_t *value2, orte_data_type_t type)
{
    return ORTE_EQUAL;
}



/* MAPPED_NODE */
int orte_rmaps_base_compare_mapped_node(orte_mapped_node_t *value1, orte_mapped_node_t *value2, orte_data_type_t type)
{
    return ORTE_EQUAL;
}

