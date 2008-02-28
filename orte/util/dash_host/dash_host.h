/*
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
/**
 * @file
 *
 */
#ifndef ORTE_UTIL_DASH_HOST_H
#define ORTE_UTIL_DASH_HOST_H

#include "orte_config.h"
#include "orte/types.h"

#include "opal/class/opal_list.h"

#include "orte/mca/plm/plm_types.h"

BEGIN_C_DECLS

ORTE_DECLSPEC int orte_util_add_dash_host_nodes(opal_list_t *nodes,
                                                bool *override_oversubscribed,
                                                orte_std_cntr_t num_map,
                                                orte_app_context_map_t **map_data);
    
ORTE_DECLSPEC int orte_util_filter_dash_host_nodes(opal_list_t *nodes,
                                                   orte_std_cntr_t num_map,
                                                   orte_app_context_map_t **map_data);

END_C_DECLS

#endif
