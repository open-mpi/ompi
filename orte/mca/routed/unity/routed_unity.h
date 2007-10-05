/*
 * Copyright (c) 2007      Los Alamos National Security, LLC.
 *                         All rights reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef MCA_ROUTED_UNITY_ROUTED_UNITY_H
#define MCA_ROUTED_UNITY_ROUTED_UNITY_H

#include "orte/mca/routed/routed.h"

BEGIN_C_DECLS


ORTE_MODULE_DECLSPEC extern orte_routed_component_t mca_routed_unity_component;

int orte_routed_unity_finalize(void);

int orte_routed_unity_update_route(orte_process_name_t *target,
                                   orte_process_name_t *route);

orte_process_name_t orte_routed_unity_get_route(orte_process_name_t *target);

int orte_routed_unity_init_routes(orte_jobid_t job, orte_gpr_notify_data_t *ndat);

int orte_routed_unity_warmup_routes(void);

END_C_DECLS

#endif
