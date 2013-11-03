/*
 * Copyright (c) 2007-2013 Los Alamos National Security, LLC.
 *                         All rights reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef MCA_ROUTED_BASE_H
#define MCA_ROUTED_BASE_H

#include "orte_config.h"

#include "opal/mca/mca.h"

#include "opal/class/opal_pointer_array.h"
#include "opal/dss/dss_types.h"

#include "orte/mca/rml/rml_types.h"
#include "orte/mca/routed/routed.h"

BEGIN_C_DECLS

/*
 * MCA Framework
 */
ORTE_DECLSPEC extern mca_base_framework_t orte_routed_base_framework;
/* select a component */
ORTE_DECLSPEC    int orte_routed_base_select(void);


ORTE_DECLSPEC extern bool orte_routed_base_wait_sync;
ORTE_DECLSPEC extern opal_pointer_array_t orte_routed_jobfams;

ORTE_DECLSPEC void orte_routed_base_xcast_routing(orte_grpcomm_collective_t *coll,
                                                  opal_list_t *my_children);
ORTE_DECLSPEC void orte_routed_base_coll_relay_routing(orte_grpcomm_collective_t *coll);
ORTE_DECLSPEC void orte_routed_base_coll_complete_routing(orte_grpcomm_collective_t *coll);
ORTE_DECLSPEC void orte_routed_base_coll_peers(orte_grpcomm_collective_t *coll,
                                               opal_list_t *my_children);

ORTE_DECLSPEC int orte_routed_base_register_sync(bool setup);
ORTE_DECLSPEC int orte_routed_base_process_callback(orte_jobid_t job,
                                                    opal_buffer_t *buffer);
ORTE_DECLSPEC void orte_routed_base_update_hnps(opal_buffer_t *buf);

END_C_DECLS

#endif /* MCA_ROUTED_BASE_H */
