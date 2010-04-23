/*
 * Copyright (c) 2007      Los Alamos National Security, LLC.
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

#include "opal/dss/dss_types.h"
#include "opal/threads/threads.h"

#include "orte/mca/rml/rml_types.h"
#include "orte/mca/routed/routed.h"

BEGIN_C_DECLS

ORTE_DECLSPEC int orte_routed_base_open(void);

#if !ORTE_DISABLE_FULL_SUPPORT

/*
 * Global functions for the ROUTED
 */

ORTE_DECLSPEC int orte_routed_base_select(void);
ORTE_DECLSPEC int orte_routed_base_close(void);

ORTE_DECLSPEC extern int orte_routed_base_output;
ORTE_DECLSPEC extern opal_list_t orte_routed_base_components;
ORTE_DECLSPEC extern opal_mutex_t orte_routed_base_lock;
ORTE_DECLSPEC extern opal_condition_t orte_routed_base_cond;
ORTE_DECLSPEC extern bool orte_routed_base_wait_sync;

ORTE_DECLSPEC extern int orte_routed_base_register_sync(bool setup);
ORTE_DECLSPEC extern int orte_routed_base_process_callback(orte_jobid_t job,
                                                           opal_buffer_t *buffer);

#endif /* ORTE_DISABLE_FULL_SUPPORT */

END_C_DECLS

#endif /* MCA_ROUTED_BASE_H */
