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
#include "orte/mca/routed/routed.h"

BEGIN_C_DECLS

/*
 * Global functions for the ROUTED
 */

ORTE_DECLSPEC int orte_routed_base_open(void);
ORTE_DECLSPEC int orte_routed_base_select(void);
ORTE_DECLSPEC int orte_routed_base_close(void);

ORTE_DECLSPEC extern int orte_routed_base_output;
ORTE_DECLSPEC extern opal_list_t orte_routed_base_components;

ORTE_DECLSPEC extern int orte_routed_base_register_sync(void);

END_C_DECLS

#endif /* MCA_ROUTED_BASE_H */
