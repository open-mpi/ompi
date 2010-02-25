/* -*- C -*-
 * 
 * Copyright (c) 2010      Cisco Systems, Inc. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 */
#ifndef GRPCOMM_MCAST_H
#define GRPCOMM_MCAST_H

#include "orte_config.h"


#include "orte/mca/grpcomm/grpcomm.h"

BEGIN_C_DECLS

/*
 * Component open / close
 */
int orte_grpcomm_mcast_open(void);
int orte_grpcomm_mcast_close(void);
int orte_grpcomm_mcast_component_query(mca_base_module_t **module, int *priority);
void orte_grpcomm_mcast_daemon_coll(orte_process_name_t* sender, opal_buffer_t* buffer);


/*
 * Grpcomm interfaces
 */

ORTE_MODULE_DECLSPEC extern orte_grpcomm_base_component_t mca_grpcomm_mcast_component;
extern orte_grpcomm_base_module_t orte_grpcomm_mcast_module;

END_C_DECLS

#endif
