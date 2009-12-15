/*
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved. 
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

#ifndef ORTE_RMCAST_TCP_H
#define ORTE_RMCAST_TCP_H

#include "orte_config.h"

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"

#include "orte/mca/rmcast/rmcast.h"


BEGIN_C_DECLS

ORTE_MODULE_DECLSPEC extern orte_rmcast_base_component_t mca_rmcast_tcp_component;
extern orte_rmcast_module_t orte_rmcast_tcp_module;

END_C_DECLS
    
#endif
