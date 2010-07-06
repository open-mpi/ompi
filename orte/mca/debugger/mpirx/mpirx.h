/* -*- C -*-
 * 
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 */
#ifndef DEBUGGER_MPIRX_H
#define DEBUGGER_MPIRX_H

#include "orte_config.h"

#include "orte/mca/debugger/debugger.h"

BEGIN_C_DECLS

ORTE_MODULE_DECLSPEC extern orte_debugger_base_component_t mca_debugger_mpirx_component;
extern orte_debugger_base_module_t orte_debugger_mpirx_module;

END_C_DECLS

#endif
