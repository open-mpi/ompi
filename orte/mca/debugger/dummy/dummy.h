/*
 * Copyright (c) 2011      Los Alamos National Security, LLC.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * Test C99 struct initialization. Remove on 1/20/2011 after MTT has run.
 *
 * $HEADER$
 */

#ifndef DEBUGGER_DUMMY_H
#define DEBUGGER_DUMMY_H

#include "orte_config.h"

#include "orte/mca/debugger/debugger.h"

BEGIN_C_DECLS

ORTE_MODULE_DECLSPEC extern orte_debugger_base_component_t mca_debugger_dummy_component;
extern orte_debugger_base_module_t orte_debugger_dummy_module;

END_C_DECLS

#endif
