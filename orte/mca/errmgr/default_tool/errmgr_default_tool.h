/*
 * Copyright (c) 2013      Intel, Inc. All rights reserved.
 *
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

#ifndef MCA_ERRMGR_default_tool_EXPORT_H
#define MCA_ERRMGR_default_tool_EXPORT_H

#include "orte_config.h"

#include "orte/mca/errmgr/errmgr.h"

BEGIN_C_DECLS

/*
 * Local Component structures
 */

ORTE_MODULE_DECLSPEC extern orte_errmgr_base_component_t mca_errmgr_default_tool_component;

ORTE_DECLSPEC extern orte_errmgr_base_module_t orte_errmgr_default_tool_module;

END_C_DECLS

#endif /* MCA_ERRMGR_tool_EXPORT_H */
