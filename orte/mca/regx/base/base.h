/*
 * Copyright (c) 2015-2018 Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/** @file:
 * regx framework base functionality.
 */

#ifndef ORTE_MCA_REGX_BASE_H
#define ORTE_MCA_REGX_BASE_H

/*
 * includes
 */
#include "orte_config.h"
#include "orte/types.h"

#include "opal/class/opal_list.h"
#include "orte/mca/mca.h"

#include "orte/runtime/orte_globals.h"

#include "orte/mca/regx/regx.h"

BEGIN_C_DECLS

/*
 * MCA Framework
 */
ORTE_DECLSPEC extern mca_base_framework_t orte_regx_base_framework;
/* select all components */
ORTE_DECLSPEC    int orte_regx_base_select(void);

END_C_DECLS

#endif
