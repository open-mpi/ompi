/*
 * Copyright (c) 2016-2017 Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef _MCA_SCHIZO_MOAB_H_
#define _MCA_SCHIZO_MOAB_H_

#include "orte_config.h"

#include "orte/types.h"

#include "opal/mca/base/base.h"
#include "orte/mca/schizo/schizo.h"


BEGIN_C_DECLS

ORTE_MODULE_DECLSPEC extern orte_schizo_base_component_t mca_schizo_moab_component;
extern orte_schizo_base_module_t orte_schizo_moab_module;

END_C_DECLS

#endif /* MCA_SCHIZO_MOAB_H_ */
