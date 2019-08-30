/*
 * Copyright (c) 2016-2018 Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef _MCA_REGX_NONE_H_
#define _MCA_REGX_NONE_H_

#include "orte_config.h"

#include "orte/types.h"

#include "opal/mca/base/base.h"
#include "orte/mca/regx/regx.h"


BEGIN_C_DECLS

struct orte_regx_naive_component_t {
    orte_regx_base_component_t super;
    bool compress_vpids;
};
typedef struct orte_regx_naive_component_t orte_regx_naive_component_t;

ORTE_MODULE_DECLSPEC extern orte_regx_naive_component_t mca_regx_naive_component;
extern orte_regx_base_module_t orte_regx_naive_module;

END_C_DECLS

#endif /* MCA_REGX_ORTE_H_ */
