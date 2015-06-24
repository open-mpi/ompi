/*
 * Copyright (c) 2014      Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_PMIX_S1_H
#define MCA_PMIX_S1_H

#include "opal_config.h"

#include "opal/mca/mca.h"
#include "opal/mca/pmix/pmix.h"
#include "opal/mca/pmix/base/pmix_base_fns.h"

BEGIN_C_DECLS

/*
 * Globally exported variable
 */

OPAL_DECLSPEC extern opal_pmix_base_component_t mca_pmix_s1_component;

OPAL_DECLSPEC extern const opal_pmix_base_module_t opal_pmix_s1_module;

END_C_DECLS

#endif /* MCA_PMIX_S1_H */
