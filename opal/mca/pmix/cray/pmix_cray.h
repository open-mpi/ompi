/*
 * Copyright (c) 2014      Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef MCA_PMIX_CRAY_H
#define MCA_PMIX_CRAY_H

#include "opal_config.h"

#include "opal/mca/mca.h"
#include "opal/mca/pmix/pmix.h"
#include "opal/mca/pmix/base/pmix_base_fns.h"
#include "opal/util/proc.h"
#include "pmix_cray_pmap_parser.h"

BEGIN_C_DECLS

typedef struct {
    opal_pmix_base_component_t super;
    opal_buffer_t *cache_local;
    opal_buffer_t *cache_global;
} opal_pmix_cray_component_t;

/*
 * Globally exported variable
 */

OPAL_DECLSPEC extern opal_pmix_cray_component_t mca_pmix_cray_component;

OPAL_DECLSPEC extern const opal_pmix_base_module_t opal_pmix_cray_module;

/*
 * proto-types for cray/pmix kvs component
 */


END_C_DECLS

#endif /* MCA_PMIX_CRAY_H */
