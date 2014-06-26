/*
 * Copyright (c) 2014      Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OPAL_PMI_H
#define OPAL_PMI_H

#include "opal_config.h"
#include "opal/types.h"

#include "opal/mca/mca.h"
#include "opal/mca/event/event.h"
#include "opal/dss/dss_types.h"


BEGIN_C_DECLS

/****    DEFINE THE PUBLIC API'S    ****/
/*
 * PMI_Init
 *
 * NOTE: calls to these APIs must be thread-protected as there
 * is NO internal thread safety.
 */
typedef int (*opal_pmi_base_module_init_fn_t)(void);

/*
 * Close a database handle
 *
 * Close the specified database handle. A -1 handle indicates
 * that ALL open database handles are to be closed.
 */
typedef int (*opal_pmi_base_module_fini_fn_t)(void);

/*
/*
 * the standard public API data structure
 */
typedef struct {
    opal_pmi_base_module_init_fn_t        init;
    opal_pmi_base_module_fini_fn_t        finalize;
} opal_pmi_base_module_t;

typedef struct {
    mca_base_component_t                      base_version;
    mca_base_component_data_t                 base_data;
} opal_pmi_base_component_t;

/*
 * Macro for use in components that are of type pmi
 */
#define OPAL_PMI_BASE_VERSION_2_0_0 \
  MCA_BASE_VERSION_2_0_0, \
  "pmi", 2, 0, 0

/* Global structure for accessing store functions */
OPAL_DECLSPEC extern opal_pmi_base_module_t opal_pmi;  /* holds base function pointers */

END_C_DECLS

#endif
