/*
 * Copyright (c) 2014      Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef MCA_PMI_S1_H
#define MCA_PMI_S1_H

#include "opal_config.h"

#include "opal/mca/mca.h"
#include "opal/mca/pmi/pmi.h"

BEGIN_C_DECLS

/*
 * Globally exported variable
 */

OPAL_DECLSPEC extern const opal_pmi_base_component_t mca_pmi_s1_component;

OPAL_DECLSPEC extern const opal_pmi_base_module_t opal_pmi_s1_module;

END_C_DECLS

#endif /* MCA_PMI_S1_H */
