/*
 * Copyright (c) 2014      Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/** @file:
 */

#ifndef MCA_PMI_BASE_H
#define MCA_PMI_BASE_H

#include "opal_config.h"
#include "opal/types.h"

#include "opal/mca/mca.h"
#include "opal/mca/base/mca_base_framework.h"

#include "opal/mca/pmi/pmi.h"

BEGIN_C_DECLS

OPAL_DECLSPEC extern mca_base_framework_t opal_pmi_base_framework;

/**
 * Select a pmi module
 */
OPAL_DECLSPEC int opal_pmi_base_select(void);

OPAL_DECLSPEC char* opal_pmi_base_error(int pmi_err);

END_C_DECLS

#endif
