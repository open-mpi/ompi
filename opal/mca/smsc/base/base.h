/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2021      Google, LLC. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OPAL_MCA_SMSC_BASE_BASE_H
#define OPAL_MCA_SMSC_BASE_BASE_H

#include "opal/mca/smsc/smsc.h"

extern mca_base_framework_t opal_smsc_base_framework;
extern mca_smsc_component_t *selected_component;
extern mca_smsc_module_t *selected_module;

int mca_smsc_base_select(void);
void mca_smsc_base_register_default_params(mca_smsc_component_t *component, int default_priority);

#endif /* OPAL_MCA_SMSC_BASE_BASE_H */
