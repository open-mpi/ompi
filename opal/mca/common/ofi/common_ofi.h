/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2015      Intel, Inc. All rights reserved.
 * Copyright (c) 2017      Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2020      Triad National Security, LLC. All rights
 *                         reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OPAL_MCA_COMMON_OFI_H
#define OPAL_MCA_COMMON_OFI_H

#include "opal_config.h"
#include "opal/mca/base/mca_base_var.h"
#include "opal/mca/base/mca_base_framework.h"
#include <rdma/fabric.h>

BEGIN_C_DECLS

typedef struct opal_common_ofi_module {
    int verbose;
    int registered;
    int output;
} opal_common_ofi_module_t;

extern opal_common_ofi_module_t opal_common_ofi;

OPAL_DECLSPEC int opal_common_ofi_register_mca_variables(const mca_base_component_t *component);
OPAL_DECLSPEC void opal_common_ofi_mca_register(void);
OPAL_DECLSPEC void opal_common_ofi_mca_deregister(void);
OPAL_DECLSPEC struct fi_info* opal_mca_common_ofi_select_provider(struct fi_info *provider_list, int rank);

END_C_DECLS

#endif /* OPAL_MCA_COMMON_OFI_H */
