/*
 * Copyright (c) 2014-2015 Intel, Inc. All rights reserved.
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

#include "opal/mca/pmix/pmix_types.h"
#include "opal/mca/pmix/pmix.h"

BEGIN_C_DECLS

OPAL_DECLSPEC extern mca_base_framework_t opal_pmix_base_framework;

/**
 * Select a pmix module
 */
OPAL_DECLSPEC int opal_pmix_base_select(void);

OPAL_DECLSPEC extern bool opal_pmix_base_allow_delayed_server;

OPAL_DECLSPEC void opal_pmix_base_register_handler(opal_pmix_errhandler_fn_t err);
OPAL_DECLSPEC void opal_pmix_base_deregister_handler(void);
OPAL_DECLSPEC void opal_pmix_base_errhandler(int status,
                                             opal_list_t *procs,
                                             opal_list_t *info);
OPAL_DECLSPEC int opal_pmix_base_exchange(opal_value_t *info,
                                          opal_pmix_pdata_t *pdat,
                                          int timeout);
END_C_DECLS

#endif
