/*
 * Copyright (c) 2014-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2020      Triad National Security, LLC. All rights
 *                         reserved.
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
#include "opal/mca/base/mca_base_framework.h"
#include "opal/mca/mca.h"
#include "opal/mca/threads/threads.h"
#include "opal/types.h"

#include "opal/mca/pmix/pmix-internal.h"

BEGIN_C_DECLS

OPAL_DECLSPEC extern mca_base_framework_t opal_pmix_base_framework;

/**
 * Select a pmix module
 */
OPAL_DECLSPEC int opal_pmix_base_select(void);

OPAL_DECLSPEC extern bool opal_pmix_base_allow_delayed_server;

OPAL_DECLSPEC int opal_pmix_base_exchange(pmix_info_t *info, pmix_pdata_t *pdat, int timeout);

typedef struct {
    opal_event_base_t *evbase;
    int timeout;
    int initialized;
    opal_pmix_lock_t lock;
} opal_pmix_base_t;

extern opal_pmix_base_t opal_pmix_base;

#define OPAL_PMIX_CONDITION_STATIC_INIT OPAL_CONDITION_STATIC_INIT

END_C_DECLS

#endif
