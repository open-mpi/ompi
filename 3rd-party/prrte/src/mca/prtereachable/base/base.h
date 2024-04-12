/*
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/** @file:
 */

#ifndef PRTE_MCA_REACHABLE_BASE_H
#define PRTE_MCA_REACHABLE_BASE_H

#include "prte_config.h"
#include "src/include/types.h"

#include "src/mca/base/pmix_mca_base_framework.h"
#include "src/mca/mca.h"

#include "src/mca/prtereachable/prtereachable.h"

BEGIN_C_DECLS

PRTE_EXPORT extern pmix_mca_base_framework_t prte_prtereachable_base_framework;

/**
 * Select a prtereachable module
 */
PRTE_EXPORT int prte_reachable_base_select(void);

PRTE_EXPORT prte_reachable_t *prte_reachable_allocate(unsigned int num_local,
                                                      unsigned int num_remote);

END_C_DECLS

#endif
