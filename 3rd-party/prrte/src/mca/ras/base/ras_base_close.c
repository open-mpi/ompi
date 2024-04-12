/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2019-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "prte_config.h"
#include "constants.h"

#include <stdio.h>

#include "src/mca/base/pmix_base.h"
#include "src/mca/mca.h"

#include "src/mca/ras/base/base.h"

#include "src/mca/ras/base/ras_private.h"

int prte_ras_base_finalize(void)
{
    if (NULL != prte_ras_base.active_module) {
        prte_ras_base.active_module->finalize();
    }

    return PRTE_SUCCESS;
}

int prte_ras_base_close(void)
{
    /* Close all remaining available components (may be one if this is a
       PRTE program, or [possibly] multiple if this is ompi_info) */

    mca_base_components_close(prte_ras_base_framework.framework_output, &prte_ras_base.ras_opened,
                              NULL);

    /* Close the framework output */
    pmix_output_close(prte_ras_base_framework.framework_output);
    prte_ras_base_framework.framework_output = -1;

    return PRTE_SUCCESS;
}
