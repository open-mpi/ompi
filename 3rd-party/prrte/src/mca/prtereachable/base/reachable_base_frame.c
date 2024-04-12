/*
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "prte_config.h"
#include "src/include/constants.h"

#include "src/mca/base/pmix_base.h"
#include "src/mca/mca.h"
#include "src/runtime/prte_globals.h"
#include "src/util/pmix_output.h"

#include "src/mca/prtereachable/base/base.h"

/*
 * The following file was created by configure.  It contains extern
 * components and the definition of an array of pointers to each
 * module's public mca_base_module_t struct.
 */

#include "src/mca/prtereachable/base/static-components.h"

prte_reachable_base_module_t prte_reachable = {0};

static int prte_reachable_base_frame_register(pmix_mca_base_register_flag_t flags)
{
    PRTE_HIDE_UNUSED_PARAMS(flags);
    return PRTE_SUCCESS;
}

static int prte_reachable_base_frame_close(void)
{
    return pmix_mca_base_framework_components_close(&prte_prtereachable_base_framework, NULL);
}

static int prte_reachable_base_frame_open(pmix_mca_base_open_flag_t flags)
{
    /* Open up all available components */
    return pmix_mca_base_framework_components_open(&prte_prtereachable_base_framework, flags);
}

PMIX_MCA_BASE_FRAMEWORK_DECLARE(prte, prtereachable, "PRTE Reachability Framework",
                                prte_reachable_base_frame_register, prte_reachable_base_frame_open,
                                prte_reachable_base_frame_close,
                                prte_prtereachable_base_static_components,
                                PMIX_MCA_BASE_FRAMEWORK_FLAG_DEFAULT);
