/*
 * Copyright (c) 2004-2009 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2012-2013 Los Alamos National Security, LLC.
 *                         All rights reserved
 * Copyright (c) 2017-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2019      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
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
#include "src/mca/base/pmix_base.h"
#include "src/mca/mca.h"
#include "src/util/pmix_output.h"

#include "src/mca/filem/base/base.h"
#include "src/mca/filem/filem.h"

#include "src/mca/filem/base/static-components.h"

/*
 * Globals
 */
PRTE_EXPORT prte_filem_base_module_t prte_filem
    = {.filem_init = prte_filem_base_module_init,
       .filem_finalize = prte_filem_base_module_finalize,
       .put = prte_filem_base_none_put,
       .put_nb = prte_filem_base_none_put_nb,
       .get = prte_filem_base_none_get,
       .get_nb = prte_filem_base_none_get_nb,
       .rm = prte_filem_base_none_rm,
       .rm_nb = prte_filem_base_none_rm_nb,
       .wait = prte_filem_base_none_wait,
       .wait_all = prte_filem_base_none_wait_all,
       .preposition_files = prte_filem_base_none_preposition_files,
       .link_local_files = prte_filem_base_none_link_local_files};
bool prte_filem_base_is_active = false;

static int prte_filem_base_close(void)
{
    /* Close the selected component */
    if (NULL != prte_filem.filem_finalize) {
        prte_filem.filem_finalize();
    }

    return pmix_mca_base_framework_components_close(&prte_filem_base_framework, NULL);
}

/**
 * Function for finding and opening either all MCA components,
 * or the one that was specifically requested via a MCA parameter.
 */
static int prte_filem_base_open(pmix_mca_base_open_flag_t flags)
{
    /* Open up all available components */
    return pmix_mca_base_framework_components_open(&prte_filem_base_framework, flags);
}

PMIX_MCA_BASE_FRAMEWORK_DECLARE(prte, filem, NULL, NULL, prte_filem_base_open,
                                prte_filem_base_close, prte_filem_base_static_components,
                                PMIX_MCA_BASE_FRAMEWORK_FLAG_DEFAULT);
