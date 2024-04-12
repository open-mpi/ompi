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
 * Copyright (c) 2008-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2014-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2019      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "pmix_config.h"

#include "pmix_common.h"
#include "src/include/pmix_globals.h"
#include "src/mca/base/pmix_base.h"
#include "src/mca/mca.h"
#include "src/mca/pstat/base/base.h"
#include "src/mca/pstat/pstat.h"
#include "src/util/pmix_output.h"

/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public pmix_mca_base_component_t struct.
 */
#include "src/mca/pstat/base/static-components.h"

/* unsupported functions */
static int pmix_pstat_base_unsupported_init(void);
static int pmix_pstat_base_unsupported_query(pid_t pid, pmix_proc_stats_t *stats,
                                             pmix_node_stats_t *nstats);
static int pmix_pstat_base_unsupported_finalize(void);

/*
 * Globals
 */
pmix_pstat_base_component_t *pmix_pstat_base_component = NULL;
pmix_pstat_base_module_t pmix_pstat = {
    pmix_pstat_base_unsupported_init,
    pmix_pstat_base_unsupported_query,
    pmix_pstat_base_unsupported_finalize
};

/* Use default register/open/close functions */
static int pmix_pstat_base_close(void)
{
    /* let the selected module finalize */
    if (NULL != pmix_pstat.finalize) {
        pmix_pstat.finalize();
    }

    return pmix_mca_base_framework_components_close(&pmix_pstat_base_framework, NULL);
}

static int pmix_pstat_base_open(pmix_mca_base_open_flag_t flags)
{
    /* Open up all available components */
    return pmix_mca_base_framework_components_open(&pmix_pstat_base_framework, flags);
}

PMIX_MCA_BASE_FRAMEWORK_DECLARE(pmix, pstat, "process statistics", NULL, pmix_pstat_base_open,
                                pmix_pstat_base_close, pmix_mca_pstat_base_static_components, 0);

static int pmix_pstat_base_unsupported_init(void)
{
    return PMIX_ERR_NOT_SUPPORTED;
}

static int pmix_pstat_base_unsupported_query(pid_t pid, pmix_proc_stats_t *stats,
                                             pmix_node_stats_t *nstats)
{
    PMIX_HIDE_UNUSED_PARAMS(pid, stats, nstats);

    return PMIX_ERR_NOT_SUPPORTED;
}

static int pmix_pstat_base_unsupported_finalize(void)
{
    return PMIX_ERR_NOT_SUPPORTED;
}
