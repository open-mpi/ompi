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
 * Copyright (c) 2008      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "opal_config.h"

#include "opal/constants.h"
#include "opal/util/output.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/pstat/pstat.h"
#include "opal/mca/pstat/base/base.h"


/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */
#include "opal/mca/pstat/base/static-components.h"

/* unsupported functions */
static int opal_pstat_base_unsupported_init(void);
static int opal_pstat_base_unsupported_query(pid_t pid, opal_pstats_t *stats, opal_node_stats_t *nstats);
static int opal_pstat_base_unsupported_finalize(void);

/*
 * Globals
 */
opal_pstat_base_component_t *opal_pstat_base_component = NULL;
opal_pstat_base_module_t opal_pstat = {
    opal_pstat_base_unsupported_init,
    opal_pstat_base_unsupported_query,
    opal_pstat_base_unsupported_finalize
};

/* Use default register/open/close functions */
MCA_BASE_FRAMEWORK_DECLARE(opal, pstat, "process statistics", NULL, NULL, NULL,
                           mca_pstat_base_static_components, 0);

static int opal_pstat_base_unsupported_init(void)
{
    return OPAL_ERR_NOT_SUPPORTED;
}

static int opal_pstat_base_unsupported_query(pid_t pid, opal_pstats_t *stats, opal_node_stats_t *nstats)
{
    return OPAL_ERR_NOT_SUPPORTED;
}

static int opal_pstat_base_unsupported_finalize(void)
{
    return OPAL_ERR_NOT_SUPPORTED;
}
