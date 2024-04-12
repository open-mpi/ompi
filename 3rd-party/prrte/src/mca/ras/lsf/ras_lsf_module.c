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
 * Copyright (c) 2007-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2014-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2016-2022 IBM Corporation.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include "prte_config.h"
#include "constants.h"

#include <errno.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#define SR1_PJOBS
#include <lsf/lsbatch.h>

#include "src/hwloc/hwloc-internal.h"
#include "src/util/pmix_argv.h"
#include "src/util/pmix_net.h"

#include "src/mca/errmgr/errmgr.h"
#include "src/mca/rmaps/base/base.h"
#include "src/mca/rmaps/rmaps_types.h"
#include "src/runtime/prte_globals.h"
#include "src/util/pmix_show_help.h"

#include "ras_lsf.h"
#include "src/mca/ras/base/base.h"
#include "src/mca/ras/base/ras_private.h"

/*
 * Local functions
 */
static int allocate(prte_job_t *jdata, pmix_list_t *nodes);
static int finalize(void);

/*
 * Global variable
 */
prte_ras_base_module_t prte_ras_lsf_module = {NULL, allocate, NULL, finalize};

static int allocate(prte_job_t *jdata, pmix_list_t *nodes)
{
    char **nodelist;
    prte_node_t *node;
    int i, num_nodes, rc;
    char *ptr;

    /* get the list of allocated nodes */
    if ((num_nodes = lsb_getalloc(&nodelist)) < 0) {
        pmix_show_help("help-ras-lsf.txt", "nodelist-failed", true);
        return PRTE_ERR_NOT_AVAILABLE;
    }

    node = NULL;

    /* step through the list */
    for (i = 0; i < num_nodes; i++) {
        if (!prte_keep_fqdn_hostnames && !pmix_net_isaddr(nodelist[i])) {
            if (NULL != (ptr = strchr(nodelist[i], '.'))) {
                *ptr = '\0';
            }
        }

        /* is this a repeat of the current node? */
        if (NULL != node && 0 == strcmp(nodelist[i], node->name)) {
            /* it is a repeat - just bump the slot count */
            ++node->slots;
            pmix_output_verbose(10, prte_ras_base_framework.framework_output,
                                "ras/lsf: +++ Node (%s) [slots=%d]", node->name, node->slots);
            continue;
        }

        /* not a repeat - create a node entry for it */
        node = PMIX_NEW(prte_node_t);
        node->name = strdup(nodelist[i]);
        node->slots_inuse = 0;
        node->slots_max = 0;
        node->slots = 1;
        node->state = PRTE_NODE_STATE_UP;
        pmix_list_append(nodes, &node->super);

        pmix_output_verbose(10, prte_ras_base_framework.framework_output,
                            "ras/lsf: New Node (%s) [slots=%d]", node->name, node->slots);
    }

    /* release the nodelist from lsf */
    PMIX_ARGV_FREE_COMPAT(nodelist);

    return PRTE_SUCCESS;
}

static int finalize(void)
{
    return PRTE_SUCCESS;
}
