/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "orte_config.h"

#include <sys/types.h>
#if HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif

#include "opal/util/argv.h"
#include "opal/class/opal_list.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/dss/dss_internal.h"

#include "orte/mca/ras/base/ras_private.h"

/*
 * RAS NODE
 */
int orte_ras_base_print_node(char **output, char *prefix, orte_ras_node_t *src, orte_data_type_t type)
{
    char *tmp, *tmp2, *pfx2;

    /* set default result */
    *output = NULL;

    /* protect against NULL prefix */
    if (NULL == prefix) {
        asprintf(&pfx2, " ");
    } else {
        asprintf(&pfx2, "%s", prefix);
    }

    asprintf(&tmp, "%sData for node: cellid: %lu\tName: %s\tLaunch id: %ld",
                    pfx2, (unsigned long)src->node_cellid, src->node_name, (long)src->launch_id);

    asprintf(&tmp2, "%s\n%s\tArch: %s\tState: %lu", tmp, pfx2,
             src->node_arch, (unsigned long)src->node_state);
    free(tmp);
    tmp = tmp2;

    asprintf(&tmp2, "%s\n%s\tNum slots: %lu\tSlots in use: %lu", tmp, pfx2,
             (unsigned long)src->node_slots, (unsigned long)src->node_slots_inuse);
    free(tmp);
    tmp = tmp2;

    asprintf(&tmp2, "%s\n%s\tNum slots allocated: %lu\tMax slots: %lu", tmp, pfx2,
             (unsigned long)src->node_slots_alloc, (unsigned long)src->node_slots_max);
    free(tmp);
    tmp = tmp2;

    asprintf(&tmp2, "%s\n%s\tUsername on node: %s\tLaunched?: %lu", tmp, pfx2,
             src->node_username, (unsigned long)src->node_launched);
    free(tmp);
    tmp = tmp2;

    /* set the return */
    *output = tmp;

    free(pfx2);
    return ORTE_SUCCESS;
}

