/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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
int orte_ras_base_size_node(size_t *size, orte_ras_node_t *src, orte_data_type_t type)
{
    /* account for the object itself */
    *size = sizeof(orte_ras_node_t);

    /* if src is NULL, then that's all we wanted */
    if (NULL == src) return ORTE_SUCCESS;

    if (NULL != src->node_name) {
        *size += strlen(src->node_name);
    }

    if (NULL != src->node_arch) {
        *size += strlen(src->node_arch);
    }

    if (NULL != src->node_username) {
        *size += strlen(src->node_username);
    }

    return ORTE_SUCCESS;
}
