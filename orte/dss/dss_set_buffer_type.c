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

#include "opal/util/output.h"

#include "orte/mca/errmgr/errmgr.h"

#include "orte/dss/dss.h"
#include "orte/dss/dss_internal.h"

int orte_dss_set_buffer_type(orte_buffer_t *buffer, orte_dss_buffer_type_t type)
{
    /** check for error */
    if (NULL == buffer) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }

    /** see if the buffer is empty - if not, generate error */
    if (buffer->base_ptr != buffer->pack_ptr) {
        ORTE_ERROR_LOG(ORTE_ERR_BUFFER);
        return ORTE_ERR_BUFFER;
    }

    /** set the type */
    buffer->type = type;

    return ORTE_SUCCESS;
}

