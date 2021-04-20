/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2020-2021 Sandia National Laboratories. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mca/part/persist/part_persist.h"
#include "ompi/mca/part/persist/part_persist_sendreq.h"


static void mca_part_persist_psend_request_construct(mca_part_persist_psend_request_t* sendreq)
{
    /* no need to reinit for every send -- never changes */
    sendreq->req_base.req_ompi.req_start = mca_part_persist_start;
    sendreq->req_base.req_ompi.req_free = mca_part_persist_free;
    sendreq->req_base.req_ompi.req_persistent = true;
    sendreq->req_base.req_ompi.req_cancel = NULL;
}

OBJ_CLASS_INSTANCE(mca_part_persist_psend_request_t,
                   mca_part_persist_request_t,
                   mca_part_persist_psend_request_construct,
                   NULL);

