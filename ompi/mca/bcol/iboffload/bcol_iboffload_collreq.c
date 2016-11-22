/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "bcol_iboffload_collreq.h"

static void
collreq_construct(struct mca_bcol_iboffload_collreq_t *collreq)
{
    int i;
    collreq->n_fragments = 0;
    collreq->n_frag_mpi_complete = 0;
    collreq->n_frag_net_complete = 0;
    collreq->user_handle_freed = false;

    for (i = 0; i < BCOL_IBOFFLOAD_BUFFERS; i++) {
        collreq->buffer_info[i].buf = NULL;
        collreq->buffer_info[i].offset = 0;
        collreq->buffer_info[i].iboffload_reg = NULL;
    }

    OBJ_CONSTRUCT(&collreq->work_requests, opal_list_t);
    OBJ_CONSTRUCT(&collreq->first_collfrag, mca_bcol_iboffload_collfrag_t);

    OBJ_CONSTRUCT(&collreq->send_convertor, opal_convertor_t);
    OBJ_CONSTRUCT(&collreq->recv_convertor, opal_convertor_t);
}

static void
collreq_destruct(struct mca_bcol_iboffload_collreq_t *collreq)
{
    OBJ_DESTRUCT(&collreq->work_requests);
    OBJ_DESTRUCT(&collreq->first_collfrag);

    OBJ_DESTRUCT(&collreq->send_convertor);
    OBJ_DESTRUCT(&collreq->recv_convertor);
}

OBJ_CLASS_INSTANCE(mca_bcol_iboffload_collreq_t,
        ompi_request_t,
        collreq_construct,
        collreq_destruct);
