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

#include <string.h>

#include "bcol_iboffload_collreq.h"
#include "bcol_iboffload_collfrag.h"

static void
collfrag_constructor(struct mca_bcol_iboffload_collfrag_t *collfrag)
{
    collfrag->n_sends = 0;
    collfrag->n_sends_completed = 0;

    memset(collfrag->pre_posted_recvs, 0,
           sizeof(struct mca_bcol_iboffload_task_t *) * MAX_MQE_TASKS);

    collfrag->signal_task_wr_id = (uint64_t) 0;
    collfrag->complete = false;

    collfrag->seq_n = -1;
    collfrag->coll_full_req = NULL;

    collfrag->unpack_size = 0;

    collfrag->tasks_posted = 0;
    collfrag->to_post = NULL;
    collfrag->task_next = NULL;
    collfrag->tasks_to_release = NULL;

    collfrag->in_pending_list = false;
}

static void
collfrag_destruct(struct mca_bcol_iboffload_collfrag_t *collfrag)
{
}

OBJ_CLASS_INSTANCE(mca_bcol_iboffload_collfrag_t,
                   ompi_free_list_item_t,
                   collfrag_constructor,
                   collfrag_destruct);
