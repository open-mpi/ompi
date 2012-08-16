/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_BCOL_IBOFFLOAD_COLLFRAG_H
#define MCA_BCOL_IBOFFLOAD_COLLFRAG_H

#include "ompi_config.h"

#include <infiniband/mqe.h>
#include <infiniband/verbs.h>
#include <infiniband/mverbs.h>

#include "bcol_iboffload.h"

#include "ompi/class/ompi_free_list.h"

BEGIN_C_DECLS

#define MAX_MQE_TASKS 128 /* Pasha - do we want to make it dynamic ?*/

struct mca_bcol_iboffload_task_t;
struct mca_bcol_iboffload_collreq_t;

/* collective fragment descriptor */
struct mca_bcol_iboffload_collfrag_t {
    ompi_free_list_item_t super;

    /* number of asynchronous sends scheduled */
    uint32_t n_sends;

    /* number of sends completed */
    uint32_t n_sends_completed;

    /* Algorithm ID that was user for this fragment*/
    int32_t alg;

    /* pre-posted receive sources */
    struct mca_bcol_iboffload_task_t *pre_posted_recvs[MAX_MQE_TASKS];

    /* cache here pointer to signaled task */
    uint64_t signal_task_wr_id;

    /* mwr completion from the mcq */
    volatile bool complete;

    /* sequence number - we use it for
       correct ordering of resources release */
    uint32_t seq_n;

    /* pointer to the full collective request descriptor */
    struct mca_bcol_iboffload_collreq_t *coll_full_req;

    size_t unpack_size;

    bool in_pending_list;

    /* Num of posted tasks */
    int tasks_posted;

    /* Pointer to head of not posted elements list */
    struct mqe_task *to_post;

    /* Pointer to tail next */
    struct mqe_task **tail_next;

    /* List of the all tasks of this coll frag */
    struct mca_bcol_iboffload_task_t *tasks_to_release;

    /* Pointer to the next elem in All tasks list */
    struct mca_bcol_iboffload_task_t **task_next;

    /* Num of needed mq credits */
    int mq_credits;

    /* MQ index, that used for this frag */
    int mq_index;

    /*
     * Last wait sequence number; zero i.e.
     * there isn't any wait in the coll request
     */
    int32_t last_wait_num;
};
typedef struct mca_bcol_iboffload_collfrag_t mca_bcol_iboffload_collfrag_t;
OBJ_CLASS_DECLARATION(mca_bcol_iboffload_collfrag_t);

static inline __opal_attribute_always_inline__
            void mca_bcol_iboffload_collfrag_init(
                          mca_bcol_iboffload_collfrag_t *cf)
{
    /* init the request */
    cf->n_sends = 0;
    cf->complete = false;
    cf->n_sends_completed = 0;
    cf->alg = -1;
    cf->in_pending_list = false;
    cf->tail_next = NULL;
    cf->tasks_posted = 0;
    cf->to_post = NULL;
    cf->mq_credits = 0;
    cf->mq_index = 0;
    cf->tasks_to_release = NULL;
    cf->task_next = &cf->tasks_to_release;
    cf->last_wait_num = 0;
}

static inline __opal_attribute_always_inline__
                struct mca_bcol_iboffload_collfrag_t *
                       mca_bcol_iboffload_get_collfrag(void)
{
    int rc;
    ompi_free_list_item_t *item;
    mca_bcol_iboffload_collfrag_t *cf;
    mca_bcol_iboffload_component_t *cm = &mca_bcol_iboffload_component;

    /* blocking allocation for collectives fragment */
    OMPI_FREE_LIST_GET(&cm->collfrags_free, item, rc);
    if (OPAL_UNLIKELY(NULL == item)) {
        IBOFFLOAD_ERROR(("Failed to allocated collfrag.\n"));
        return NULL;
    }

    cf = (mca_bcol_iboffload_collfrag_t*) item;
    mca_bcol_iboffload_collfrag_init(cf);

    return cf;
}

END_C_DECLS

#endif
