/*
 * Copyright (c) 2018-2020 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2020      Bull S.A.S. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/**
 * @file
 *
 * This files contains all the hierarchical implementations of allgather
 */

#include "coll_han.h"
#include "ompi/mca/coll/base/coll_base_functions.h"
#include "ompi/mca/coll/base/coll_tags.h"
#include "ompi/mca/pml/pml.h"
#include "coll_han_trigger.h"

static int mca_coll_han_allgather_lb_task(void *task_args);
static int mca_coll_han_allgather_lg_task(void *task_args);
static int mca_coll_han_allgather_uag_task(void *task_args);

static inline void
mca_coll_han_set_allgather_args(mca_coll_han_allgather_t * args,
                                mca_coll_task_t * cur_task,
                                void *sbuf,
                                void *sbuf_inter_free,
                                int scount,
                                struct ompi_datatype_t *sdtype,
                                void *rbuf,
                                int rcount,
                                struct ompi_datatype_t *rdtype,
                                int root_low_rank,
                                struct ompi_communicator_t *up_comm,
                                struct ompi_communicator_t *low_comm,
                                int w_rank,
                                bool noop,
                                bool is_mapbycore,
                                int *topo,
                                ompi_request_t * req)
{
    args->cur_task = cur_task;
    args->sbuf = sbuf;
    args->sbuf_inter_free = sbuf_inter_free;
    args->scount = scount;
    args->sdtype = sdtype;
    args->rbuf = rbuf;
    args->rcount = rcount;
    args->rdtype = rdtype;
    args->root_low_rank = root_low_rank;
    args->up_comm = up_comm;
    args->low_comm = low_comm;
    args->w_rank = w_rank;
    args->noop = noop;
    args->is_mapbycore = is_mapbycore;
    args->topo = topo;
    args->req = req;
}


/**
 * Main function for taskified allgather: calls lg task, a gather on low comm
 */
int
mca_coll_han_allgather_intra(const void *sbuf, int scount,
                             struct ompi_datatype_t *sdtype,
                             void *rbuf, int rcount,
                             struct ompi_datatype_t *rdtype,
                             struct ompi_communicator_t *comm,
                             mca_coll_base_module_t * module)
{
    /* Create the subcommunicators */
    mca_coll_han_module_t *han_module = (mca_coll_han_module_t *) module;
    if( OMPI_SUCCESS != mca_coll_han_comm_create_new(comm, han_module) ) {
        OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                             "han cannot handle allgather within this communicator. Fall back on another component\n"));
        /* HAN cannot work with this communicator so fallback on all collectives */
        HAN_LOAD_FALLBACK_COLLECTIVES(han_module, comm);
        return comm->c_coll->coll_allgather(sbuf, scount, sdtype, rbuf, rcount, rdtype,
                                            comm, comm->c_coll->coll_allgather_module);
    }
    ompi_communicator_t *low_comm = han_module->sub_comm[INTRA_NODE];
    ompi_communicator_t *up_comm = han_module->sub_comm[INTER_NODE];
    int low_rank = ompi_comm_rank(low_comm);
    int w_rank = ompi_comm_rank(comm);

    /* Init topo */
    int *topo = mca_coll_han_topo_init(comm, han_module, 2);
    /* unbalanced case needs algo adaptation */
    if (han_module->are_ppn_imbalanced) {
        OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                             "han cannot handle allgather with this communicator (imbalance). Fall back on another component\n"));
        HAN_LOAD_FALLBACK_COLLECTIVE(han_module, comm, allgather);
        return comm->c_coll->coll_allgather(sbuf, scount, sdtype, rbuf, rcount, rdtype,
                                            comm, comm->c_coll->coll_allgather_module);
    }

    ompi_request_t *temp_request;
    /* Set up request */
    temp_request = OBJ_NEW(ompi_request_t);
    temp_request->req_state = OMPI_REQUEST_ACTIVE;
    temp_request->req_type = OMPI_REQUEST_COLL;
    temp_request->req_free = han_request_free;
    temp_request->req_status = (ompi_status_public_t){0};
    temp_request->req_complete = REQUEST_PENDING;

    int root_low_rank = 0;
    /* Create lg (lower level gather) task */
    mca_coll_task_t *lg = OBJ_NEW(mca_coll_task_t);
    /* Setup lg task arguments */
    mca_coll_han_allgather_t *lg_args = malloc(sizeof(mca_coll_han_allgather_t));
    mca_coll_han_set_allgather_args(lg_args, lg, (char *) sbuf, NULL, scount, sdtype, rbuf, rcount,
                                    rdtype, root_low_rank, up_comm, low_comm, w_rank,
                                    low_rank != root_low_rank, han_module->is_mapbycore, topo,
                                    temp_request);
    /* Init and issue lg task */
    init_task(lg, mca_coll_han_allgather_lg_task, (void *) (lg_args));
    issue_task(lg);

    ompi_request_wait(&temp_request, MPI_STATUS_IGNORE);

    return OMPI_SUCCESS;
}

/* lg: lower level gather task */
int mca_coll_han_allgather_lg_task(void *task_args)
{
    mca_coll_han_allgather_t *t = (mca_coll_han_allgather_t *) task_args;
    char *tmp_buf = NULL, *tmp_rbuf = NULL;
    char *tmp_send = NULL;

    OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output, "[%d] HAN Allgather:  lg\n",
                         t->w_rank));

    /* If the process is one of the node leader */
    ptrdiff_t rlb, rext;
    ompi_datatype_get_extent (t->rdtype, &rlb, &rext);
    if (MPI_IN_PLACE == t->sbuf) {
        t->sdtype = t->rdtype;
        t->scount = t->rcount;
    }
    if (!t->noop) {
        int low_size = ompi_comm_size(t->low_comm);
        ptrdiff_t rsize, rgap = 0;
        rsize = opal_datatype_span(&t->rdtype->super, (int64_t) t->rcount * low_size, &rgap);
        tmp_buf = (char *) malloc(rsize);
        tmp_rbuf = tmp_buf - rgap;
        if (MPI_IN_PLACE == t->sbuf) {
            tmp_send = ((char*)t->rbuf) + (ptrdiff_t)t->w_rank * (ptrdiff_t)t->rcount * rext;
            ompi_datatype_copy_content_same_ddt(t->rdtype, t->rcount, tmp_rbuf, tmp_send);
        }
    }
    /* Lower level (shared memory or intra-node) gather */
    if (MPI_IN_PLACE == t->sbuf) {
        if (!t->noop) {
            t->low_comm->c_coll->coll_gather(MPI_IN_PLACE, t->scount, t->sdtype, 
                                             tmp_rbuf, t->rcount, t->rdtype, t->root_low_rank, 
                                             t->low_comm, t->low_comm->c_coll->coll_gather_module);
        }
        else {
            tmp_send = ((char*)t->rbuf) + (ptrdiff_t)t->w_rank * (ptrdiff_t)t->rcount * rext;
            t->low_comm->c_coll->coll_gather(tmp_send, t->rcount, t->rdtype, 
                                             NULL, t->rcount, t->rdtype, t->root_low_rank, 
                                             t->low_comm, t->low_comm->c_coll->coll_gather_module);
        }
    }
    else {
        t->low_comm->c_coll->coll_gather((char *) t->sbuf, t->scount, t->sdtype, tmp_rbuf, t->rcount,
                                         t->rdtype, t->root_low_rank, t->low_comm,
                                         t->low_comm->c_coll->coll_gather_module);
    }

    t->sbuf = tmp_rbuf;
    t->sbuf_inter_free = tmp_buf;

    /* Create uag (upper level all-gather) task */
    mca_coll_task_t *uag = t->cur_task;
    /* Init and issue uag task */
    init_task(uag, mca_coll_han_allgather_uag_task, (void *) t);
    issue_task(uag);

    return OMPI_SUCCESS;
}

/* uag: upper level (inter-node) all-gather task */
int mca_coll_han_allgather_uag_task(void *task_args)
{
    mca_coll_han_allgather_t *t = (mca_coll_han_allgather_t *) task_args;

    if (t->noop) {
        OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                             "[%d] HAN Allgather:  uag noop\n", t->w_rank));
    } else {
        int low_size = ompi_comm_size(t->low_comm);
        int up_size = ompi_comm_size(t->up_comm);
        char *reorder_buf = NULL;
        char *reorder_rbuf = NULL;
        if (t->is_mapbycore) {
            OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                                 "[%d]: HAN Allgather is bycore: ", t->w_rank));
            reorder_rbuf = (char *) t->rbuf;
        } else {
            ptrdiff_t rsize, rgap = 0;
            rsize =
                opal_datatype_span(&t->rdtype->super,
                                   (int64_t) t->rcount * low_size * up_size,
                                   &rgap);
            reorder_buf = (char *) malloc(rsize);
            reorder_rbuf = reorder_buf - rgap;
        }

        /* Inter node allgather */
        t->up_comm->c_coll->coll_allgather((char *) t->sbuf, t->scount * low_size, t->sdtype,
                                           reorder_rbuf, t->rcount * low_size, t->rdtype,
                                           t->up_comm, t->up_comm->c_coll->coll_allgather_module);

        if (t->sbuf_inter_free != NULL) {
            free(t->sbuf_inter_free);
            t->sbuf_inter_free = NULL;
        }

        OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                             "[%d] HAN Allgather:  ug allgather finish\n", t->w_rank));

        /* Reorder the node leader's rbuf, copy data from tmp_rbuf to rbuf */
        if (!t->is_mapbycore) {
            int i, j;
            ptrdiff_t rextent;
            ompi_datatype_type_extent(t->rdtype, &rextent);
            for (i = 0; i < up_size; i++) {
                for (j = 0; j < low_size; j++) {
                    OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                                         "[%d]: HAN Allgather copy from %d %d\n", t->w_rank,
                                         (i * low_size + j) * 2 + 1,
                                         t->topo[(i * low_size + j) * 2 + 1]));
                    ompi_datatype_copy_content_same_ddt(t->rdtype,
                                                        (ptrdiff_t) t->rcount,
                                                        (char *) t->rbuf +
                                                        rextent *
                                                        (ptrdiff_t) t->topo[(i * low_size + j) * 2 +
                                                                            1] *
                                                        (ptrdiff_t) t->rcount,
                                                        reorder_rbuf + rextent * (i * low_size +
                                                                                  j) *
                                                        (ptrdiff_t) t->rcount);
                }
            }
            free(reorder_buf);
            reorder_buf = NULL;
        }
    }


    /* Create lb (low level broadcast) task */
    mca_coll_task_t *lb = t->cur_task;
    /* Init and issue lb task */
    init_task(lb, mca_coll_han_allgather_lb_task, (void *) t);
    issue_task(lb);

    return OMPI_SUCCESS;
}

/* lb: low level broadcast task */
int mca_coll_han_allgather_lb_task(void *task_args)
{
    mca_coll_han_allgather_t *t = (mca_coll_han_allgather_t *) task_args;
    OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output, "[%d] HAN Allgather:  uag noop\n",
                         t->w_rank));
    OBJ_RELEASE(t->cur_task);
    int low_size = ompi_comm_size(t->low_comm);
    int up_size = ompi_comm_size(t->up_comm);
    t->low_comm->c_coll->coll_bcast((char *) t->rbuf, t->rcount * low_size * up_size, t->rdtype,
                                    t->root_low_rank, t->low_comm,
                                    t->low_comm->c_coll->coll_bcast_module);

    ompi_request_t *temp_req = t->req;
    free(t);
    ompi_request_complete(temp_req, 1);
    return OMPI_SUCCESS;

}

/**
 * Short implementation of allgather that only does hierarchical
 * communications without tasks.
 */
int
mca_coll_han_allgather_intra_simple(const void *sbuf, int scount,
                                    struct ompi_datatype_t *sdtype,
                                    void* rbuf, int rcount,
                                    struct ompi_datatype_t *rdtype,
                                    struct ompi_communicator_t *comm,
                                    mca_coll_base_module_t *module){

    /* create the subcommunicators */
    mca_coll_han_module_t *han_module = (mca_coll_han_module_t *)module;

    if( OMPI_SUCCESS != mca_coll_han_comm_create_new(comm, han_module) ) {
        OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                             "han cannot handle allgather within this communicator. Fall back on another component\n"));
        /* HAN cannot work with this communicator so fallback on all collectives */
        HAN_LOAD_FALLBACK_COLLECTIVES(han_module, comm);
        return comm->c_coll->coll_allgather(sbuf, scount, sdtype, rbuf, rcount, rdtype,
                                            comm, comm->c_coll->coll_allgather_module);
    }
    /* discovery topology */
    int *topo = mca_coll_han_topo_init(comm, han_module, 2);

    /* unbalanced case needs algo adaptation */
    if (han_module->are_ppn_imbalanced) {
        OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                             "han cannot handle allgather within this communicator (imbalance). Fall back on another component\n"));
        /* Put back the fallback collective support and call it once. All
         * future calls will then be automatically redirected.
         */
        HAN_LOAD_FALLBACK_COLLECTIVE(han_module, comm, allgather);
        return comm->c_coll->coll_allgather(sbuf, scount, sdtype, rbuf, rcount, rdtype,
                                            comm, comm->c_coll->coll_allgather_module);
    }

    ompi_communicator_t *low_comm = han_module->sub_comm[INTRA_NODE];
    ompi_communicator_t *up_comm = han_module->sub_comm[INTER_NODE];
    int w_rank = ompi_comm_rank(comm);
    /* setup up/low coordinates */
    int low_rank = ompi_comm_rank(low_comm);
    int low_size = ompi_comm_size(low_comm);
    int up_rank = ompi_comm_rank(up_comm);
    int up_size = ompi_comm_size(up_comm);
    int root_low_rank = 0; // node leader will be 0 on each rank

    /* allocate the intermediary buffer
     * to gather on leaders on the low sub communicator */
    ptrdiff_t rlb, rext;
    ompi_datatype_get_extent (rdtype, &rlb, &rext);
    char *tmp_buf = NULL;
    char *tmp_buf_start = NULL;
    char *tmp_send = NULL;
    if (MPI_IN_PLACE == sbuf) {
        scount = rcount;
        sdtype = rdtype;
    }
    if (low_rank == root_low_rank) {
        ptrdiff_t rsize, rgap = 0;
        /* Compute the size to receive all the local data, including datatypes empty gaps */
        rsize = opal_datatype_span(&rdtype->super, (int64_t)rcount * low_size, &rgap);
        /* intermediary buffer on node leaders to gather on low comm */
        tmp_buf = (char *) malloc(rsize);
        tmp_buf_start = tmp_buf - rgap;
        if (MPI_IN_PLACE == sbuf) {
            tmp_send = ((char*)rbuf) + (ptrdiff_t)w_rank * (ptrdiff_t)rcount * rext;
            ompi_datatype_copy_content_same_ddt(rdtype, rcount, tmp_buf_start, tmp_send);
        }
    }

    /* 1. low gather on node leaders into tmp_buf */
    if (MPI_IN_PLACE == sbuf) {
        if (low_rank == root_low_rank) {
            low_comm->c_coll->coll_gather(MPI_IN_PLACE, scount, sdtype,
                                          tmp_buf_start, rcount, rdtype, root_low_rank,
                                          low_comm, low_comm->c_coll->coll_gather_module);
        }
        else {
            tmp_send = ((char*)rbuf) + (ptrdiff_t)w_rank * (ptrdiff_t)rcount * rext;
            low_comm->c_coll->coll_gather(tmp_send, rcount, rdtype,
                                          NULL, rcount, rdtype, root_low_rank,
                                          low_comm, low_comm->c_coll->coll_gather_module);
        }
    }
    else {
        low_comm->c_coll->coll_gather((char *)sbuf, scount, sdtype,
                                      tmp_buf_start, rcount, rdtype, root_low_rank,
                                      low_comm, low_comm->c_coll->coll_gather_module);
    }
    /* 2. allgather between node leaders, from tmp_buf to reorder_buf */
    if (low_rank == root_low_rank) {
        /* allocate buffer to store unordered result on node leaders
         * if the processes are mapped-by core, no need to reorder:
         * distribution of ranks on core first and node next,
         * in a increasing order for both patterns.
         */
        char *reorder_buf = NULL;
        char *reorder_buf_start = NULL;
        if (han_module->is_mapbycore) {
            reorder_buf_start = rbuf;
        } else {
            if (0 == low_rank && 0 == up_rank) { // first rank displays message
                OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                                     "[%d]: Future Allgather needs reordering: ", up_rank));
            }
            ptrdiff_t rsize, rgap = 0;
            rsize = opal_datatype_span(&rdtype->super, (int64_t)rcount * low_size * up_size, &rgap);
            reorder_buf = (char *) malloc(rsize);
            reorder_buf_start = reorder_buf - rgap;
        }

        /* 2a. inter node allgather */
        up_comm->c_coll->coll_allgather(tmp_buf_start, scount*low_size, sdtype,
                                        reorder_buf_start, rcount*low_size, rdtype,
                                        up_comm, up_comm->c_coll->coll_allgather_module);

        if (tmp_buf != NULL) {
            free(tmp_buf);
            tmp_buf = NULL;
            tmp_buf_start = NULL;
        }

        /* 2b. reorder the node leader's into rbuf.
         * if ranks are not mapped in topological order, data needs to be reordered
         * (see reorder_gather)
         */
        if (!han_module->is_mapbycore) {
            ompi_coll_han_reorder_gather(reorder_buf_start,
                                         rbuf, rcount, rdtype,
                                         comm, topo);
            free(reorder_buf);
            reorder_buf = NULL;
        }

    }

    /* 3. up broadcast: leaders broadcast on their nodes */
    low_comm->c_coll->coll_bcast(rbuf, rcount*low_size*up_size, rdtype,
                                 root_low_rank, low_comm,
                                 low_comm->c_coll->coll_bcast_module);


    return OMPI_SUCCESS;
}
