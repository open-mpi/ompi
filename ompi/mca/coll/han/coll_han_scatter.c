/*
 * Copyright (c) 2018-2020 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "coll_han.h"
#include "ompi/mca/coll/base/coll_base_functions.h"
#include "ompi/mca/coll/base/coll_tags.h"
#include "ompi/mca/pml/pml.h"
#include "coll_han_trigger.h"

/* Only work with regular situation (each node has equal number of processes) */

void mac_coll_han_set_scatter_argu(mca_scatter_argu_t * argu,
                                   mca_coll_task_t * cur_task,
                                   void *sbuf,
                                   void *sbuf_inter_free,
                                   void *sbuf_reorder_free,
                                   int scount,
                                   struct ompi_datatype_t *sdtype,
                                   void *rbuf,
                                   int rcount,
                                   struct ompi_datatype_t *rdtype,
                                   int root,
                                   int root_up_rank,
                                   int root_low_rank,
                                   struct ompi_communicator_t *up_comm,
                                   struct ompi_communicator_t *low_comm,
                                   int w_rank, bool noop, ompi_request_t * req)
{
    argu->cur_task = cur_task;
    argu->sbuf = sbuf;
    argu->sbuf_inter_free = sbuf_inter_free;
    argu->sbuf_reorder_free = sbuf_reorder_free;
    argu->scount = scount;
    argu->sdtype = sdtype;
    argu->rbuf = rbuf;
    argu->rcount = rcount;
    argu->rdtype = rdtype;
    argu->root = root;
    argu->root_up_rank = root_up_rank;
    argu->root_low_rank = root_low_rank;
    argu->up_comm = up_comm;
    argu->low_comm = low_comm;
    argu->w_rank = w_rank;
    argu->noop = noop;
    argu->req = req;
}

int
mca_coll_han_scatter_intra(const void *sbuf, int scount,
                            struct ompi_datatype_t *sdtype,
                            void *rbuf, int rcount,
                            struct ompi_datatype_t *rdtype,
                            int root,
                            struct ompi_communicator_t *comm, mca_coll_base_module_t * module)
{
    int i, j;
    int w_rank, w_size;
    w_rank = ompi_comm_rank(comm);
    w_size = ompi_comm_size(comm);

    mca_coll_han_module_t *han_module = (mca_coll_han_module_t *) module;
    int *topo = mca_coll_han_topo_init(comm, han_module, 2);
    /* Topo must be initialized to know rank distribution which then is used to
     * determine if han can be used */
    mca_coll_han_topo_init(comm, han_module, 2);
    if (han_module->are_ppn_imbalanced){
        OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                             "han cannot handle scatter with this communicator. It needs to fall back on another component\n"));
        goto prev_scatter_intra;
    }

    /* Create the subcommunicators */
    mca_coll_han_comm_create(comm, han_module);
    ompi_communicator_t *low_comm =
         han_module->cached_low_comms[mca_coll_han_component.han_scatter_low_module];
    ompi_communicator_t *up_comm =
         han_module->cached_up_comms[mca_coll_han_component.han_scatter_up_module];
    int *vranks = han_module->cached_vranks;
    int low_rank = ompi_comm_rank(low_comm);
    int low_size = ompi_comm_size(low_comm);
    int up_size = ompi_comm_size(up_comm);

    ompi_request_t *temp_request = NULL;
    /* Set up request */
    temp_request = OBJ_NEW(ompi_request_t);
    OMPI_REQUEST_INIT(temp_request, false);
    temp_request->req_state = OMPI_REQUEST_ACTIVE;
    temp_request->req_type = 0;
    temp_request->req_free = han_request_free;
    temp_request->req_status.MPI_SOURCE = 0;
    temp_request->req_status.MPI_TAG = 0;
    temp_request->req_status.MPI_ERROR = 0;
    temp_request->req_status._cancelled = 0;
    temp_request->req_status._ucount = 0;

    int root_low_rank;
    int root_up_rank;


    mca_coll_han_get_ranks(vranks, root, low_size, &root_low_rank, &root_up_rank);
    OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                         "[%d]: Han Scatter root %d root_low_rank %d root_up_rank %d\n", w_rank,
                         root, root_low_rank, root_up_rank));

    /* Reorder sbuf based on rank.
     * Suppose, message is 0 1 2 3 4 5 6 7
     * and the processes are mapped on 2 nodes (the processes on the node 0 is 0 2 4 6 and the processes on the node 1 is 1 3 5 7),
     * so the message needs to be reordered to 0 2 4 6 1 3 5 7
     */
    char *reorder_buf = NULL;
    char *reorder_sbuf = NULL;

    if (w_rank == root) {
        /* If the processes are mapped-by core, no need to reorder */
        if (han_module->is_mapbycore) {
            OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                                 "[%d]: Han Scatter is_bycore: ", w_rank));
            reorder_sbuf = (char *) sbuf;
        } else {
            ptrdiff_t ssize, sgap = 0, sextent;
            ompi_datatype_type_extent(sdtype, &sextent);
            ssize = opal_datatype_span(&sdtype->super, (int64_t) scount * w_size, &sgap);
            reorder_buf = (char *) malloc(ssize);
            reorder_sbuf = reorder_buf - sgap;
            for (i = 0; i < up_size; i++) {
                for (j = 0; j < low_size; j++) {
                    OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                                         "[%d]: Han Scatter copy from %d %d\n", w_rank,
                                         (i * low_size + j) * 2 + 1,
                                         topo[(i * low_size + j) * 2 + 1]));
                    ompi_datatype_copy_content_same_ddt(sdtype, (ptrdiff_t) scount,
                                                        reorder_sbuf + sextent * (i * low_size +
                                                                                  j) *
                                                        (ptrdiff_t) scount,
                                                        (char *) sbuf +
                                                        sextent *
                                                        (ptrdiff_t) topo[(i * low_size + j) * 2 +
                                                                         1] * (ptrdiff_t) scount);
                }
            }
        }
    }

    /* Create us task */
    mca_coll_task_t *us = OBJ_NEW(mca_coll_task_t);
    /* Setup us task arguments */
    mca_scatter_argu_t *us_argu = malloc(sizeof(mca_scatter_argu_t));
    mac_coll_han_set_scatter_argu(us_argu, us, reorder_sbuf, NULL, reorder_buf, scount, sdtype,
                                  (char *) rbuf, rcount, rdtype, root, root_up_rank, root_low_rank,
                                  up_comm, low_comm, w_rank, low_rank != root_low_rank,
                                  temp_request);
    /* Init us task */
    init_task(us, mca_coll_han_scatter_us_task, (void *) (us_argu));
    /* Issure us task */
    issue_task(us);

    ompi_request_wait(&temp_request, MPI_STATUS_IGNORE);
    return OMPI_SUCCESS;

prev_scatter_intra:
    return han_module->previous_scatter(sbuf, scount, sdtype,
                                        rbuf, rcount, rdtype,
                                        root, comm,
                                        han_module->previous_scatter_module);
}

/* us: upper level (intra-node) scatter task */
int mca_coll_han_scatter_us_task(void *task_argu)
{
    mca_scatter_argu_t *t = (mca_scatter_argu_t *) task_argu;
    OBJ_RELEASE(t->cur_task);

    if (t->noop) {
        OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output, "[%d] Han Scatter:  us noop\n",
                             t->w_rank));
    } else {
        int low_size = ompi_comm_size(t->low_comm);
        ptrdiff_t rsize, rgap = 0;
        rsize = opal_datatype_span(&t->rdtype->super, (int64_t) t->rcount * low_size, &rgap);
        char *tmp_buf = (char *) malloc(rsize);
        char *tmp_rbuf = tmp_buf - rgap;
        OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                             "[%d] Han Scatter:  us scatter\n", t->w_rank));
        /* Inter node scatter */
        t->up_comm->c_coll->coll_scatter((char *) t->sbuf, t->scount * low_size, t->sdtype,
                                         tmp_rbuf, t->rcount * low_size, t->rdtype, t->root_up_rank,
                                         t->up_comm, t->up_comm->c_coll->coll_scatter_module);
        t->sbuf = tmp_rbuf;
        t->sbuf_inter_free = tmp_buf;
    }

    if (t->sbuf_reorder_free != NULL && t->root == t->w_rank) {
        free(t->sbuf_reorder_free);
        t->sbuf_reorder_free = NULL;
    }
    /* Create ls tasks for the current union segment */
    mca_coll_task_t *ls = OBJ_NEW(mca_coll_task_t);
    /* Setup up ls task arguments */
    t->cur_task = ls;
    /* Init ls task */
    init_task(ls, mca_coll_han_scatter_ls_task, (void *) t);
    /* Issure ls task */
    issue_task(ls);

    return OMPI_SUCCESS;
}

/* ls: lower level (shared memory) scatter task */
int mca_coll_han_scatter_ls_task(void *task_argu)
{
    mca_scatter_argu_t *t = (mca_scatter_argu_t *) task_argu;
    OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output, "[%d] Han Scatter:  ls\n",
                         t->w_rank));
    OBJ_RELEASE(t->cur_task);
    /* Shared memory scatter */
    t->low_comm->c_coll->coll_scatter((char *) t->sbuf, t->scount, t->sdtype, (char *) t->rbuf,
                                      t->rcount, t->rdtype, t->root_low_rank, t->low_comm,
                                      t->low_comm->c_coll->coll_scatter_module);

    if (t->sbuf_inter_free != NULL && t->noop != true) {
        free(t->sbuf_inter_free);
        t->sbuf_inter_free = NULL;
    }
    OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output, "[%d] Han Scatter:  ls finish\n",
                         t->w_rank));
    ompi_request_t *temp_req = t->req;
    free(t);
    ompi_request_complete(temp_req, 1);
    return OMPI_SUCCESS;
}
