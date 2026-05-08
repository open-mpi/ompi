/*
 * Copyright (c) 2018-2023 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2022      IBM Corporation. All rights reserved
 * Copyright (c) 2024      NVIDIA Corporation.  All rights reserved.
 * Copyright (c) 2026      Amazon.com, Inc. or its affiliates.
 *                         All rights reserved.
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

/*
 * @file
 *
 * This files contains all the hierarchical implementations of scatter
 */

static int mca_coll_han_scatter_us_task(void *task_args);
static int mca_coll_han_scatter_ls_task(void *task_args);

/**
 * Allocate from tiered freelists (large then small), falling back to malloc.
 */
static char *scatter_alloc_tiered(opal_free_list_t *large_fl, size_t large_size,
                                  opal_free_list_t *small_fl, size_t small_size,
                                  size_t needed, opal_free_list_item_t **item,
                                  int *src)
{
    *item = NULL;
    *src = HAN_ALLOC_MALLOC;
    if (large_size > 0 && needed <= large_size) {
        large_fragment_item_t *lfi = (large_fragment_item_t *)opal_free_list_get(large_fl);
        if (lfi != NULL) {
            *item = (opal_free_list_item_t *)lfi;
            *src = HAN_ALLOC_LARGE;
            return (char *)lfi->buffer;
        }
    }
    if (small_size > 0 && needed <= small_size) {
        fragment_item_t *fi = (fragment_item_t *)opal_free_list_get(small_fl);
        if (fi != NULL) {
            *item = (opal_free_list_item_t *)fi;
            *src = HAN_ALLOC_SMALL;
            return (char *)fi->buffer;
        }
    }
    return (char *)malloc(needed);
}

/**
 * Free a tiered allocation based on src tag.
 */
static void scatter_free_tiered(opal_free_list_t *large_fl,
                                opal_free_list_t *small_fl,
                                opal_free_list_item_t *item, char *buf, int src)
{
    if (src == HAN_ALLOC_LARGE) {
        opal_free_list_return(large_fl, item);
    } else if (src == HAN_ALLOC_SMALL) {
        opal_free_list_return(small_fl, item);
    } else {
        free(buf);
    }
}

/* Only work with regular situation (each node has equal number of processes) */

static inline void
mca_coll_han_set_scatter_args(mca_coll_han_scatter_args_t * args,
                              mca_coll_task_t * cur_task,
                              void *sbuf,
                              void *sbuf_inter_free,
                              void *sbuf_reorder_free,
                              size_t scount,
                              struct ompi_datatype_t *sdtype,
                              void *rbuf,
                              size_t rcount,
                              struct ompi_datatype_t *rdtype,
                              int root,
                              int root_up_rank,
                              int root_low_rank,
                              struct ompi_communicator_t *up_comm,
                              struct ompi_communicator_t *low_comm,
                              int w_rank, bool noop, ompi_request_t * req,
                              mca_coll_han_module_t *han_module)
{
    args->cur_task = cur_task;
    args->sbuf = sbuf;
    args->sbuf_inter_free = sbuf_inter_free;
    args->sbuf_reorder_free = sbuf_reorder_free;
    args->scount = scount;
    args->sdtype = sdtype;
    args->rbuf = rbuf;
    args->rcount = rcount;
    args->rdtype = rdtype;
    args->root = root;
    args->root_up_rank = root_up_rank;
    args->root_low_rank = root_low_rank;
    args->up_comm = up_comm;
    args->low_comm = low_comm;
    args->w_rank = w_rank;
    args->noop = noop;
    args->req = req;
    args->han_module = han_module;
    args->reorder_fl_item = NULL;
    args->reorder_fl_src = HAN_ALLOC_MALLOC;
}

/*
 * Main function for taskified scatter:
 * after data reordring, calls us task, a scatter on up communicator
 */
int
mca_coll_han_scatter_intra(const void *sbuf, size_t scount,
                           struct ompi_datatype_t *sdtype,
                           void *rbuf, size_t rcount,
                           struct ompi_datatype_t *rdtype,
                           int root,
                           struct ompi_communicator_t *comm, mca_coll_base_module_t * module)
{
    mca_coll_han_module_t *han_module = (mca_coll_han_module_t *) module;
    int w_rank, w_size;
    w_rank = ompi_comm_rank(comm);
    w_size = ompi_comm_size(comm);

    /* Create the subcommunicators */
    if( OMPI_SUCCESS != mca_coll_han_comm_create(comm, han_module) ) {
        OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                             "han cannot handle scatter with this communicator. Fall back on another component\n"));
        /* HAN cannot work with this communicator so fallback on all collectives */
        HAN_LOAD_FALLBACK_COLLECTIVES(comm, han_module);
        return han_module->previous_scatter(sbuf, scount, sdtype, rbuf, rcount, rdtype, root,
                                            comm, han_module->previous_scatter_module);
    }

    /* Topo must be initialized to know rank distribution which then is used to
     * determine if han can be used */
    int* topo = mca_coll_han_topo_init(comm, han_module, 2);
    if (han_module->are_ppn_imbalanced) {
        OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                             "han cannot handle scatter with this communicator (imbalance). Fall back on another component\n"));
        /* Put back the fallback collective support and call it once. All
         * future calls will then be automatically redirected.
         */
        HAN_UNINSTALL_COLL_API(comm, han_module, scatter);
        return han_module->previous_scatter(sbuf, scount, sdtype, rbuf, rcount, rdtype, root,
                                            comm, han_module->previous_scatter_module);
    }

    ompi_communicator_t *low_comm =
        han_module->cached_low_comms[mca_coll_han_component.han_scatter_low_module];
    ompi_communicator_t *up_comm =
        han_module->cached_up_comms[mca_coll_han_component.han_scatter_up_module];
    int *vranks = han_module->cached_vranks;
    int low_rank = ompi_comm_rank(low_comm);
    int low_size = ompi_comm_size(low_comm);
    int up_size = ompi_comm_size(up_comm);

    /* Set up request */
    ompi_request_t *temp_request = OBJ_NEW(ompi_request_t);
    temp_request->req_state = OMPI_REQUEST_ACTIVE;
    temp_request->req_type = OMPI_REQUEST_COLL;
    temp_request->req_free = ompi_coll_han_request_free;
    temp_request->req_status = (ompi_status_public_t){0};
    temp_request->req_complete = REQUEST_PENDING;

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
    opal_free_list_item_t *reorder_fl_item = NULL;
    int reorder_fl_src = HAN_ALLOC_MALLOC;

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
            if (mca_coll_han_component.han_use_persist_buffers) {
                reorder_buf = scatter_alloc_tiered(
                    &han_module->large_fragment_freelist,
                    mca_coll_han_component.han_large_fragment_size,
                    &han_module->fragment_freelist,
                    mca_coll_han_component.han_fragment_size,
                    ssize, &reorder_fl_item, &reorder_fl_src);
            } else {
                reorder_buf = (char *)malloc(ssize);
            }
            if (NULL == reorder_buf) {
                return OMPI_ERR_OUT_OF_RESOURCE;
            }
            reorder_sbuf = reorder_buf - sgap;
            for (int i = 0; i < up_size; i++) {
                for (int j = 0; j < low_size; j++) {
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
    mca_coll_han_scatter_args_t *us_args = malloc(sizeof(mca_coll_han_scatter_args_t));
    mca_coll_han_set_scatter_args(us_args, us, reorder_sbuf, NULL, reorder_buf, scount, sdtype,
                                  (char *) rbuf, rcount, rdtype, root, root_up_rank, root_low_rank,
                                  up_comm, low_comm, w_rank, low_rank != root_low_rank,
                                  temp_request, han_module);
    us_args->reorder_fl_item = reorder_fl_item;
    us_args->reorder_fl_src = reorder_fl_src;
    /* Init us task */
    init_task(us, mca_coll_han_scatter_us_task, (void *) (us_args));
    /* Issure us task */
    issue_task(us);

    ompi_request_wait(&temp_request, MPI_STATUS_IGNORE);
    return OMPI_SUCCESS;

}

/* us: upper level (inter-node) scatter task */
int mca_coll_han_scatter_us_task(void *task_args)
{
    mca_coll_han_scatter_args_t *t = (mca_coll_han_scatter_args_t *) task_args;

    if (t->noop) {
        OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output, "[%d] Han Scatter:  us noop\n",
                             t->w_rank));
    } else {
        size_t count;
        ompi_datatype_t *dtype;
        if (t->w_rank == t->root) {
            dtype = t->sdtype;
            count = t->scount;
        } else {
            dtype = t->rdtype;
            count = t->rcount;
        }
        int low_size = ompi_comm_size(t->low_comm);
        ptrdiff_t rsize, rgap = 0;
        rsize = opal_datatype_span(&dtype->super, (int64_t) count * low_size, &rgap);

        /* Inter-node receive buffer: persistent realloc-to-HWM or malloc */
        char *tmp_buf;
        tmp_buf = han_scratch_or_malloc(&t->han_module->scratch_buf[1],
                                        &t->han_module->scratch_buf_size[1],
                                        (size_t)rsize,
                                        mca_coll_han_component.han_use_persist_buffers);
        if (NULL == tmp_buf) return OMPI_ERR_OUT_OF_RESOURCE;
        char *tmp_rbuf = tmp_buf - rgap;

        OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                             "[%d] Han Scatter:  us scatter\n", t->w_rank));
        /* Inter node scatter */
        t->up_comm->c_coll->coll_scatter((char *) t->sbuf, t->scount * low_size, t->sdtype,
                                         tmp_rbuf, count * low_size, dtype, t->root_up_rank,
                                         t->up_comm, t->up_comm->c_coll->coll_scatter_module);
        t->sbuf = tmp_rbuf;
        t->sbuf_inter_free = tmp_buf;
        t->sdtype = dtype;
        t->scount = count;
    }

    /* Free reorder buffer (root only) */
    if (t->sbuf_reorder_free != NULL && t->root == t->w_rank) {
        if (mca_coll_han_component.han_use_persist_buffers) {
            scatter_free_tiered(&t->han_module->large_fragment_freelist,
                                &t->han_module->fragment_freelist,
                                t->reorder_fl_item, t->sbuf_reorder_free,
                                t->reorder_fl_src);
        } else {
            free(t->sbuf_reorder_free);
        }
        t->sbuf_reorder_free = NULL;
        t->reorder_fl_item = NULL;
    }
    /* Create ls tasks for the current union segment */
    mca_coll_task_t *ls = t->cur_task;
    /* Init ls task */
    init_task(ls, mca_coll_han_scatter_ls_task, (void *) t);
    /* Issure ls task */
    issue_task(ls);

    return OMPI_SUCCESS;
}

/* ls: lower level (shared memory or intra-node) scatter task */
int mca_coll_han_scatter_ls_task(void *task_args)
{
    mca_coll_han_scatter_args_t *t = (mca_coll_han_scatter_args_t *) task_args;
    OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output, "[%d] Han Scatter:  ls\n",
                         t->w_rank));
    OBJ_RELEASE(t->cur_task);

    t->low_comm->c_coll->coll_scatter((char *) t->sbuf, t->scount, t->sdtype, (char *) t->rbuf,
                                      t->rcount, t->rdtype, t->root_low_rank, t->low_comm,
                                      t->low_comm->c_coll->coll_scatter_module);

    /* Free inter-node buffer when not using persist buffers */
    if (!mca_coll_han_component.han_use_persist_buffers) {
        if (t->sbuf_inter_free != NULL && !t->noop) {
            free(t->sbuf_inter_free);
            t->sbuf_inter_free = NULL;
        }
    }
    OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output, "[%d] Han Scatter:  ls finish\n",
                         t->w_rank));
    ompi_request_t *temp_req = t->req;
    free(t);
    ompi_request_complete(temp_req, 1);
    return OMPI_SUCCESS;
}


int
mca_coll_han_scatter_intra_simple(const void *sbuf, size_t scount,
                                  struct ompi_datatype_t *sdtype,
                                  void *rbuf, size_t rcount,
                                  struct ompi_datatype_t *rdtype,
                                  int root,
                                  struct ompi_communicator_t *comm,
                                  mca_coll_base_module_t * module)
{
    int w_rank, w_size;
    struct ompi_datatype_t * dtype;
    int count;

    w_rank = ompi_comm_rank(comm);
    w_size = ompi_comm_size(comm);

    mca_coll_han_module_t *han_module = (mca_coll_han_module_t *) module;
    /* create the subcommunicators */
    if( OMPI_SUCCESS != mca_coll_han_comm_create_new(comm, han_module) ) {
        OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                             "han cannot handle allgather within this communicator."
                             " Fall back on another component\n"));
        /* HAN cannot work with this communicator so fallback on all collectives */
        HAN_LOAD_FALLBACK_COLLECTIVES(comm, han_module);
        return han_module->previous_scatter(sbuf, scount, sdtype, rbuf, rcount, rdtype, root,
                                            comm, han_module->previous_scatter_module);
    }
    /* Topo must be initialized to know rank distribution which then is used to
     * determine if han can be used */
    int *topo = mca_coll_han_topo_init(comm, han_module, 2);
    if (han_module->are_ppn_imbalanced){
        OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                             "han cannot handle scatter with this communicator. It needs to fall back on another component\n"));
        HAN_UNINSTALL_COLL_API(comm, han_module, scatter);
        return han_module->previous_scatter(sbuf, scount, sdtype, rbuf, rcount, rdtype, root,
                                            comm, han_module->previous_scatter_module);
    }
    ompi_communicator_t *low_comm = han_module->sub_comm[INTRA_NODE];
    ompi_communicator_t *up_comm = han_module->sub_comm[INTER_NODE];

    /* Get the 'virtual ranks' mapping corresponding to the communicators */
    int *vranks = han_module->cached_vranks;
    /* information about sub-communicators */
    int low_rank = ompi_comm_rank(low_comm);
    int low_size = ompi_comm_size(low_comm);
    /* Get root ranks for low and up comms */
    int root_low_rank, root_up_rank;
    mca_coll_han_get_ranks(vranks, root, low_size, &root_low_rank, &root_up_rank);

    if (w_rank == root) {
        dtype = sdtype;
        count = scount;
    } else {
        dtype = rdtype;
        count = rcount;
    }

    /* allocate buffer to store unordered result on root
     * if the processes are mapped-by core, no need to reorder:
     * distribution of ranks on core first and node next,
     * in a increasing order for both patterns */
    char *reorder_buf = NULL;
    bool reorder_is_sbuf = false;
    size_t block_size;

    ompi_datatype_type_size(dtype, &block_size);
    block_size *= count;

    if (w_rank == root) {
        int is_contiguous = ompi_datatype_is_contiguous_memory_layout(dtype, count);

        if (han_module->is_mapbycore && is_contiguous) {
            /* The copy of the data is avoided */
            OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                                 "[%d]: Han scatter: no need to reorder: ", w_rank));
            reorder_buf = (char *)sbuf;
            reorder_is_sbuf = true;
        } else {
            /* Data must be copied, let's be efficient packing it */
            OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                                 "[%d]: Han scatter: needs reordering or compacting: ", w_rank));

            size_t reorder_size = (size_t)block_size * w_size;
            reorder_buf = han_scratch_or_malloc(&han_module->scratch_buf[0],
                                                 &han_module->scratch_buf_size[0],
                                                 reorder_size,
                                                 mca_coll_han_component.han_use_persist_buffers);
            if (NULL == reorder_buf) return OMPI_ERROR;

            ptrdiff_t extent, block_extent;
            ompi_datatype_type_extent(dtype, &extent);
            block_extent = extent * (ptrdiff_t)count;

            for(int i = 0 ; i < w_size ; ++i){
                ompi_datatype_sndrcv((char*)sbuf + block_extent*topo[2*i+1], count, dtype,
                                     reorder_buf + block_size*i, block_size, MPI_BYTE);
            }
            dtype = MPI_BYTE;
            count = block_size;
        }
    }

    /*
     * Persistent inter-node receive buffer.
     * Grows to high-water mark via realloc so the virtual address
     * stabilises after the first large call, keeping the NIC MR cache
     * entry valid across iterations.
     *
     * When the total fits in a freelist item, use the freelist instead
     * (the item address is also stable across get/return cycles).
     */
    size_t tmp_total = block_size * low_size;
    char *tmp_buf = NULL;
    opal_free_list_item_t *tmp_fl_item = NULL;
    int tmp_fl_src = HAN_ALLOC_MALLOC;

    if (low_rank == root_low_rank) {
        if (mca_coll_han_component.han_use_persist_buffers) {
            tmp_buf = scatter_alloc_tiered(
                &han_module->large_fragment_freelist,
                mca_coll_han_component.han_large_fragment_size,
                &han_module->fragment_freelist,
                mca_coll_han_component.han_fragment_size,
                tmp_total, &tmp_fl_item, &tmp_fl_src);
            /* If tiered alloc fell back to malloc (src==0), use persist instead */
            if (tmp_fl_src == HAN_ALLOC_MALLOC && tmp_buf != NULL) {
                free(tmp_buf);
                tmp_buf = NULL;
            }
            if (tmp_fl_src == HAN_ALLOC_MALLOC) {
                tmp_buf = han_scratch_alloc(&han_module->scratch_buf[1],
                                                &han_module->scratch_buf_size[1],
                                                tmp_total);
                if (NULL == tmp_buf) return OMPI_ERR_OUT_OF_RESOURCE;
            }
        } else {
            tmp_buf = (char *)malloc(tmp_total);
            if (NULL == tmp_buf) return OMPI_ERR_OUT_OF_RESOURCE;
        }

        up_comm->c_coll->coll_scatter((char *)reorder_buf,
                    count * low_size, dtype,
                    tmp_buf,
                    block_size * low_size, MPI_BYTE,
                    root_up_rank, up_comm,
                    up_comm->c_coll->coll_scatter_module);
    }

    low_comm->c_coll->coll_scatter(tmp_buf,
                     block_size, MPI_BYTE,
                     (char *)rbuf, rcount, rdtype,
                     root_low_rank, low_comm,
                     low_comm->c_coll->coll_scatter_module);

    if (low_rank == root_low_rank) {
        if (mca_coll_han_component.han_use_persist_buffers) {
            if (tmp_fl_src != HAN_ALLOC_MALLOC) {
                scatter_free_tiered(&han_module->large_fragment_freelist,
                                    &han_module->fragment_freelist,
                                    tmp_fl_item, tmp_buf, tmp_fl_src);
            }
            /* persist buffer (src==0) is not freed */
        } else {
            free(tmp_buf);
        }
    }

    if (!mca_coll_han_component.han_use_persist_buffers && !reorder_is_sbuf) {
        free(reorder_buf);
    }

    return OMPI_SUCCESS;
}
