/*
 * Copyright (c) 2018-2023 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2020      Bull S.A.S. All rights reserved.
 * Copyright (c) 2022      IBM Corporation. All rights reserved
 * Copyright (c) 2024      NVIDIA Corporation.  All rights reserved.
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

/* Minimum number of fragments before the pipeline path is used.
 * Below this threshold the simple path avoids pipeline setup overhead. */
#define HAN_MIN_PIPELINE_FRAGS 4

static int mca_coll_han_allgather_lb_task(void *task_args);
static int mca_coll_han_allgather_lg_task(void *task_args);
static int mca_coll_han_allgather_uag_task(void *task_args);

/**
 * Allocate a buffer from the small fragment freelist, falling back to malloc.
 * On return, *item is non-NULL if the buffer came from the freelist.
 */
static char *han_alloc_frag(opal_free_list_t *fl, size_t frag_size,
                            size_t needed, opal_free_list_item_t **item)
{
    *item = NULL;
    if (mca_coll_han_component.han_use_persist_buffers
        && frag_size > 0 && needed <= frag_size) {
        fragment_item_t *fi = (fragment_item_t *)opal_free_list_get(fl);
        if (fi != NULL) {
            *item = (opal_free_list_item_t *)fi;
            return (char *)fi->buffer;
        }
    }
    return (char *)malloc(needed);
}

/**
 * Free a buffer: return to freelist if item is non-NULL, else free().
 */
static void han_free_frag(opal_free_list_t *fl, opal_free_list_item_t *item,
                          char *buf)
{
    if (item != NULL) {
        opal_free_list_return(fl, item);
    } else {
        free(buf);
    }
}

/**
 * Tiered allocation: try large freelist, then small freelist, then malloc.
 * Sets *item and *src (1=large, 2=small, 0=malloc) for han_free_tiered().
 */
static char *han_alloc_tiered(opal_free_list_t *large_fl, size_t large_size,
                              opal_free_list_t *small_fl, size_t small_size,
                              size_t needed, opal_free_list_item_t **item,
                              int *src)
{
    *item = NULL;
    *src = HAN_ALLOC_MALLOC;
    if (!mca_coll_han_component.han_use_persist_buffers) {
        return (char *)malloc(needed);
    }
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
static void han_free_tiered(opal_free_list_t *large_fl,
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

static inline void
mca_coll_han_set_allgather_args(mca_coll_han_allgather_t * args,
                                mca_coll_task_t * cur_task,
                                void *sbuf,
                                void *sbuf_inter_free,
                                size_t scount,
                                struct ompi_datatype_t *sdtype,
                                void *rbuf,
                                size_t rcount,
                                struct ompi_datatype_t *rdtype,
                                int root_low_rank,
                                struct ompi_communicator_t *up_comm,
                                struct ompi_communicator_t *low_comm,
                                int w_rank,
                                bool noop,
                                bool is_mapbycore,
                                int *topo,
                                ompi_request_t * req,
                                mca_coll_han_module_t *han_module)
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
    args->han_module = han_module;
    args->inter_frag = NULL;
}


/**
 * Main function for taskified allgather: calls lg task, a gather on low comm
 */
int
mca_coll_han_allgather_intra(const void *sbuf, size_t scount,
                             struct ompi_datatype_t *sdtype,
                             void *rbuf, size_t rcount,
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
        HAN_LOAD_FALLBACK_COLLECTIVES(comm, han_module);
        return han_module->previous_allgather(sbuf, scount, sdtype, rbuf, rcount, rdtype,
                                              comm, han_module->previous_allgather_module);
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
        HAN_UNINSTALL_COLL_API(comm, han_module, allgather);
        return han_module->previous_allgather(sbuf, scount, sdtype, rbuf, rcount, rdtype,
                                              comm, han_module->previous_allgather_module);
    }

    ompi_request_t *temp_request;
    /* Set up request */
    temp_request = OBJ_NEW(ompi_request_t);
    temp_request->req_state = OMPI_REQUEST_ACTIVE;
    temp_request->req_type = OMPI_REQUEST_COLL;
    temp_request->req_free = ompi_coll_han_request_free;
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
                                    temp_request, han_module);
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

    ptrdiff_t rlb, rext;
    ompi_datatype_get_extent (t->rdtype, &rlb, &rext);
    if (MPI_IN_PLACE == t->sbuf) {
        t->sdtype = t->rdtype;
        t->scount = t->rcount;
    }
    
    /* If the process is one of the node leaders, allocate receive buffer */
    if (!t->noop) {
        int low_size = ompi_comm_size(t->low_comm);
        ptrdiff_t rsize, rgap = 0;

        /* Mapbycore in-place: gather directly into rbuf slot, skip tmp_buf */
        if (t->is_mapbycore && mca_coll_han_component.han_use_persist_buffers) {
            int up_rank = ompi_comm_rank(t->up_comm);
            size_t total_count = t->rcount * low_size;
            char *my_slot = (char *)t->rbuf
                + (ptrdiff_t)up_rank * (ptrdiff_t)total_count * rext;

            if (MPI_IN_PLACE == t->sbuf) {
                char *my_data = ((char*)t->rbuf)
                    + (ptrdiff_t)t->w_rank * (ptrdiff_t)t->rcount * rext;
                ompi_datatype_copy_content_same_ddt(t->rdtype, t->rcount,
                                                    my_slot, my_data);
                t->low_comm->c_coll->coll_gather(MPI_IN_PLACE, t->scount, t->sdtype,
                                                 my_slot, t->rcount, t->rdtype,
                                                 t->root_low_rank, t->low_comm,
                                                 t->low_comm->c_coll->coll_gather_module);
            } else {
                t->low_comm->c_coll->coll_gather((char *)t->sbuf, t->scount, t->sdtype,
                                                 my_slot, t->rcount, t->rdtype,
                                                 t->root_low_rank, t->low_comm,
                                                 t->low_comm->c_coll->coll_gather_module);
            }
            t->sbuf = my_slot;
            t->sbuf_inter_free = NULL;
            t->inter_frag = NULL;

            /* Create uag task */
            mca_coll_task_t *uag = t->cur_task;
            init_task(uag, mca_coll_han_allgather_uag_task, (void *) t);
            issue_task(uag);
            return OMPI_SUCCESS;
        }

        rsize = opal_datatype_span(&t->rdtype->super, (int64_t) t->rcount * low_size, &rgap);

        t->inter_frag = NULL;
        if (mca_coll_han_component.han_use_persist_buffers && t->han_module != NULL) {
            if ((size_t)rsize <= mca_coll_han_component.han_fragment_size
                && mca_coll_han_component.han_fragment_size > 0) {
                tmp_buf = han_alloc_frag(&t->han_module->fragment_freelist,
                                         mca_coll_han_component.han_fragment_size,
                                         (size_t)rsize, &t->inter_frag);
            } else {
                /* Too large for freelist — use shared scratch buffer */
                tmp_buf = han_scratch_alloc(&t->han_module->scratch_buf[0],
                                            &t->han_module->scratch_buf_size[0],
                                            (size_t)rsize);
                if (NULL == tmp_buf) return OMPI_ERR_OUT_OF_RESOURCE;
            }
        } else {
            tmp_buf = (char *) malloc(rsize);
            if (NULL == tmp_buf) return OMPI_ERR_OUT_OF_RESOURCE;
        }
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
    /* When using persist gather buffer, don't free it in uag_task */
    if (mca_coll_han_component.han_use_persist_buffers
        && t->inter_frag == NULL && t->han_module != NULL
        && tmp_buf == t->han_module->scratch_buf[0]) {
        t->sbuf_inter_free = NULL;
    }

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

            /* When persist buffers gathered directly into rbuf, use in-place */
            if (mca_coll_han_component.han_use_persist_buffers
                && t->sbuf_inter_free == NULL) {
                t->up_comm->c_coll->coll_allgather(MPI_IN_PLACE,
                    t->scount * low_size, t->sdtype,
                    reorder_rbuf, t->rcount * low_size, t->rdtype,
                    t->up_comm, t->up_comm->c_coll->coll_allgather_module);
                goto allgather_done;
            }
        } else {
            ptrdiff_t rsize, rgap = 0;
            rsize =
                opal_datatype_span(&t->rdtype->super,
                                   (int64_t) t->rcount * low_size * up_size,
                                   &rgap);
            if (mca_coll_han_component.han_use_persist_buffers && t->han_module != NULL) {
                reorder_buf = han_scratch_alloc(&t->han_module->scratch_buf[1],
                                                &t->han_module->scratch_buf_size[1],
                                                (size_t)rsize);
                if (NULL == reorder_buf) return OMPI_ERR_OUT_OF_RESOURCE;
            } else {
                reorder_buf = (char *) malloc(rsize);
            }
            reorder_rbuf = reorder_buf - rgap;
        }

        /* Inter node allgather */
        t->up_comm->c_coll->coll_allgather((char *) t->sbuf, t->scount * low_size, t->sdtype,
                                           reorder_rbuf, t->rcount * low_size, t->rdtype,
                                           t->up_comm, t->up_comm->c_coll->coll_allgather_module);

        if (t->sbuf_inter_free != NULL) {
            han_free_frag(&t->han_module->fragment_freelist,
                          t->inter_frag, t->sbuf_inter_free);
            t->inter_frag = NULL;
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
            if (!mca_coll_han_component.han_use_persist_buffers) {
                free(reorder_buf);
            }
            reorder_buf = NULL;
        }
    }

allgather_done:
    ; /* empty statement required after label before declaration */
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
 * Reorder a fragment from gathered layout into rbuf at the correct offset.
 * Used by the pipeline and single-fragment paths.
 */
static inline void
han_reorder_frag(char *rbuf, const char *src_buf,
                 struct ompi_datatype_t *rdtype, ptrdiff_t rextent,
                 size_t frag_count, size_t frag_offset, size_t rcount,
                 int up_size, int low_size, const int *topo)
{
    for (int i = 0; i < up_size; i++) {
        for (int j = 0; j < low_size; j++) {
            int global_idx = i * low_size + j;
            int dest_rank = topo[global_idx * 2 + 1];
            ompi_datatype_copy_content_same_ddt(rdtype,
                (ptrdiff_t)frag_count,
                rbuf + rextent * ((ptrdiff_t)dest_rank * (ptrdiff_t)rcount
                                  + (ptrdiff_t)frag_offset),
                (char *)src_buf + rextent * (ptrdiff_t)global_idx
                                * (ptrdiff_t)frag_count);
        }
    }
}

/**
 * Mapbycore fast path: gather directly into rbuf, in-place allgather,
 * single bcast. No temporary buffers needed.
 */
static int
han_allgather_mapbycore(const void *sbuf, size_t scount,
                        struct ompi_datatype_t *sdtype,
                        void *rbuf, size_t rcount,
                        struct ompi_datatype_t *rdtype,
                        struct ompi_communicator_t *up_comm,
                        struct ompi_communicator_t *low_comm,
                        int w_rank, int low_rank, int up_rank,
                        int low_size, int up_size, int root_low_rank)
{
    ptrdiff_t rlb, rext;
    ompi_datatype_get_extent(rdtype, &rlb, &rext);
    size_t total_count = rcount * low_size;
    char *my_slot = (char *)rbuf + (ptrdiff_t)up_rank * (ptrdiff_t)total_count * rext;

    if (MPI_IN_PLACE == sbuf) {
        if (low_rank == root_low_rank) {
            low_comm->c_coll->coll_gather(MPI_IN_PLACE, scount, sdtype,
                                          my_slot, rcount, rdtype, root_low_rank,
                                          low_comm, low_comm->c_coll->coll_gather_module);
        } else {
            char *my_data = ((char*)rbuf) + (ptrdiff_t)w_rank * (ptrdiff_t)rcount * rext;
            low_comm->c_coll->coll_gather(my_data, rcount, rdtype,
                                          NULL, rcount, rdtype, root_low_rank,
                                          low_comm, low_comm->c_coll->coll_gather_module);
        }
    } else {
        low_comm->c_coll->coll_gather((char *)sbuf, scount, sdtype,
                                      my_slot, rcount, rdtype, root_low_rank,
                                      low_comm, low_comm->c_coll->coll_gather_module);
    }

    if (low_rank == root_low_rank) {
        up_comm->c_coll->coll_allgather(MPI_IN_PLACE, total_count, rdtype,
                                        rbuf, total_count, rdtype,
                                        up_comm, up_comm->c_coll->coll_allgather_module);
    }

    low_comm->c_coll->coll_bcast(rbuf, rcount*low_size*up_size, rdtype,
                                 root_low_rank, low_comm,
                                 low_comm->c_coll->coll_bcast_module);
    return OMPI_SUCCESS;
}

/**
 * Single-fragment freelist path: gather into reorder_buf, in-place
 * allgather, reorder into rbuf, bcast.
 */
static int
han_allgather_single_frag(const void *sbuf, size_t scount,
                          struct ompi_datatype_t *sdtype,
                          void *rbuf, size_t rcount,
                          struct ompi_datatype_t *rdtype,
                          mca_coll_han_module_t *han_module,
                          struct ompi_communicator_t *up_comm,
                          struct ompi_communicator_t *low_comm,
                          struct ompi_communicator_t *comm,
                          int w_rank, int low_rank, int up_rank,
                          int low_size, int up_size, int root_low_rank,
                          size_t frag_size, const int *topo)
{
    ptrdiff_t rlb, rext, rextent;
    ompi_datatype_get_extent(rdtype, &rlb, &rext);
    ompi_datatype_type_extent(rdtype, &rextent);
    size_t total_count = rcount * low_size;
    char *reorder_buf = NULL;
    char *reorder_buf_start = NULL;
    char *my_slot = NULL;
    opal_free_list_item_t *fl_item = NULL;

    if (low_rank == root_low_rank) {
        ptrdiff_t rsize, rgap = 0;
        rsize = opal_datatype_span(&rdtype->super,
            (int64_t)rcount * low_size * up_size, &rgap);

        reorder_buf = han_alloc_frag(&han_module->fragment_freelist,
                                     frag_size, (size_t)rsize, &fl_item);
        if (NULL == reorder_buf) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        reorder_buf_start = reorder_buf - rgap;
        my_slot = reorder_buf_start
            + rextent * (ptrdiff_t)up_rank * (ptrdiff_t)total_count;
        if (MPI_IN_PLACE == sbuf) {
            char *my_data = ((char*)rbuf) + (ptrdiff_t)w_rank * (ptrdiff_t)rcount * rext;
            ompi_datatype_copy_content_same_ddt(rdtype, rcount, my_slot, my_data);
        }
    }

    if (MPI_IN_PLACE == sbuf) {
        if (low_rank == root_low_rank) {
            low_comm->c_coll->coll_gather(MPI_IN_PLACE, scount, sdtype,
                my_slot, rcount, rdtype, root_low_rank,
                low_comm, low_comm->c_coll->coll_gather_module);
        } else {
            char *my_data = ((char*)rbuf) + (ptrdiff_t)w_rank * (ptrdiff_t)rcount * rext;
            low_comm->c_coll->coll_gather(my_data, rcount, rdtype,
                NULL, rcount, rdtype, root_low_rank,
                low_comm, low_comm->c_coll->coll_gather_module);
        }
    } else {
        low_comm->c_coll->coll_gather((char *)sbuf, scount, sdtype,
            my_slot, rcount, rdtype, root_low_rank,
            low_comm, low_comm->c_coll->coll_gather_module);
    }

    if (low_rank == root_low_rank) {
        up_comm->c_coll->coll_allgather(MPI_IN_PLACE, total_count, rdtype,
            reorder_buf_start, total_count, rdtype,
            up_comm, up_comm->c_coll->coll_allgather_module);

        ompi_coll_han_reorder_gather(reorder_buf_start, rbuf, rcount, rdtype, comm, topo);
        han_free_frag(&han_module->fragment_freelist, fl_item, reorder_buf);
    }

    low_comm->c_coll->coll_bcast(rbuf, rcount * low_size * up_size, rdtype,
        root_low_rank, low_comm, low_comm->c_coll->coll_bcast_module);
    return OMPI_SUCCESS;
}

/**
 * Pipeline path: double-buffered igather+ibcast with blocking low_comm
 * gather as the sync point.
 */
static int
han_allgather_pipeline(const void *sbuf, size_t scount,
                       struct ompi_datatype_t *sdtype,
                       void *rbuf, size_t rcount,
                       struct ompi_datatype_t *rdtype,
                       mca_coll_han_module_t *han_module,
                       struct ompi_communicator_t *up_comm,
                       struct ompi_communicator_t *low_comm,
                       int w_rank, int low_rank,
                       int low_size, int up_size, int root_low_rank,
                       size_t frag_size, size_t frag_count, size_t num_frags,
                       const int *topo)
{
    ptrdiff_t rlb, rext, rextent;
    int root_up_rank = 0;
    ompi_datatype_get_extent(rdtype, &rlb, &rext);
    ompi_datatype_type_extent(rdtype, &rextent);

    /* Pipeline requires non-blocking collectives on up_comm */
    if (NULL == up_comm->c_coll->coll_ibcast
        || NULL == up_comm->c_coll->coll_igather) {
        return OMPI_ERR_NOT_SUPPORTED;
    }

    /* Allocate double-buffered reorder buffers */
    char *frag_reorder[2] = {NULL, NULL};
    opal_free_list_item_t *frag_reorder_item[2] = {NULL, NULL};
    int frag_reorder_src[2] = {HAN_ALLOC_MALLOC, HAN_ALLOC_MALLOC};
    if (low_rank == root_low_rank) {
        size_t frag_reorder_size = (size_t)frag_count * low_size * up_size * rextent;
        size_t large_frag_size = mca_coll_han_component.han_large_fragment_size;
        for (int b = 0; b < 2; b++) {
            frag_reorder[b] = han_alloc_tiered(
                &han_module->large_fragment_freelist, large_frag_size,
                &han_module->fragment_freelist, frag_size,
                frag_reorder_size, &frag_reorder_item[b],
                &frag_reorder_src[b]);
            if (NULL == frag_reorder[b]) {
                for (int i = 0; i < b; i++) {
                    han_free_tiered(&han_module->large_fragment_freelist,
                                    &han_module->fragment_freelist,
                                    frag_reorder_item[i], frag_reorder[i],
                                    frag_reorder_src[i]);
                }
                return OMPI_ERR_OUT_OF_RESOURCE;
            }
        }
    }

    opal_free_list_item_t *inter_frag_item = NULL;
    char *gather_buf = NULL;
    ompi_request_t *igather_req = NULL;
    ompi_request_t *ibcast_req = NULL;
    size_t prev_frag_count = 0;
    size_t prev_frag_offset = 0;
    int cur_buf = 0;

    for (size_t frag = 0; frag < num_frags; frag++) {
        size_t frag_offset = frag * frag_count;
        size_t this_count = frag_count;
        if (frag_offset + this_count > rcount)
            this_count = rcount - frag_offset;

        int prev_buf = 1 - cur_buf;

        if (frag > 0 && low_rank == root_low_rank) {
            ompi_request_wait(&igather_req, MPI_STATUS_IGNORE);
            igather_req = NULL;

            han_free_frag(&han_module->fragment_freelist,
                          inter_frag_item, gather_buf);
            inter_frag_item = NULL;
            gather_buf = NULL;

            size_t prev_ag = prev_frag_count * low_size * up_size;
            up_comm->c_coll->coll_ibcast(frag_reorder[prev_buf], prev_ag, rdtype,
                root_up_rank, up_comm, &ibcast_req,
                up_comm->c_coll->coll_ibcast_module);
        }

        if (low_rank == root_low_rank) {
            gather_buf = han_alloc_frag(&han_module->fragment_freelist,
                                        frag_size,
                                        (size_t)this_count * low_size * rextent,
                                        &inter_frag_item);
            if (NULL == gather_buf) {
                /* Clean up any outstanding ibcast and double buffers */
                if (ibcast_req != NULL) {
                    ompi_request_wait(&ibcast_req, MPI_STATUS_IGNORE);
                }
                for (int b = 0; b < 2; b++) {
                    han_free_tiered(&han_module->large_fragment_freelist,
                                    &han_module->fragment_freelist,
                                    frag_reorder_item[b], frag_reorder[b],
                                    frag_reorder_src[b]);
                }
                return OMPI_ERR_OUT_OF_RESOURCE;
            }
            if (MPI_IN_PLACE == sbuf) {
                char *my_data = ((char*)rbuf)
                    + ((ptrdiff_t)w_rank * (ptrdiff_t)rcount + (ptrdiff_t)frag_offset) * rext;
                ompi_datatype_copy_content_same_ddt(rdtype, this_count,
                    gather_buf, my_data);
            }
        }

        /* ALL ranks: blocking low_comm gather — SYNC POINT */
        if (MPI_IN_PLACE == sbuf) {
            if (low_rank == root_low_rank) {
                low_comm->c_coll->coll_gather(MPI_IN_PLACE, this_count, rdtype,
                    gather_buf, this_count, rdtype, root_low_rank,
                    low_comm, low_comm->c_coll->coll_gather_module);
            } else {
                char *my_data = ((char*)rbuf)
                    + ((ptrdiff_t)w_rank * (ptrdiff_t)rcount + (ptrdiff_t)frag_offset) * rext;
                low_comm->c_coll->coll_gather(my_data, this_count, rdtype,
                    NULL, this_count, rdtype, root_low_rank,
                    low_comm, low_comm->c_coll->coll_gather_module);
            }
        } else {
            low_comm->c_coll->coll_gather(
                (char *)sbuf + (ptrdiff_t)frag_offset * rext, this_count, sdtype,
                gather_buf, this_count, rdtype, root_low_rank,
                low_comm, low_comm->c_coll->coll_gather_module);
        }

        if (low_rank == root_low_rank) {
            size_t ag_count = this_count * low_size;
            up_comm->c_coll->coll_igather(gather_buf, ag_count, rdtype,
                frag_reorder[cur_buf], ag_count, rdtype, root_up_rank,
                up_comm, &igather_req, up_comm->c_coll->coll_igather_module);
        }

        if (frag > 0 && low_rank == root_low_rank) {
            ompi_request_wait(&ibcast_req, MPI_STATUS_IGNORE);
            ibcast_req = NULL;

            han_reorder_frag(rbuf, frag_reorder[prev_buf], rdtype, rextent,
                             prev_frag_count, prev_frag_offset, rcount,
                             up_size, low_size, topo);
        }

        prev_frag_count = this_count;
        prev_frag_offset = frag_offset;
        cur_buf = 1 - cur_buf;
    }

    /* Epilogue: last frag */
    if (low_rank == root_low_rank) {
        int last_buf = 1 - cur_buf;

        if (igather_req != NULL) {
            ompi_request_wait(&igather_req, MPI_STATUS_IGNORE);
        }

        han_free_frag(&han_module->fragment_freelist,
                      inter_frag_item, gather_buf);

        size_t last_ag = prev_frag_count * low_size * up_size;
        up_comm->c_coll->coll_ibcast(frag_reorder[last_buf], last_ag, rdtype,
            root_up_rank, up_comm, &ibcast_req,
            up_comm->c_coll->coll_ibcast_module);
        ompi_request_wait(&ibcast_req, MPI_STATUS_IGNORE);

        han_reorder_frag(rbuf, frag_reorder[last_buf], rdtype, rextent,
                         prev_frag_count, prev_frag_offset, rcount,
                         up_size, low_size, topo);

        for (int b = 0; b < 2; b++) {
            han_free_tiered(&han_module->large_fragment_freelist,
                            &han_module->fragment_freelist,
                            frag_reorder_item[b], frag_reorder[b],
                            frag_reorder_src[b]);
        }
    }

    low_comm->c_coll->coll_bcast(rbuf, rcount * low_size * up_size, rdtype,
                                 root_low_rank, low_comm,
                                 low_comm->c_coll->coll_bcast_module);
    return OMPI_SUCCESS;
}

/**
 * Short implementation of allgather that only does hierarchical
 * communications without tasks.
 */
int
mca_coll_han_allgather_intra_simple(const void *sbuf, size_t scount,
                                    struct ompi_datatype_t *sdtype,
                                    void* rbuf, size_t rcount,
                                    struct ompi_datatype_t *rdtype,
                                    struct ompi_communicator_t *comm,
                                    mca_coll_base_module_t *module){


    /* create the subcommunicators */
    mca_coll_han_module_t *han_module = (mca_coll_han_module_t *)module;

    if( OMPI_SUCCESS != mca_coll_han_comm_create_new(comm, han_module) ) {
        OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                             "han cannot handle allgather within this communicator. Fall back on another component\n"));
        /* HAN cannot work with this communicator so fallback on all collectives */
        HAN_LOAD_FALLBACK_COLLECTIVES(comm, han_module);
        return han_module->previous_allgather(sbuf, scount, sdtype, rbuf, rcount, rdtype,
                                              comm, han_module->previous_allgather_module);
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
        HAN_UNINSTALL_COLL_API(comm, han_module, allgather);
        return han_module->previous_allgather(sbuf, scount, sdtype, rbuf, rcount, rdtype,
                                              comm, han_module->previous_allgather_module);
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

    /* Check if freelist-based optimization is enabled.
     * Only use optimized path when the gathered data per node is large
     * enough that pipeline/mapbycore benefit outweighs setup overhead.
     * With MIN_PIPELINE_FRAGS=4, the threshold ensures at least 4
     * fragments worth of data, so the pipeline has enough stages to
     * overlap igather and ibcast effectively. */
    size_t frag_size = mca_coll_han_component.han_fragment_size;
    ptrdiff_t rextent_check;
    ompi_datatype_type_extent(rdtype, &rextent_check);
    size_t gathered_size = (size_t)rcount * (size_t)low_size * (size_t)rextent_check;
    if (frag_size == 0 || gathered_size <= (HAN_MIN_PIPELINE_FRAGS * frag_size)) {
        /*
         * Simple path: gather to tmp_buf, allgather between leaders,
         * reorder if needed, bcast to all ranks.
         */
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
            tmp_buf = han_scratch_or_malloc(&han_module->scratch_buf[1],
                                            &han_module->scratch_buf_size[1],
                                            rsize, mca_coll_han_component.han_use_persist_buffers);
            if (NULL == tmp_buf) {
                return OMPI_ERR_OUT_OF_RESOURCE;
            }
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
                reorder_buf = han_scratch_or_malloc(&han_module->scratch_buf[0],
                                                     &han_module->scratch_buf_size[0],
                                                     rsize, mca_coll_han_component.han_use_persist_buffers);
                reorder_buf_start = reorder_buf - rgap;
            }

            /* 2a. inter node allgather */
            up_comm->c_coll->coll_allgather(tmp_buf_start, scount*low_size, sdtype,
                                            reorder_buf_start, rcount*low_size, rdtype,
                                            up_comm, up_comm->c_coll->coll_allgather_module);

            if (tmp_buf != NULL && !mca_coll_han_component.han_use_persist_buffers) {
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
                if (!mca_coll_han_component.han_use_persist_buffers) {
                    free(reorder_buf);
                }
                reorder_buf = NULL;
            }

        }

        /* 3. up broadcast: leaders broadcast on their nodes */
        low_comm->c_coll->coll_bcast(rbuf, rcount*low_size*up_size, rdtype,
                                     root_low_rank, low_comm,
                                     low_comm->c_coll->coll_bcast_module);

    } else {
        /*
         * Freelist path: uses pre-allocated buffers and mapbycore/pipeline
         * optimizations for large messages.
         */
        ptrdiff_t rextent;
        size_t frag_count;
        size_t num_frags;
        size_t max_elems;

        ompi_datatype_type_extent(rdtype, &rextent);

        if (MPI_IN_PLACE == sbuf) {
            scount = rcount;
            sdtype = rdtype;
        }

        /* Compute per-fragment element count */
        frag_count = rcount;
        if (frag_size > 0 && rextent > 0) {
            max_elems = frag_size / ((size_t)low_size * (size_t)rextent);
            if (max_elems < 1) max_elems = 1;
            if (max_elems < rcount) frag_count = max_elems;
        }
        num_frags = (rcount + frag_count - 1) / frag_count;

        if (han_module->is_mapbycore) {
            return han_allgather_mapbycore(sbuf, scount, sdtype, rbuf, rcount,
                rdtype, up_comm, low_comm, w_rank, low_rank, up_rank,
                low_size, up_size, root_low_rank);
        } else if (num_frags == 1) {
            return han_allgather_single_frag(sbuf, scount, sdtype, rbuf, rcount,
                rdtype, han_module, up_comm, low_comm, comm,
                w_rank, low_rank, up_rank, low_size, up_size,
                root_low_rank, frag_size, topo);
        } else {
            int rc = han_allgather_pipeline(sbuf, scount, sdtype, rbuf, rcount,
                rdtype, han_module, up_comm, low_comm,
                w_rank, low_rank, low_size, up_size,
                root_low_rank, frag_size, frag_count, num_frags, topo);
            if (OMPI_ERR_NOT_SUPPORTED == rc) {
                return han_allgather_single_frag(sbuf, scount, sdtype, rbuf, rcount,
                    rdtype, han_module, up_comm, low_comm, comm,
                    w_rank, low_rank, up_rank, low_size, up_size,
                    root_low_rank, frag_size, topo);
            }
            return rc;
        }
    }

    return OMPI_SUCCESS;
}