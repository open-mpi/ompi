/*
 * Copyright (c) 2018-2023 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2020      Bull S.A.S. All rights reserved.
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2022      IBM Corporation. All rights reserved
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
 * This files contains all the hierarchical implementations of gather.
 * Only work with regular situation (each node has equal number of processes)
 */

static int mca_coll_han_gather_lg_task(void *task_args);
static int mca_coll_han_gather_ug_task(void *task_args);

/* only work with regular situation (each node has equal number of processes) */

static inline void
mca_coll_han_set_gather_args(mca_coll_han_gather_args_t * args,
                             mca_coll_task_t * cur_task,
                             void *sbuf,
                             void *sbuf_inter_free,
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
                             int w_rank, bool noop, bool is_mapbycore, ompi_request_t * req)
{
    args->cur_task = cur_task;
    args->sbuf = sbuf;
    args->sbuf_inter_free = sbuf_inter_free;
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
    args->is_mapbycore = is_mapbycore;
    args->req = req;
}


/*
 * Main function for taskified gather: calls lg task, a gather on low comm
 */
int
mca_coll_han_gather_intra(const void *sbuf, int scount,
                          struct ompi_datatype_t *sdtype,
                          void *rbuf, int rcount,
                          struct ompi_datatype_t *rdtype,
                          int root,
                          struct ompi_communicator_t *comm,
                          mca_coll_base_module_t * module)
{
    mca_coll_han_module_t *han_module = (mca_coll_han_module_t *) module;
    int w_rank, w_size; /* information about the global communicator */
    int root_low_rank, root_up_rank; /* root ranks for both sub-communicators */
    char *reorder_buf = NULL, *reorder_rbuf = NULL;
    int err, *vranks, low_rank, low_size, *topo;
    ompi_request_t *temp_request = NULL;

    /* Create the subcommunicators */
    err = mca_coll_han_comm_create(comm, han_module);
    if( OMPI_SUCCESS != err ) {
        OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                             "han cannot handle gather with this communicator. Fall back on another component\n"));
        /* HAN cannot work with this communicator so fallback on all collectives */
        HAN_LOAD_FALLBACK_COLLECTIVES(han_module, comm);
        return han_module->previous_gather(sbuf, scount, sdtype, rbuf, rcount, rdtype, root,
                                          comm, han_module->previous_gather_module);
    }

    /* Topo must be initialized to know rank distribution which then is used to
     * determine if han can be used */
    topo = mca_coll_han_topo_init(comm, han_module, 2);
    if (han_module->are_ppn_imbalanced) {
        OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                             "han cannot handle gather with this communicator (imbalance). Fall back on another component\n"));
        /* Put back the fallback collective support and call it once. All
         * future calls will then be automatically redirected.
         */
        HAN_LOAD_FALLBACK_COLLECTIVE(han_module, comm, gather);
        return han_module->previous_gather(sbuf, scount, sdtype, rbuf, rcount, rdtype, root,
                                          comm, han_module->previous_gather_module);
    }

    w_rank = ompi_comm_rank(comm);
    w_size = ompi_comm_size(comm);

    /* Set up request */
    temp_request = OBJ_NEW(ompi_request_t);
    temp_request->req_state = OMPI_REQUEST_ACTIVE;
    temp_request->req_type = OMPI_REQUEST_COLL;
    temp_request->req_free = ompi_coll_han_request_free;
    temp_request->req_status = (ompi_status_public_t){0};
    temp_request->req_complete = REQUEST_PENDING;

    /* create the subcommunicators */
    ompi_communicator_t *low_comm =
        han_module->cached_low_comms[mca_coll_han_component.han_gather_low_module];
    ompi_communicator_t *up_comm =
        han_module->cached_up_comms[mca_coll_han_component.han_gather_up_module];

    /* Get the 'virtual ranks' mapping correspondong to the communicators */
    vranks = han_module->cached_vranks;
    /* information about sub-communicators */
    low_rank = ompi_comm_rank(low_comm);
    low_size = ompi_comm_size(low_comm);
    /* Get root ranks for low and up comms */
    mca_coll_han_get_ranks(vranks, root, low_size, &root_low_rank, &root_up_rank);

    OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                         "[%d]: Han Gather root %d root_low_rank %d root_up_rank %d\n",
                         w_rank, root, root_low_rank, root_up_rank));


    /* Allocate reorder buffers */
    if (w_rank == root) {
        /* if the processes are mapped-by core, no need to reorder:
         * distribution of ranks on core first and node next,
         * in a increasing order for both patterns */
        if (han_module->is_mapbycore) {
            OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                                 "[%d]: Han Gather is_bycore: ", w_rank));
            reorder_rbuf = (char *)rbuf;

        } else {
            /* Need a buffer to store unordered final result */
            ptrdiff_t rsize, rgap;
            rsize = opal_datatype_span(&rdtype->super,
                                       (int64_t)rcount * w_size,
                                       &rgap);
            reorder_buf = (char *)malloc(rsize);        //TODO:free
            /* rgap is the size of unused space at the start of the datatype */
            reorder_rbuf = reorder_buf - rgap;

            if (MPI_IN_PLACE == sbuf) {
                ptrdiff_t rextent;
                ompi_datatype_type_extent(rdtype, &rextent);
                ptrdiff_t block_size = rextent * (ptrdiff_t)rcount;
                ptrdiff_t shift = block_size * w_rank;
                ompi_datatype_copy_content_same_ddt(rdtype,
                                                    (ptrdiff_t)rcount,
                                                    (char *)rbuf + shift,
                                                    reorder_rbuf + shift);
            }
        }
    }


    /* Create lg task */
    mca_coll_task_t *lg = OBJ_NEW(mca_coll_task_t);
    /* Setup lg task arguments */
    mca_coll_han_gather_args_t *lg_args = malloc(sizeof(mca_coll_han_gather_args_t));
    mca_coll_han_set_gather_args(lg_args, lg, (char *) sbuf, NULL, scount, sdtype, reorder_rbuf,
                                 rcount, rdtype, root, root_up_rank, root_low_rank, up_comm,
                                 low_comm, w_rank, low_rank != root_low_rank, han_module->is_mapbycore, temp_request);
    /* Init lg task */
    init_task(lg, mca_coll_han_gather_lg_task, (void *) (lg_args));
    /* Issure lg task */
    issue_task(lg);

    ompi_request_wait(&temp_request, MPI_STATUS_IGNORE);

    /* reorder rbuf based on rank */
    if (w_rank == root && !han_module->is_mapbycore) {
        ompi_coll_han_reorder_gather(reorder_buf,
                                     rbuf, rcount, rdtype,
                                     comm, topo);
        free(reorder_buf);
    }

    return OMPI_SUCCESS;
}

/* Perform a intra node gather and when it ends launch the inter node gather */
int mca_coll_han_gather_lg_task(void *task_args)
{
    mca_coll_han_gather_args_t *t = (mca_coll_han_gather_args_t *) task_args;
    OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output, "[%d] Han Gather:  lg\n",
                         t->w_rank));
    ompi_datatype_t* dtype = (t->w_rank == t->root) ? t->rdtype : t->sdtype;
    size_t count = (t->w_rank == t->root) ? t->rcount : t->scount;

    /* If the process is one of the node leader */
    char *tmp_buf = NULL;
    char *tmp_rbuf = NULL;
    if (!t->noop) {
        /* if the process is one of the node leader, allocate the intermediary
         * buffer to gather on the low sub communicator */
        int low_size = ompi_comm_size(t->low_comm);
        int low_rank = ompi_comm_rank(t->low_comm);
        ptrdiff_t rsize, rgap = 0;
        rsize = opal_datatype_span(&dtype->super,
                                   count * low_size,
                                   &rgap);
        tmp_buf = (char *) malloc(rsize);
        tmp_rbuf = tmp_buf - rgap;
        if (t->w_rank == t->root && MPI_IN_PLACE == t->sbuf) {
            ptrdiff_t rextent;
            ompi_datatype_type_extent(dtype, &rextent);
            ptrdiff_t block_size = rextent * (ptrdiff_t)count;
            ptrdiff_t src_shift = block_size * t->w_rank;
            ptrdiff_t dest_shift = block_size * low_rank;
            ompi_datatype_copy_content_same_ddt(dtype,
                                                (ptrdiff_t)count,
                                                tmp_rbuf + dest_shift,
                                                (char *)t->rbuf + src_shift);
        }
    }

    /* Low level (usually intra-node or shared memory) node gather */
    t->low_comm->c_coll->coll_gather((char *)t->sbuf,
                                     count,
                                     dtype,
                                     tmp_rbuf,
                                     count,
                                     dtype,
                                     t->root_low_rank,
                                     t->low_comm,
                                     t->low_comm->c_coll->coll_gather_module);

    /* Prepare up comm gather */
    t->sbuf = tmp_rbuf;
    t->sbuf_inter_free = tmp_buf;

    /* Create ug (upper level all-gather) task */
    mca_coll_task_t *ug = t->cur_task;
    /* Init ug task */
    init_task(ug, mca_coll_han_gather_ug_task, (void *) t);
    /* Issure ug task */
    issue_task(ug);

    return OMPI_SUCCESS;
}

/* ug: upper level (intra-node) gather task */
int mca_coll_han_gather_ug_task(void *task_args)
{
    mca_coll_han_gather_args_t *t = (mca_coll_han_gather_args_t *) task_args;
    OBJ_RELEASE(t->cur_task);

    if (t->noop) {
        OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                             "[%d] Han Gather:  ug noop\n", t->w_rank));
    } else {
        ompi_datatype_t* dtype = (t->w_rank == t->root) ? t->rdtype : t->sdtype;
        size_t count = (t->w_rank == t->root) ? t->rcount : t->scount;


        int low_size = ompi_comm_size(t->low_comm);
        /* inter node gather */
        t->up_comm->c_coll->coll_gather((char *)t->sbuf,
                                        count*low_size,
                                        dtype,
                                        (char *)t->rbuf,
                                        count*low_size,
                                        dtype,
                                        t->root_up_rank,
                                        t->up_comm,
                                        t->up_comm->c_coll->coll_gather_module);

        if (t->sbuf_inter_free != NULL) {
            free(t->sbuf_inter_free);
            t->sbuf_inter_free = NULL;
        }
        OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                             "[%d] Han Gather:  ug gather finish\n", t->w_rank));
    }
    ompi_request_t *temp_req = t->req;
    free(t);
    ompi_request_complete(temp_req, 1);
    return OMPI_SUCCESS;
}

/* only work with regular situation (each node has equal number of processes) */
int
mca_coll_han_gather_intra_simple(const void *sbuf, int scount,
                                 struct ompi_datatype_t *sdtype,
                                 void *rbuf, int rcount,
                                 struct ompi_datatype_t *rdtype,
                                 int root,
                                 struct ompi_communicator_t *comm,
                                 mca_coll_base_module_t *module)
{
    mca_coll_han_module_t *han_module = (mca_coll_han_module_t *)module;
    int *topo, w_rank = ompi_comm_rank(comm);
    int w_size = ompi_comm_size(comm);

    /* Create the subcommunicators */
    if( OMPI_SUCCESS != mca_coll_han_comm_create_new(comm, han_module) ) {
        OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                             "han cannot handle gather with this communicator. Fall back on another component\n"));
        /* HAN cannot work with this communicator so fallback on all collectives */
        HAN_LOAD_FALLBACK_COLLECTIVES(han_module, comm);
        return han_module->previous_gather(sbuf, scount, sdtype, rbuf, rcount, rdtype, root,
                                          comm, han_module->previous_gather_module);
    }

    /* Topo must be initialized to know rank distribution which then is used to
     * determine if han can be used */
    topo = mca_coll_han_topo_init(comm, han_module, 2);
    if (han_module->are_ppn_imbalanced){
        OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                             "han cannot handle gather with this communicator (imbalance). Fall back on another component\n"));
        /* Put back the fallback collective support and call it once. All
         * future calls will then be automatically redirected.
         */
        HAN_LOAD_FALLBACK_COLLECTIVE(han_module, comm, gather);
        return han_module->previous_gather(sbuf, scount, sdtype, rbuf, rcount, rdtype, root,
                                          comm, han_module->previous_gather_module);
    }

    ompi_communicator_t *low_comm = han_module->sub_comm[INTRA_NODE];
    ompi_communicator_t *up_comm = han_module->sub_comm[INTER_NODE];

    ompi_datatype_t* dtype = (w_rank == root) ? rdtype : sdtype;
    size_t count = (w_rank == root) ? rcount : scount;

    /* Get the 'virtual ranks' mapping corresponding to the communicators */
    int *vranks = han_module->cached_vranks;
    /* information about sub-communicators */
    int low_rank = ompi_comm_rank(low_comm);
    int low_size = ompi_comm_size(low_comm);
    /* Get root ranks for low and up comms */
    int root_low_rank, root_up_rank; /* root ranks for both sub-communicators */
    mca_coll_han_get_ranks(vranks, root, low_size, &root_low_rank, &root_up_rank);

    /* allocate buffer to store unordered result on root
     * if the processes are mapped-by core, no need to reorder:
     * distribution of ranks on core first and node next,
     * in a increasing order for both patterns */
    char *reorder_buf = NULL;  // allocated memory
    char *reorder_buf_start = NULL; // start of the data
    if (w_rank == root) {
        if (MPI_IN_PLACE == sbuf) {
            ptrdiff_t rextent;
            ompi_datatype_type_extent(rdtype, &rextent);
            sbuf = (char*)rbuf + rextent * (ptrdiff_t)rcount * w_rank;
        }
        if (han_module->is_mapbycore) {
            reorder_buf_start = (char *)rbuf;
        } else {
            OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                                 "[%d]: Future Gather needs reordering: ", w_rank));
            ptrdiff_t rgap = 0;
            ptrdiff_t rsize = opal_datatype_span(&rdtype->super,
                                                 (int64_t)rcount * w_size,
                                                 &rgap);
            reorder_buf = (char *)malloc(rsize);
            /* rgap is the size of unused space at the start of the datatype */
            reorder_buf_start = reorder_buf - rgap;
        }

    }

    /* allocate the intermediary buffer
     * to gather on leaders on the low sub communicator */
    char *tmp_buf = NULL; // allocated memory
    char *tmp_buf_start = NULL; // start of the data
    if (low_rank == root_low_rank) {
        ptrdiff_t rsize, rgap = 0;
        rsize = opal_datatype_span(&dtype->super,
                                   count * low_size,
                                   &rgap);
        tmp_buf = (char *) malloc(rsize);
        tmp_buf_start = tmp_buf - rgap;
    }

    /* 1. low gather on nodes leaders */
    low_comm->c_coll->coll_gather((char *)sbuf,
                                  count,
                                  dtype,
                                  tmp_buf_start,
                                  count,
                                  dtype,
                                  root_low_rank,
                                  low_comm,
                                  low_comm->c_coll->coll_gather_module);

    /* 2. upper gather (inter-node) between node leaders */
    if (low_rank == root_low_rank) {
        up_comm->c_coll->coll_gather((char *)tmp_buf_start,
                                     count*low_size,
                                     dtype,
                                     (char *)reorder_buf_start,
                                     count*low_size,
                                     dtype,
                                     root_up_rank,
                                     up_comm,
                                     up_comm->c_coll->coll_gather_module);

        if (tmp_buf != NULL) {
            free(tmp_buf);
            tmp_buf = NULL;
            tmp_buf_start = NULL;
        }
        OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                             "[%d] Future Gather:  ug gather finish\n", w_rank));
    }

    /* 3. reorder data on root into rbuf
     * if ranks are not mapped in topological order, data needs to be reordered
     * (see reorder_gather)
     */
    if (w_rank == root && !han_module->is_mapbycore) {
        ompi_coll_han_reorder_gather(reorder_buf_start,
                                     rbuf, rcount, rdtype,
                                     comm, topo);
        free(reorder_buf);
    }

    return OMPI_SUCCESS;
}

/* Reorder after gather operation, for unordered ranks
 *
 * Suppose, the expected message is 0 1 2 3 4 5 6 7 but the processes are
 * mapped on 2 nodes, for example |0 2 4 6| |1 3 5 7|. The messages from
 * low gather will be 0 2 4 6 and 1 3 5 7.
 * So the upper gather result is 0 2 4 6 1 3 5 7 which must be reordered.
 * The 3rd element (4) must be recopied at the 4th place. In general, the
 * i-th element must be recopied at the place given by the i-th entry of the
 * topology, which is topo[i*topolevel +1]
 */
void
ompi_coll_han_reorder_gather(const void *sbuf,
                             void *rbuf, int count,
                             struct ompi_datatype_t *dtype,
                             struct ompi_communicator_t *comm,
                             int * topo)
{
    int i, topolevel = 2; // always 2 levels in topo
#if OPAL_ENABLE_DEBUG
    int w_rank = ompi_comm_rank(comm);
#endif
    int w_size = ompi_comm_size(comm);
    ptrdiff_t rextent;
    ompi_datatype_type_extent(dtype, &rextent);
    const ptrdiff_t block_size = rextent * (ptrdiff_t)count;
    for ( i = 0; i < w_size; i++ ) {
        OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                             "[%d]: HAN Gather reorder from %d to %d\n",
                             w_rank, i, topo[i * topolevel + 1]));
        ptrdiff_t src_shift = block_size * i;
        ptrdiff_t dest_shift = block_size * (ptrdiff_t)topo[i * topolevel + 1];
        ompi_datatype_copy_content_same_ddt(dtype,
                                            (ptrdiff_t)count,
                                            (char *)rbuf + dest_shift,
                                            (char *)sbuf + src_shift);
    }
}
