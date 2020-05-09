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

#include "coll_han.h"
#include "ompi/mca/coll/base/coll_base_functions.h"
#include "ompi/mca/coll/base/coll_tags.h"
#include "ompi/mca/pml/pml.h"
#include "coll_han_trigger.h"

/* only work with regular situation (each node has equal number of processes) */

void mac_coll_han_set_gather_argu(mca_gather_argu_t * argu,
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
                                  int w_rank, bool noop, ompi_request_t * req)
{
    argu->cur_task = cur_task;
    argu->sbuf = sbuf;
    argu->sbuf_inter_free = sbuf_inter_free;
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
mca_coll_han_gather_intra(const void *sbuf, int scount,
                          struct ompi_datatype_t *sdtype,
                          void *rbuf, int rcount,
                          struct ompi_datatype_t *rdtype,
                          int root,
                          struct ompi_communicator_t *comm,
                          mca_coll_base_module_t * module)
{
    int i;
    int w_rank, w_size; /* information about the global communicator */
    int root_low_rank, root_up_rank; /* root ranks for both sub-communicators */
    char *reorder_buf = NULL, *reorder_rbuf = NULL;
    ptrdiff_t rsize, rgap = 0, rextent;
    int *vranks, low_rank, low_size;
    int * topo;

    ompi_request_t *temp_request = NULL;

    w_rank = ompi_comm_rank(comm);
    w_size = ompi_comm_size(comm);
    /* Create the subcommunicators */
    mca_coll_han_module_t *han_module = (mca_coll_han_module_t *) module;
    /* Topo must be initialized to know rank distribution which then is used to
     * determine if han can be used */
    topo = mca_coll_han_topo_init(comm, han_module, 2);

    if (han_module->are_ppn_imbalanced){
        OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                        "han cannot handle gather with this communicator. It need to fall back on another component\n"));
        return han_module->previous_gather(sbuf, scount, sdtype, rbuf,
                    rcount, rdtype, root,
                    comm, han_module->previous_gather_module);
    }

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

    /* create the subcommunicators */
    mca_coll_han_comm_create(comm, han_module);
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

    ompi_datatype_type_extent(rdtype, &rextent);

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
            rsize = opal_datatype_span(&rdtype->super,
                                       (int64_t)rcount * w_size,
                                       &rgap);
            reorder_buf = (char *)malloc(rsize);        //TODO:free
            /* rgap is the size of unused space at the start of the datatype */
            reorder_rbuf = reorder_buf - rgap;
        }
    }


    /* Create lg task */
    mca_coll_task_t *lg = OBJ_NEW(mca_coll_task_t);
    /* Setup lg task arguments */
    mca_gather_argu_t *lg_argu = malloc(sizeof(mca_gather_argu_t));
    mac_coll_han_set_gather_argu(lg_argu, lg, (char *) sbuf, NULL, scount, sdtype, reorder_rbuf,
                                 rcount, rdtype, root, root_up_rank, root_low_rank, up_comm,
                                 low_comm, w_rank, low_rank != root_low_rank, temp_request);
    /* Init lg task */
    init_task(lg, mca_coll_han_gather_lg_task, (void *) (lg_argu));
    /* Issure lg task */
    issue_task(lg);

    ompi_request_wait(&temp_request, MPI_STATUS_IGNORE);

    /* Suppose, the expected message is 0 1 2 3 4 5 6 7 but the processes are
     * mapped on 2 nodes, for example |0 2 4 6| |1 3 5 7|. The messages from
     * low gather will be 0 2 4 6 and 1 3 5 7.
     * So the upper gather result is 0 2 4 6 1 3 5 7 which must be reordered.
     * The 3rd element (4) must be recopied at the 4th place. In general, the
     * i-th element must be recopied at the place given by the i-th entry of the
     * topology, which is topo[i*topolevel +1]
     */
    /* reorder rbuf based on rank */
    if (w_rank == root && !han_module->is_mapbycore) {
        for (i=0; i<w_size; i++) {
                OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                                "[%d]: Han Gather copy from %d to %d\n",
                                w_rank,
                                i * 2 + 1,
                                topo[i * 2 + 1]));
                ptrdiff_t block_size = rextent * (ptrdiff_t)rcount;
                ptrdiff_t src_shift = block_size * i;
                ptrdiff_t dest_shift = block_size * (ptrdiff_t)topo[i * 2 + 1];
                ompi_datatype_copy_content_same_ddt(rdtype,
                                                    (ptrdiff_t)rcount,
                                                    (char *)rbuf + dest_shift,
                                                    reorder_rbuf + src_shift);
        }
        free(reorder_buf);
    }

    return OMPI_SUCCESS;
}

/* Perform a intra node gather and when it ends launch the inter node gather */
int mca_coll_han_gather_lg_task(void *task_argu)
{
    mca_gather_argu_t *t = (mca_gather_argu_t *) task_argu;
    OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output, "[%d] Han Gather:  lg\n",
                         t->w_rank));
    OBJ_RELEASE(t->cur_task);

    /* If the process is one of the node leader */
    char *tmp_buf = NULL;
    char *tmp_rbuf = NULL;
    if (!t->noop) {
        /* if the process is one of the node leader, allocate the intermediary
         * buffer to gather on the low sub communicator */
        int low_size = ompi_comm_size(t->low_comm);
        ptrdiff_t rsize, rgap = 0;
        rsize = opal_datatype_span(&t->rdtype->super,
                    (int64_t)t->rcount * low_size,
                    &rgap);
        tmp_buf = (char *) malloc(rsize);
        tmp_rbuf = tmp_buf - rgap;
    }

    /* shared memory node gather */
    t->low_comm->c_coll->coll_gather((char *)t->sbuf,
                     t->scount,
                     t->sdtype,
                     tmp_rbuf,
                     t->rcount,
                     t->rdtype,
                     t->root_low_rank,
                     t->low_comm,
                     t->low_comm->c_coll->coll_gather_module);

    /* Prepare up comm gather */
    t->sbuf = tmp_rbuf;
    t->sbuf_inter_free = tmp_buf;

    /* Create ug (upper level all-gather) task */
    mca_coll_task_t *ug = OBJ_NEW(mca_coll_task_t);
    /* Setup ug task arguments */
    t->cur_task = ug;
    /* Init ug task */
    init_task(ug, mca_coll_han_gather_ug_task, (void *) t);
    /* Issure ug task */
    issue_task(ug);

    return OMPI_SUCCESS;
}

/* ug: upper level (intra-node) gather task */
int mca_coll_han_gather_ug_task(void *task_argu)
{
    mca_gather_argu_t *t = (mca_gather_argu_t *) task_argu;
    OBJ_RELEASE(t->cur_task);

    if (t->noop) {
        OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                             "[%d] Han Gather:  ug noop\n", t->w_rank));
    } else {
        int low_size = ompi_comm_size(t->low_comm);
        /* inter node gather */
        t->up_comm->c_coll->coll_gather((char *)t->sbuf,
                    t->scount*low_size,
                    t->sdtype,
                    (char *)t->rbuf,
                    t->rcount*low_size,
                    t->rdtype,
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
    int w_rank = ompi_comm_rank(comm);
    int w_size = ompi_comm_size(comm);

    mca_coll_han_module_t *han_module = (mca_coll_han_module_t *)module;
    /* Topo must be initialized to know rank distribution which then is used to
     * determine if han can be used */
    int *topo = mca_coll_han_topo_init(comm, han_module, 2);

    /* Here root needs to reach all nodes on up_comm.
     * But in case of unbalance some up_comms are smaller,
     * as the comm_split is made on the base of low_rank */
    if (han_module->are_ppn_imbalanced){
        OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                        "han cannot handle gather with this communicator. It need to fall back on another component\n"));
        return han_module->previous_gather(sbuf, scount, sdtype, rbuf,
                    rcount, rdtype, root,
                    comm, han_module->previous_gather_module);
    }

    /* create the subcommunicators */
    mca_coll_han_comm_create_new(comm, han_module);
    ompi_communicator_t *low_comm = han_module->sub_comm[INTRA_NODE];
    ompi_communicator_t *up_comm = han_module->sub_comm[INTER_NODE];

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
 *      * to gather on leaders on the low sub communicator */
    char *tmp_buf = NULL; // allocated memory
    char *tmp_buf_start = NULL; // start of the data
    if (low_rank == root_low_rank) {
       ptrdiff_t rsize, rgap = 0;
        rsize = opal_datatype_span(&rdtype->super,
                    (int64_t)rcount * low_size,
                    &rgap);
        tmp_buf = (char *) malloc(rsize);
        tmp_buf_start = tmp_buf - rgap;
    }

    /* 1. low gather on nodes leaders */
    low_comm->c_coll->coll_gather((char *)sbuf,
                     scount,
                     sdtype,
                     tmp_buf_start,
                     rcount,
                     rdtype,
                     root_low_rank,
                     low_comm,
                     low_comm->c_coll->coll_gather_module);

    /* 2. upper gather (inter-node) between node leaders */
    if (low_rank == root_low_rank) {
        up_comm->c_coll->coll_gather((char *)tmp_buf_start,
                    scount*low_size,
                    sdtype,
                    (char *)reorder_buf_start,
                    rcount*low_size,
                    rdtype,
                    root_up_rank,
                    up_comm,
                    up_comm->c_coll->coll_gather_module);

        if (tmp_buf != NULL) {
            free(tmp_buf);
            tmp_buf = NULL;
            tmp_buf_start = NULL;
        }
        OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                        "[%d] Future Gather:  ug gather finish\n", t->w_rank));
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
                   void *rbuf, int rcount,
                   struct ompi_datatype_t *rdtype,
                   struct ompi_communicator_t *comm,
                   int * topo) {
   int i;
   int topolevel = 2; // always 2 levels in topo
   int w_rank = ompi_comm_rank(comm);
   int w_size = ompi_comm_size(comm);
   ptrdiff_t rextent;
   ompi_datatype_type_extent(rdtype, &rextent);
   for (i=0; i<w_size; i++) {
           OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                           "[%d]: Future reorder from %d to %d\n",
                           w_rank,
                           i * topolevel + 1,
                           topo[i * topolevel + 1]));
           ptrdiff_t block_size = rextent * (ptrdiff_t)rcount;
           ptrdiff_t src_shift = block_size * i;
           ptrdiff_t dest_shift = block_size * (ptrdiff_t)topo[i * topolevel + 1];
           ompi_datatype_copy_content_same_ddt(rdtype,
                                               (ptrdiff_t)rcount,
                                               (char *)rbuf + dest_shift,
                                               (char *)sbuf + src_shift);
   }
}
