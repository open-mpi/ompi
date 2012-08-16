/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_BCOL_IBOFFLOAD_COLLREQ_H
#define MCA_BCOL_IBOFFLOAD_COLLREQ_H

#include "ompi_config.h"

#include <infiniband/mqe.h>
#include <infiniband/verbs.h>
#include <infiniband/mverbs.h>

#include "ompi/class/ompi_free_list.h"

#include "bcol_iboffload.h"
#include "bcol_iboffload_device.h"
#include "bcol_iboffload_collfrag.h"

#define SBUF 0
#define RBUF 1

#define BCOL_IBOFFLOAD_BUFFERS 2

BEGIN_C_DECLS

struct mca_bcol_iboffload_reg_t;

/*
 * collective progress function
 */
typedef int (*collective_message_progress_function)(
        struct mca_bcol_iboffload_module_t *iboffload,
        struct mca_bcol_iboffload_collreq_t *full_message_descriptor);
/*
 * callback function to be called after the collective work request
 * completes.  This is invoked in user-space, and is typically where
 * data may be copied out of library buffers, or when any other user-
 * level protocol may be completed
 *
 * input:
 * callback data: typically, this may be the work request just finished
 */
typedef int (*collective_message_completion_callback_function)(
        void *callback_data);

struct mca_bcol_iboffload_buff_info {
    void *buf;
    size_t offset;
    uint32_t lkey;
    struct mca_bcol_iboffload_reg_t *iboffload_reg;
};
typedef struct mca_bcol_iboffload_buff_info mca_bcol_iboffload_buff_info;

/*
 * Collective message descriptor
 * the mca_bcol_iboffload_message_desc_t was replaced with mca_bcol_iboffload_collreq_t
 * *************************************************************************************************
 *
 * Brief  description of iboffload collective request dependencies:
 *
 * mca_bcol_iboffload_collreq_t                      <----<< Full coll request
 *          |
 *          --(0)-- mca_bcol_iboffload_collfrag_t    <----<< Fragment of coll request ( for example
 *          |                   |                            10MB Bcast maybe split to 2MB fragments )
 *          |                   |
 *          |                   --(0)-- mca_bcol_iboffload_task_t---mqe_task
 *          |                   |                    |
 *          |                   |                     ---mca_bcol_iboffload_frag_t---ibv_sge
 *          |                   --(1)-- mca_bcol_iboffload_task_t---mqe_task
 *          |                   |                    |
 *          |                   |                     ---mca_bcol_iboffload_frag_t---ibv_sge
 *          |                   ..(M)..
 *          |
 *          --(1)-- mca_bcol_iboffload_collfrag_t
 *          |
 *          ..(N)..
 *
 * *************************************************************************************************
 */

struct mca_bcol_iboffload_collreq_t {
    ompi_request_t super;

    /* op type */
    struct ompi_op_t *op;

    /* Sometimes the operation that should be performed
       by the IB is different than the mpi_op and is then set
       by the pack_data_for_calc function */
    enum ibv_m_wr_calc_op actual_ib_op;

    /* Sometimes the data type that should be used by the IB
       to peroform the calc s different than the mpi dtype,
       and is then set by the pack_data_for_calc function */
    enum ibv_m_wr_data_type actual_ib_dtype;

    /* data type */
    struct ompi_datatype_t *dtype;

    /* convertor for send operation */
    opal_convertor_t send_conv;

    /* convertor for recv operation */
    opal_convertor_t recv_conv;

    /*
     * count (in data type units)
     */
    uint64_t count;

    /*
     * root of collective operation
     */
    int root;

    /* number of message fragments */
    int n_fragments;

    /* number of fragments sent - all resrouces for a fragment are allocated
     * or none at all are
     */
    int n_frags_sent;

    /* number of fragments completed from the MPI perspective */
    int n_frag_mpi_complete;

    /* number of fragments completed from a network perspective */
    int n_frag_net_complete;

    /* collective free and may be released  - message complete from the
     ** MPI perspective, the network prespective, and the user is done
     ** with the message handle */
    volatile bool user_handle_freed;

    /* list of collective fragements - only 1 for now */
    opal_list_t work_requests;

    /* message progress function */
    collective_message_progress_function progress_fn;

    /* work request completion callback function */
    collective_message_completion_callback_function completion_cb_fn;

    /* index of qp with enough length of buffs for this collective */
    int qp_index;

    bool if_bcol_last;

    /* The flag is used for the last bcol to indicate if the calculation should be done by the cpu */
    bool do_calc_in_cpu;

    /* in Allreduce case, if (true == do_calc_in_cpu) =>
       the final result will be calc on local CPU */
    uint64_t l_operand;
    uint64_t r_operand;

    /* caching ML-rdma buffer descriptor */
    mca_bcol_iboffload_rdma_buffer_desc_t *ml_rdma_desc;

    /* ML buffer index code */
    int ml_buffer_index;

    /* In the current implementation the collrequest connected to 1 single
       iboffload module */
    struct mca_bcol_iboffload_module_t *module;

    mca_bcol_iboffload_collfrag_t first_collfrag;

    /* Send/recv buffs info - user buffers registration if needed etc. */
    mca_bcol_iboffload_buff_info buffer_info[BCOL_IBOFFLOAD_BUFFERS];

    /* My bi nominal tree children in this collective */
    int *bi_nominal_tree_children;

    /* Convertors for send/recv if needed */
    opal_convertor_t send_convertor;
    opal_convertor_t recv_convertor;

    /* Order info from upper layer */
    mca_bcol_base_order_info_t *order_info;
};
typedef struct mca_bcol_iboffload_collreq_t mca_bcol_iboffload_collreq_t;
OBJ_CLASS_DECLARATION(mca_bcol_iboffload_collreq_t);

#define COLLREQ_IS_DONE(cr) (cr->user_handle_freed &&   \
        (cr->n_frag_mpi_complete == cr->n_fragments) && \
        (cr->n_frag_net_complete == cr->n_fragments))

#define RELEASE_COLLREQ(cr)                                            \
do {                                                                   \
    (cr)->user_handle_freed = false;                                   \
    OMPI_FREE_LIST_RETURN(&mca_bcol_iboffload_component.collreqs_free, \
        (ompi_free_list_item_t *) (cr));                               \
} while (0)

static inline __opal_attribute_always_inline__
            int mca_bcol_iboffload_free_resources_and_move_to_pending(
                     mca_bcol_iboffload_collfrag_t *coll_fragment,
                     mca_bcol_iboffload_module_t *iboffload)
{
    int rc = mca_bcol_iboffload_free_tasks_frags_resources(coll_fragment,
                iboffload->device->frags_free);

    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        return rc;
    }

    IBOFFLOAD_VERBOSE(10, ("iboffload - %p, coll_fragment - %p, "
                          "coll frag in_pending_list ? - %d, pending_list size - %d.\n",
                           iboffload, coll_fragment, coll_fragment->in_pending_list,
                           opal_list_get_size(&iboffload->collfrag_pending)));

    BCOL_IBOFFLOAD_MQ_RETURN_CREDITS(iboffload, coll_fragment->mq_index, coll_fragment->mq_credits);

    /* Remove coll frag from coll request opal_list */
    opal_list_remove_item(&coll_fragment->coll_full_req->work_requests,
                          (opal_list_item_t *) coll_fragment);

    if (false == coll_fragment->in_pending_list) {
        /* Put the collfrag on pending list */
        coll_fragment->in_pending_list = true;
        opal_list_append(&iboffload->collfrag_pending,
                            (opal_list_item_t *) coll_fragment);
    } else {
        /* The item is already on pending list =>
           insert it first that not break order
           between frags on the list */
        opal_list_prepend(&iboffload->collfrag_pending,
                         (opal_list_item_t *) coll_fragment);
    }

    return OMPI_SUCCESS;
}

/* Forward declaration */
struct mca_bcol_iboffload_reg_t;
static inline __opal_attribute_always_inline__
      int mca_bcol_iboffload_prepare_buffer(
            void *buffer,
            size_t size,
            struct mca_bcol_iboffload_reg_t **registration_handler,
            mca_bcol_iboffload_module_t *iboffload)
{
    int rc;
    mca_mpool_base_registration_t *reg = NULL;

    assert(size > 0);
    rc = iboffload->device->mpool->mpool_register(
                            iboffload->device->mpool,
                            buffer, size,
                            (uint32_t) 0 /* flags */,
                            &reg);

    *registration_handler =
        (struct mca_bcol_iboffload_reg_t *) reg;

    return rc;
}

int mca_bcol_iboffload_coll_req_implement(
                            mca_bcol_iboffload_module_t *iboffload,
                            mca_bcol_iboffload_collreq_t *coll_request);

END_C_DECLS

#endif
