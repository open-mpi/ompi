/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_BCOL_IBOFFLOAD_TASK_H
#define MCA_BCOL_IBOFFLOAD_TASK_H

#include "ompi_config.h"

#include <infiniband/verbs.h>
#include <infiniband/mverbs.h>
#include <infiniband/mqe.h>

#include "bcol_iboffload.h"
#include "bcol_iboffload_frag.h"
#include "bcol_iboffload_collreq.h"
#include "bcol_iboffload_endpoint.h"
#include "bcol_iboffload_collfrag.h"

#define SENDWR(task)  ((task)->element.post.send_wr)

BEGIN_C_DECLS

/* the mca_bcol_ibv_mwr_task_t name was replaced with mca_bcol_iboffload_task_t */
struct mca_bcol_iboffload_task_t {
    ompi_free_list_item_t super;

    /* pointer to the memory descriptor associated with the task */
    mca_bcol_iboffload_frag_t *frag;

    /* pointer to the bcol descriptor,
     * we need it for send task only becasue we complete them in async maner
     */
    mca_bcol_iboffload_collfrag_t *collfrag;

    /* task to be posted */
    struct mqe_task element;

    /* allocate ibv_sge structs array - in a CALC case
     * for example it will have two entries.
     */
    struct ibv_sge *sg_entries;

    /* sg_entries array length */
    int sg_entries_num;

    /* Each task is a member of some free list,
       if the pointer is NULL => we assume the task
       is a member of the common task list (tasks_free) */
    ompi_free_list_t *task_list;

    /* Pointer to the next task */
    struct mca_bcol_iboffload_task_t *next_task;

    /* pasha - it is crappy work around for driver interface
     * the send_wr and recv_wr should be part of mqe_task and not pointers !
     */
    union {
        struct ibv_m_send_wr  send_wr;
        struct ibv_recv_wr    recv_wr;
    } wr;

    /* If we'll decide to post a task to a different qp */
    struct mqe_qp_entry task_mqe_qp_entry;

    /* Pointer to endpoint for this task */
    mca_bcol_iboffload_endpoint_t *endpoint;
};
typedef struct mca_bcol_iboffload_task_t mca_bcol_iboffload_task_t;
OBJ_CLASS_DECLARATION(mca_bcol_iboffload_task_t);


/* calc_tasks_free free list init function */
void
mca_bcol_iboffload_calc_task_init(ompi_free_list_item_t* item, void* ctx);

/* iovec_tasks_free free list init function */
void
mca_bcol_iboffload_iovec_task_init(ompi_free_list_item_t* item, void* ctx);

static inline __opal_attribute_always_inline__ void
        mca_bcol_iboffload_return_frag_tolist(
                        mca_bcol_iboffload_frag_t *frag,
                        ompi_free_list_t *list)
{
    if (NULL != frag) {
        mca_bcol_iboffload_component_t *cm = &mca_bcol_iboffload_component;
        assert(MCA_BCOL_IBOFFLOAD_NONE_OWNER != frag->type);

        if (MCA_BCOL_IBOFFLOAD_DUMMY_OWNER != frag->type &&
                                      0 == frag->ref_counter) {
            if (MCA_BCOL_IBOFFLOAD_BCOL_OWNER == frag->type) {
                OMPI_FREE_LIST_RETURN((&(list[frag->qp_index])),
                        (ompi_free_list_item_t*) frag);
            } else if (MCA_BCOL_IBOFFLOAD_ML_OWNER == frag->type) {
                OMPI_FREE_LIST_RETURN((&(cm->ml_frags_free)),
                        (ompi_free_list_item_t*) frag);
            }
        }
    }
}

static inline __opal_attribute_always_inline__ void
        mca_bcol_iboffload_return_recv_frags_toendpoint(
                        mca_bcol_iboffload_frag_t *frags,
                        mca_bcol_iboffload_endpoint_t *ep,
                        int qp_index)
{
    mca_bcol_iboffload_frag_t *recv_frag = frags;
    mca_bcol_iboffload_component_t *cm = &mca_bcol_iboffload_component;

    while (NULL != recv_frag) {
        assert(MCA_BCOL_IBOFFLOAD_NONE_OWNER != recv_frag->type);
        if (MCA_BCOL_IBOFFLOAD_ML_OWNER != recv_frag->type) {
            opal_list_prepend(&ep->qps[qp_index].preposted_frags,
                            (opal_list_item_t *) recv_frag);
        } else {
            OMPI_FREE_LIST_RETURN((&(cm->ml_frags_free)),
                (ompi_free_list_item_t*) recv_frag);
        }

        recv_frag = recv_frag->next;
    }
}

/* Wait task allocation and initialization */
static inline __opal_attribute_always_inline__ mca_bcol_iboffload_task_t*
        mca_bcol_iboffload_get_wait_task(mca_bcol_iboffload_module_t *iboffload,
                                         uint32_t source, int num_waits,
                                         mca_bcol_iboffload_frag_t *frags,
                                         int qp_index, struct ibv_qp *qp)
{
    int rc;
    ompi_free_list_item_t *item;
    mca_bcol_iboffload_task_t *task;

    mca_bcol_iboffload_component_t *cm = &mca_bcol_iboffload_component;
    mca_bcol_iboffload_endpoint_t *endpoint = iboffload->endpoints[source];

    /* blocking allocation for send fragment */
    OMPI_FREE_LIST_GET(&cm->tasks_free, item, rc);
    if (OPAL_UNLIKELY(NULL == item)) {
        mca_bcol_iboffload_return_recv_frags_toendpoint(frags, endpoint, qp_index);
        return NULL;
    }

    task = (mca_bcol_iboffload_task_t *) item;
    /* set pointer to corresponding recv fragment */
    IBOFFLOAD_SET_FRAGS_ON_TASK(frags, task);

    task->next_task = NULL;
    task->endpoint = endpoint;

    /* set opcode */
    task->element.opcode = MQE_WR_CQE_WAIT;
    task->element.flags = 0; /* Here maybe ANY flag, anyway driver ignore it */
    /* set task id */
    task->element.wr_id = (uint64_t) (uintptr_t) task;
    /* set CQ */
    task->element.wait.cq = endpoint->qp_config.init_attr[qp_index].recv_cq;

    /* set number of tasks to task */
    task->element.wait.count = num_waits;
    /* set pointer to QP */

    if (NULL == qp) { /* NULL means use MQ's QP */
        task->element.wait.mqe_qp = NULL;
    } else { /* Post wait to the SQ of this QP */
        task->task_mqe_qp_entry.next = NULL;
        task->task_mqe_qp_entry.qp = qp;

        task->element.wait.mqe_qp = &task->task_mqe_qp_entry;
    }

    IBOFFLOAD_VERBOSE(10, ("Allocating task %p, cq: %p, num waits: %d, qp_index - %d, "
                           "destination %d for comm rank: %d.\n",
                           (void *) task, (void *) task->element.wait.cq,
                            task->element.wait.count, qp_index, source,
                            endpoint->iboffload_module->ibnet->super.group_list[endpoint->index]));
    return task;
}

static inline __opal_attribute_always_inline__ mca_bcol_iboffload_task_t*
mca_bcol_iboffload_prepare_send_task(
        mca_bcol_iboffload_module_t *iboffload,
        mca_bcol_iboffload_endpoint_t *endpoint,
        int qp_index, ompi_free_list_t *task_list,
        mca_bcol_iboffload_collfrag_t *collfrag)
{
    int rc;

    ompi_free_list_item_t *item;
    mca_bcol_iboffload_task_t *task;

    IBOFFLOAD_VERBOSE(10, ("Destination rank - %d, QP index - %d, "
                           "for comm rank - %d\n", endpoint->index, qp_index,
                            endpoint->iboffload_module->ibnet->super.group_list[endpoint->index]));

    /* get item from free list */
    OMPI_FREE_LIST_GET(task_list, item, rc);
    if (OPAL_UNLIKELY(NULL == item)) {
        return NULL;
    }

    task = (mca_bcol_iboffload_task_t*) item;
    task->endpoint = endpoint;

    ++(collfrag->n_sends);
    task->collfrag = collfrag;

    task->next_task = NULL;
    task->element.wr_id = (uint64_t) (uintptr_t) task;

    task->element.post.qp = endpoint->qps[qp_index].qp->lcl_qp;

    task->element.opcode = MQE_WR_SEND;

    /* define send work request */
    SENDWR(task) = &(task->wr.send_wr);

    SENDWR(task)->next = NULL;

    SENDWR(task)->wr_id = (uint64_t) (uintptr_t) collfrag;
    IBOFFLOAD_VERBOSE(10, ("coll_frag - %p.\n", collfrag));

    /* Allways send IMM on sends ! */
    task->element.flags  = MQE_WR_FLAG_IMM_EXE;

    /* Always signal completion */
    SENDWR(task)->send_flags = IBV_SEND_SIGNALED;

    return task;
}

static inline __opal_attribute_always_inline__ mca_bcol_iboffload_task_t*
mca_bcol_iboffload_get_send_task(
        mca_bcol_iboffload_module_t *iboffload,
        uint32_t destination, int qp_index,
        mca_bcol_iboffload_frag_t *frag,
        mca_bcol_iboffload_collfrag_t *collfrag,
        bool enable_inline)
{
    mca_bcol_iboffload_task_t *task;

    mca_bcol_iboffload_component_t *cm = &mca_bcol_iboffload_component;
    mca_bcol_iboffload_endpoint_t *endpoint = iboffload->endpoints[destination];

    IBOFFLOAD_VERBOSE(10, ("mca_bcol_iboffload_get_send_task qp_index %d\n",
                qp_index));

    task = mca_bcol_iboffload_prepare_send_task(iboffload, endpoint, qp_index,
                                                &cm->tasks_free,
                                                collfrag);

    if (OPAL_UNLIKELY(NULL == task)) {
        mca_bcol_iboffload_return_frag_tolist(frag, iboffload->device->frags_free);
        return NULL;
    }

    /* no support for multiple frags */
    IBOFFLOAD_SET_SINGLE_FRAG_ON_TASK(frag, task);

    /* We can not do send with 0 byte but we can do zero byte RDMA with immidiate */
    if (0 == frag->sg_entry.length) {
        SENDWR(task)->imm_data = 0;
        SENDWR(task)->opcode = IBV_WR_RDMA_WRITE_WITH_IMM;

        SENDWR(task)->wr.rdma.rkey = endpoint->remote_zero_rdma_addr.rkey;
        SENDWR(task)->wr.rdma.remote_addr = endpoint->remote_zero_rdma_addr.addr;
    } else {
        SENDWR(task)->opcode = IBV_WR_SEND;
    }

    /* single sge */
    SENDWR(task)->num_sge = 1;
    SENDWR(task)->sg_list = &(frag->sg_entry);

    /* Use inline send when it is possible */
    if (enable_inline &&
            frag->sg_entry.length < cm->max_inline_data) {
        IBOFFLOAD_VERBOSE(10, ("Setting inline for len %d\n", frag->sg_entry.length));
        SENDWR(task)->send_flags |= IBV_SEND_INLINE;
    }

    return task;
}

static inline __opal_attribute_always_inline__ mca_bcol_iboffload_task_t*
mca_bcol_iboffload_get_send_vec_task(
        mca_bcol_iboffload_module_t *iboffload,
        uint32_t destination, int qp_index,
        size_t nitems,
        struct iovec *buff_iovec,
        uint32_t lkey,
        mca_bcol_iboffload_frag_t *frag,
        mca_bcol_iboffload_collfrag_t *collfrag,
        bool enable_inline)
{
    mca_bcol_iboffload_task_t *task;
    int i;

    mca_bcol_iboffload_component_t *cm = &mca_bcol_iboffload_component;
    mca_bcol_iboffload_endpoint_t *endpoint = iboffload->endpoints[destination];

    IBOFFLOAD_VERBOSE(10, ("mca_bcol_iboffload_get_send_task qp_index %d\n",
                qp_index));

    task = mca_bcol_iboffload_prepare_send_task(iboffload, endpoint, qp_index,
                                                &iboffload->iovec_tasks_free,
                                                collfrag);

    if (OPAL_UNLIKELY(NULL == task)) {
        mca_bcol_iboffload_return_frag_tolist(frag, iboffload->device->frags_free);
        return NULL;
    }

    /* no support for multiple frags */
    IBOFFLOAD_SET_SINGLE_FRAG_ON_TASK(frag, task);

    /* We can not do send with 0 byte but we can do zero byte RDMA with immidiate */
    SENDWR(task)->opcode = IBV_WR_SEND;

    assert (task->sg_entries != NULL);

    for (i = 0; (size_t) i < nitems; ++i){
        task->sg_entries[i].length = buff_iovec[i].iov_len;
        task->sg_entries[i].addr = (uint64_t) buff_iovec[i].iov_base;
        task->sg_entries[i].lkey = lkey;
    }

    /* multiple sge */
    SENDWR(task)->num_sge = nitems;
    SENDWR(task)->sg_list = (task->sg_entries);

   /* Use inline send when it is possible */
    if (enable_inline &&
            frag->sg_entry.length < cm->max_inline_data) {
        IBOFFLOAD_VERBOSE(10, ("Setting inline for len %d\n", frag->sg_entry.length));
        SENDWR(task)->send_flags |= IBV_SEND_INLINE;
    }

    return task;
}
static inline __opal_attribute_always_inline__ mca_bcol_iboffload_task_t*
    mca_bcol_iboffload_get_rdma_vec_task(
        uint32_t destination, size_t offset, size_t nitems,
        mca_bcol_iboffload_frag_t *frag,
        mca_bcol_iboffload_module_t *iboffload,
        struct iovec *buff_iovec, uint32_t lkey,
        mca_bcol_iboffload_collfrag_t *collfrag)
{
    int i;
    mca_bcol_iboffload_collreq_t *coll_request = collfrag->coll_full_req;

    mca_bcol_iboffload_task_t *task;
    mca_bcol_iboffload_endpoint_t *endpoint =
                            iboffload->endpoints[destination];

    task = mca_bcol_iboffload_prepare_send_task(iboffload, endpoint,
                                                coll_request->qp_index,
                                                &iboffload->iovec_tasks_free,
                                                collfrag);
    if (OPAL_UNLIKELY(NULL == task)) {
        mca_bcol_iboffload_return_frag_tolist(frag, iboffload->device->frags_free);
        return NULL;
    }

    /* no support for multiple frags */
    IBOFFLOAD_SET_SINGLE_FRAG_ON_TASK(frag, task);

    SENDWR(task)->imm_data = 0;
    SENDWR(task)->opcode = IBV_WR_RDMA_WRITE_WITH_IMM;
    SENDWR(task)->wr.rdma.rkey = endpoint->remote_rdma_block.ib_info.rkey;

    SENDWR(task)->wr.rdma.remote_addr = (uint64_t) (uintptr_t)
       ((unsigned char *) endpoint->remote_rdma_block.rdma_desc[coll_request->ml_buffer_index].data_addr + offset);

    for (i = 0; (size_t) i < nitems; ++i){
        task->sg_entries[i].length = buff_iovec[i].iov_len;
        task->sg_entries[i].addr = (uint64_t) buff_iovec[i].iov_base;
        task->sg_entries[i].lkey = lkey;
    }

    /* single sge */
    SENDWR(task)->num_sge = nitems;
    SENDWR(task)->sg_list = (task->sg_entries);

    IBOFFLOAD_VERBOSE(10, ("The remote offset %ld \n", offset));
    return task;
}

static inline __opal_attribute_always_inline__ mca_bcol_iboffload_task_t*
    mca_bcol_iboffload_get_rdma_task(
        uint32_t destination, size_t offset,
        mca_bcol_iboffload_frag_t *frag,
        mca_bcol_iboffload_module_t *iboffload,
        mca_bcol_iboffload_collfrag_t *collfrag)
{
    mca_bcol_iboffload_collreq_t *coll_request = collfrag->coll_full_req;

    mca_bcol_iboffload_task_t *task;
    mca_bcol_iboffload_endpoint_t *endpoint =
                            iboffload->endpoints[destination];

    mca_bcol_iboffload_component_t *cm = &mca_bcol_iboffload_component;
    task = mca_bcol_iboffload_prepare_send_task(iboffload, endpoint,
                                                coll_request->qp_index,
                                                &cm->tasks_free, collfrag);
    if (OPAL_UNLIKELY(NULL == task)) {
        mca_bcol_iboffload_return_frag_tolist(frag, iboffload->device->frags_free);
        return NULL;
    }

    /* no support for multiple frags */
    IBOFFLOAD_SET_SINGLE_FRAG_ON_TASK(frag, task);

    SENDWR(task)->imm_data = 0;
    SENDWR(task)->opcode = IBV_WR_RDMA_WRITE_WITH_IMM;
    SENDWR(task)->wr.rdma.rkey = endpoint->remote_rdma_block.ib_info.rkey;
    /* Pasha: I really not happy with the way we calculate remote addresses.
       why we don't use rbuf + offset ?*/
    SENDWR(task)->wr.rdma.remote_addr = (uint64_t) (uintptr_t)
       ((unsigned char *) endpoint->remote_rdma_block.rdma_desc[coll_request->ml_buffer_index].data_addr + offset);
    /* single sge */
    SENDWR(task)->num_sge = 1;
    SENDWR(task)->sg_list = &(frag->sg_entry);

    IBOFFLOAD_VERBOSE(10, ("The remote offset %ld \n", offset));
    return task;
}

/* Pasha: hacking version of calc operation */
    static inline __opal_attribute_always_inline__ mca_bcol_iboffload_task_t*
mca_bcol_iboffload_get_calc_task(mca_bcol_iboffload_module_t *iboffload,
        uint32_t destination, int qp_index, mca_bcol_iboffload_frag_t *frag,
        struct ibv_sge *l_operand, struct ibv_sge *r_operand,
        mca_bcol_iboffload_collreq_t *coll_request,
        bool enable_inline)
/* Some specifications for this function:
 *  1) We assume that the len of two operands (ibv_sge structs) is a same.
 *  2) Possibly we use the results (ibv_sge structs) from previous
 *     calc operations => maybe the frag pointer is NULL.
 */
{
    mca_bcol_iboffload_task_t *task;
    mca_bcol_iboffload_endpoint_t *endpoint =
                                     iboffload->endpoints[destination];

    mca_bcol_iboffload_collfrag_t *collfrag =
                                    (mca_bcol_iboffload_collfrag_t *)
                                     opal_list_get_last(&coll_request->work_requests);

    mca_bcol_iboffload_component_t *cm = &mca_bcol_iboffload_component;
    task = mca_bcol_iboffload_prepare_send_task(iboffload, endpoint, qp_index,
                                                &cm->calc_tasks_free, collfrag);
    if (OPAL_UNLIKELY(NULL == task)) {
        mca_bcol_iboffload_return_frag_tolist(frag, iboffload->device->frags_free);
        return NULL;
    }

    if (NULL != frag) {
        IBOFFLOAD_SET_SINGLE_FRAG_ON_TASK(frag, task);
    } else {
        task->frag = NULL;
    }

    task->sg_entries[0] = *l_operand;
    task->sg_entries[1] = *r_operand;

    SENDWR(task)->num_sge = 2;
    SENDWR(task)->sg_list = task->sg_entries;

    SENDWR(task)->opcode = MCA_BCOL_IBOFFLOAD_SEND_CALC;
#if OMPI_HAVE_IBOFFLOAD_CALC_RDMA
    SENDWR(task)->wr.calc_send.data_type = coll_request->actual_ib_dtype;
    SENDWR(task)->wr.calc_send.calc_op = coll_request->actual_ib_op;
#else
    SENDWR(task)->wr.calc.data_type = coll_request->actual_ib_dtype;
    SENDWR(task)->wr.calc.calc_op = coll_request->actual_ib_op;
#endif

    return task;
}

static inline __opal_attribute_always_inline__ mca_bcol_iboffload_task_t*
    mca_bcol_iboffload_get_rdma_calc_task(mca_bcol_iboffload_module_t *iboffload,
        uint32_t destination, int qp_index, mca_bcol_iboffload_frag_t *frag,
        struct ibv_sge *l_operand, struct ibv_sge *r_operand,
        mca_bcol_iboffload_collreq_t *coll_request,
        size_t offset)
/* Some specifications for this function:
 *  1) We assume that the len of two operands (ibv_sge structs) is a same.
 *  2) Possibly we use the results (ibv_sge structs) from previous
 *     calc operations => maybe the frag pointer is NULL.
 */
{
    mca_bcol_iboffload_task_t *task;
    mca_bcol_iboffload_endpoint_t *endpoint =
                                     iboffload->endpoints[destination];

    mca_bcol_iboffload_collfrag_t *collfrag =
                                    (mca_bcol_iboffload_collfrag_t *)
                                     opal_list_get_last(&coll_request->work_requests);

    mca_bcol_iboffload_component_t *cm = &mca_bcol_iboffload_component;
    task = mca_bcol_iboffload_prepare_send_task(iboffload, endpoint, qp_index,
                                                &cm->calc_tasks_free, collfrag);
    if (OPAL_UNLIKELY(NULL == task)) {
        mca_bcol_iboffload_return_frag_tolist(frag, iboffload->device->frags_free);
        return NULL;
    }

    if (NULL != frag) {
        IBOFFLOAD_SET_SINGLE_FRAG_ON_TASK(frag, task);
    } else {
        task->frag = NULL;
    }

    task->sg_entries[0] = *l_operand;

    /* Hack - we don't really use it.
    task->sg_entries[1] = *r_operand;
    */
    /* We use only single entry
    SENDWR(task)->num_sge = 2;
    */
    SENDWR(task)->num_sge = 1;
    SENDWR(task)->sg_list = task->sg_entries;

#if OMPI_HAVE_IBOFFLOAD_CALC_RDMA
    SENDWR(task)->opcode = IBV_M_WR_CALC_RDMA_WRITE_WITH_IMM;
    SENDWR(task)->wr.calc_rdma.data_type = coll_request->actual_ib_dtype;
    SENDWR(task)->wr.calc_rdma.calc_op = coll_request->actual_ib_op;
    SENDWR(task)->wr.calc_rdma.rkey = endpoint->remote_rdma_block.ib_info.rkey;
    SENDWR(task)->wr.calc_rdma.remote_addr = (uint64_t) (uintptr_t)
        ((unsigned char *) endpoint->remote_rdma_block.rdma_desc[coll_request->ml_buffer_index].data_addr + offset);
#else
    ML_ERROR(("Fatal error: RDMA CALC was called, but the driver does not support this operation"));
    return NULL;
#endif

    return task;
}

static inline __opal_attribute_always_inline__
              int release_frags_on_task(mca_bcol_iboffload_task_t *task,
                                        ompi_free_list_t *list)
{
    int rc, qp_index;

    mca_bcol_iboffload_frag_t *temp_frag = task->frag;
    mca_bcol_iboffload_endpoint_t *endpoint = task->endpoint;

    mca_bcol_iboffload_component_t *cm =
                       &mca_bcol_iboffload_component;

    IBOFFLOAD_VERBOSE(10, ("\nCalling release_frags_on_task"));

    while (NULL != temp_frag) {
        qp_index = temp_frag->qp_index;

        --(temp_frag->ref_counter);

        /* Return credits */
        if (MQE_WR_CQE_WAIT == task->element.opcode) {
            ++(endpoint->qps[qp_index].rd_wqe);

            IBOFFLOAD_VERBOSE(10, ("Return rd_wqe %d pp_win %d",
                        endpoint->qps[qp_index].rd_wqe,
                        cm->qp_infos[qp_index].rd_pp_win));

            /* Call for recv prepost */
            if (endpoint->qps[qp_index].rd_wqe >=
                        cm->qp_infos[qp_index].rd_pp_win) {
                IBOFFLOAD_VERBOSE(10, ("Prepost to endpoint->index - %d, qp_index - %d", endpoint->index, qp_index));
                rc = mca_bcol_iboffload_prepost_recv(endpoint, qp_index,
                        endpoint->qps[qp_index].rd_wqe);
                if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
                    IBOFFLOAD_ERROR(("QP %d: failed to prepost.\n", qp_index));
                    return OMPI_ERROR;
                }
                /* What happens if we can not prepost ?*/
            }
        } else if (MQE_WR_SEND == task->element.opcode) {
            ++(endpoint->qps[qp_index].sd_wqe);

            assert(endpoint->qps[qp_index].sd_wqe <= cm->qp_infos[qp_index].rd_num);

            IBOFFLOAD_VERBOSE(10, ("Return sd_wqe %d, qp_index - %d, endpoint - %p",
                                    endpoint->qps[qp_index].sd_wqe, qp_index, endpoint));
        } else {
            /* We should not arrive to this case */
            IBOFFLOAD_ERROR(("Unsupporeted operation"));

            return OMPI_ERROR;
        }

        mca_bcol_iboffload_return_frag_tolist(temp_frag, list);
        temp_frag = temp_frag->next;
    }

    return OMPI_SUCCESS;
}

END_C_DECLS

#endif
