/*
 * Copyright (C) Mellanox Technologies Ltd. 2001-2011.  ALL RIGHTS RESERVED.
 * Copyright (c) 2016      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2019      Huawei Technologies Co., Ltd. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "coll_ucx.h"
#include "coll_ucx_request.h"

#include "ompi/message/message.h"
#include <inttypes.h>

int mca_coll_ucx_start(size_t count, ompi_request_t** requests)
{
    mca_coll_ucx_persistent_op_t *preq=NULL;
    ompi_request_t *tmp_req=NULL;
    size_t i;

    for (i = 0; i < count; ++i) {
        preq = (mca_coll_ucx_persistent_op_t *)requests[i];
        if ((preq == NULL) || (OMPI_REQUEST_COLL != preq->ompi.req_type)) {
            /* Skip irrelevant requests */
            continue;
        }

        COLL_UCX_ASSERT(preq->ompi.req_state != OMPI_REQUEST_INVALID);
        preq->ompi.req_state = OMPI_REQUEST_ACTIVE;
        mca_coll_ucx_request_reset(&preq->ompi);

        tmp_req = ucg_collective_start_nb(preq->coll_desc);
        if (tmp_req == NULL) {
            COLL_UCX_VERBOSE(8, "collective completed immediately, completing persistent request %p",
                    (void*)preq);
            mca_coll_ucx_set_coll_status(&preq->ompi.req_status, UCS_OK);
            ompi_request_complete(&preq->ompi, true);
        } else if (!UCS_PTR_IS_ERR(tmp_req)) {
            if (REQUEST_COMPLETE(tmp_req)) {
                /* tmp_req is already completed */
                COLL_UCX_VERBOSE(8, "completing persistent request %p", (void*)preq);
                mca_coll_ucx_persistent_op_complete(preq, tmp_req);
            } else {
                /* tmp_req would be completed by callback and trigger completion
                 * of preq */
                COLL_UCX_VERBOSE(8, "temporary request %p will complete persistent request %p",
                        (void*)tmp_req, (void*)preq);
                tmp_req->req_complete_cb_data = preq;
                preq->tmp_req                 = tmp_req;
            }
        } else {
            COLL_UCX_ERROR("ucx collective failed: %s", ucs_status_string(UCS_PTR_STATUS(tmp_req)));
            return OMPI_ERROR;
        }
    }

    return OMPI_SUCCESS;
}

/*
 * For each type of collectives there are 3 varieties of function calls:
 * blocking, non-blocking and persistent initialization. For example, for
 * the allreduce collective operations, those would be called:
 * - mca_coll_ucx_allreduce
 * - mca_coll_ucx_iallreduce
 * - mca_coll_ucx_iallreduce_init
 *
 * In the blocking version, request is placed on the stack, awaiting completion.
 * For non-blocking, request is allocated by UCX, awaiting completion.
 * For persistent requests, the collective starts later - only then the
 * (internal) request is created (by UCX) and placed as "tmp_req" inside
 * the persistent (external) request structure.
 */

#define COLL_UCX_TRACE(_msg, _sbuf, _rbuf, _count, _datatype, _comm, ...)        \
        COLL_UCX_VERBOSE(8, _msg " sbuf %p rbuf %p count %i type '%s' comm %d '%s'", \
                __VA_ARGS__, (_sbuf), (_rbuf), (_count), (_datatype)->name, \
                (_comm)->c_contextid, (_comm)->c_name);

#define COLL_UCX_REQ_ALLOCA(ucx_module) \
        ((char *)alloca(mca_coll_ucx_component.request_size) + \
                mca_coll_ucx_component.request_size);

int mca_coll_ucx_allreduce(const void *sbuf, void *rbuf, int count,
                           struct ompi_datatype_t *dtype, struct ompi_op_t *op,
                           struct ompi_communicator_t *comm,
                           mca_coll_base_module_t *module)
{
    mca_coll_ucx_module_t *ucx_module = (mca_coll_ucx_module_t*)module;

    COLL_UCX_TRACE("%s", sbuf, rbuf, count, dtype, comm, "allreduce START");

    /* coverity[bad_alloc_arithmetic] */
    ucs_status_ptr_t req = COLL_UCX_REQ_ALLOCA(ucx_module);

    int dtype_size;
    ucg_coll_h coll;
    MPI_Type_size(dtype, &dtype_size);
    ucs_status_t ret = ucg_coll_allreduce_init(sbuf, rbuf, count,
            dtype_size, dtype, ucx_module->ucg_group, 0,
            op, 0, 0, &coll);
    if (OPAL_UNLIKELY(ret != UCS_OK)) {
        COLL_UCX_ERROR("ucx allreduce init failed: %s", ucs_status_string(ret));
        return OMPI_ERROR;
    }

    ret = ucg_collective_start_nbr(coll, req);
    if (OPAL_UNLIKELY(UCS_STATUS_IS_ERR(ret))) {
        COLL_UCX_ERROR("ucx allreduce start failed: %s", ucs_status_string(ret));
        return OMPI_ERROR;
    }

    if (ucs_unlikely(ret == UCS_OK)) {
        return OMPI_SUCCESS;
    }

    ucp_worker_h ucp_worker = mca_coll_ucx_component.ucg_worker;
    MCA_COMMON_UCX_WAIT_LOOP(req, OPAL_COMMON_UCX_REQUEST_TYPE_UCG,
            ucp_worker, "ucx allreduce", (void)0);
    COLL_UCX_TRACE("%s", sbuf, rbuf, count, dtype, comm, "allreduce END");
}

int mca_coll_ucx_iallreduce(const void *sbuf, void *rbuf, int count,
                            struct ompi_datatype_t *dtype, struct ompi_op_t *op,
                            struct ompi_communicator_t *comm,
                            struct ompi_request_t **request,
                            mca_coll_base_module_t *module)
{
    mca_coll_ucx_module_t *ucx_module = (mca_coll_ucx_module_t*)module;

    COLL_UCX_TRACE("iallreduce request *%p", sbuf, rbuf, count, dtype, comm,
                   (void*)request);

    int dtype_size;
    ucg_coll_h coll;
    MPI_Type_size(dtype, &dtype_size);
    ucs_status_t ret = ucg_coll_allreduce_init(sbuf, rbuf, count,
            dtype_size, dtype, ucx_module->ucg_group,
            mca_coll_ucx_coll_completion, op, 0, 0, &coll);
    if (OPAL_UNLIKELY(ret != UCS_OK)) {
        COLL_UCX_ERROR("ucx allreduce init failed: %s", ucs_status_string(ret));
        return OMPI_ERROR;
    }

    ompi_request_t *req = (ompi_request_t*)ucg_collective_start_nb(coll);
    if (OPAL_UNLIKELY(UCS_STATUS_IS_ERR(ret))) {
        COLL_UCX_ERROR("ucx allreduce start failed: %s",
                       ucs_status_string(UCS_PTR_STATUS(req)));
        return OMPI_ERROR;
    }

    if (req == NULL) {
        COLL_UCX_VERBOSE(8, "returning completed request");
        *request = &mca_coll_ucx_component.completed_send_req;
        return OMPI_SUCCESS;
    }

    COLL_UCX_VERBOSE(8, "got request %p", (void*)req);
    *request = req;
    return OMPI_SUCCESS;
}

int mca_coll_ucx_allreduce_init(const void *sbuf, void *rbuf, int count,
                                struct ompi_datatype_t *dtype, struct ompi_op_t *op,
                                struct ompi_communicator_t *comm,
                                struct ompi_info_t *info,
                                struct ompi_request_t **request,
                                mca_coll_base_module_t *module)
{
    mca_coll_ucx_module_t *ucx_module = (mca_coll_ucx_module_t*)module;

    mca_coll_ucx_persistent_op_t *req =
            (mca_coll_ucx_persistent_op_t *)
            COLL_UCX_FREELIST_GET(&mca_coll_ucx_component.persistent_ops);
    if (OPAL_UNLIKELY(req == NULL)) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    COLL_UCX_TRACE("iallreduce_init request *%p=%p",
            sbuf, rbuf, count, dtype, comm, (void*)request, (void*)req);

    int dtype_size;
    MPI_Type_size(dtype, &dtype_size);
    ucs_status_t ret = ucg_coll_allreduce_init(sbuf, rbuf, count,
            dtype_size, dtype, ucx_module->ucg_group,
            mca_coll_ucx_pcoll_completion, op, 0,
            UCG_GROUP_COLLECTIVE_MODIFIER_PERSISTENT, &req->coll_desc);
    if (OPAL_UNLIKELY(ret != UCS_OK)) {
        COLL_UCX_ERROR("ucx allreduce failed: %s", ucs_status_string(ret));
        return OMPI_ERROR;
    }

    req->ompi.req_state = OMPI_REQUEST_INACTIVE;
    *request = &req->ompi;
    return OMPI_SUCCESS;
}

int mca_coll_ucx_reduce(const void *sbuf, void* rbuf, int count,
                        struct ompi_datatype_t *dtype, struct ompi_op_t *op,
                        int root, struct ompi_communicator_t *comm,
                        mca_coll_base_module_t *module)
{
    mca_coll_ucx_module_t *ucx_module = (mca_coll_ucx_module_t*)module;

    COLL_UCX_TRACE("%s", sbuf, rbuf, count, dtype, comm, "allreduce");

    /* coverity[bad_alloc_arithmetic] */
    ucs_status_ptr_t req = COLL_UCX_REQ_ALLOCA(ucx_module);

    int dtype_size;
    ucg_coll_h coll;
    MPI_Type_size(dtype, &dtype_size);
    ucs_status_t ret = ucg_coll_reduce_init(sbuf, rbuf, count,
            dtype_size, dtype, ucx_module->ucg_group, 0,
            op, root, 0, &coll);
    if (OPAL_UNLIKELY(ret != UCS_OK)) {
        COLL_UCX_ERROR("ucx reduce init failed: %s", ucs_status_string(ret));
        return OMPI_ERROR;
    }

    ret = ucg_collective_start_nbr(coll, req);
    if (OPAL_UNLIKELY(UCS_STATUS_IS_ERR(ret))) {
        COLL_UCX_ERROR("ucx reduce start failed: %s", ucs_status_string(ret));
        return OMPI_ERROR;
    }

    if (ucs_unlikely(ret == UCS_OK)) {
        return OMPI_SUCCESS;
    }

    ucp_worker_h ucp_worker = mca_coll_ucx_component.ucg_worker;
    MCA_COMMON_UCX_WAIT_LOOP(req, OPAL_COMMON_UCX_REQUEST_TYPE_UCG,
            ucp_worker, "ucx reduce", (void)0);
}

int mca_coll_ucx_scatter(const void *sbuf, int scount, struct ompi_datatype_t *sdtype,
                               void *rbuf, int rcount, struct ompi_datatype_t *rdtype,
                         int root, struct ompi_communicator_t *comm, mca_coll_base_module_t *module)
{
    mca_coll_ucx_module_t *ucx_module = (mca_coll_ucx_module_t*)module;

    COLL_UCX_TRACE("%s", sbuf, rbuf, scount, sdtype, comm, "scatter");

    /* coverity[bad_alloc_arithmetic] */
    ucs_status_ptr_t req = COLL_UCX_REQ_ALLOCA(ucx_module);

    ucg_coll_h coll;
    int sdtype_size, rdtype_size;
    MPI_Type_size(sdtype, &sdtype_size);
    MPI_Type_size(rdtype, &rdtype_size);
    ucs_status_t ret = ucg_coll_scatter_init(sbuf, scount, sdtype_size, sdtype,
                                             rbuf, rcount, rdtype_size, rdtype,
                                             ucx_module->ucg_group, 0, 0, root,
                                             0, &coll);
    if (OPAL_UNLIKELY(ret != UCS_OK)) {
        COLL_UCX_ERROR("ucx reduce init failed: %s", ucs_status_string(ret));
        return OMPI_ERROR;
    }

    ret = ucg_collective_start_nbr(coll, req);
    if (OPAL_UNLIKELY(UCS_STATUS_IS_ERR(ret))) {
        COLL_UCX_ERROR("ucx reduce start failed: %s", ucs_status_string(ret));
        return OMPI_ERROR;
    }

    if (ucs_unlikely(ret == UCS_OK)) {
        return OMPI_SUCCESS;
    }

    ucp_worker_h ucp_worker = mca_coll_ucx_component.ucg_worker;
    MCA_COMMON_UCX_WAIT_LOOP(req, OPAL_COMMON_UCX_REQUEST_TYPE_UCG,
            ucp_worker, "ucx scatter", (void)0);
}

int mca_coll_ucx_gather(const void *sbuf, int scount, struct ompi_datatype_t *sdtype,
                              void *rbuf, int rcount, struct ompi_datatype_t *rdtype,
                        int root, struct ompi_communicator_t *comm,
                        mca_coll_base_module_t *module)
{
    mca_coll_ucx_module_t *ucx_module = (mca_coll_ucx_module_t*)module;

    COLL_UCX_TRACE("%s", sbuf, rbuf, scount, sdtype, comm, "gather");

    /* coverity[bad_alloc_arithmetic] */
    ucs_status_ptr_t req = COLL_UCX_REQ_ALLOCA(ucx_module);

    ucg_coll_h coll;
    int sdtype_size, rdtype_size;
    MPI_Type_size(sdtype, &sdtype_size);
    MPI_Type_size(rdtype, &rdtype_size);
    ucs_status_t ret = ucg_coll_gather_init(sbuf, scount, sdtype_size, sdtype,
                                            rbuf, rcount, rdtype_size, rdtype,
                                            ucx_module->ucg_group, 0, 0, root,
                                            0, &coll);
    if (OPAL_UNLIKELY(ret != UCS_OK)) {
        COLL_UCX_ERROR("ucx reduce init failed: %s", ucs_status_string(ret));
        return OMPI_ERROR;
    }

    ret = ucg_collective_start_nbr(coll, req);
    if (OPAL_UNLIKELY(UCS_STATUS_IS_ERR(ret))) {
        COLL_UCX_ERROR("ucx reduce start failed: %s", ucs_status_string(ret));
        return OMPI_ERROR;
    }

    if (ucs_unlikely(ret == UCS_OK)) {
        return OMPI_SUCCESS;
    }

    ucp_worker_h ucp_worker = mca_coll_ucx_component.ucg_worker;
    MCA_COMMON_UCX_WAIT_LOOP(req, OPAL_COMMON_UCX_REQUEST_TYPE_UCG,
            ucp_worker, "ucx gather", (void)0);
}

int mca_coll_ucx_allgather(const void *sbuf, int scount, struct ompi_datatype_t *sdtype,
                                 void *rbuf, int rcount, struct ompi_datatype_t *rdtype,
                           struct ompi_communicator_t *comm,
                           mca_coll_base_module_t *module)
{
    mca_coll_ucx_module_t *ucx_module = (mca_coll_ucx_module_t*)module;

    COLL_UCX_TRACE("%s", sbuf, rbuf, scount, sdtype, comm, "allgather");

    /* coverity[bad_alloc_arithmetic] */
    ucs_status_ptr_t req = COLL_UCX_REQ_ALLOCA(ucx_module);

    ucg_coll_h coll;
    int sdtype_size, rdtype_size;
    MPI_Type_size(sdtype, &sdtype_size);
    MPI_Type_size(rdtype, &rdtype_size);
    ucs_status_t ret = ucg_coll_allgather_init(sbuf, scount, sdtype_size, sdtype,
                                               rbuf, rcount, rdtype_size, rdtype,
                                               ucx_module->ucg_group, 0, 0, 0,
                                               0, &coll);
    if (OPAL_UNLIKELY(ret != UCS_OK)) {
        COLL_UCX_ERROR("ucx allgather init failed: %s", ucs_status_string(ret));
        return OMPI_ERROR;
    }

    ret = ucg_collective_start_nbr(coll, req);
    if (OPAL_UNLIKELY(UCS_STATUS_IS_ERR(ret))) {
        COLL_UCX_ERROR("ucx allgather start failed: %s", ucs_status_string(ret));
        return OMPI_ERROR;
    }

    if (ucs_unlikely(ret == UCS_OK)) {
        return OMPI_SUCCESS;
    }

    ucp_worker_h ucp_worker = mca_coll_ucx_component.ucg_worker;
    MCA_COMMON_UCX_WAIT_LOOP(req, OPAL_COMMON_UCX_REQUEST_TYPE_UCG,
	        ucp_worker, "ucx allgather", (void)0);
}

int mca_coll_ucx_alltoall(const void *sbuf, int scount, struct ompi_datatype_t *sdtype,
                                void *rbuf, int rcount, struct ompi_datatype_t *rdtype,
                          struct ompi_communicator_t *comm,
                          mca_coll_base_module_t *module)
{
    mca_coll_ucx_module_t *ucx_module = (mca_coll_ucx_module_t*)module;

    COLL_UCX_TRACE("%s", sbuf, rbuf, scount, sdtype, comm, "alltoall");

    /* coverity[bad_alloc_arithmetic] */
    ucs_status_ptr_t req = COLL_UCX_REQ_ALLOCA(ucx_module);

    ucg_coll_h coll;
    int sdtype_size, rdtype_size;
    MPI_Type_size(sdtype, &sdtype_size);
    MPI_Type_size(rdtype, &rdtype_size);
    ucs_status_t ret = ucg_coll_alltoall_init(sbuf, scount, sdtype_size, sdtype,
                                              rbuf, rcount, rdtype_size, rdtype,
                                              ucx_module->ucg_group, 0, 0, 0,
                                              0, &coll);
    if (OPAL_UNLIKELY(ret != UCS_OK)) {
        COLL_UCX_ERROR("ucx alltoall init failed: %s", ucs_status_string(ret));
        return OMPI_ERROR;
    }

    ret = ucg_collective_start_nbr(coll, req);
    if (OPAL_UNLIKELY(UCS_STATUS_IS_ERR(ret))) {
        COLL_UCX_ERROR("ucx alltoall start failed: %s", ucs_status_string(ret));
        return OMPI_ERROR;
    }

    if (ucs_unlikely(ret == UCS_OK)) {
        return OMPI_SUCCESS;
    }

    ucp_worker_h ucp_worker = mca_coll_ucx_component.ucg_worker;
    MCA_COMMON_UCX_WAIT_LOOP(req, OPAL_COMMON_UCX_REQUEST_TYPE_UCG,
	        ucp_worker, "ucx alltoall", (void)0);
}

int mca_coll_ucx_barrier(struct ompi_communicator_t *comm,
        mca_coll_base_module_t *module)
{
    mca_coll_ucx_module_t *ucx_module = (mca_coll_ucx_module_t*)module;

    /* coverity[bad_alloc_arithmetic] */
    ucs_status_ptr_t req = COLL_UCX_REQ_ALLOCA(ucx_module);

    ucg_coll_h coll;
    ucs_status_t ret = ucg_coll_barrier_init(0, ucx_module->ucg_group, 0, 0, 0,
            0, &coll);
    if (OPAL_UNLIKELY(ret != UCS_OK)) {
        COLL_UCX_ERROR("ucx barrier init failed: %s", ucs_status_string(ret));
        return OMPI_ERROR;
    }

    ret = ucg_collective_start_nbr(coll, req);
    if (OPAL_UNLIKELY(UCS_STATUS_IS_ERR(ret))) {
        COLL_UCX_ERROR("ucx barrier start failed: %s", ucs_status_string(ret));
        return OMPI_ERROR;
    }

    if (ucs_unlikely(ret == UCS_OK)) {
        return OMPI_SUCCESS;
    }

    ucp_worker_h ucp_worker = mca_coll_ucx_component.ucg_worker;
    MCA_COMMON_UCX_WAIT_LOOP(req, OPAL_COMMON_UCX_REQUEST_TYPE_UCG,
            ucp_worker, "ucx barrier", (void)0);
}

int mca_coll_ucx_bcast(void *buff, int count, struct ompi_datatype_t *dtype,
        int root, struct ompi_communicator_t *comm, mca_coll_base_module_t *module)
{
    mca_coll_ucx_module_t *ucx_module = (mca_coll_ucx_module_t*)module;

    COLL_UCX_TRACE("%s", buff, buff, count, dtype, comm, "bcast");

    /* coverity[bad_alloc_arithmetic] */
    ucs_status_ptr_t req = COLL_UCX_REQ_ALLOCA(ucx_module);

    int dtype_size;
    ucg_coll_h coll;
    MPI_Type_size(dtype, &dtype_size);
    ucs_status_t ret = ucg_coll_bcast_init(buff, buff, count,
            dtype_size, dtype, ucx_module->ucg_group, 0,
            0, root, 0, &coll);
    if (OPAL_UNLIKELY(UCS_STATUS_IS_ERR(ret))) {
        COLL_UCX_ERROR("ucx bcast init failed: %s", ucs_status_string(ret));
        return OMPI_ERROR;
    }

    ret = ucg_collective_start_nbr(coll, req);
    if (OPAL_UNLIKELY(UCS_STATUS_IS_ERR(ret))) {
        COLL_UCX_ERROR("ucx bcast start failed: %s", ucs_status_string(ret));
        return OMPI_ERROR;
    }

    if (ucs_unlikely(ret == UCS_OK)) {
        return OMPI_SUCCESS;
    }

    ucp_worker_h ucp_worker = mca_coll_ucx_component.ucg_worker;
    MCA_COMMON_UCX_WAIT_LOOP(req, OPAL_COMMON_UCX_REQUEST_TYPE_UCG,
            ucp_worker, "ucx bcast", (void)0);
}
