/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2019-2025 Bull SAS.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/**
 * @file pml_ubcl_requests.h
 *
 * UBCL PML Requests
 *
 * Several specific cases are to be handled with care, namely:
 *   - Persistant requests:
 *       Not much but need to be reset at each restart. Some fields are erased
 *       by OMPI_REQUEST_INIT() and need to be set again.
 *   - Matching requests (mprobe/mrecv):
 *       Once matched by a matching probe, an incoming message must be locked
 *       and can only be received thanks to a corresponding mrecv. Two fields
 *       are given to allow quick access to ompi_message_t and internal request
 *       from the pml request.
 *   - Final trick:
 *       You can have the following combinations:
 *         - A persistant any source receive request
 *         - A matching any source receive request
 */

#ifndef MCA_PML_UBCL_REQUEST_H
#define MCA_PML_UBCL_REQUEST_H

#include "ompi/mca/pml/base/base.h"
#include "ompi/mca/pml/pml_constants.h"
#include "ompi/message/message.h"
#include "ompi/proc/proc.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/communicator/communicator.h"
#include "ompi/request/request.h"
#include "ompi/mca/pml/base/pml_base_bsend.h"
#include "opal/include/opal/sys/atomic.h"
#include "opal/mca/common/ubcl/common_ubcl.h"

#include <ubcl_api.h>

BEGIN_C_DECLS

/**
 * Requests type enum
 */
typedef enum { MCA_PML_UBCL_REQUEST_SEND, MCA_PML_UBCL_REQUEST_RECV } mca_pml_ubcl_request_type_t;

/**
 * Request structure
 *
 * Fields map the usual MPI calls
 */
struct mca_pml_ubcl_request_t {
    opal_free_list_item_t super;
    ompi_request_t ompi_req; /**< Base request */
    mca_pml_ubcl_request_type_t type;

    /* PML parameters */
    uint64_t to_free:1;
    uint64_t completed:1;
    uint64_t need_xpack:1;
    uint64_t is_buffered:1;
    uint64_t is_buffer_malloced:1;

    /* Any source parameters */
    uint64_t is_any_tag:1; /**< Remember any_tag status for persistant resets */
    uint64_t is_any_src:1; /**< Remember any_src status for persistant resets and
                             * internal requests cleanup */
    uint64_t pad:57;

    /* MPI API parameters */
    const void *buf;
    size_t count;
    ompi_datatype_t *datatype;
    int rank; /**< src or dest */
    int32_t tag;
    int error;                        /**< Statuts error */
    mca_pml_base_send_mode_t mode;    /**< Send mode for send requests */
    struct ompi_communicator_t *comm; /**< Communicator */
    struct ompi_proc_t *proc;         /**< Remote ompi proc */
    opal_convertor_t convertor;       /**< Data convertor */
    ompi_request_complete_fn_t saved_complete_cb; /**< Saved callback from another component (e.g OSC pt2pt) */
    void *saved_complete_cb_data; /**< Saved callback data from another component (e.g OSC pt2pt) */

    /* Matching message parameters */
    ompi_message_t *message;
    void *prematched_req; /**< Save matched internal request for quick mrecv */

    /* Cancel/complete concurrency protection */
    opal_atomic_lock_t req_lock;

    /* Operation handle used for cancel */
    void *ubcl_operation_handle;
};
typedef struct mca_pml_ubcl_request_t mca_pml_ubcl_request_t;
OBJ_CLASS_DECLARATION(mca_pml_ubcl_request_t);

/**
 * Callback functions from request system
 */
int mca_pml_ubcl_request_start(size_t count, struct ompi_request_t **requests);
int mca_pml_ubcl_request_free(struct ompi_request_t **request);
int mca_pml_ubcl_request_cancel(struct ompi_request_t *request, int flag);
int mca_pml_ubcl_request_complete_cb(struct ompi_request_t *request);
void ubcl_request_send_complete_cb(ubcl_status_t status, void *cb_data);
void ubcl_request_recv_complete_cb(ubcl_status_t status, void *cb_data);
void mca_pml_ubcl_request_finalize(mca_pml_ubcl_request_t *req);
int mca_pml_ubcl_request_probe_send(mca_pml_ubcl_request_t *req);
void pml_ubcl_bufferize(mca_pml_ubcl_request_t *req);
bool pml_ubcl_request_is_cuda_buf(mca_pml_ubcl_request_t *req);
int mca_pml_ubcl_request_need_xpack(mca_pml_ubcl_request_t *req,
                                    ubcl_endpoint_type_t type);

/**
 * Requests accessors.
 */
#define MCA_PML_UBCL_REQUEST_ANYSRC(req)     ((req)->is_any_src)
#define MCA_PML_UBCL_REQUEST_ANYTAG(req)     ((req)->is_any_tag)
#define MCA_PML_UBCL_REQUEST_COMM(req)       ((req)->comm)
#define MCA_PML_UBCL_REQUEST_CONVERTOR(req)  ((req)->convertor)
#define MCA_PML_UBCL_REQUEST_NEED_XPACK(req) ((req)->need_xpack)
#define MCA_PML_UBCL_REQUEST_IS_ACTIVE(req)  (OMPI_REQUEST_ACTIVE == (req)->ompi_req.req_state)

/**
 * Macros for any_source messages. MOSTLY USELESS and can be put in
 * pml_ubcl_request_handle_match now that it is the only place where it is called
 **/
#define MCA_PML_UBCL_RECV_REQUEST_UPDATE_SRC(_req, _rank)                 \
    do {                                                                  \
        (_req)->rank = _rank;                                             \
        (_req)->proc = ompi_comm_peer_lookup((_req)->comm, (_req)->rank); \
        MCA_PML_UBCL_RECV_REQUEST_CONVERTOR_INIT(_req);                   \
    } while (0)
#define MCA_PML_UBCL_RECV_REQUEST_UPDATE_TAG(_req, _tag) ((_req)->tag = _tag)

/**
 * Macros to handle MPI matching interface. SAME AS ABOVE, move in corresponding
 * function in pml_ubcl_request.c
 **/
#define MCA_PML_UBCL_RECV_REQUEST_PREMATCH(req, _prematched_req, _rank) \
    do {                                                                \
        (req)->message->req_ptr = req;                                  \
        (req)->prematched_req = _prematched_req;                        \
        (req)->rank = _rank;                                            \
    } while (0)
#define MCA_PML_UBCL_RECV_REQUEST_NEED_PREMATCH(req)  (NULL != (req)->message)
#define MCA_PML_UBCL_RECV_REQUEST_IS_PREMATCHED(req)  (NULL != (req)->prematched_req)
#define MCA_PML_UBCL_RECV_REQUEST_PREMATCHED_REQ(req) ((req)->prematched_req)

/**
 * Generic convinience macros
 */
#define MCA_PML_UBCL_SEND_REQUEST_INIT(req, _buf, _count, _datatype, _dst, _tag, _mode, _comm,   \
                                       _proc, _persistent)                           \
    do {                                                                                         \
        OBJ_RETAIN(_comm);                                                                       \
        OMPI_DATATYPE_RETAIN(_datatype);                                                         \
        OBJ_CONSTRUCT(&(req)->ompi_req, ompi_request_t);                                         \
        OMPI_REQUEST_INIT(&req->ompi_req, _persistent);                                          \
        (req)->ompi_req.req_type = OMPI_REQUEST_PML;                                             \
        (req)->ompi_req.req_start = mca_pml_ubcl_request_start;                                  \
        (req)->ompi_req.req_free = mca_pml_ubcl_request_free;                                    \
        (req)->ompi_req.req_cancel = mca_pml_ubcl_request_cancel;                                \
        (req)->ompi_req.req_complete_cb = mca_pml_ubcl_request_complete_cb;                      \
        (req)->ompi_req.req_mpi_object.comm = _comm;                                             \
        (req)->saved_complete_cb = NULL;                                                         \
        (req)->saved_complete_cb_data = NULL;                                                    \
        (req)->type = MCA_PML_UBCL_REQUEST_SEND;                                                 \
        (req)->to_free = 0;                                                                      \
        (req)->completed = 0;                                                                    \
        (req)->is_buffered = 0;                                                                  \
        (req)->is_buffer_malloced = 0;                                                           \
        (req)->buf = _buf;                                                                       \
        (req)->count = _count;                                                                   \
        (req)->datatype = _datatype;                                                             \
        (req)->rank = _dst;                                                                      \
        (req)->tag = _tag;                                                                       \
        (req)->error = MPI_SUCCESS;                                                              \
        (req)->mode = _mode;                                                                     \
        (req)->comm = _comm;                                                                     \
        (req)->proc = _proc;                                                                     \
        OBJ_CONSTRUCT(&(req)->convertor, opal_convertor_t);                                      \
        opal_convertor_copy_and_prepare_for_send(_proc->super.proc_convertor, &_datatype->super, \
                                                 _count, _buf, 0, &(req)->convertor);            \
        (req)->need_xpack = mca_pml_ubcl_request_need_xpack((req),                               \
                ((mca_common_ubcl_endpoint_t *)(req)->proc->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PML])->type);                    \
        (req)->message = NULL;                                                                   \
        (req)->prematched_req = NULL;                                                            \
        (req)->is_any_tag = 0;                                                                   \
        (req)->is_any_src = 0;                                                                   \
        opal_atomic_lock_init(&((req)->req_lock), OPAL_ATOMIC_LOCK_UNLOCKED);                    \
        (req)->ubcl_operation_handle = NULL;                                                     \
    } while (0)

#define MCA_PML_UBCL_RECV_REQUEST_CONVERTOR_INIT(req)                                                            \
    do {                                                                                                         \
        if ((req)->is_any_src) {                                                                                 \
            /* Remote proc is unknown, let assume its architecture is the same as local proc */                  \
            opal_convertor_copy_and_prepare_for_recv(ompi_proc_local()->super.proc_convertor,                    \
                                                     &(req)->datatype->super, (req)->count,                      \
                                                     (req)->buf, 0, &(req)->convertor);                          \
            /* Do not ask for endpoint capabilities and enable by default need_xpack */                          \
            (req)->need_xpack = (0 != (req)->datatype->super.true_lb)                                            \
                                || opal_convertor_need_buffers(&req->convertor);                                 \
        } else {                                                                                                 \
            opal_convertor_copy_and_prepare_for_recv((req)->proc->super.proc_convertor,                          \
                                                     &(req)->datatype->super, (req)->count,                      \
                                                     (req)->buf, 0, &(req)->convertor);                          \
            (req)->need_xpack = mca_pml_ubcl_request_need_xpack((req),                                           \
                    ((mca_common_ubcl_endpoint_t *)(req)->proc->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PML])->type); \
        }                                                                                                        \
    } while (0)

#define MCA_PML_UBCL_RECV_REQUEST_INIT(req, _buf, _count, _datatype, _src,     \
                                       _tag, _comm, _proc, _persistent,        \
                                       _probe, _mes)                           \
    do {                                                                       \
        OBJ_RETAIN(_comm);                                                     \
        OMPI_DATATYPE_RETAIN(_datatype);                                       \
        OBJ_CONSTRUCT(&(req)->ompi_req, ompi_request_t);                       \
        OMPI_REQUEST_INIT(&req->ompi_req, _persistent);                        \
        (req)->ompi_req.req_type = OMPI_REQUEST_PML;                           \
        (req)->ompi_req.req_start = mca_pml_ubcl_request_start;                \
        (req)->ompi_req.req_free = mca_pml_ubcl_request_free;                  \
        (req)->ompi_req.req_cancel = mca_pml_ubcl_request_cancel;              \
        (req)->ompi_req.req_complete_cb = mca_pml_ubcl_request_complete_cb;    \
        (req)->ompi_req.req_mpi_object.comm = _comm;                           \
        (req)->saved_complete_cb = NULL;                                       \
        (req)->saved_complete_cb_data = NULL;                                  \
        (req)->type = MCA_PML_UBCL_REQUEST_RECV;                               \
        (req)->to_free = 0;                                                    \
        (req)->completed = 0;                                                  \
        (req)->is_buffered = 0;                                                \
        (req)->is_buffer_malloced = 0;                                         \
        (req)->buf = _buf;                                                     \
        (req)->count = _count;                                                 \
        (req)->datatype = _datatype;                                           \
        (req)->rank = _src;                                                    \
        (req)->tag = _tag;                                                     \
        (req)->error = MPI_SUCCESS;                                            \
        (req)->mode = MCA_PML_BASE_SEND_SIZE;                                  \
        (req)->comm = _comm;                                                   \
        (req)->proc = _proc;                                                   \
        OBJ_CONSTRUCT(&(req)->convertor, opal_convertor_t);                    \
        (req)->message = (void *) _mes;                                        \
        (req)->prematched_req = NULL;                                          \
        (req)->is_any_tag = (_tag == OMPI_ANY_TAG);                            \
        opal_atomic_lock_init(&((req)->req_lock), OPAL_ATOMIC_LOCK_UNLOCKED);  \
        (req)->ubcl_operation_handle = NULL;                                   \
        if (OMPI_ANY_SOURCE == (req)->rank) {                                  \
            (req)->is_any_src = 1;                                             \
        } else {                                                               \
            (req)->is_any_src = 0;                                             \
        }                                                                      \
        MCA_PML_UBCL_RECV_REQUEST_CONVERTOR_INIT(req);                         \
    } while (0)

#define MCA_PML_UBCL_RECV_REQUEST_MPROBE_TO_MRECV(req, _buf, _count, _datatype) \
    do {                                                                        \
        OMPI_DATATYPE_RETAIN(_datatype);                                        \
        (req)->type = MCA_PML_UBCL_REQUEST_RECV;                                \
        (req)->buf = _buf;                                                      \
        (req)->count = _count;                                                  \
        (req)->datatype = _datatype;                                            \
        (req)->proc = ompi_comm_peer_lookup((req)->comm, (req)->rank);          \
        MCA_PML_UBCL_RECV_REQUEST_CONVERTOR_INIT(req);                          \
    } while (0)

#define MCA_PML_UBCL_REQUEST_ACTIVATE(req)                       \
    do {                                                         \
        (req)->ompi_req.req_state = OMPI_REQUEST_ACTIVE;         \
        (req)->ompi_req.req_complete = REQUEST_PENDING;          \
        (req)->ompi_req.req_status.MPI_SOURCE = OMPI_ANY_SOURCE; \
        (req)->ompi_req.req_status.MPI_TAG = OMPI_ANY_TAG;       \
        (req)->ompi_req.req_status.MPI_ERROR = OMPI_SUCCESS;     \
        (req)->ompi_req.req_status._ucount = 0;                  \
        (req)->ompi_req.req_status._cancelled = 0;               \
    } while (0)

#define MCA_PML_UBCL_STATUS_SET(stat, rank, tag, err, size) \
    do {                                                    \
        (stat)->MPI_SOURCE = rank;                          \
        (stat)->MPI_TAG = tag;                              \
        (stat)->MPI_ERROR = err;                            \
        (stat)->_ucount = size;                             \
        (stat)->_cancelled = false;                         \
    } while (0)

#define MCA_PML_UBCL_REQUEST_SET_STATUS(req, rank, tag, err, size)                  \
    do {                                                                            \
        MCA_PML_UBCL_STATUS_SET(&(req)->ompi_req.req_status, rank, tag, err, size); \
    } while (0)

#define MCA_PML_UBCL_REQUEST_CPY_STATUS(status, req)                \
    do {                                                            \
        status->MPI_SOURCE = (req)->ompi_req.req_status.MPI_SOURCE; \
        status->MPI_TAG = (req)->ompi_req.req_status.MPI_TAG;       \
        status->MPI_ERROR = (req)->ompi_req.req_status.MPI_ERROR;   \
        status->_ucount = (req)->ompi_req.req_status._ucount;       \
        status->_cancelled = (req)->ompi_req.req_status._cancelled; \
    } while (0)

END_C_DECLS

#endif /* MCA_PML_UBCL_REQUEST_H */
