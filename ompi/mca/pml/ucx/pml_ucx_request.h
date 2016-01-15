/*
 * Copyright (C) Mellanox Technologies Ltd. 2001-2015.  ALL RIGHTS RESERVED.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PML_UCX_REQUEST_H_
#define PML_UCX_REQUEST_H_

#include "pml_ucx.h"
#include "pml_ucx_datatype.h"


enum {
    MCA_PML_UCX_REQUEST_FLAG_SEND         = (1 << 0), /* Persistent send */
    MCA_PML_UCX_REQUEST_FLAG_FREE_CALLED  = (1 << 1),
    MCA_PML_UCX_REQUEST_FLAG_COMPLETED    = (1 << 2)
};

/*
 * UCX tag structure:
 *
 * 01234567 01234567 01234567 01234567 01234567 01234567 01234567 01234567
 *                           |                          |
 *      message tag (24)     |     source rank (24)     |  context id (16)
 *                           |                          |
 */
#define PML_UCX_TAG_BITS                       24
#define PML_UCX_RANK_BITS                      24
#define PML_UCX_CONTEXT_BITS                   16


#define PML_UCX_MAKE_SEND_TAG(_tag, _comm) \
    ((((uint64_t) (_tag)            ) << (PML_UCX_RANK_BITS + PML_UCX_CONTEXT_BITS)) | \
     (((uint64_t)(_comm)->c_my_rank ) << PML_UCX_CONTEXT_BITS) | \
     ((uint64_t)(_comm)->c_contextid))


#define PML_UCX_MAKE_RECV_TAG(_ucp_tag, _ucp_tag_mask, _tag, _src, _comm) \
    { \
        if ((_src) == MPI_ANY_SOURCE) { \
            _ucp_tag_mask = 0x800000000000fffful; \
        } else { \
            _ucp_tag_mask = 0x800000fffffffffful; \
        } \
        \
        _ucp_tag = (((uint64_t)(_src) & UCS_MASK(PML_UCX_RANK_BITS)) << PML_UCX_CONTEXT_BITS) | \
                   (_comm)->c_contextid; \
        \
        if ((_tag) != MPI_ANY_TAG) { \
            _ucp_tag_mask |= 0x7fffff0000000000ul; \
            _ucp_tag      |= ((uint64_t)(_tag)) << (PML_UCX_RANK_BITS + PML_UCX_CONTEXT_BITS); \
        } \
    }

#define PML_UCX_TAG_GET_SOURCE(_tag) \
    (((_tag) >> PML_UCX_CONTEXT_BITS) & UCS_MASK(PML_UCX_RANK_BITS))


#define PML_UCX_TAG_GET_MPI_TAG(_tag) \
    ((_tag) >> (PML_UCX_CONTEXT_BITS + PML_UCX_RANK_BITS))


#define PML_UCX_MESSAGE_NEW(_comm, _ucp_msg, _info, _message) \
    { \
        struct ompi_message_t *msg = ompi_message_alloc(); \
        if (msg == NULL) { \
            /* TODO release UCP message */ \
            return OMPI_ERR_OUT_OF_RESOURCE; \
        } \
        \
        msg->comm    = (_comm); \
        msg->req_ptr = (_ucp_msg); \
        msg->peer    = PML_UCX_TAG_GET_SOURCE((_info)->sender_tag); \
        msg->count   = (_info)->length; \
        *(_message)  = msg; \
    }


#define PML_UCX_MESSAGE_RELEASE(_message) \
    { \
        ompi_message_return(*(_message)); \
        *(_message) = NULL; \
    }


struct pml_ucx_persistent_request {
    ompi_request_t                    ompi;
    ompi_request_t                    *tmp_req;
    unsigned                          flags;
    void                              *buffer;
    size_t                            count;
    ucp_datatype_t                    datatype;
    ucp_tag_t                         tag;
    struct {
        mca_pml_base_send_mode_t      mode;
        ucp_ep_h                      ep;
    } send;
    struct {
        ucp_tag_t                     tag_mask;
    } recv;
};


void mca_pml_ucx_send_completion(void *request, ucs_status_t status);

void mca_pml_ucx_recv_completion(void *request, ucs_status_t status,
                                 ucp_tag_recv_info_t *info);

void mca_pml_ucx_psend_completion(void *request, ucs_status_t status);

void mca_pml_ucx_precv_completion(void *request, ucs_status_t status,
                                  ucp_tag_recv_info_t *info);

void mca_pml_ucx_persistent_request_complete(mca_pml_ucx_persistent_request_t *preq,
                                             ompi_request_t *tmp_req);

void mca_pml_ucx_completed_request_init(ompi_request_t *ompi_req);

void mca_pml_ucx_request_init(void *request);

void mca_pml_ucx_request_cleanup(void *request);


static inline ucp_ep_h mca_pml_ucx_get_ep(ompi_communicator_t *comm, int dst)
{
    ucp_ep_h ep = ompi_comm_peer_lookup(comm,dst)->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PML];
    if (OPAL_UNLIKELY(NULL == ep)) {
        ep = mca_pml_ucx_add_proc(comm, dst);
    }

    return ep;
}

static inline void mca_pml_ucx_request_reset(ompi_request_t *req)
{
    req->req_complete          = false;
    req->req_status._cancelled = false;
}

static void mca_pml_ucx_set_send_status(ompi_status_public_t* mpi_status,
                                        ucs_status_t status)
{
    if (status == UCS_OK) {
        mpi_status->MPI_ERROR  = MPI_SUCCESS;
    } else if (status == UCS_ERR_CANCELED) {
        mpi_status->_cancelled = true;
    } else {
        mpi_status->MPI_ERROR  = MPI_ERR_INTERN;
    }
}

static inline void mca_pml_ucx_set_recv_status(ompi_status_public_t* mpi_status,
                                               ucs_status_t ucp_status,
                                               const ucp_tag_recv_info_t *info)
{
    int64_t tag;

    if (ucp_status == UCS_OK) {
        tag = info->sender_tag;
        mpi_status->MPI_ERROR  = MPI_SUCCESS;
        mpi_status->MPI_SOURCE = PML_UCX_TAG_GET_SOURCE(tag);
        mpi_status->MPI_TAG    = PML_UCX_TAG_GET_MPI_TAG(tag);
        mpi_status->_ucount    = info->length;
    } else if (ucp_status == UCS_ERR_MESSAGE_TRUNCATED) {
        mpi_status->MPI_ERROR = MPI_ERR_TRUNCATE;
    } else if (ucp_status == UCS_ERR_CANCELED) {
        mpi_status->_cancelled = true;
    } else {
        mpi_status->MPI_ERROR = MPI_ERR_INTERN;
    }
}

static inline void mca_pml_ucx_set_recv_status_safe(ompi_status_public_t* mpi_status,
                                                    ucs_status_t ucp_status,
                                                    const ucp_tag_recv_info_t *info)
{
    if (mpi_status != MPI_STATUS_IGNORE) {
        mca_pml_ucx_set_recv_status(mpi_status, ucp_status, info);
    }
}

OBJ_CLASS_DECLARATION(mca_pml_ucx_persistent_request_t);


#endif /* PML_UCX_REQUEST_H_ */
