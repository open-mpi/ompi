/*
 * Copyright (C) Mellanox Technologies Ltd. 2001-2011.  ALL RIGHTS RESERVED.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PML_YALLA_REQUEST_H_
#define PML_YALLA_REQUEST_H_

#include "pml_yalla.h"
#include "pml_yalla_datatype.h"


#define MCA_PML_YALLA_REQUEST_FLAG_SEND          0x1 /* Persistent send */
#define MCA_PML_YALLA_REQUEST_FLAG_BSEND         0x2 /* Persistent buffered send */
#define MCA_PML_YALLA_REQUEST_FLAG_FREE_CALLED   0x4

struct pml_yalla_base_request {
    ompi_request_t               ompi;
    mca_pml_yalla_convertor_t    *convertor;
    int                          flags;
    mxm_req_base_t               mxm_base[0]; /* overlaps with base of send/recv */
};

struct pml_yalla_send_request {
    mca_pml_yalla_base_request_t super;
    mxm_send_req_t               mxm;
};

struct pml_yalla_bsend_request {
    ompi_free_list_item_t        super;
    mxm_send_req_t               mxm;
};

struct pml_yalla_recv_request {
    mca_pml_yalla_base_request_t super;
    mxm_recv_req_t               mxm;
};


OBJ_CLASS_DECLARATION(mca_pml_yalla_send_request_t);
OBJ_CLASS_DECLARATION(mca_pml_yalla_bsend_request_t);
OBJ_CLASS_DECLARATION(mca_pml_yalla_recv_request_t);

void mca_pml_yalla_init_reqs(void);

#define PML_YALLA_RESET_OMPI_REQ(_ompi_req, _state) \
    { \
        (_ompi_req)->req_state = _state; \
        (_ompi_req)->req_complete = false; \
        (_ompi_req)->req_status._cancelled = false; \
    }

#define PML_YALLA_INIT_OMPI_REQ(_ompi_req, _comm, _state) \
    { \
        PML_YALLA_RESET_OMPI_REQ(_ompi_req, _state); \
        (_ompi_req)->req_mpi_object.comm = _comm; \
        OBJ_RETAIN(_comm); \
    }

#define PML_YALLA_RESET_PML_REQ(_pml_req) \
    { \
        (_pml_req)->mxm_base[0].state = MXM_REQ_NEW; \
        PML_YALLA_RESET_PML_REQ_DATA(_pml_req); \
    }

#define PML_YALLA_INIT_MXM_REQ_BASE(_req_base, _comm) \
    { \
        (_req_base)->state = MXM_REQ_NEW; \
        (_req_base)->mq    = (void*)(_comm)->c_pml_comm; \
    }

#define PML_YALLA_PEER_CONN(_comm, _rank) \
    ompi_comm_peer_lookup(_comm, _rank)->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PML]

#define PML_YALLA_INIT_MXM_SEND_REQ(_sreq, _buf, _count, _dtype, _rank, _tag, _mode, _comm, _stream_type, ...) \
    { \
        PML_YALLA_INIT_MXM_REQ_BASE(&(_sreq)->base, _comm); \
        PML_YALLA_INIT_MXM_REQ_DATA(&(_sreq)->base, _buf, _count, _dtype, _stream_type, ## __VA_ARGS__); \
        (_sreq)->base.conn        = PML_YALLA_PEER_CONN(_comm, _rank); \
        (_sreq)->opcode           = ((_mode) == MCA_PML_BASE_SEND_SYNCHRONOUS) ? MXM_REQ_OP_SEND_SYNC : MXM_REQ_OP_SEND; \
        (_sreq)->op.send.tag      = _tag; \
        (_sreq)->op.send.imm_data = ompi_comm_rank(_comm); \
   }

#define PML_YALLA_INIT_MXM_RECV_REQ_ENVELOPE(_rreq, _rank, _tag, _comm) \
    { \
        (_rreq)->base.conn = ((_rank) == MPI_ANY_SOURCE) ? NULL : PML_YALLA_PEER_CONN(_comm, _rank); \
        if ((_tag) == MPI_ANY_TAG) { \
            (_rreq)->tag = 0; \
            (_rreq)->tag_mask = 0x80000000u; \
        } else { \
            (_rreq)->tag = _tag; \
            (_rreq)->tag_mask  = 0xffffffffu; \
        } \
    }

#define PML_YALLA_INIT_MXM_RECV_REQ(_rreq, _buf, _count, _dtype, _rank, _tag, _comm, _stream_type, ...) \
    { \
        PML_YALLA_INIT_MXM_REQ_BASE(&(_rreq)->base, _comm); \
        PML_YALLA_INIT_MXM_REQ_DATA(&(_rreq)->base, _buf, _count, _dtype, _stream_type, ## __VA_ARGS__); \
        PML_YALLA_INIT_MXM_RECV_REQ_ENVELOPE(_rreq, _rank, _tag, _comm); \
    }

#define PML_YALLA_INIT_BLOCKING_MXM_SEND_REQ(_sreq) \
    { \
        (_sreq)->base.completed_cb = NULL; \
        (_sreq)->flags             = MXM_REQ_SEND_FLAG_BLOCKING; \
    }

#define PML_YALLA_INIT_BLOCKING_MXM_RECV_REQ(_rreq) \
    { \
        (_rreq)->base.completed_cb = NULL; \
    }

#define PML_YALLA_FREE_BLOCKING_MXM_REQ(_req) \
    { \
        if ((_req)->completed_cb != NULL) { \
            mca_pml_yalla_convertor_free((mca_pml_yalla_convertor_t*)((_req)->context)); \
        } \
    }

#define MCA_PML_YALLA_RREQ_INIT(_buf, _count, _datatype, _src, _tag, _comm, _state) \
    ({ \
        mca_pml_yalla_recv_request_t *rreq = PML_YALLA_FREELIST_GET(&ompi_pml_yalla.recv_reqs); \
        \
        PML_YALLA_INIT_OMPI_REQ(&rreq->super.ompi, _comm, _state); \
        PML_YALLA_INIT_MXM_RECV_REQ(&rreq->mxm, _buf, _count, _datatype, _src, _tag, \
                                 _comm, irecv, rreq); \
        rreq; \
    })

#define MCA_PML_YALLA_SREQ_INIT(_buf, _count, _datatype, _dst, _tag, _mode, _comm, _state) \
    ({ \
        mca_pml_yalla_send_request_t *sreq = PML_YALLA_FREELIST_GET(&ompi_pml_yalla.send_reqs); \
        \
        PML_YALLA_INIT_OMPI_REQ(&sreq->super.ompi, _comm, _state); \
        PML_YALLA_INIT_MXM_SEND_REQ(&sreq->mxm, _buf, _count, _datatype, _dst, _tag, \
                                    mode, _comm, isend, sreq); \
        sreq->super.ompi.req_status.MPI_TAG    = _tag; \
        sreq->super.ompi.req_status.MPI_SOURCE = (_comm)->c_my_rank; \
        sreq->super.ompi.req_status._ucount    = _count; \
        sreq; \
    })

#define PML_YALLA_INIT_MXM_PROBE_REQ(_rreq, _rank, _tag, _comm) \
    { \
        PML_YALLA_INIT_MXM_REQ_BASE(&(_rreq)->base, _comm); \
        PML_YALLA_INIT_MXM_RECV_REQ_ENVELOPE(_rreq, _rank, _tag, _comm); \
    }

/*
 * For multi-threaded MPI, avoid blocking inside mxm_wait(), since it prevents
 * from other threads making progress.
 */
#define PML_YALLA_WAIT_MXM_REQ(_req_base) \
    { \
        if (opal_using_threads()) { \
            while (!mxm_req_test(_req_base)) { \
                sched_yield(); \
                opal_progress(); \
            } \
        } else if (!mxm_req_test(_req_base)) { \
            mxm_wait_t wait; \
            wait.progress_cb = (mxm_progress_cb_t)opal_progress; \
            wait.progress_arg = NULL; \
            wait.req          = (_req_base); \
            wait.state        = MXM_REQ_COMPLETED; \
            mxm_wait(&wait); \
        } \
    }

#define PML_YALLA_SET_RECV_STATUS(_rreq, _length, _mpi_status) \
    { \
        if ((_mpi_status) != MPI_STATUS_IGNORE) { \
            switch ((_rreq)->base.error) { \
            case MXM_OK: \
                (_mpi_status)->MPI_ERROR  = OMPI_SUCCESS; \
                break; \
            case MXM_ERR_CANCELED: \
                (_mpi_status)->_cancelled = true; \
                break; \
            case MXM_ERR_MESSAGE_TRUNCATED: \
                (_mpi_status)->MPI_ERROR  = MPI_ERR_TRUNCATE; \
                break; \
            default: \
                (_mpi_status)->MPI_ERROR  = MPI_ERR_INTERN; \
                break; \
            } \
            \
            (_mpi_status)->MPI_TAG    = (_rreq)->completion.sender_tag; \
            (_mpi_status)->MPI_SOURCE = (_rreq)->completion.sender_imm; \
            (_mpi_status)->_ucount    = (_length); \
        } \
    }

#define PML_YALLA_SET_MESSAGE(_rreq, _comm, _mxm_msg, _message) \
    { \
        *(_message) = ompi_message_alloc(); \
        (*(_message))->comm    = (_comm); \
        (*(_message))->count   = (_rreq)->completion.sender_len; \
        (*(_message))->peer    = (_rreq)->completion.sender_imm; \
        (*(_message))->req_ptr = (_mxm_msg); \
    }

#define PML_YALLA_MESSAGE_RELEASE(_message) \
    { \
        ompi_message_return(*(_message)); \
        *(_message) = NULL; \
    }

#endif /* PML_YALLA_REQUEST_H_ */
