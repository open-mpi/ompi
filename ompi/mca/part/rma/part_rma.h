/*
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2017      Intel, Inc. All rights reserved
 * Copyright (c) 2019-2020 The University of Tennessee at Chattanooga and The University
 *                         of Tennessee Research Foundation. All rights 
 *                         reserved.
 * Copyright (c) 2019-2020 Sandia National Laboratories. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PART_RMA_H
#define PART_RMA_H

#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#endif

#include "ompi_config.h"
#include "ompi/request/request.h"
#include "ompi/mca/part/part.h"
#include "ompi/mca/part/base/base.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/communicator/communicator.h"
#include "ompi/request/request.h"
#include "opal/sys/atomic.h"

#include "part_rma_request.h"
#include "ompi/mca/part/base/part_base_precvreq.h"
#include "part_rma_recvreq.h"
#include "part_rma_sendreq.h"
#include "ompi/message/message.h"

BEGIN_C_DECLS

typedef struct mca_part_rma_list_t {
    opal_list_item_t        super;
    mca_part_rma_request_t *item;
} mca_part_rma_list_t;

OPAL_DECLSPEC OBJ_CLASS_DECLARATION(mca_part_rma_list_t);


struct ompi_part_rma_t {
    mca_part_base_module_t super;
    int                    free_list_num;
    int                    free_list_max;
    int                    free_list_inc;
    opal_list_t           *progress_list;

    /* This is used to protect the progress engine, in cases were progress threads and thread multiple are active. */
    opal_mutex_t lock; 
};
typedef struct ompi_part_rma_t ompi_part_rma_t;
extern ompi_part_rma_t ompi_part_rma;

/**
 * This is a helper function that frees a request. This requires ompi_part_rma.lock be held before calling.
 */
__opal_attribute_always_inline__ static inline int
mca_part_rma_free_req(struct mca_part_rma_request_t* req)
{
    int err = OMPI_SUCCESS;

    opal_list_remove_item(ompi_part_rma.progress_list, (opal_list_item_t*)req->progress_elem);
    OBJ_RELEASE(req->progress_elem);
    err = MPI_Win_flush_all(req->req_flags_window);
    if(MPI_SUCCESS != err) return OMPI_ERROR;
    err = MPI_Win_flush_all(req->req_flags_window);
    if(MPI_SUCCESS != err) return OMPI_ERROR;
    err = MPI_Win_unlock_all(req->req_data_window);
    if(MPI_SUCCESS != err) return OMPI_ERROR;
    err = MPI_Win_unlock_all(req->req_flags_window);
    if(MPI_SUCCESS != err) return OMPI_ERROR;

    err = MPI_Comm_free(&(req->req_window_comm));
    if(MPI_SUCCESS != err) return OMPI_ERROR;

    free((void*)req->req_counters);
    free((void*)req->req_flags);

    if( MCA_PART_RMA_REQUEST_PRECV == req->req_type ) {
        MCA_PART_RMA_PRECV_REQUEST_RETURN(req);
    } else {
        MCA_PART_RMA_PSEND_REQUEST_RETURN(req);
    }
    return err;
}


/**
 * This is a helper function for the progress engine, it checks a single request for completion
 * and sets the complete flag based on that. 
 */
__opal_attribute_always_inline__ static inline int
mca_part_rma_test(struct mca_part_rma_request_t* request)
{
    int finished = 1;
    int i;
    for(i = 0; i < request->req_flags_size; i++) {
        finished = request->req_flags[i] && finished;
    }
    if(1 == finished) {
        if(MCA_PART_RMA_REQUEST_PRECV == request->req_type) {
            request->req_ompi.req_status.MPI_SOURCE = request->req_peer; 
        } else {
            request->req_ompi.req_status.MPI_SOURCE = request->req_comm->c_my_rank;
        }
        request->req_ompi.req_complete_cb = NULL;
        request->req_ompi.req_status.MPI_TAG = request->req_tag;  
        request->req_ompi.req_status._ucount = request->req_bytes;
        request->req_ompi.req_status.MPI_ERROR = OMPI_SUCCESS;
        request->req_part_complete = true;
        ompi_request_complete(&(request->req_ompi), true );
    }
    return finished;
}

__opal_attribute_always_inline__ static inline void mca_part_rma_init_lists(void)
{
    opal_free_list_init (&mca_part_base_precv_requests,
                         sizeof(mca_part_rma_precv_request_t),
                         opal_cache_line_size,
                         OBJ_CLASS(mca_part_rma_precv_request_t),
                         0,opal_cache_line_size,
                         ompi_part_rma.free_list_num,
                         ompi_part_rma.free_list_max,
                         ompi_part_rma.free_list_inc,
                         NULL, 0, NULL, NULL, NULL);
    opal_free_list_init (&mca_part_base_psend_requests,
                         sizeof(mca_part_rma_psend_request_t),
                         opal_cache_line_size,
                         OBJ_CLASS(mca_part_rma_psend_request_t),
                         0,opal_cache_line_size,
                         ompi_part_rma.free_list_num,
                         ompi_part_rma.free_list_max,
                         ompi_part_rma.free_list_inc,
                         NULL, 0, NULL, NULL, NULL);
     ompi_part_rma.progress_list = OBJ_NEW(opal_list_t);
}

/**
 * mca_part_rma_progress is the progress function that will be registered. It handles 
 * both send and recv request testing and completion. It also handles freeing requests,
 * after MPI_Free is called and the requests have become inactive.
 */
__opal_attribute_always_inline__ static inline int
mca_part_rma_progress(void)
{
    mca_part_rma_list_t *current;

    OPAL_THREAD_LOCK(&ompi_part_rma.lock);
 
    mca_part_rma_request_t* to_delete = NULL;

    OPAL_LIST_FOREACH(current, ompi_part_rma.progress_list, mca_part_rma_list_t) {
        mca_part_rma_request_t *req = (mca_part_rma_request_t *) current->item;

        if(false == req->req_part_complete && REQUEST_COMPLETED != req->req_ompi.req_complete && OMPI_REQUEST_ACTIVE == req->req_ompi.req_state) {
            mca_part_rma_test(req);
        }

        if(true == req->req_free_called && true == req->req_part_complete && REQUEST_COMPLETED == req->req_ompi.req_complete &&  OMPI_REQUEST_INACTIVE == req->req_ompi.req_state) {
            to_delete = req;
        }

    }
    if(to_delete) {
        int err = mca_part_rma_free_req(to_delete);
        if(MPI_SUCCESS != err) return OMPI_ERROR;
    }
    OPAL_THREAD_UNLOCK(&ompi_part_rma.lock);

    return OMPI_SUCCESS;
}

/*
 * This is a helper function for creating a communicater in psend_init and precv_init
 */
OMPI_DECLSPEC extern int mca_part_rma_create_pcomm(ompi_communicator_t* comm,
                                                   int rank_count,
                                                   const int ranks[],
                                                   int tag,
                                                   ompi_communicator_t** new_comm);

/*
 * This is a helper function for the init calls
 */
__opal_attribute_always_inline__ static inline int 
mca_part_rma_gcd(int a, int b)
{
    if (0 == a || 0 == b) return 0;
    else if (a == b)      return a;
    else if (a > b)       return mca_part_rma_gcd(a-b, b);
    else                  return mca_part_rma_gcd(a, b-a);
}

__opal_attribute_always_inline__ static inline int
mca_part_rma_precv_init(void *buf,
                        size_t parts, 
                        size_t count,
                        ompi_datatype_t * datatype,
                        int src,
                        int tag,
                        struct ompi_communicator_t *comm,
                        struct ompi_request_t **request)
{
    int err = MPI_SUCCESS;
    int rank_super;
    int ranks[2];
    int remote_part = 0;
    int dt_size;
    mca_part_rma_list_t* new_progress_elem = NULL;
    mca_part_rma_precv_request_t *recvreq;

    /* Allocate a new request */
    MCA_PART_RMA_PRECV_REQUEST_ALLOC(recvreq);
    if (OPAL_UNLIKELY(NULL == recvreq)) return OMPI_ERR_OUT_OF_RESOURCE;

    MCA_PART_RMA_PRECV_REQUEST_INIT(recvreq, ompi_proc, comm, tag, src,
                                     datatype, buf, parts, count, flags);

    mca_part_rma_request_t *req = (mca_part_rma_request_t *) recvreq;

    /* Initialize the new request */
    err = MPI_Comm_rank(comm, &rank_super);
    if(MPI_SUCCESS != err) return OMPI_ERROR; 

    ranks[0] = src; ranks[1] = rank_super;
    mca_part_rma_create_pcomm(comm, 2, ranks, tag, &req->req_window_comm);

    err = MPI_Recv(&remote_part, 1, MPI_INT, 0, 0, req->req_window_comm, MPI_STATUS_IGNORE);
    if(MPI_SUCCESS != err) return OMPI_ERROR;
    err = MPI_Send(&parts, 1, MPI_INT, 0, 0, req->req_window_comm);
    if(MPI_SUCCESS != err) return OMPI_ERROR;

    req->req_flags_size = mca_part_rma_gcd(parts,remote_part);
    req->req_counter_thresh = parts / req->req_flags_size;

    req->req_counters = (int*) malloc(sizeof(int)* req->req_flags_size);
    req->req_flags  = (int*) malloc(sizeof(int)* req->req_flags_size);

    err = MPI_Type_size(req->req_datatype, &dt_size);
    if(MPI_SUCCESS != err) return OMPI_ERROR;
    req->req_bytes = parts * count * dt_size;
    err = MPI_Win_create((void*)(req->req_addr),
                         parts * count * dt_size,
                         dt_size,
                         MPI_INFO_NULL,
                         req->req_window_comm,
                         &req->req_data_window);
    if(MPI_SUCCESS != err) return OMPI_ERROR;
 
    err = MPI_Win_lock_all(0, req->req_data_window);
    if(MPI_SUCCESS != err) return OMPI_ERROR;

    err = MPI_Win_create(req->req_flags,
                         req->req_flags_size * sizeof(int),
                         sizeof(int),
                         MPI_INFO_NULL,
                         req->req_window_comm,
                         &req->req_flags_window);
    if(MPI_SUCCESS != err) return OMPI_ERROR;

    err = MPI_Win_lock_all(0, req->req_flags_window);
    if(MPI_SUCCESS != err) return OMPI_ERROR;

    new_progress_elem = OBJ_NEW(mca_part_rma_list_t);
    new_progress_elem->item = req;
    req->progress_elem = new_progress_elem; 

    req->req_ompi.req_persistent = true;
    req->req_part_complete = true;
    req->req_ompi.req_complete = REQUEST_COMPLETED;
    req->req_ompi.req_state = OMPI_REQUEST_INACTIVE;

    *request = (ompi_request_t*) recvreq;

    OPAL_THREAD_LOCK(&ompi_part_rma.lock);
    opal_list_append(ompi_part_rma.progress_list, (opal_list_item_t*)new_progress_elem);
    OPAL_THREAD_UNLOCK(&ompi_part_rma.lock);

    return err;
}

__opal_attribute_always_inline__ static inline int
mca_part_rma_psend_init(const void* buf,
                        size_t parts,
                        size_t count,
                        ompi_datatype_t* datatype,
                        int dst,
                        int tag,
                        ompi_communicator_t* comm,
                        ompi_request_t** request)
{
    int err = MPI_SUCCESS;
    int rank_super;
    int ranks[2];
    int remote_part = 0;
    int dt_size;
    mca_part_rma_list_t* new_progress_elem = NULL;
    mca_part_rma_psend_request_t *sendreq;



    MCA_PART_RMA_PSEND_REQUEST_ALLOC(sendreq, comm, dst, ompi_proc);
    if (OPAL_UNLIKELY(NULL == sendreq)) return OMPI_ERR_OUT_OF_RESOURCE;

    MCA_PART_RMA_PSEND_REQUEST_INIT(sendreq, ompi_proc, comm, tag, dst,
                                    datatype, buf, parts, count, flags);

    mca_part_rma_request_t *req = (mca_part_rma_request_t *) sendreq;

    err = MPI_Comm_rank(comm, &rank_super);
    if(MPI_SUCCESS != err) return OMPI_ERROR;

    ranks[0] = rank_super; ranks[1] = dst;
    mca_part_rma_create_pcomm(comm, 2, ranks, tag, &req->req_window_comm); 

    err = MPI_Send(&parts, 1, MPI_INT, 1, 0, req->req_window_comm);
    if(MPI_SUCCESS != err) return OMPI_ERROR;

    err = MPI_Recv(&remote_part, 1, MPI_INT, 1, 0, req->req_window_comm, MPI_STATUS_IGNORE);
    if(MPI_SUCCESS != err) return OMPI_ERROR;

    req->req_flags_size = mca_part_rma_gcd(parts,remote_part);
    req->req_counter_thresh = parts / req->req_flags_size;

    req->req_counters = (int*) malloc(sizeof(int)* req->req_flags_size);
    req->req_flags    = (int*) malloc(sizeof(int)* req->req_flags_size);

    err = MPI_Type_size(req->req_datatype, &dt_size);
    if(MPI_SUCCESS != err) return OMPI_ERROR;
    req->req_bytes = parts * count * dt_size;

    err = MPI_Win_create(0,
                         0,
                         dt_size,
                         MPI_INFO_NULL,
                         req->req_window_comm,
                         &req->req_data_window);
    if(MPI_SUCCESS != err) return OMPI_ERROR;

    err = MPI_Win_lock_all(0, req->req_data_window);
    if(MPI_SUCCESS != err) return OMPI_ERROR;

    err = MPI_Win_create(0,
                         0,
                         sizeof(int),
                         MPI_INFO_NULL,
                         req->req_window_comm,
                         &req->req_flags_window);
    if(MPI_SUCCESS != err) return OMPI_ERROR;

    err = MPI_Win_lock_all(0, req->req_flags_window);
    if(MPI_SUCCESS != err) return OMPI_ERROR;

    new_progress_elem = OBJ_NEW(mca_part_rma_list_t);
    new_progress_elem->item = req;
    req->progress_elem = new_progress_elem; 

    sendreq->req_base.req_ompi.req_persistent = true;


    req->req_part_complete = true;
    req->req_ompi.req_complete = REQUEST_COMPLETED;
    req->req_ompi.req_state = OMPI_REQUEST_INACTIVE;

    *request = (ompi_request_t*) sendreq;

    OPAL_THREAD_LOCK(&ompi_part_rma.lock);
    opal_list_append(ompi_part_rma.progress_list, (opal_list_item_t*)new_progress_elem);
    OPAL_THREAD_UNLOCK(&ompi_part_rma.lock);

    return err;
}

__opal_attribute_always_inline__ static inline int
mca_part_rma_start(size_t count, ompi_request_t** requests)
{
    int err = MPI_SUCCESS;
    size_t i;
    int32_t j;

    for(i = 0; i < count; i++) {
        mca_part_rma_request_t *req = (mca_part_rma_request_t*) requests[i];
        if(true == req->req_free_called) return OMPI_ERROR;

        for(j = 0; j < req->req_flags_size; ++j) {
            req->req_counters[j] = req->req_flags[j] = 0;
        }

        /* init/re-init the request */                                      
        req->req_ompi.req_state = OMPI_REQUEST_ACTIVE;         
        req->req_ompi.req_status.MPI_TAG = MPI_ANY_TAG;       
        req->req_ompi.req_status.MPI_ERROR = OMPI_SUCCESS;     
        req->req_ompi.req_status._cancelled = 0;               
        req->req_part_complete = false;
        req->req_ompi.req_complete = false;
        OPAL_ATOMIC_SWAP_PTR(&req->req_ompi.req_complete, REQUEST_PENDING);
    }
    return err;
}

__opal_attribute_always_inline__ static inline int
mca_part_rma_pready(size_t min_part,
                    size_t max_part,
                    ompi_request_t* request)
{
    int err = MPI_SUCCESS;
    size_t i;
    for(i = min_part; i <= max_part; i++) {
        int datatype_size;
        int receiver = 1;
        MPI_Aint displacement;
        void* head;
        int bin;
        int32_t current_bin_size;

        mca_part_rma_request_t *req = (mca_part_rma_request_t *)request;

        err = MPI_Type_size(req->req_datatype, &datatype_size);
        if(MPI_SUCCESS != err) return OMPI_ERROR;

        displacement = i * req->req_count;
        head = ((char*)req->req_addr)+(displacement * datatype_size);
        err = MPI_Put(head, req->req_count, req->req_datatype, receiver,
                     displacement, req->req_count, req->req_datatype, req->req_data_window);
        if(MPI_SUCCESS != err) return OMPI_ERROR;

        bin = i / req->req_counter_thresh;
        current_bin_size = opal_atomic_add_fetch_32(&(req->req_flags[bin]), 1);

        if(current_bin_size == req->req_counter_thresh) {
            req->req_flags[bin] = 1;

            err = MPI_Win_flush_all(req->req_data_window);
            if(MPI_SUCCESS != err) return OMPI_ERROR;

            err = MPI_Put(&(req->req_flags[bin]), 1, MPI_INT, receiver, bin, 1, MPI_INT, req->req_flags_window);
            if(MPI_SUCCESS != err) return OMPI_ERROR;

            err = MPI_Win_flush_all(req->req_flags_window);
            if(MPI_SUCCESS != err) return OMPI_ERROR;
        }

    }
    return err;
}

__opal_attribute_always_inline__ static inline int
mca_part_rma_parrived(size_t min_part,
                      size_t max_part,
                      int* flag, 
                      ompi_request_t* request)
{
    int err = MPI_SUCCESS;
    size_t i, index;
    int _flag = 1;
    for(i = min_part; i <= max_part; i++) {
        mca_part_rma_request_t *req = (mca_part_rma_request_t *)request;
        index = i / req->req_counter_thresh;

        _flag = req->req_flags[index] && _flag;
    }
    *flag = _flag;
    return err;
}


/**
 * mca_part_rma_free marks an entry as free called and sets the request to 
 * MPI_REQUEST_NULL. Note: requests get freed in the progress engine. 
 */
__opal_attribute_always_inline__ static inline int
mca_part_rma_free(ompi_request_t** request)
{
    mca_part_rma_request_t* req = *(mca_part_rma_request_t**)request;

    if(true == req->req_free_called) return OMPI_ERROR;
    req->req_free_called = true;

    *request = MPI_REQUEST_NULL;
    return OMPI_SUCCESS;
}

END_C_DECLS

#endif  /* PART_RMA_H_HAS_BEEN_INCLUDED */
