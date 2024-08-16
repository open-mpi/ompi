/*
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2017      Intel, Inc. All rights reserved
 * Copyright (c) 2019-2021 The University of Tennessee at Chattanooga and The University
 *                         of Tennessee Research Foundation. All rights reserved.
 * Copyright (c) 2019-2021 Sandia National Laboratories. All rights reserved.
 * Copyright (c) 2021      University of Alabama at Birmingham. All rights reserved.
 * Copyright (c) 2021      Tennessee Technological University. All rights reserved.
 * Copyright (c) 2021      Cisco Systems, Inc.  All rights reserved
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PART_DIRECT_H
#define PART_DIRECT_H

#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#endif

#include<math.h>

#include "ompi_config.h"
#include "ompi/request/request.h"
#include "ompi/mca/part/part.h"
#include "ompi/mca/part/base/base.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/communicator/communicator.h"
#include "ompi/request/request.h"
#include "opal/sys/atomic.h"

#include "ompi/mca/part/direct/part_direct_request.h"
#include "ompi/mca/part/base/part_base_precvreq.h"
#include "ompi/mca/part/direct/part_direct_recvreq.h"
#include "ompi/mca/part/direct/part_direct_sendreq.h"
#include "ompi/message/message.h"
#include "ompi/mca/pml/pml.h"
BEGIN_C_DECLS

typedef struct mca_part_direct_list_t {
    opal_list_item_t        super;
    mca_part_direct_request_t *item;
} mca_part_direct_list_t;

OPAL_DECLSPEC OBJ_CLASS_DECLARATION(mca_part_direct_list_t);


struct ompi_part_direct_t {
    mca_part_base_module_t super;
    int                    free_list_num;
    int                    free_list_max;
    int                    free_list_inc;
    opal_list_t           *progress_list;

    opal_atomic_int32_t    block_entry;
    opal_mutex_t lock; 
};
typedef struct ompi_part_direct_t ompi_part_direct_t;
extern ompi_part_direct_t ompi_part_direct;


/**
 * This is a helper function that frees a request. This requires ompi_part_direct.lock be held before calling.
 */
__opal_attribute_always_inline__ static inline int
mca_part_direct_free_req(struct mca_part_direct_request_t* req)
{
    int err = OMPI_SUCCESS;
    size_t i;
    opal_list_remove_item(ompi_part_direct.progress_list, (opal_list_item_t*)req->progress_elem);
    OBJ_RELEASE(req->progress_elem);

    // TODO: Close Window
    // TODO: Close Flag Window
    // TODO: Free Communicator
    free(req->flags);

    if( MCA_PART_DIRECT_REQUEST_PRECV == req->req_type ) {
        MCA_PART_DIRECT_PRECV_REQUEST_RETURN(req);
    } else {
        MCA_PART_DIRECT_PSEND_REQUEST_RETURN(req);
    }
    return err;
}


__opal_attribute_always_inline__ static inline void mca_part_direct_init_lists(void)
{
    opal_free_list_init (&mca_part_base_precv_requests,
                         sizeof(mca_part_direct_precv_request_t),
                         opal_cache_line_size,
                         OBJ_CLASS(mca_part_direct_precv_request_t),
                         0,opal_cache_line_size,
                         ompi_part_direct.free_list_num,
                         ompi_part_direct.free_list_max,
                         ompi_part_direct.free_list_inc,
                         NULL, 0, NULL, NULL, NULL);
    opal_free_list_init (&mca_part_base_psend_requests,
                         sizeof(mca_part_direct_psend_request_t),
                         opal_cache_line_size,
                         OBJ_CLASS(mca_part_direct_psend_request_t),
                         0,opal_cache_line_size,
                         ompi_part_direct.free_list_num,
                         ompi_part_direct.free_list_max,
                         ompi_part_direct.free_list_inc,
                         NULL, 0, NULL, NULL, NULL);
     ompi_part_direct.progress_list = OBJ_NEW(opal_list_t);
}

__opal_attribute_always_inline__ static inline void
mca_part_direct_complete(struct mca_part_direct_request_t* request)
{
    if(MCA_PART_DIRECT_REQUEST_PRECV == request->req_type) {
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

/**
 * mca_part_direct_progress is the progress function that will be registered. It handles 
 * both send and recv request testing and completion. It also handles freeing requests,
 * after MPI_Free is called and the requests have become inactive.
 */
__opal_attribute_always_inline__ static inline int
mca_part_direct_progress(void)
{
    mca_part_direct_list_t *current;
    int err;
    size_t i;

    /* prevent re-entry, */
    int block_entry = opal_atomic_add_fetch_32(&(ompi_part_direct.block_entry), 1);
    if(1 < block_entry)
    {
        block_entry = opal_atomic_add_fetch_32(&(ompi_part_direct.block_entry), -1);
        return OMPI_SUCCESS;
    }

    OPAL_THREAD_LOCK(&ompi_part_direct.lock);
 
    mca_part_direct_request_t* to_delete = NULL;

    OPAL_LIST_FOREACH(current, ompi_part_direct.progress_list, mca_part_direct_list_t) {
        mca_part_direct_request_t *req = (mca_part_direct_request_t *) current->item;
        if(MCA_PART_DIRECT_REQUEST_PSEND == req->req_type)
        {
            if(false == req->req_part_complete && REQUEST_COMPLETED != req->req_ompi.req_complete && OMPI_REQUEST_ACTIVE == req->req_ompi.req_state) {
               for(i = 0; i < req->parts; i++) {
                    /* Check to see if partition is queued for being started. Only applicable to sends. */ 
                    if(-2 ==  req->flags[i]) {
                        //TODO Put
                        req->flags[i] = 0;
                    }
                }

                /* Check for completion and complete the requests */
                if(req->done_count == req->parts)
                {
                    // TODO: INC tround.
                    mca_part_direct_complete(req);
                }

	    }	
        } else {
            //TODO RECV complete
	}

        if(true == req->req_free_called && true == req->req_part_complete && REQUEST_COMPLETED == req->req_ompi.req_complete &&  OMPI_REQUEST_INACTIVE == req->req_ompi.req_state) {
            to_delete = req;
        }
    }
    OPAL_THREAD_UNLOCK(&ompi_part_direct.lock);
    block_entry = opal_atomic_add_fetch_32(&(ompi_part_direct.block_entry), -1);
    if(to_delete) {
        err =  mca_part_direct_free_req(to_delete);
        if (OMPI_SUCCESS != err) {
            return OMPI_ERROR;
        }
    }

    return OMPI_SUCCESS;
}

__opal_attribute_always_inline__ static inline int
mca_part_direct_precv_init(void *buf,
                        size_t parts, 
                        size_t count,
                        ompi_datatype_t * datatype,
                        int src,
                        int tag,
                        struct ompi_communicator_t *comm,
			struct ompi_info_t * info,
                        struct ompi_request_t **request)
{
    int err = OMPI_SUCCESS;
    size_t dt_size_;
    int dt_size;
    mca_part_direct_list_t* new_progress_elem = NULL;

    fprintf(stderr,"precv_init\n");

    mca_part_direct_precv_request_t *recvreq;


    /* Allocate a new request */
    MCA_PART_DIRECT_PRECV_REQUEST_ALLOC(recvreq);
    if (OPAL_UNLIKELY(NULL == recvreq)) return OMPI_ERR_OUT_OF_RESOURCE;

    MCA_PART_DIRECT_PRECV_REQUEST_INIT(recvreq, ompi_proc, comm, tag, src,
                                     datatype, buf, parts, count, flags);

    mca_part_direct_request_t *req = (mca_part_direct_request_t *) recvreq;

    /* Set lazy initializion flags */
    req->flags = NULL;
    /* Non-blocking recive on setup info */

    /* Compute total number of bytes */
    err = opal_datatype_type_size(&(req->req_datatype->super), &dt_size_);
    if(OMPI_SUCCESS != err) return OMPI_ERROR;
    dt_size = (dt_size_ > (size_t) INT_MAX) ? MPI_UNDEFINED : (int) dt_size_;
    req->req_bytes = parts * count * dt_size;


    /* Set ompi request initial values */
    req->req_ompi.req_persistent = true;
    req->req_part_complete = true;
    req->req_ompi.req_type = OMPI_REQUEST_PART;
    req->req_ompi.req_complete = REQUEST_COMPLETED;
    req->req_ompi.req_state = OMPI_REQUEST_INACTIVE;

    /* Add element to progress engine */
    new_progress_elem = OBJ_NEW(mca_part_direct_list_t);
    new_progress_elem->item = req;
    req->progress_elem = new_progress_elem; 
    OPAL_THREAD_LOCK(&ompi_part_direct.lock);
    opal_list_append(ompi_part_direct.progress_list, (opal_list_item_t*)new_progress_elem);
    OPAL_THREAD_UNLOCK(&ompi_part_direct.lock);

    /* set return values */
    *request = (ompi_request_t*) recvreq;
    return err;
}

__opal_attribute_always_inline__ static inline int
mca_part_direct_psend_init(const void* buf,
                        size_t parts,
                        size_t count,
                        ompi_datatype_t* datatype,
                        int dst,
                        int tag,
                        ompi_communicator_t* comm,
			struct ompi_info_t * info,
                        ompi_request_t** request)
{
    int err = OMPI_SUCCESS;
    size_t dt_size_;
    int dt_size;
    mca_part_direct_list_t* new_progress_elem = NULL;
    mca_part_direct_psend_request_t *sendreq;
    fprintf(stderr, "psend_init\n");

    /* Create new request object */
    MCA_PART_DIRECT_PSEND_REQUEST_ALLOC(sendreq, comm, dst, ompi_proc);
    if (OPAL_UNLIKELY(NULL == sendreq)) return OMPI_ERR_OUT_OF_RESOURCE;
    MCA_PART_DIRECT_PSEND_REQUEST_INIT(sendreq, ompi_proc, comm, tag, dst,
                                    datatype, buf, parts, count, flags);
    mca_part_direct_request_t *req = (mca_part_direct_request_t *) sendreq;


    /* Determine total bytes to send. */
    err = opal_datatype_type_size(&(req->req_datatype->super), &dt_size_);
    if(OMPI_SUCCESS != err) return OMPI_ERROR;
    dt_size = (dt_size_ > (size_t) INT_MAX) ? MPI_UNDEFINED : (int) dt_size_;
    req->req_bytes = parts * count * dt_size;


    req->parts = parts;
    req->count = count;


    req->flags = (int*) calloc(req->parts, sizeof(int));


    // TODO Make Comm
    // TODO Make Window
    // TODO Make Flag Window


    /* Initilaize completion variables */
    sendreq->req_base.req_ompi.req_persistent = true;
    req->req_part_complete = true;
    req->req_ompi.req_type = OMPI_REQUEST_PART;
    req->req_ompi.req_complete = REQUEST_COMPLETED;
    req->req_ompi.req_state = OMPI_REQUEST_INACTIVE;
 
    /* add element to progress queue */
    new_progress_elem = OBJ_NEW(mca_part_direct_list_t);
    new_progress_elem->item = req;
    req->progress_elem = new_progress_elem;
    OPAL_THREAD_LOCK(&ompi_part_direct.lock);
    opal_list_append(ompi_part_direct.progress_list, (opal_list_item_t*)new_progress_elem);
    OPAL_THREAD_UNLOCK(&ompi_part_direct.lock);

    /* Set return values */
    *request = (ompi_request_t*) sendreq;
    return err;
}

__opal_attribute_always_inline__ static inline int
mca_part_direct_start(size_t count, ompi_request_t** requests)
{
    int err = OMPI_SUCCESS;
    size_t _count = count;
    size_t i;

    fprintf(stderr,"Yay we crashed in the right spot at least?\n");

    // TODO do we need to anthing else RMA here.
    // TODO Incriment round variable?

    for(i = 0; i < _count && OMPI_SUCCESS == err; i++) {
        mca_part_direct_request_t *req = (mca_part_direct_request_t *)(requests[i]);
        /* First use is a special case, to support lazy initialization */
        if(MCA_PART_DIRECT_REQUEST_PSEND == req->req_type) {
            req->done_count = 0;
            for(i = 0; i < req->parts && OMPI_SUCCESS == err; i++) {
                req->flags[i] = -1;
            }
        } else {
            req->done_count = 0;
	    // TODO INC RTS round.
        } 
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
mca_part_direct_pready(size_t min_part,
                    size_t max_part,
                    ompi_request_t* request)
{
    int err = OMPI_SUCCESS;
    size_t i;

    mca_part_direct_request_t *req = (mca_part_direct_request_t *)(request);
    if(req->round == req->tround)
    {
        // TODO MPI PUT 
        for(i = min_part; i <= max_part && OMPI_SUCCESS == err; i++) {
            req->flags[i] = 0; /* Mark partion as ready for testing */
        }
    }
    else
    {
        for(i = min_part; i <= max_part && OMPI_SUCCESS == err; i++) {
            req->flags[i] = -2; /* Mark partition as queued */
        }
    }
    return err;
}

__opal_attribute_always_inline__ static inline int
mca_part_direct_parrived(size_t min_part,
                      size_t max_part,
                      int* flag, 
                      ompi_request_t* request)
{
    int err = OMPI_SUCCESS;
    mca_part_direct_request_t *req = (mca_part_direct_request_t *)request;

    *flag = (req->round == req->tround); /* Rationale: RMA is performant implementation for n->1 partitions, and this is an opt-in performance version, we implement all partitioned communications as n->1 for this module. */
    return err;
}


/**
 * mca_part_direct_free marks an entry as free called and sets the request to 
 * MPI_REQUEST_NULL. Note: requests get freed in the progress engine. 
 */
__opal_attribute_always_inline__ static inline int
mca_part_direct_free(ompi_request_t** request)
{
    mca_part_direct_request_t* req = *(mca_part_direct_request_t**)request;

    if(true == req->req_free_called) return OMPI_ERROR;
    req->req_free_called = true;

    *request = MPI_REQUEST_NULL;
    return OMPI_SUCCESS;
}

END_C_DECLS

#endif  /* PART_DIRECT_H_HAS_BEEN_INCLUDED */
