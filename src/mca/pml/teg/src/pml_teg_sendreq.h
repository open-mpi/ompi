/*
 * $HEADER$
 */
/**
 * @file
 */
#ifndef OMPI_PML_TEG_SEND_REQUEST_H
#define OMPI_PML_TEG_SEND_REQUEST_H

#include "mca/ptl/ptl.h"
#include "mca/pml/base/pml_base_sendreq.h"
#include "mca/ptl/base/ptl_base_sendfrag.h"
#include "mca/ptl/base/ptl_base_comm.h"
#include "pml_teg_proc.h"
#include "pml_teg_ptl.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
typedef mca_pml_base_send_request_t mca_pml_teg_send_request_t;

OBJ_CLASS_DECLARATION(mca_pml_teg_send_request_t);


#define MCA_PML_TEG_SEND_REQUEST_ALLOC(                                    \
    comm,                                                                  \
    dst,                                                                   \
    sendreq,                                                               \
    rc)                                                                    \
{                                                                          \
    mca_pml_proc_t *proc = mca_pml_teg_proc_lookup_remote(comm,dst);       \
    mca_ptl_proc_t* ptl_proc;                                              \
    mca_pml_base_ptl_t* ptl_base;                                          \
                                                                           \
    OMPI_THREAD_SCOPED_LOCK(&proc->proc_lock,                              \
       (ptl_proc = mca_ptl_array_get_next(&proc->proc_ptl_first)));        \
    ptl_base = ptl_proc->ptl_base;                                         \
    /*                                                                     \
     * check to see if there is a cache of send requests associated with   \
     * this ptl - if so try the allocation from there.                     \
    */                                                                     \
    if(NULL != ptl_base) {                                                 \
        OMPI_THREAD_LOCK(&ptl_base->ptl_cache_lock);                       \
        sendreq = (mca_pml_base_send_request_t*)                           \
            ompi_list_remove_first(&ptl_base->ptl_cache);                  \
        if(NULL != sendreq) {                                              \
            OMPI_THREAD_UNLOCK(&ptl_base->ptl_cache_lock);                 \
            rc = OMPI_SUCCESS;                                             \
        } else if (ptl_base->ptl_cache_alloc < ptl_base->ptl_cache_size) { \
            /*                                                             \
             * allocate an additional request to the cache                 \
            */                                                             \
            mca_ptl_base_module_t* ptl = ptl_base->ptl;                    \
            ompi_list_item_t* item;                                        \
            OMPI_FREE_LIST_WAIT(&mca_pml_teg.teg_send_requests, item, rc); \
            sendreq = (mca_pml_base_send_request_t*)item;                  \
            sendreq->req_ptl = ptl;                                        \
            if(ptl->ptl_request_init(ptl, sendreq) == OMPI_SUCCESS) {      \
                sendreq->req_cached = true;                                \
                ptl_base->ptl_cache_alloc++;                               \
            }                                                              \
            OMPI_THREAD_UNLOCK(&ptl_base->ptl_cache_lock);                 \
        } else {                                                           \
            /*                                                             \
             * take a request from the global pool                         \
            */                                                             \
            ompi_list_item_t* item;                                        \
            OMPI_THREAD_UNLOCK(&ptl_base->ptl_cache_lock);                 \
            OMPI_FREE_LIST_WAIT(&mca_pml_teg.teg_send_requests, item, rc); \
            sendreq = (mca_pml_base_send_request_t*)item;                  \
            sendreq->req_ptl = ptl_proc->ptl;                              \
        }                                                                  \
                                                                           \
    /* otherwise - take the allocation from the global list */             \
    } else {                                                               \
        ompi_list_item_t* item;                                            \
        OMPI_FREE_LIST_WAIT(&mca_pml_teg.teg_send_requests, item, rc);     \
        sendreq = (mca_pml_base_send_request_t*)item;                      \
        sendreq->req_ptl = ptl_proc->ptl;                                  \
    }                                                                      \
    /* update request to point to current peer */                          \
    sendreq->req_peer = ptl_proc->ptl_peer;                                \
}


#define MCA_PML_TEG_SEND_REQUEST_RETURN(sendreq)                           \
{                                                                          \
    mca_ptl_base_module_t* ptl = (sendreq)->req_ptl;                       \
    mca_pml_base_ptl_t* ptl_base = ptl->ptl_base;                          \
                                                                           \
    /*  Decrement reference count on communicator. */                      \
    OBJ_RELEASE((sendreq)->req_base.req_comm);                             \
                                                                           \
    /*                                                                     \
     * If there is a cache associated with the ptl - first attempt         \
     * to return the send descriptor to the cache.                         \
     */                                                                    \
    if(NULL != ptl->ptl_base && (sendreq)->req_cached) {                   \
        OMPI_THREAD_LOCK(&ptl_base->ptl_cache_lock);                       \
        ompi_list_prepend(&ptl_base->ptl_cache,                            \
                          (ompi_list_item_t*)sendreq);                     \
        OMPI_THREAD_UNLOCK(&ptl_base->ptl_cache_lock);                     \
    } else {                                                               \
        OMPI_FREE_LIST_RETURN(                                             \
            &mca_pml_teg.teg_send_requests, (ompi_list_item_t*)sendreq);   \
    }                                                                      \
}


/**
 * Start a send request. 
 */
static inline int mca_pml_teg_send_request_start(
    mca_pml_base_send_request_t* req)
{
    mca_ptl_base_module_t* ptl = req->req_ptl;
    size_t first_fragment_size = ptl->ptl_first_frag_size;
    int flags, rc;

    /* init/reinit request - do this here instead of init 
     * as a persistent request may be reused, and there is
     * no additional cost
    */
    req->req_offset = 0;
    req->req_bytes_sent = 0;
    req->req_peer_match.lval = 0;
    req->req_peer_addr.lval = 0;
    req->req_peer_size = 0;
    req->req_base.req_pml_complete = false;
    req->req_base.req_ompi.req_complete = false;
    req->req_base.req_ompi.req_state = OMPI_REQUEST_ACTIVE;
    req->req_base.req_sequence = mca_pml_ptl_comm_send_sequence(
        req->req_base.req_comm->c_pml_comm, req->req_base.req_peer);
 
    /* handle buffered send */
    if(req->req_send_mode == MCA_PML_BASE_SEND_BUFFERED) {
        mca_pml_base_bsend_request_start(&req->req_base.req_ompi);
    }

    /* start the first fragment */
    if(first_fragment_size == 0 || req->req_bytes_packed <= first_fragment_size) {
        first_fragment_size = req->req_bytes_packed;
        flags = (req->req_send_mode == MCA_PML_BASE_SEND_SYNCHRONOUS) ?
        MCA_PTL_FLAGS_ACK_MATCHED : 0;
    } else {
        /* require match for first fragment of a multi-fragment */
        flags = MCA_PTL_FLAGS_ACK_MATCHED;
    }
    rc = ptl->ptl_send(ptl, req->req_peer, req, 0, first_fragment_size, flags);
    if(rc != OMPI_SUCCESS)
        return rc;
    return OMPI_SUCCESS;
}


/**
 *  Schedule any data that was not delivered in the first fragment
 *  across the available PTLs.
 */
int mca_pml_teg_send_request_schedule(mca_pml_base_send_request_t* req);


/**
 *  Update the request to reflect the number of bytes delivered. If this
 *  was the first fragment - schedule the rest of the data.
 */
void mca_pml_teg_send_request_progress(
    struct mca_ptl_base_module_t* ptl,
    mca_pml_base_send_request_t* send_request,
    size_t bytes_sent
);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

