/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/**
 *  @file 
 */

#ifndef MCA_PML_UNIQ_H
#define MCA_PML_UNIQ_H

#include "opal/threads/thread.h"
#include "opal/threads/condition.h"
#include "class/ompi_free_list.h"
#include "util/cmd_line.h"
#include "request/request.h"
#include "mca/pml/pml.h"
#include "mca/pml/base/pml_base_request.h"
#include "mca/pml/base/pml_base_bsend.h"
#include "mca/pml/base/pml_base_sendreq.h"
#include "mca/ptl/ptl.h"


#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
/**
 * UNIQ PML module
 */

struct mca_pml_uniq_t {
    mca_pml_base_module_t super; 

    mca_ptl_base_component_t **uniq_ptl_components;
    size_t uniq_num_ptl_components;

    mca_ptl_base_module_t** uniq_ptl_modules;
    size_t uniq_num_ptl_modules;

    mca_ptl_base_component_progress_fn_t* uniq_ptl_progress;
    size_t uniq_num_ptl_progress;

    opal_list_t  uniq_procs;
    opal_mutex_t uniq_lock;

    int uniq_free_list_num;   /* initial size of free list */
    int uniq_free_list_max;   /* maximum size of free list */
    int uniq_free_list_inc;   /* number of elements to grow free list */
    int uniq_poll_iterations; /* number of iterations to poll for completion */

    /* free list of requests */
    ompi_free_list_t uniq_send_requests;
    ompi_free_list_t uniq_recv_requests;

    /* list of pending send requests */
    opal_list_t uniq_send_pending;
};
typedef struct mca_pml_uniq_t mca_pml_uniq_t; 

extern mca_pml_uniq_t mca_pml_uniq;


/*
 * PML module functions.
 */


extern int mca_pml_uniq_component_open(void);
extern int mca_pml_uniq_component_close(void);

extern mca_pml_base_module_t* mca_pml_uniq_component_init(
    int *priority, 
    bool enable_progress_threads,
    bool enable_mpi_threads
);

extern int mca_pml_uniq_component_fini(void);



/*
 * PML interface functions.
 */

extern int mca_pml_uniq_add_comm(
    struct ompi_communicator_t* comm
);

extern int mca_pml_uniq_del_comm(
    struct ompi_communicator_t* comm
);

extern int mca_pml_uniq_add_procs(
    struct ompi_proc_t **procs,
    size_t nprocs
);

extern int mca_pml_uniq_del_procs(
    struct ompi_proc_t **procs,
    size_t nprocs
);

extern int mca_pml_uniq_add_ptls(void);

extern int mca_pml_uniq_enable(
    bool enable
);

extern int mca_pml_uniq_progress(void);

extern int mca_pml_uniq_iprobe(
    int dst,
    int tag,
    struct ompi_communicator_t* comm,
    int *matched,
    ompi_status_public_t* status
);

extern int mca_pml_uniq_probe(
    int dst,
    int tag,
    struct ompi_communicator_t* comm,
    ompi_status_public_t* status
);

extern int mca_pml_uniq_cancelled(
    ompi_request_t* request,
    int *flag
);


extern int mca_pml_uniq_isend_init(
    void *buf,
    size_t count,
    struct ompi_datatype_t *datatype,
    int dst,
    int tag,
    mca_pml_base_send_mode_t mode,
    struct ompi_communicator_t* comm,
    struct ompi_request_t **request
);

extern int mca_pml_uniq_isend(
    void *buf,
    size_t count,
    struct ompi_datatype_t *datatype,
    int dst,
    int tag,
    mca_pml_base_send_mode_t mode,
    struct ompi_communicator_t* comm,
    struct ompi_request_t **request
);

extern int mca_pml_uniq_send(
    void *buf,
    size_t count,
    struct ompi_datatype_t *datatype,
    int dst,
    int tag,
    mca_pml_base_send_mode_t mode,
    struct ompi_communicator_t* comm
);

extern int mca_pml_uniq_irecv_init(
    void *buf,
    size_t count,
    struct ompi_datatype_t *datatype,
    int src,
    int tag,
    struct ompi_communicator_t* comm,
    struct ompi_request_t **request
);

extern int mca_pml_uniq_irecv(
    void *buf,
    size_t count,
    struct ompi_datatype_t *datatype,
    int src,
    int tag,
    struct ompi_communicator_t* comm,
    struct ompi_request_t **request
);

extern int mca_pml_uniq_recv(
    void *buf,
    size_t count,
    struct ompi_datatype_t *datatype,
    int src,
    int tag,
    struct ompi_communicator_t* comm,
    ompi_status_public_t* status
);

extern int mca_pml_uniq_progress(void);

extern int mca_pml_uniq_start(
    size_t count,
    ompi_request_t** requests
);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
 
#define MCA_PML_UNIQ_FREE(request) \
{ \
    mca_pml_base_request_t* pml_request = *(mca_pml_base_request_t**)(request); \
    pml_request->req_free_called = true; \
    if( pml_request->req_pml_complete == true) \
    { \
        switch(pml_request->req_type) { \
        case MCA_PML_REQUEST_SEND: \
            { \
                mca_ptl_base_send_request_t* sendreq = (mca_ptl_base_send_request_t*)pml_request; \
                while(sendreq->req_lock > 0); \
                 if(sendreq->req_send.req_send_mode == MCA_PML_BASE_SEND_BUFFERED) { \
                     mca_pml_base_bsend_request_fini((ompi_request_t*)sendreq); \
                 } \
                MCA_PML_UNIQ_SEND_REQUEST_RETURN(sendreq); \
                break; \
            } \
        case MCA_PML_REQUEST_RECV: \
            { \
                mca_ptl_base_recv_request_t* recvreq = (mca_ptl_base_recv_request_t*)pml_request; \
                MCA_PML_UNIQ_RECV_REQUEST_RETURN(recvreq); \
                break; \
            } \
        default: \
            break; \
        } \
    } \
    *(request) = MPI_REQUEST_NULL; \
}

#define MCA_PML_UNIQ_FINI(request) \
{ \
    mca_pml_base_request_t* pml_request = *(mca_pml_base_request_t**)(request); \
    if( (pml_request->req_persistent) && !(pml_request->req_free_called) ) { \
       pml_request->req_ompi.req_state = OMPI_REQUEST_INACTIVE; \
    } else { \
        MCA_PML_UNIQ_FREE(request); \
    } \
}

#endif

