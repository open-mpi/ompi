/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/**
 *  @file 
 */

#ifndef MCA_PML_TEG_H
#define MCA_PML_TEG_H

#include "threads/thread.h"
#include "threads/condition.h"
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
 * TEG PML module
 */

struct mca_pml_teg_t {
    mca_pml_base_module_t super; 

    mca_ptl_base_component_t **teg_ptl_components;
    size_t teg_num_ptl_components;

    mca_ptl_base_module_t** teg_ptl_modules;
    size_t teg_num_ptl_modules;

    ompi_list_t  teg_procs;
    ompi_mutex_t teg_lock;

    int teg_free_list_num;   /* initial size of free list */
    int teg_free_list_max;   /* maximum size of free list */
    int teg_free_list_inc;   /* number of elements to grow free list */
    int teg_poll_iterations; /* number of iterations to poll for completion */

    /* free list of requests */
    ompi_free_list_t teg_send_requests;
    ompi_free_list_t teg_recv_requests;

    /* list of pending send requests */
    ompi_list_t teg_send_pending;
};
typedef struct mca_pml_teg_t mca_pml_teg_t; 

OMPI_COMP_EXPORT extern mca_pml_teg_t mca_pml_teg;


/*
 * PML module functions.
 */

OMPI_COMP_EXPORT extern mca_pml_base_component_1_0_0_t mca_pml_teg_component;


OMPI_COMP_EXPORT extern int mca_pml_teg_component_open(void);
OMPI_COMP_EXPORT extern int mca_pml_teg_component_close(void);

OMPI_COMP_EXPORT extern mca_pml_base_module_t* mca_pml_teg_component_init(
    int *priority, 
    bool *allow_multi_user_threads,
    bool *have_hidden_threads
);

OMPI_COMP_EXPORT extern int mca_pml_teg_component_fini(void);



/*
 * PML interface functions.
 */

OMPI_COMP_EXPORT extern int mca_pml_teg_add_comm(
    struct ompi_communicator_t* comm
);

OMPI_COMP_EXPORT extern int mca_pml_teg_del_comm(
    struct ompi_communicator_t* comm
);

OMPI_COMP_EXPORT extern int mca_pml_teg_add_procs(
    struct ompi_proc_t **procs,
    size_t nprocs
);

OMPI_COMP_EXPORT extern int mca_pml_teg_del_procs(
    struct ompi_proc_t **procs,
    size_t nprocs
);

OMPI_COMP_EXPORT extern int mca_pml_teg_add_ptls(
    ompi_list_t *ptls
);

OMPI_COMP_EXPORT extern int mca_pml_teg_control(
    int param,
    void *size,
    size_t value
);

OMPI_COMP_EXPORT extern int mca_pml_teg_progress(void);

OMPI_COMP_EXPORT extern int mca_pml_teg_iprobe(
    int dst,
    int tag,
    struct ompi_communicator_t* comm,
    int *matched,
    ompi_status_public_t* status
);

OMPI_COMP_EXPORT extern int mca_pml_teg_probe(
    int dst,
    int tag,
    struct ompi_communicator_t* comm,
    ompi_status_public_t* status
);

OMPI_COMP_EXPORT extern int mca_pml_teg_cancel(
    ompi_request_t* request
);

OMPI_COMP_EXPORT extern int mca_pml_teg_cancelled(
    ompi_request_t* request,
    int *flag
);


OMPI_COMP_EXPORT extern int mca_pml_teg_isend_init(
    void *buf,
    size_t count,
    ompi_datatype_t *datatype,
    int dst,
    int tag,
    mca_pml_base_send_mode_t mode,
    struct ompi_communicator_t* comm,
    struct ompi_request_t **request
);

OMPI_COMP_EXPORT extern int mca_pml_teg_isend(
    void *buf,
    size_t count,
    ompi_datatype_t *datatype,
    int dst,
    int tag,
    mca_pml_base_send_mode_t mode,
    struct ompi_communicator_t* comm,
    struct ompi_request_t **request
);

OMPI_COMP_EXPORT extern int mca_pml_teg_send(
    void *buf,
    size_t count,
    ompi_datatype_t *datatype,
    int dst,
    int tag,
    mca_pml_base_send_mode_t mode,
    struct ompi_communicator_t* comm
);

OMPI_COMP_EXPORT extern int mca_pml_teg_irecv_init(
    void *buf,
    size_t count,
    ompi_datatype_t *datatype,
    int src,
    int tag,
    struct ompi_communicator_t* comm,
    struct ompi_request_t **request
);

OMPI_COMP_EXPORT extern int mca_pml_teg_irecv(
    void *buf,
    size_t count,
    ompi_datatype_t *datatype,
    int src,
    int tag,
    struct ompi_communicator_t* comm,
    struct ompi_request_t **request
);

OMPI_COMP_EXPORT extern int mca_pml_teg_recv(
    void *buf,
    size_t count,
    ompi_datatype_t *datatype,
    int src,
    int tag,
    struct ompi_communicator_t* comm,
    ompi_status_public_t* status
);

OMPI_COMP_EXPORT extern int mca_pml_teg_progress(void);

OMPI_COMP_EXPORT extern int mca_pml_teg_start(
    size_t count,
    ompi_request_t** requests
);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#define MCA_PML_TEG_FINI(request) \
{ \
    mca_pml_base_request_t* pml_request = *(mca_pml_base_request_t**)(request); \
    if(pml_request->req_persistent) { \
       if(pml_request->req_free_called) { \
           MCA_PML_TEG_FREE(request); \
       } else {  \
           pml_request->req_ompi.req_state = OMPI_REQUEST_INACTIVE; \
       }  \
    } else { \
        MCA_PML_TEG_FREE(request); \
    } \
}

 
#define MCA_PML_TEG_FREE(request) \
{ \
    mca_pml_base_request_t* pml_request = *(mca_pml_base_request_t**)(request); \
    pml_request->req_free_called = true; \
    if( pml_request->req_pml_complete == true) \
    { \
        OMPI_REQUEST_FINI(*(request)); \
        switch(pml_request->req_type) { \
        case MCA_PML_REQUEST_SEND: \
            { \
            mca_pml_base_send_request_t* sendreq = (mca_pml_base_send_request_t*)pml_request; \
            while(sendreq->req_lock > 0); \
            if(sendreq->req_send_mode == MCA_PML_BASE_SEND_BUFFERED) { \
                mca_pml_base_bsend_request_fini((ompi_request_t*)sendreq); \
            } \
            MCA_PML_TEG_SEND_REQUEST_RETURN(sendreq); \
            break; \
            } \
        case MCA_PML_REQUEST_RECV: \
            { \
            mca_pml_base_recv_request_t* recvreq = (mca_pml_base_recv_request_t*)pml_request; \
            MCA_PML_TEG_RECV_REQUEST_RETURN(recvreq); \
            break; \
            } \
        default: \
            break; \
        } \
    } \
    *(request) = MPI_REQUEST_NULL; \
}

#endif

