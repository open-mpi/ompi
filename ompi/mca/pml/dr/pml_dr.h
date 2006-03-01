/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
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

#ifndef MCA_PML_DR_H
#define MCA_PML_DR_H

#include "ompi_config.h"
#include "opal/threads/threads.h"
#include "opal/threads/condition.h"
#include "ompi/class/ompi_free_list.h"
#include "opal/util/cmd_line.h"
#include "ompi/request/request.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/pml/base/pml_base_request.h"
#include "ompi/mca/pml/base/pml_base_bsend.h"
#include "ompi/mca/pml/base/pml_base_sendreq.h"
#include "ompi/mca/btl/btl.h"
#include "ompi/datatype/datatype.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/**
 * DR PML module
 */

struct mca_pml_dr_t {
    mca_pml_base_module_t super; 

    int priority;
    int free_list_num;      /* initial size of free list */
    int free_list_max;      /* maximum size of free list */
    int free_list_inc;      /* number of elements to grow free list */
    size_t eager_limit;     /* maximum eager limit size - overrides btl setting */
    size_t send_pipeline_depth;
    bool enabled;

    time_t tout_ack;
    time_t tout_watch_dog;

    /* lock queue access */
    opal_mutex_t lock;

    /* pending lists */
    opal_list_t send_pending;
    opal_list_t acks_pending;

    /* free lists */
    ompi_free_list_t send_requests;
    ompi_free_list_t recv_requests;
    ompi_free_list_t recv_frags;
    ompi_free_list_t vfrags;
    ompi_free_list_t buffers;

    int timer_wdog_sec;
    int timer_wdog_usec;
    int timer_ack_sec;
    int timer_ack_usec;
    
};
typedef struct mca_pml_dr_t mca_pml_dr_t; 

extern mca_pml_dr_t mca_pml_dr;


/*
 * PML module functions.
 */


extern int mca_pml_dr_component_open(void);
extern int mca_pml_dr_component_close(void);

extern mca_pml_base_module_t* mca_pml_dr_component_init(
    int *priority, 
    bool enable_progress_threads,
    bool enable_mpi_threads
);

extern int mca_pml_dr_component_fini(void);



/*
 * PML interface functions.
 */

extern int mca_pml_dr_add_comm(
    struct ompi_communicator_t* comm
);

extern int mca_pml_dr_del_comm(
    struct ompi_communicator_t* comm
);

extern int mca_pml_dr_add_procs(
    struct ompi_proc_t **procs,
    size_t nprocs
);

extern int mca_pml_dr_del_procs(
    struct ompi_proc_t **procs,
    size_t nprocs
);

extern int mca_pml_dr_enable(
    bool enable
);

extern int mca_pml_dr_progress(void);

extern int mca_pml_dr_iprobe(
    int dst,
    int tag,
    struct ompi_communicator_t* comm,
    int *matched,
    ompi_status_public_t* status
);

extern int mca_pml_dr_probe(
    int dst,
    int tag,
    struct ompi_communicator_t* comm,
    ompi_status_public_t* status
);

extern int mca_pml_dr_isend_init(
    void *buf,
    size_t count,
    ompi_datatype_t *datatype,
    int dst,
    int tag,
    mca_pml_base_send_mode_t mode,
    struct ompi_communicator_t* comm,
    struct ompi_request_t **request
);

extern int mca_pml_dr_isend(
    void *buf,
    size_t count,
    ompi_datatype_t *datatype,
    int dst,
    int tag,
    mca_pml_base_send_mode_t mode,
    struct ompi_communicator_t* comm,
    struct ompi_request_t **request
);

extern int mca_pml_dr_send(
    void *buf,
    size_t count,
    ompi_datatype_t *datatype,
    int dst,
    int tag,
    mca_pml_base_send_mode_t mode,
    struct ompi_communicator_t* comm
);

extern int mca_pml_dr_irecv_init(
    void *buf,
    size_t count,
    ompi_datatype_t *datatype,
    int src,
    int tag,
    struct ompi_communicator_t* comm,
    struct ompi_request_t **request
);

extern int mca_pml_dr_irecv(
    void *buf,
    size_t count,
    ompi_datatype_t *datatype,
    int src,
    int tag,
    struct ompi_communicator_t* comm,
    struct ompi_request_t **request
);

extern int mca_pml_dr_recv(
    void *buf,
    size_t count,
    ompi_datatype_t *datatype,
    int src,
    int tag,
    struct ompi_communicator_t* comm,
    ompi_status_public_t* status
);

extern int mca_pml_dr_progress(void);

extern int mca_pml_dr_start(
    size_t count,
    ompi_request_t** requests
);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif


#define MCA_PML_DR_FREE(request) \
{ \
    mca_pml_base_request_t* pml_request = *(mca_pml_base_request_t**)(request); \
    pml_request->req_free_called = true; \
    if( pml_request->req_pml_complete == true) \
    { \
        switch(pml_request->req_type) { \
        case MCA_PML_REQUEST_SEND: \
            { \
                mca_pml_dr_send_request_t* sendreq = (mca_pml_dr_send_request_t*)pml_request; \
                if(sendreq->req_send.req_send_mode == MCA_PML_BASE_SEND_BUFFERED && \
                   sendreq->req_send.req_addr != sendreq->req_send.req_base.req_addr) { \
                    mca_pml_base_bsend_request_fini((ompi_request_t*)sendreq); \
                } \
                MCA_PML_DR_SEND_REQUEST_RETURN(sendreq); \
                break; \
            } \
        case MCA_PML_REQUEST_RECV: \
            { \
                mca_pml_dr_recv_request_t* recvreq = (mca_pml_dr_recv_request_t*)pml_request; \
                MCA_PML_DR_RECV_REQUEST_RETURN(recvreq); \
                break; \
            } \
        default: \
            break; \
        } \
    } \
    *(request) = MPI_REQUEST_NULL; \
}

#define MCA_PML_DR_DES_ALLOC(bml_btl, des, size) \
MCA_BML_BASE_BTL_DES_ALLOC(bml_btl, des,  \
   sizeof(mca_pml_dr_hdr_t) + (sizeof(mca_btl_base_segment_t) << 4), size)


/* ADLER_NMAX is the largest n such that 255n(n+1)/2 + (n+1)(BASE-1) <= 2^32-1 */
#define ADLER_NMAX 5551
#define MOD_ADLER 65521

#define DO1(buf,i)  {_a += buf[i]; _b += _a;}
#define DO2(buf,i)  DO1(buf,i); DO1(buf,i+1);
#define DO4(buf,i)  DO2(buf,i); DO2(buf,i+2);
#define DO8(buf,i)  DO4(buf,i); DO4(buf,i+4);
#define DO16(buf)   DO8(buf,0); DO8(buf,8);

#define COMPUTE_SPECIFIC_CHECKSUM( DATA, LENGTH, ADLER32) \
do { \
    uint8_t *_data = (DATA);   /* Pointer to the data to be summed */ \
    size_t _len = (LENGTH);    /* Length in bytes */ \
    uint32_t _a = (ADLER32) & 0xffff, \
             _b = ((ADLER32) >> 16) & 0xffff; \
\
    while( _len > 0 ) { \
        unsigned _tlen = _len > ADLER_NMAX ? ADLER_NMAX : _len; \
        _len -= _tlen; \
        while( _tlen >= 16 ) { \
            DO16(_data); \
            _data += 16; \
            _tlen -= 16; \
        } \
        if( 0 != _tlen ) do { \
            _a += *_data++; _b += _a; \
        } while( --_tlen > 0 ); \
        _a = _a % MOD_ADLER; \
        _b = _b % MOD_ADLER; \
    } \
    (ADLER32) = _b << 16 | _a; \
} while(0)

#endif

