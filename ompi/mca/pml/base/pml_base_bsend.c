/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2015-2017 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2017      IBM Corporation.  All rights reserved.
 * Copyright (c) 2018-2025 Triad National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "opal/mca/threads/mutex.h"
#include "opal/mca/threads/condition.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/communicator/communicator.h"
#include "opal/mca/allocator/base/base.h"
#include "opal/mca/allocator/allocator.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/pml/base/pml_base_request.h"
#include "ompi/mca/pml/base/pml_base_sendreq.h"
#include "ompi/mca/pml/base/pml_base_bsend.h"
#include "opal/mca/mpool/mpool.h"
#include "ompi/runtime/mpiruntime.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */

static void mca_pml_bsend_buffer_construct(mca_pml_bsend_buffer_t *buffer);
static void mca_pml_bsend_buffer_construct(mca_pml_bsend_buffer_t *buffer)
{
    buffer->bsend_allocator = NULL;
    buffer->bsend_userbase = NULL;
    buffer->bsend_base = NULL;
    buffer->bsend_addr = NULL;
    buffer->bsend_size = 0UL;
    buffer->bsend_count = 0UL;
    buffer->bsend_pagebits = 0;
    OBJ_CONSTRUCT(&buffer->bsend_mutex, opal_mutex_t);
    OBJ_CONSTRUCT(&buffer->bsend_condition, opal_condition_t);
}

static void mca_pml_bsend_buffer_destruct(mca_pml_bsend_buffer_t *buffer);
static void mca_pml_bsend_buffer_destruct(mca_pml_bsend_buffer_t *buffer)
{
    OBJ_DESTRUCT(&buffer->bsend_mutex);
    OBJ_DESTRUCT(&buffer->bsend_condition);
}

OBJ_CLASS_INSTANCE (mca_pml_bsend_buffer_t,  opal_object_t,
                    mca_pml_bsend_buffer_construct,
                    mca_pml_bsend_buffer_destruct);

static size_t           mca_pml_bsend_pagesz;     /* mmap page size */
static int              mca_pml_bsend_pagebits;   /* number of bits in pagesz */
static opal_atomic_int32_t          mca_pml_bsend_init = 0;

/* defined in pml_base_open.c */
extern char *ompi_pml_base_bsend_allocator_name;

static mca_pml_bsend_buffer_t *mca_pml_bsend_buffer=NULL;
static mca_allocator_base_component_t* mca_pml_bsend_allocator_component;

static int mca_pml_base_bsend_fini (void);

/*
 * Routine to select which buffer to used based on section 3.6 of the MPI 5 standard
 */

static mca_pml_bsend_buffer_t *mca_pml_bsend_buffer_get(ompi_communicator_t *comm)
{
    mca_pml_bsend_buffer_t *buffer = NULL;

    /* 
     * first see if a buffer has been attached to the communicator
     */

    buffer = ompi_comm_bsend_buffer_get(comm);
    if (NULL != buffer) {
        return buffer;
    }

    /*
     * maybe the instance (aka session) has a buffer associated with it.
     */

    if (MPI_SESSION_NULL != comm->instance) {
        buffer = ompi_instance_bsend_buffer_get(comm->instance);
        if (NULL != buffer) {
            return buffer;
        }
    }

    /*
     * okay see if the old MPI-1 style buffer is available
     */

    if (NULL != mca_pml_bsend_buffer) {
        return mca_pml_bsend_buffer;
    }

    return NULL;
}

/*
 * Routine to return pages to sub-allocator as needed
 */
static void* mca_pml_bsend_alloc_segment(void *ctx, size_t *size_inout)
{
    void *addr;
    size_t size = *size_inout;
    mca_pml_bsend_buffer_t *buf_instance = (mca_pml_bsend_buffer_t *)ctx;

    if(buf_instance->bsend_addr + size > buf_instance->bsend_base + buf_instance->bsend_size) {
        return NULL;
    }
    /* allocate all that is left */
    size = buf_instance->bsend_size - (buf_instance->bsend_addr - buf_instance->bsend_base);
    addr = buf_instance->bsend_addr;
    buf_instance->bsend_addr += size;
    *size_inout = size;
    return addr;
}

/*
 * Routines to implement MPI_BUFFER_AUTOMATIC (MPI 4.0)
 */

static void* mca_pml_bsend_alloc_seg_auto(void *ctx, size_t *size_inout)
{
    void *addr;
    size_t size = *size_inout;
    
    addr = malloc(size);
    if (NULL != addr) {
        *size_inout = size;
    }
    return addr;
}

static void mca_pml_bsend_dealloc_seg_auto(void *ctx, void *segment)
{
    free(segment);
}

/*
 * One time initialization at startup
 */
int mca_pml_base_bsend_init (void)
{
    size_t tmp;

    if(OPAL_THREAD_ADD_FETCH32(&mca_pml_bsend_init, 1) > 1)
        return OMPI_SUCCESS;

    /* lookup name of the allocator to use for buffered sends */
    if(NULL == ( mca_pml_bsend_allocator_component = mca_allocator_component_lookup(ompi_pml_base_bsend_allocator_name))) {
        return OMPI_ERR_BUFFER;
    }

    /* determine page size */
    tmp = mca_pml_bsend_pagesz = sysconf(_SC_PAGESIZE);
    mca_pml_bsend_pagebits = 0;
    while( tmp != 0 ) {
        tmp >>= 1;
        mca_pml_bsend_pagebits++;
    }

    ompi_mpi_instance_append_finalize (mca_pml_base_bsend_fini);

    return OMPI_SUCCESS;
}


/*
 * One-time cleanup at shutdown - release any resources.
 */
static int mca_pml_base_bsend_fini (void)
{
    if(OPAL_THREAD_ADD_FETCH32(&mca_pml_bsend_init,-1) > 0)
        return OMPI_SUCCESS;

    mca_pml_bsend_allocator_component = NULL;
    mca_pml_bsend_buffer = NULL;

    mca_pml_bsend_pagebits = 0;

    return OMPI_SUCCESS;
}


/*
 * User-level call to attach buffer
 */
int mca_pml_base_bsend_attach(ompi_bsend_buffer_type_t type, void *obj, void* addr, size_t size)
{
    int align;
    int ret = MPI_SUCCESS;
    mca_pml_bsend_buffer_t *buf_instance;
    ompi_communicator_t *comm = NULL;
    ompi_instance_t *instance = NULL;

    if(NULL == addr || ((MPI_BUFFER_AUTOMATIC != addr) && size <= 0)) {
        return OMPI_ERR_BUFFER;
    }

    /*
     * check if object already has a buffer associated with it
     */

    switch (type) {
        case BASE_BSEND_BUF:
            if (NULL != mca_pml_bsend_buffer) {
                ret = OMPI_ERR_BUFFER;
                goto fn_exit;
            }
            break;

        case COMM_BSEND_BUF:
            comm = (ompi_communicator_t *)obj;
            if (NULL != ompi_comm_bsend_buffer_get(comm)) {
                ret = OMPI_ERR_BUFFER;
                goto fn_exit;
            }
            break;

        case SESSION_BSEND_BUF:
            instance = (ompi_instance_t *)obj;
            if (NULL != ompi_instance_bsend_buffer_get(instance)) {
                ret = OMPI_ERR_BUFFER;
                goto fn_exit;
            }
            break;

        default:
            /* This should not happen */
            assert(0);
            ret = OMPI_ERR_BAD_PARAM;
            goto fn_exit;
            break;
    }


    buf_instance = OBJ_NEW(mca_pml_bsend_buffer_t);
    OBJ_CONSTRUCT(&buf_instance->bsend_mutex, opal_mutex_t);
    OBJ_CONSTRUCT(&buf_instance->bsend_condition, opal_condition_t);

    /* try to create an instance of the allocator - to determine thread safety level */
    if (MPI_BUFFER_AUTOMATIC != addr) {
        buf_instance->bsend_allocator = mca_pml_bsend_allocator_component->allocator_init(ompi_mpi_thread_multiple, 
                                                                                          mca_pml_bsend_alloc_segment, 
                                                                                          NULL, 
                                                                                          buf_instance);
   } else {
        /*
         * TODO: this is where we might want to enhance the allocator type based off of, perhaps
         * presence of certain keys in the info object bound to the communicator in the case of COMM_BSEND_BUF
         * buffering.
         */
        buf_instance->bsend_allocator = mca_pml_bsend_allocator_component->allocator_init(ompi_mpi_thread_multiple, 
                                                                                          mca_pml_bsend_alloc_seg_auto, 
                                                                                          mca_pml_bsend_dealloc_seg_auto, 
                                                                                          buf_instance);
    }
    if(NULL == buf_instance->bsend_allocator) {
        return OMPI_ERR_BUFFER;
    }

    /*
     * Save away what the user handed in.  This is done in case the
     * base and size are modified for alignment issues.
     */
    buf_instance->bsend_userbase = (unsigned char*)addr;
    buf_instance->bsend_usersize = size;
    /*
     * Align to pointer boundaries. The bsend overhead is large enough
     * to account for this.  Compute any alignment that needs to be done.
     */
    align = sizeof(void *) - ((size_t)addr & (sizeof(void *) - 1));

    /* setup local variables */
    buf_instance->bsend_base = (unsigned char *)addr + align;
    buf_instance->bsend_addr = (unsigned char *)addr + align;
    buf_instance->bsend_size = size - align;
    buf_instance->bsend_count = 0;
    buf_instance->bsend_pagebits = mca_pml_bsend_pagebits;

    switch (type) {
        case BASE_BSEND_BUF:
            mca_pml_bsend_buffer = buf_instance;
            break;

        case COMM_BSEND_BUF:
            ret = ompi_comm_bsend_buffer_set(comm, buf_instance);
            if(OMPI_SUCCESS != ret) {
                goto fn_exit;
            }
            OBJ_RETAIN(comm);
            break;

        case SESSION_BSEND_BUF:
            ret = ompi_instance_bsend_buffer_set(instance, buf_instance);
            if(OMPI_SUCCESS != ret) {
                goto fn_exit;
            }
            OBJ_RETAIN(instance);
            break;

        default:
            /* This should not happen */
            assert(0);
            break;
    }

fn_exit:
    return ret;
}

/*
 * User-level call to detach buffer
 */
int mca_pml_base_bsend_detach(ompi_bsend_buffer_type_t type, void *obj, void* addr, size_t* size)
{
    int ret = OMPI_SUCCESS;
    mca_pml_bsend_buffer_t *buf_instance = NULL;
    ompi_communicator_t *comm = NULL;
    ompi_instance_t *instance = NULL;

    switch (type) {
        case BASE_BSEND_BUF:
            buf_instance = mca_pml_bsend_buffer;
            if (NULL == buf_instance) {
                ret = OMPI_ERR_BUFFER;
                goto fn_exit;
            }
            break;

        case COMM_BSEND_BUF:
            comm = (ompi_communicator_t *)obj;
            buf_instance = ompi_comm_bsend_buffer_get(comm);
            if (NULL == buf_instance) {
                ret = OMPI_ERR_BUFFER;
                goto fn_exit;
            }
            break;

        case SESSION_BSEND_BUF:
            instance = (ompi_instance_t *)obj;
            buf_instance = ompi_instance_bsend_buffer_get(instance);
            if (NULL == buf_instance) {
                ret = OMPI_ERR_BUFFER;
                goto fn_exit;
            }
            break;

        default:
            /* This should not happen */
            assert(0);
            break;
    }

    while(buf_instance->bsend_count != 0) {
        opal_condition_wait(&buf_instance->bsend_condition, &buf_instance->bsend_mutex);
        opal_progress();
    }

    /* free resources associated with the allocator */
    buf_instance->bsend_allocator->alc_finalize(buf_instance->bsend_allocator);
    buf_instance->bsend_allocator = NULL;

    /* return current settings */
    if(NULL != addr) {
        *((void**)addr) = buf_instance->bsend_userbase;
    }

    if ((NULL != size) && (buf_instance->bsend_userbase != MPI_BUFFER_AUTOMATIC)) {
        *size = buf_instance->bsend_usersize;
    }

    switch (type) {
        case BASE_BSEND_BUF:
            mca_pml_bsend_buffer= NULL;
            break;

        case COMM_BSEND_BUF:
            ret = ompi_comm_bsend_buffer_set(comm, NULL);
            if (OMPI_SUCCESS != ret) {
                goto fn_exit;
            }
            OBJ_RELEASE(comm);
            break;
        
        case SESSION_BSEND_BUF:
            ret = ompi_instance_bsend_buffer_set(instance, NULL);
            if (OMPI_SUCCESS != ret) {
                goto fn_exit;
            }
            OBJ_RELEASE(instance);
            break;
        
        default:
            /* This should not happen */
            assert(0);
            break;
    }

    OBJ_DESTRUCT(buf_instance);

fn_exit:
    return ret;
}

int mca_pml_base_bsend_flush(ompi_bsend_buffer_type_t type, void *obj)
{
    int ret = OMPI_SUCCESS;
    mca_pml_bsend_buffer_t *buf_instance = NULL;
    ompi_communicator_t *comm = NULL;
    ompi_instance_t *instance = NULL;

    switch (type) {
        case BASE_BSEND_BUF:
            buf_instance = mca_pml_bsend_buffer;
            if (NULL == buf_instance) {
                ret = OMPI_ERR_BUFFER;
                goto fn_exit;
            }
            break;
    
        case COMM_BSEND_BUF:
            comm = (ompi_communicator_t *)obj;
            buf_instance = ompi_comm_bsend_buffer_get(comm);
            if (NULL == buf_instance) {
                ret = OMPI_ERR_BUFFER;
                goto fn_exit;
            }
            break;
            
        case SESSION_BSEND_BUF:
            instance = (ompi_instance_t *)obj;
            buf_instance = ompi_instance_bsend_buffer_get(instance);
            if (NULL == buf_instance) {
                ret = OMPI_ERR_BUFFER;
                goto fn_exit;
            }
            break;

        default:
            /* This should not happen */
            assert(0);
            break;
    }

    while(buf_instance->bsend_count != 0) {
        opal_condition_wait(&buf_instance->bsend_condition, &buf_instance->bsend_mutex);
        opal_progress();
    }

fn_exit:
    return ret;
}

/*
 * TODO:  right now treating this as blocking
 */
int mca_pml_base_bsend_iflush(ompi_bsend_buffer_type_t type, void *obj, ompi_request_t **req)
{
    int ret = OMPI_SUCCESS;
    mca_pml_bsend_buffer_t *buf_instance = NULL;
    ompi_communicator_t *comm = NULL;
    ompi_instance_t *instance = NULL;
#if 0
    ompi_request_t *flush_request = NULL;
#endif

    switch (type) {
        case BASE_BSEND_BUF:
            buf_instance = mca_pml_bsend_buffer;
            if (NULL == buf_instance) {
                ret = OMPI_ERR_BUFFER;
                goto fn_exit;
            }
            break;
   
        case COMM_BSEND_BUF:
            comm = (ompi_communicator_t *)obj;
            buf_instance = ompi_comm_bsend_buffer_get(comm);
            if (NULL == buf_instance) {
                ret = OMPI_ERR_BUFFER;
                goto fn_exit;
            }
            break;

        case SESSION_BSEND_BUF:
            instance = (ompi_instance_t *)obj;
            buf_instance = ompi_instance_bsend_buffer_get(instance);
            if (NULL == buf_instance) {
                ret = OMPI_ERR_BUFFER;
                goto fn_exit;
            }
            break;

        default:
            /* This should not happen */
            assert(0);
            break;
    }


    while(buf_instance->bsend_count != 0) {
        opal_condition_wait(&buf_instance->bsend_condition, &buf_instance->bsend_mutex);
        opal_progress();
    }

fn_exit:
    *req = MPI_REQUEST_NULL;
    return ret;
}


/*
 * pack send buffer into buffer
 */

int mca_pml_base_bsend_request_start(ompi_request_t* request)
{
    mca_pml_base_send_request_t* sendreq = (mca_pml_base_send_request_t*)request;
    mca_pml_bsend_buffer_t *buffer = NULL;
    struct iovec iov;
    unsigned int iov_count;
    size_t max_data;
    int rc;

    if(sendreq->req_bytes_packed > 0) {

        /* has a buffer been provided */
        buffer = mca_pml_bsend_buffer_get(request->req_mpi_object.comm);
        if (NULL == buffer) {
            sendreq->req_addr = NULL;
            return OMPI_ERR_BUFFER;
        }

        /* allocate a buffer to hold packed message */
        OPAL_THREAD_LOCK(&buffer->bsend_mutex);
        sendreq->req_addr = buffer->bsend_allocator->alc_alloc(
                                buffer->bsend_allocator, sendreq->req_bytes_packed, 0);
        if(NULL == sendreq->req_addr) {
            /* release resources when request is freed */
            sendreq->req_base.req_pml_complete = true;
            OPAL_THREAD_UNLOCK(&buffer->bsend_mutex);
            return OMPI_ERR_BUFFER;
        }

        /* The convertor is already initialized in the beginning so we just have to
         * pack the data in the newly allocated buffer.
         */
        iov.iov_base = (IOVBASE_TYPE*)sendreq->req_addr;
        iov.iov_len = sendreq->req_bytes_packed;
        iov_count = 1;
        max_data = iov.iov_len;
        if((rc = opal_convertor_pack( &sendreq->req_base.req_convertor,
                                      &iov,
                                      &iov_count,
                                      &max_data )) < 0) {
            OPAL_THREAD_UNLOCK(&buffer->bsend_mutex);
            return OMPI_ERROR;
        }

        /* setup convertor to point to packed buffer (at position zero) */
        opal_convertor_prepare_for_send( &sendreq->req_base.req_convertor, &(ompi_mpi_packed.dt.super),
                                         max_data, sendreq->req_addr );
        /* increment count of pending requests */
        buffer->bsend_count++;

        OPAL_THREAD_UNLOCK(&buffer->bsend_mutex);

    }
    return OMPI_SUCCESS;
}


/*
 * allocate buffer
 */

int mca_pml_base_bsend_request_alloc(ompi_request_t* request)
{
    mca_pml_base_send_request_t* sendreq = (mca_pml_base_send_request_t*)request;
    mca_pml_bsend_buffer_t *buffer = NULL;

    assert( sendreq->req_bytes_packed > 0 );

    /* has a buffer been provided */
    buffer = mca_pml_bsend_buffer_get(request->req_mpi_object.comm);
    if (NULL == buffer) {
        sendreq->req_addr = NULL;
        return OMPI_ERR_BUFFER;
    }

    OPAL_THREAD_LOCK(&buffer->bsend_mutex);

    /* allocate a buffer to hold packed message */
    sendreq->req_addr = buffer->bsend_allocator->alc_alloc(
        buffer->bsend_allocator, sendreq->req_bytes_packed, 0);
    if(NULL == sendreq->req_addr) {
        /* release resources when request is freed */
        sendreq->req_base.req_pml_complete = true;
        OPAL_THREAD_UNLOCK(&buffer->bsend_mutex);
        /* progress communications, with the hope that more resources
         *   will be freed */
        opal_progress();
        return OMPI_ERR_BUFFER;
    }

    /* increment count of pending requests */
    buffer->bsend_count++;
    OPAL_THREAD_UNLOCK(&buffer->bsend_mutex);

    return OMPI_SUCCESS;
}

/*
 * allocate buffer
 */

void*  mca_pml_base_bsend_request_alloc_buf(ompi_communicator_t *comm,  size_t length )
{
    void* buf = NULL;
    mca_pml_bsend_buffer_t *ompi_buffer = NULL;

    /* has a buffer been provided */
    ompi_buffer = mca_pml_bsend_buffer_get(comm);
    if (NULL == ompi_buffer) {
        return NULL;
    }

    OPAL_THREAD_LOCK(&ompi_buffer->bsend_mutex);
    /* allocate a buffer to hold packed message */
    buf = ompi_buffer->bsend_allocator->alc_alloc(
                           ompi_buffer->bsend_allocator, length, 0);
    if(NULL == buf) {
        /* release resources when request is freed */
        OPAL_THREAD_UNLOCK(&ompi_buffer->bsend_mutex);
        /* progress communications, with the hope that more resources
         *   will be freed */
        opal_progress();
        return NULL;
    }

    /* increment count of pending requests */
    ompi_buffer->bsend_count++;
    OPAL_THREAD_UNLOCK(&ompi_buffer->bsend_mutex);

    return buf;
}


/*
 *  Request completed - free buffer and decrement pending count
 */
int mca_pml_base_bsend_request_free(ompi_communicator_t *comm, void* addr)
{
    mca_pml_bsend_buffer_t *buffer = NULL;

    buffer = mca_pml_bsend_buffer_get(comm);
    assert(NULL != buffer);
    if (NULL == buffer) {
        return OMPI_ERR_BUFFER;
    }

    OPAL_THREAD_LOCK(&buffer->bsend_mutex);

    /* free buffer */
    buffer->bsend_allocator->alc_free(buffer->bsend_allocator, addr);

    /* decrement count of buffered requests for this buffer pool */
    if(--buffer->bsend_count == 0)
        opal_condition_signal(&buffer->bsend_condition);

    OPAL_THREAD_UNLOCK(&buffer->bsend_mutex);
    return OMPI_SUCCESS;
}



/*
 *  Request completed - free buffer and decrement pending count
 */
int mca_pml_base_bsend_request_fini(ompi_request_t* request)
{
    mca_pml_bsend_buffer_t *buffer = mca_pml_bsend_buffer_get(request->req_mpi_object.comm);
    if(NULL == buffer) {
        return OMPI_ERR_BUFFER;
    }

    mca_pml_base_send_request_t* sendreq = (mca_pml_base_send_request_t*)request;
    if(sendreq->req_bytes_packed == 0 ||
       sendreq->req_addr == NULL ||
       sendreq->req_addr == sendreq->req_base.req_addr)
        return OMPI_SUCCESS;

    /* remove from list of pending requests */
    OPAL_THREAD_LOCK(&buffer->bsend_mutex);

    /* free the buffer memory associated with this request */
    buffer->bsend_allocator->alc_free(buffer->bsend_allocator, (void *)sendreq->req_addr);
    sendreq->req_addr = sendreq->req_base.req_addr;

    /* decrement count of buffered requests */
    if(--buffer->bsend_count == 0)
        opal_condition_signal(&buffer->bsend_condition);

    OPAL_THREAD_UNLOCK(&buffer->bsend_mutex);
    return OMPI_SUCCESS;
}


