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

#include "ompi_config.h"
#include "opal/threads/mutex.h"
#include "opal/threads/condition.h"
#include "mca/allocator/base/base.h"
#include "mca/allocator/allocator.h"
#include "mca/base/mca_base_param.h"
#include "mca/pml/pml.h"
#include "mca/pml/base/pml_base_request.h"
#include "mca/pml/base/pml_base_sendreq.h"
#include "mca/pml/base/pml_base_bsend.h"
#include "mca/mpool/mpool.h" 


static opal_mutex_t     mca_pml_bsend_mutex;      /* lock for thread safety */
static opal_condition_t mca_pml_bsend_condition;  /* condition variable to block on detach */
static mca_allocator_base_component_t* mca_pml_bsend_allocator_component;  
static mca_allocator_base_module_t* mca_pml_bsend_allocator;  /* sub-allocator to manage users buffer */
static unsigned char   *mca_pml_bsend_base;       /* base address of users buffer */
static unsigned char   *mca_pml_bsend_addr;       /* current offset into users buffer */
static size_t           mca_pml_bsend_size;       /* size of users buffer */
static size_t           mca_pml_bsend_count;      /* number of outstanding requests */
static size_t           mca_pml_bsend_pagesz;     /* mmap page size */
static int              mca_pml_bsend_pagebits;   /* number of bits in pagesz */
static int32_t          mca_pml_bsend_init = 0; 



/*
 * Routine to return pages to sub-allocator as needed 
 */
static void* mca_pml_bsend_alloc_segment(
    struct mca_mpool_base_module_t* module, 
    size_t* size_inout, 
    mca_mpool_base_registration_t** registration)
{
    void *addr;
    size_t size = *size_inout;
    if(mca_pml_bsend_addr + size > mca_pml_bsend_base + mca_pml_bsend_size) {
        return NULL;
    }
    /* allocate all that is left */
    size = mca_pml_bsend_size - (mca_pml_bsend_addr - mca_pml_bsend_base); 
    addr = mca_pml_bsend_addr;
    mca_pml_bsend_addr += size;
    *size_inout = size;
    *registration = NULL;
    return addr;
}


/*
 * One time initialization at startup
 */
int mca_pml_base_bsend_init(bool thread_safe)
{
    int id = mca_base_param_register_string("pml", "base", "bsend_allocator", NULL, "basic");
    char *name;
    size_t tmp;

    if(OPAL_THREAD_ADD32(&mca_pml_bsend_init, 1) > 1)
        return OMPI_SUCCESS;

    /* initialize static objects */
    OBJ_CONSTRUCT(&mca_pml_bsend_mutex, opal_mutex_t);
    OBJ_CONSTRUCT(&mca_pml_bsend_condition, opal_condition_t);

    /* lookup name of the allocator to use for buffered sends */
    mca_base_param_lookup_string(id, &name);
    if(NULL == (mca_pml_bsend_allocator_component = mca_allocator_component_lookup(name))) {
        free(name);
        return OMPI_ERR_BUFFER;
    }
    free(name);

    /* determine page size */
    tmp = mca_pml_bsend_pagesz = sysconf(_SC_PAGESIZE);
    mca_pml_bsend_pagebits = 0;
    while( tmp != 0 ) {
        tmp >>= 1;
        mca_pml_bsend_pagebits++;
    }
    return OMPI_SUCCESS;
}


/*
 * One-time cleanup at shutdown - release any resources.
 */
int mca_pml_base_bsend_fini()
{
    if(OPAL_THREAD_ADD32(&mca_pml_bsend_init,-1) > 0) 
        return OMPI_SUCCESS;

    if(NULL != mca_pml_bsend_allocator)
        mca_pml_bsend_allocator->alc_finalize(mca_pml_bsend_allocator);
    mca_pml_bsend_allocator = NULL;

    OBJ_DESTRUCT(&mca_pml_bsend_condition);
    OBJ_DESTRUCT(&mca_pml_bsend_mutex);
    return OMPI_SUCCESS;
}


/*
 * User-level call to attach buffer.
 */
int mca_pml_base_bsend_attach(void* addr, int size)
{
    bool thread_safe = ompi_mpi_thread_multiple;
    if(NULL == addr || size <= 0) {
        return OMPI_ERR_BUFFER;
    }

    /* check for buffer already attached */
    OPAL_THREAD_LOCK(&mca_pml_bsend_mutex);
    if(NULL != mca_pml_bsend_allocator) {
        OPAL_THREAD_UNLOCK(&mca_pml_bsend_mutex);
        return OMPI_ERR_BUFFER;
    }

    /* try to create an instance of the allocator - to determine thread safety level */
    mca_pml_bsend_allocator = mca_pml_bsend_allocator_component->allocator_init(thread_safe, mca_pml_bsend_alloc_segment, NULL, NULL);
    if(NULL == mca_pml_bsend_allocator) {
        OPAL_THREAD_UNLOCK(&mca_pml_bsend_mutex);
        return OMPI_ERR_BUFFER;
    }

    /* setup local variables */
    mca_pml_bsend_base = addr;
    mca_pml_bsend_addr = addr;
    mca_pml_bsend_size = size;
    mca_pml_bsend_count = 0;
    OPAL_THREAD_UNLOCK(&mca_pml_bsend_mutex);
    return OMPI_SUCCESS;
}

/*
 * User-level call to detach buffer 
 */
int mca_pml_base_bsend_detach(void* addr, int* size)
{
    OPAL_THREAD_LOCK(&mca_pml_bsend_mutex);

    /* is buffer attached */
    if(NULL == mca_pml_bsend_allocator) {
        OPAL_THREAD_UNLOCK(&mca_pml_bsend_mutex);
        return OMPI_ERR_BUFFER;
    }

    /* wait on any pending requests */
    while(mca_pml_bsend_count != 0)
        opal_condition_wait(&mca_pml_bsend_condition, &mca_pml_bsend_mutex);
    
    /* free resources associated with the allocator */
    mca_pml_bsend_allocator->alc_finalize(mca_pml_bsend_allocator);
    mca_pml_bsend_allocator = NULL;

    /* return current settings */
    if(NULL != addr)
        *((void**)addr) = mca_pml_bsend_base;
    if(NULL != size)
        *size = mca_pml_bsend_size;

    /* reset local variables */
    mca_pml_bsend_base = NULL;
    mca_pml_bsend_addr = NULL;
    mca_pml_bsend_size = 0;
    mca_pml_bsend_count = 0;
    OPAL_THREAD_UNLOCK(&mca_pml_bsend_mutex);
    return OMPI_SUCCESS;
} 

     
/* 
 * pack send buffer into buffer
 */

int mca_pml_base_bsend_request_start(ompi_request_t* request)
{
    mca_pml_base_send_request_t* sendreq = (mca_pml_base_send_request_t*)request;
    struct iovec iov;
    unsigned int iov_count;
    size_t max_data;
    int rc, freeAfter;

    if(sendreq->req_count > 0) {

        /* has a buffer been provided */
        OPAL_THREAD_LOCK(&mca_pml_bsend_mutex);
        if(NULL == mca_pml_bsend_addr) {
            sendreq->req_addr = NULL;
            OPAL_THREAD_UNLOCK(&mca_pml_bsend_mutex);
            return OMPI_ERR_BUFFER;
        }

        /* allocate a buffer to hold packed message */
        sendreq->req_addr = mca_pml_bsend_allocator->alc_alloc(
            mca_pml_bsend_allocator, sendreq->req_bytes_packed, 0, NULL);
        if(NULL == sendreq->req_addr) {
            /* release resources when request is freed */
            sendreq->req_base.req_pml_complete = true;
            OPAL_THREAD_UNLOCK(&mca_pml_bsend_mutex);
            return OMPI_ERR_BUFFER;
        }
    
        /* setup request to reflect the contigous buffer */
        sendreq->req_count = sendreq->req_bytes_packed;
        sendreq->req_datatype = MPI_BYTE;

        /* increment count of pending requests */
        mca_pml_bsend_count++;
        OPAL_THREAD_UNLOCK(&mca_pml_bsend_mutex);

        /* The convertor is already initialized in the begining so we just have to
         * pack the data in the newly allocated buffer.
         */
        iov.iov_base = sendreq->req_addr;
        iov.iov_len = sendreq->req_count;
        iov_count = 1;
        max_data = iov.iov_len;
        if((rc = ompi_convertor_pack( &sendreq->req_convertor, &iov, &iov_count, 
                                      &max_data, &freeAfter )) <= 0) {
            return OMPI_ERROR;
        }
 
        /* setup convertor to point to packed buffer (at position zero) */
        ompi_convertor_prepare_for_send( &sendreq->req_convertor, MPI_PACKED,
                                         max_data, sendreq->req_addr );
    }
    sendreq->req_base.req_ompi.req_complete = true;
    return OMPI_SUCCESS;
}


/*
 *  Request completed - free buffer and decrement pending count 
 */
int mca_pml_base_bsend_request_fini(ompi_request_t* request)
{
    mca_pml_base_send_request_t* sendreq = (mca_pml_base_send_request_t*)request;
    if(sendreq->req_count == 0 || sendreq->req_addr == NULL)
        return OMPI_SUCCESS;

    /* remove from list of pending requests */
    OPAL_THREAD_LOCK(&mca_pml_bsend_mutex);

    /* free buffer */
    mca_pml_bsend_allocator->alc_free(mca_pml_bsend_allocator, sendreq->req_addr);
    sendreq->req_addr = NULL;

    /* decrement count of buffered requests */
    if(--mca_pml_bsend_count == 0)
        opal_condition_signal(&mca_pml_bsend_condition);

    OPAL_THREAD_UNLOCK(&mca_pml_bsend_mutex);
    return OMPI_SUCCESS;
}
                                                                                                             

