#include "threads/mutex.h"
#include "threads/condition.h"
#include "mca/allocator/base/base.h"
#include "mca/allocator/allocator.h"
#include "mca/base/mca_base_param.h"
#include "mca/pml/pml.h"
#include "mca/pml/base/pml_base_request.h"
#include "mca/pml/base/pml_base_sendreq.h"
#include "pml_base_bsend.h"
 
static ompi_mutex_t     mca_pml_bsend_mutex;      /* lock for thread safety */
static ompi_condition_t mca_pml_bsend_condition;  /* condition variable to block on detach */
static mca_allocator_base_module_t* mca_pml_bsend_allocator_component;  
static mca_allocator_t* mca_pml_bsend_allocator;  /* sub-allocator to manage users buffer */
static unsigned char   *mca_pml_bsend_base;       /* base address of users buffer */
static unsigned char   *mca_pml_bsend_addr;       /* current offset into users buffer */
static size_t           mca_pml_bsend_size;       /* size of users buffer */
static size_t           mca_pml_bsend_count;      /* number of outstanding requests */
static size_t           mca_pml_bsend_pagesz;     /* mmap page size */
static int              mca_pml_bsend_pagebits;   /* number of bits in pagesz */



/*
 * Routine to return pages to sub-allocator as needed 
 */
static void* mca_pml_bsend_alloc_segment(size_t* size_inout)
{
    void *addr;
    size_t size = *size_inout;
    size_t pages = 1;

    /* determine number of pages to allocate */
    while(size > mca_pml_bsend_pagesz) {
        size >>= mca_pml_bsend_pagebits;
        pages++;
    }

    if(mca_pml_bsend_addr + size > mca_pml_bsend_base + mca_pml_bsend_size) {
        if( mca_pml_bsend_addr + *size_inout <= mca_pml_bsend_base + mca_pml_bsend_size )  {
            size = *size_inout;
        } else {
            OMPI_THREAD_UNLOCK(&mca_pml_bsend_mutex);
            return NULL;
        }
    }
    addr = mca_pml_bsend_addr;
    mca_pml_bsend_addr += size;
    *size_inout = size;
    return addr;
}


/*
 * One time initialization at startup
 */
int mca_pml_base_bsend_init(bool* thread_safe)
{
    int id = mca_base_param_register_string("pml", "base", "bsend_allocator", NULL, "bucket");
    mca_allocator_t *allocator;
    char *name;
    size_t tmp;

    /* initialize static objects */
    OBJ_CONSTRUCT(&mca_pml_bsend_mutex, ompi_mutex_t);
    OBJ_CONSTRUCT(&mca_pml_bsend_condition, ompi_condition_t);

    /* lookup name of the allocator to use for buffered sends */
    mca_base_param_lookup_string(id, &name);
    if(NULL == (mca_pml_bsend_allocator_component = mca_allocator_component_lookup(name))) {
        free(name);
        return OMPI_ERR_BUFFER;
    }
    free(name);

    /* try to create an instance of the allocator - to determine thread safety level */
    allocator = mca_pml_bsend_allocator_component->allocator_init(thread_safe, mca_pml_bsend_alloc_segment, NULL);
    if(NULL == allocator) {
        return OMPI_ERR_BUFFER;
    }
    allocator->alc_finalize(allocator);

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
    bool thread_safe;
    if(NULL == addr || size <= 0) {
        return OMPI_ERR_BUFFER;
    }

    /* check for buffer already attached */
    OMPI_THREAD_LOCK(&mca_pml_bsend_mutex);
    if(NULL != mca_pml_bsend_allocator) {
        OMPI_THREAD_UNLOCK(&mca_pml_bsend_mutex);
        return OMPI_ERR_BUFFER;
    }

    /* try to create an instance of the allocator - to determine thread safety level */
    mca_pml_bsend_allocator = mca_pml_bsend_allocator_component->allocator_init(&thread_safe, mca_pml_bsend_alloc_segment, NULL);
    if(NULL == mca_pml_bsend_allocator) {
        OMPI_THREAD_UNLOCK(&mca_pml_bsend_mutex);
        return OMPI_ERR_BUFFER;
    }

    /* setup local variables */
    mca_pml_bsend_base = addr;
    mca_pml_bsend_addr = addr;
    mca_pml_bsend_size = size;
    mca_pml_bsend_count = 0;
    OMPI_THREAD_UNLOCK(&mca_pml_bsend_mutex);
    return OMPI_SUCCESS;
}

/*
 * User-level call to detach buffer 
 */
int mca_pml_base_bsend_detach(void* addr, int* size)
{
    OMPI_THREAD_LOCK(&mca_pml_bsend_mutex);

    /* is buffer attached */
    if(NULL == mca_pml_bsend_allocator) {
        OMPI_THREAD_UNLOCK(&mca_pml_bsend_mutex);
        return OMPI_ERR_BUFFER;
    }

    /* wait on any pending requests */
    while(mca_pml_bsend_count != 0)
        ompi_condition_wait(&mca_pml_bsend_condition, &mca_pml_bsend_mutex);
    
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
    OMPI_THREAD_UNLOCK(&mca_pml_bsend_mutex);
    return OMPI_SUCCESS;
} 

     
/*
 *  Initialize a request for use w/ buffered send 
 */                                                                                                        
int mca_pml_base_bsend_request_init(ompi_request_t* request, bool persistent)
{
    mca_pml_base_send_request_t* sendreq = (mca_pml_base_send_request_t*)request;
    struct iovec iov;
    void* buf;
    int rc;
 
    OMPI_THREAD_LOCK(&mca_pml_bsend_mutex);
    if(NULL == mca_pml_bsend_addr) {
        OMPI_THREAD_UNLOCK(&mca_pml_bsend_mutex);
        return OMPI_ERR_BUFFER;
    }

    /* allocate a buffer to hold packed message */
    buf = mca_pml_bsend_allocator->alc_alloc(mca_pml_bsend_allocator, sendreq->req_bytes_packed, 0);
    if(NULL == buf) {
        OMPI_THREAD_UNLOCK(&mca_pml_bsend_mutex);
        return OMPI_ERR_BUFFER;
    }
    
    /* pack users message into buffer */
    iov.iov_base = buf;
    iov.iov_len = sendreq->req_bytes_packed;
    if((rc = ompi_convertor_pack(&sendreq->req_convertor, &iov, 1)) < 0) {
        mca_pml_bsend_allocator->alc_free(mca_pml_bsend_allocator, buf);
        OMPI_THREAD_UNLOCK(&mca_pml_bsend_mutex);
        return OMPI_ERROR;
    }

    /* setup convertor to reflect contiguous buffer */
    if((rc = ompi_convertor_init_for_send(&sendreq->req_convertor, 0, MPI_BYTE, iov.iov_len, iov.iov_base, 0)) != OMPI_SUCCESS) {
        mca_pml_bsend_allocator->alc_free(mca_pml_bsend_allocator, buf);
        OMPI_THREAD_UNLOCK(&mca_pml_bsend_mutex);
        return rc;
    }

    /* increment count of pending requests */
    mca_pml_bsend_count++;

    /* set flag indicating mpi layer is done */
    sendreq->super.req_persistent = persistent;
    sendreq->super.req_mpi_done = true;
    OMPI_THREAD_UNLOCK(&mca_pml_bsend_mutex);
    return OMPI_SUCCESS;
}
                                                                                                             

/*
 *  Request completed - free buffer and decrement pending count 
 */                                                                                                        
int mca_pml_base_bsend_request_fini(ompi_request_t* request)
{
    mca_pml_base_send_request_t* sendreq = (mca_pml_base_send_request_t*)request;

    /* remove from list of pending requests */
    OMPI_THREAD_LOCK(&mca_pml_bsend_mutex);

    /* free buffer */
    mca_pml_bsend_allocator->alc_free(mca_pml_bsend_allocator, sendreq->req_convertor.pBaseBuf);

    /* decrement count of buffered requests */
    if(--mca_pml_bsend_count == 0)
        ompi_condition_signal(&mca_pml_bsend_condition);

    OMPI_THREAD_UNLOCK(&mca_pml_bsend_mutex);
    return OMPI_SUCCESS;
}
                                                                                                             

