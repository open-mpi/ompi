/*
 *  Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                          All rights reserved.
 *  Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                          All rights reserved.
 *  $COPYRIGHT$
 *  
 *  Additional copyrights may follow
 *  
 *  $HEADER$
 */

#include "ompi_config.h"
#include "mpi.h"
#include "file/file.h"
#include "request/request.h"
#include "io_romio.h"
#include "mca/io/base/io_base_request.h"


/*
 * JMS This function is pretty incomplete and depends on discussions
 * with Tim about asynchronous progress.  Keep watching this space...
 */
int
mca_io_romio_test (mca_io_base_request_t * request,
                   int *flag,
                   ompi_status_public_t * status)
{
#if 0
    int         ret;
    mca_io_romio_request_t *rq;
    ROMIO_PREFIX(MPIO_Request) romio_rq;

    rq = (mca_io_romio_request_t *) (*request);
    romio_rq = rq->romio_rq;

    OMPI_THREAD_LOCK (&mca_io_romio_mutex);
    ret = ROMIO_PREFIX(MPIO_Test)(&romio_rq, flag, status);
    OMPI_THREAD_UNLOCK (&mca_io_romio_mutex);

    if (*flag) {
        free (*request);
        *request = MPI_REQUEST_NULL;
    }

    return ret;
#else
    return OMPI_ERR_NOT_IMPLEMENTED;
#endif
}


/*
 * JMS This function is pretty incomplete and depends on discussions
 * with Tim about asynchronous progress.  Keep watching this space...
 */
int
mca_io_romio_wait (mca_io_base_request_t * request,
                   ompi_status_public_t * status)
{
#if 0
    int         ret;
    mca_io_romio_request_t *rq;
    ROMIO_PREFIX(MPIO_Request) romio_rq;

    rq = (mca_io_romio_request_t *) (*request);
    romio_rq = rq->romio_rq;


    OMPI_THREAD_LOCK (&mca_io_romio_mutex);
    ret = ROMIO_PREFIX(MPIO_Wait)(&romio_rq, status);
    OMPI_THREAD_UNLOCK (&mca_io_romio_mutex);

    free (*request);
    *request = MPI_REQUEST_NULL;
    return ret;
#else
    return OMPI_ERR_NOT_IMPLEMENTED;
#endif
}
