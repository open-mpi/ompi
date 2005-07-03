/*
 *  Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                          All rights reserved.
 *  Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                          All rights reserved.
 *  Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                          University of Stuttgart.  All rights reserved.
 *  Copyright (c) 2004-2005 The Regents of the University of California.
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


int mca_io_romio_request_fini(ompi_request_t **req)
{
    mca_io_base_request_t *ioreq = *((mca_io_base_request_t**) req);
    int ret = OMPI_SUCCESS;

    OPAL_THREAD_LOCK(&mca_io_romio_mutex);

    /* clean up the fortran stuff, mark us as invalid */
    OMPI_REQUEST_FINI(*req);
    /* and shove us back in the free list */
    mca_io_base_request_free(ioreq->req_file, ioreq);

    OPAL_THREAD_UNLOCK(&mca_io_romio_mutex);

    *req = MPI_REQUEST_NULL;
    return ret;
}


int mca_io_romio_request_free(ompi_request_t **req)
{
    mca_io_base_request_t *ioreq = *((mca_io_base_request_t**) req);
    int ret = OMPI_SUCCESS;

    OPAL_THREAD_LOCK(&mca_io_romio_mutex);

    ioreq->free_called = true;

    /* if the thing is done already, finalize it and get out... */
    if (ioreq->super.req_complete) {
        ret = ioreq->super.req_fini(req);
    }

    OPAL_THREAD_UNLOCK(&mca_io_romio_mutex);

    *req = MPI_REQUEST_NULL;
    return ret;
}


/*
 * ROMIO doesn't allow anything to be cancelled
 */
int mca_io_romio_request_cancel(ompi_request_t *req, int flag)
{
    /* BWB - do we really want to return an error here or just a bad
       flag? */
    return OMPI_ERROR;
}
