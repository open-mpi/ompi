/*
 *  Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                          University Research and Technology
 *                          Corporation.  All rights reserved.
 *  Copyright (c) 2004-2005 The University of Tennessee and The University
 *                          of Tennessee Research Foundation.  All rights
 *                          reserved.
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
#include "ompi/file/file.h"
#include "ompi/request/request.h"
#include "ompi/mca/io/base/io_base_request.h"
#include "io_romio.h"


int mca_io_romio_request_free(ompi_request_t **req)
{
    mca_io_base_request_t *ioreq = *((mca_io_base_request_t**) req);

    OPAL_THREAD_LOCK(&mca_io_romio_mutex);

    /* clean up the fortran stuff, mark us as invalid */
    OMPI_REQUEST_FINI(*req);

    ioreq->free_called = true;

    /* if the thing is done already, finalize it and get out... */
    if (ioreq->super.req_complete) {
        mca_io_base_request_free(ioreq->req_file, ioreq);
    }

    OPAL_THREAD_UNLOCK(&mca_io_romio_mutex);

    *req = MPI_REQUEST_NULL;
    return OMPI_SUCCESS;
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
