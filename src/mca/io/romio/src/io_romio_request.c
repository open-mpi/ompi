/*
 *  Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                          All rights reserved.
 *  Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                          All rights reserved.
 *  Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                          University of Stuttgart.  All rights reserved.
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
    return OMPI_SUCCESS;
}


int mca_io_romio_request_free(ompi_request_t **req)
{
    return OMPI_SUCCESS;
}


/*
 * ROMIO doesn't allow anything to be cancelled
 */
int mca_io_romio_request_cancel(ompi_request_t *req, int flag)
{
    return OMPI_ERROR;
}
