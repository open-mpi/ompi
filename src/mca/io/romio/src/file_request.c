/*
 *  $HEADER$
 */

#include "mpi.h"
#include "file/file.h"
#include "io_romio.h"
#include "request/request.h"
#include <string.h>


int mca_io_romio_Test(MPI_Request *request, int *flag, MPI_Status *status){ 
    int ret;
    mca_io_romio_request_t *rq;
    mca_io_romio_MPIO_Request romio_rq;

    /* extract the ROMIO request */
    rq=(mca_io_romio_request_t *)(*request);
    romio_rq = rq->romio_rq;


    OMPI_THREAD_LOCK(&mca_io_romio_mutex);
    ret=mca_io_romio_MPIO_Test(&romio_rq, flag,status);
    if (*flag) {
        free(*request);  
        *request = MPI_REQUEST_NULL;
    }
    OMPI_THREAD_UNLOCK(&mca_io_romio_mutex);
  
    return ret;
}


int mca_io_romio_Wait(MPI_Request *request, MPI_Status *status){ 
    int ret;
    mca_io_romio_request_t *rq;
    mca_io_romio_MPIO_Request romio_rq;

    /* extract the ROMIO request */
    rq=(mca_io_romio_request_t *)(*request);
    romio_rq = rq->romio_rq;


    OMPI_THREAD_LOCK(&mca_io_romio_mutex);
    ret=mca_io_romio_MPIO_Wait(&romio_rq, status);
    OMPI_THREAD_UNLOCK(&mca_io_romio_mutex);
    
    free(*request);  
    *request = MPI_REQUEST_NULL;
    return ret;
}

