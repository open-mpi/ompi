/*
 *  $HEADER$
 */

#include "mpi.h"
#include "mpi/file/file.h"
#include "io_romio.h"
#include "mpi/request/request.h"
#include "lam/mem/malloc.h"

int mca_io_romio_File_iwrite(MPI_File fh, void *buf, int count, 
                              MPI_Datatype datatype, lam_request_t **request);
int mca_io_romio_File_iwrite(MPI_File fh, void *buf, int count, 
                         MPI_Datatype datatype, lam_request_t **request)
{
    mca_io_romio_request_t *rq;
    mca_io_romio_MPI_File romio_fh;
    int ret;

    rq = LAM_MALLOC(sizeof(mca_io_romio_request_t));
    *request = (lam_request_t*) rq;
    (*request)->req_type = LAM_REQUEST_IO;

    romio_fh = fh->f_io_file->romio_fh;

    THREAD_LOCK(&mca_io_romio_mutex);
    ret=mca_io_romio_MPI_File_iwrite(romio_fh, buf, count, datatype,
                                 &rq->romio_rq);
    THREAD_UNLOCK(&mca_io_romio_mutex);
    return ret;
}
