/*
 *  $HEADER$
 */

#include "mpi.h"
#include "mpi/file/file.h"
#include "io_romio.h"
#include "mpi/request/request.h"
#include "lam/mem/malloc.h"
#include <string.h>

int mca_io_romio_File_open(MPI_Comm comm, char *filename, int amode,
                         MPI_Info info, MPI_File *fh){

    int ret;
    mca_io_romio_MPI_File romio_fh;
    mca_io_romio_file_t *mca_romio_fh;  

    mca_romio_fh = LAM_MALLOC(sizeof(mca_io_romio_file_t));
    (*fh) = (lam_file_t *) mca_romio_fh;

    strncpy((*fh)->f_name,filename,MPI_MAX_OBJECT_NAME);

    
    romio_fh = mca_romio_fh->romio_fh;

    THREAD_LOCK(&mca_io_romio_mutex);
    ret=mca_io_romio_MPI_File_open(comm,filename,amode,info,&romio_fh);
    THREAD_UNLOCK(&mca_io_romio_mutex);

    return ret;
}
    



