/*
 * $HEADER
 */

#ifndef MCA_IO_ROMIO_H
#define MCA_IO_ROMIO_H

#include "mpi/request/request.h"
#include "mpi/file/file.h"
#include "lam/threads/mutex.h"


#include "romio-1.2.5.1/include/mpio.h" 


struct mca_io_romio_request_t {
    lam_request_t  super;
    mca_io_romio_MPIO_Request romio_rq; 
};
typedef struct mca_io_romio_request_t  mca_io_romio_request_t;



struct mca_io_romio_file_t {
    lam_file_t super;
    mca_io_romio_MPI_File romio_fh;
};
typedef struct mca_io_romio_file_t mca_io_romio_file_t;



/* global variables, instantiated in global.c  */
extern lam_mutex_t mca_io_romio_mutex;




/* function prototypes */
int mca_io_romio_File_open(MPI_Comm comm, char *filename, int amode,
                          MPI_Info info, MPI_File *fh);
int mca_io_romio_File_close(MPI_File *fh);
int mca_io_romio_File_delete(char *filename, MPI_Info info);
int mca_io_romio_File_set_size(MPI_File fh, MPI_Offset size);
int mca_io_romio_File_preallocate(MPI_File fh, MPI_Offset size);
int mca_io_romio_File_get_size(MPI_File fh, MPI_Offset *size);
int mca_io_romio_File_get_group(MPI_File fh, MPI_Group *group);
int mca_io_romio_File_get_amode(MPI_File fh, int *amode);
int mca_io_romio_File_set_info(MPI_File fh, MPI_Info info);
int mca_io_romio_File_get_info(MPI_File fh, MPI_Info *info_used);


int mca_io_romio_File_iwrite(MPI_File fh, void *buf, int count, 
                             MPI_Datatype datatype, lam_request_t **request);


#endif /* MCA_IO_ROMIO_H */
