/*
 * $HEADER
 */

#ifndef MCA_IO_ROMIO_DUMMY_H
#define MCA_IO_ROMIO_DUMMY_H


typedef char *mca_io_romio_MPIO_Request;
typedef char *mca_io_romio_MPI_File;


int mca_io_romio_MPI_File_iwrite(mca_io_romio_MPI_File, void *,
                             int, MPI_Datatype, mca_io_romio_MPIO_Request);


int mca_io_romio_MPI_File_open(MPI_Comm comm, char *filename, int amode,
                         MPI_Info info, mca_io_romio_MPI_File *fh);


#endif
