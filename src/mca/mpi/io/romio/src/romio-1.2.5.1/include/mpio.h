/*
 * $HEADER
 *
 * The actual ROMIO routines.
 * all of these routines have been renamed: prefix with mca_io_romio
 *
 * ROMIO specific types have also been renamed:  (cant do this with #define
 * since this file is included by LAM's I/O module)
 * all ROMIO references to:
 *    MPIO_Request changed to:  mca_io_romio_MPIO_Request
 *    MPI_File     changed to:  mca_io_romio_MPI_File
 *
 *  All other types have been deleted, since they appear in LAM's mpi.h
 *  
 */

#ifndef MCA_IO_ROMIO_DUMMY_H
#define MCA_IO_ROMIO_DUMMY_H

typedef struct ADIOI_FileD *mca_io_romio_MPI_File;
typedef struct ADIOI_RequestD *mca_io_romio_MPIO_Request;  





/* Section 9.2 */
int mca_io_romio_MPI_File_open(MPI_Comm comm, char *filename, int amode,
                          MPI_Info info, mca_io_romio_MPI_File *fh);
int mca_io_romio_MPI_File_close(mca_io_romio_MPI_File *fh);
int mca_io_romio_MPI_File_delete(char *filename, MPI_Info info);
int mca_io_romio_MPI_File_set_size(mca_io_romio_MPI_File fh, MPI_Offset size);
int mca_io_romio_MPI_File_preallocate(mca_io_romio_MPI_File fh, MPI_Offset size);
int mca_io_romio_MPI_File_get_size(mca_io_romio_MPI_File fh, MPI_Offset *size);
int mca_io_romio_MPI_File_get_group(mca_io_romio_MPI_File fh, MPI_Group *group);
int mca_io_romio_MPI_File_get_amode(mca_io_romio_MPI_File fh, int *amode);
int mca_io_romio_MPI_File_set_info(mca_io_romio_MPI_File fh, MPI_Info info);
int mca_io_romio_MPI_File_get_info(mca_io_romio_MPI_File fh, MPI_Info *info_used);




/* Section 9.3 */
int mca_io_romio_MPI_File_set_view(mca_io_romio_MPI_File fh, MPI_Offset disp, MPI_Datatype etype,
	         MPI_Datatype filetype, char *datarep, MPI_Info info);
int mca_io_romio_MPI_File_get_view(mca_io_romio_MPI_File fh, MPI_Offset *disp, 
                 MPI_Datatype *etype, MPI_Datatype *filetype, char *datarep);

/* Section 9.4.2 */
int mca_io_romio_MPI_File_read_at(mca_io_romio_MPI_File fh, MPI_Offset offset, void *buf,
	      int count, MPI_Datatype datatype, MPI_Status *status);
int mca_io_romio_MPI_File_read_at_all(mca_io_romio_MPI_File fh, MPI_Offset offset, void *buf,
	      int count, MPI_Datatype datatype, MPI_Status *status);
int mca_io_romio_MPI_File_write_at(mca_io_romio_MPI_File fh, MPI_Offset offset, void *buf,
	      int count, MPI_Datatype datatype, MPI_Status *status);
int mca_io_romio_MPI_File_write_at_all(mca_io_romio_MPI_File fh, MPI_Offset offset, void *buf,
	      int count, MPI_Datatype datatype, MPI_Status *status);


int mca_io_romio_MPI_File_iread_at(mca_io_romio_MPI_File fh, MPI_Offset offset, void *buf,
	      int count, MPI_Datatype datatype, mca_io_romio_MPIO_Request *request);
int mca_io_romio_MPI_File_iwrite_at(mca_io_romio_MPI_File fh, MPI_Offset offset, void *buf,
	      int count, MPI_Datatype datatype, mca_io_romio_MPIO_Request *request);


/* Section 9.4.3 */
int mca_io_romio_MPI_File_read(mca_io_romio_MPI_File fh, void *buf, int count, MPI_Datatype
	     datatype, MPI_Status *status); 
int mca_io_romio_MPI_File_read_all(mca_io_romio_MPI_File fh, void *buf, int count, MPI_Datatype
	     datatype, MPI_Status *status); 
int mca_io_romio_MPI_File_write(mca_io_romio_MPI_File fh, void *buf, int count, MPI_Datatype
	      datatype, MPI_Status *status);
int mca_io_romio_MPI_File_write_all(mca_io_romio_MPI_File fh, void *buf, int count, MPI_Datatype
	      datatype, MPI_Status *status);


int mca_io_romio_MPI_File_iread(mca_io_romio_MPI_File fh, void *buf, int count, MPI_Datatype
	     datatype, mca_io_romio_MPIO_Request *request); 
int mca_io_romio_MPI_File_iwrite(mca_io_romio_MPI_File fh, void *buf, int count, 
                             MPI_Datatype datatype, mca_io_romio_MPIO_Request *request);


int mca_io_romio_MPI_File_seek(mca_io_romio_MPI_File fh, MPI_Offset offset, int whence);
int mca_io_romio_MPI_File_get_position(mca_io_romio_MPI_File fh, MPI_Offset *offset);
int mca_io_romio_MPI_File_get_byte_offset(mca_io_romio_MPI_File fh, MPI_Offset offset, 
                                     MPI_Offset *disp);

/* Section 9.4.4 */
int mca_io_romio_MPI_File_read_shared(mca_io_romio_MPI_File fh, void *buf, int count, 
                         MPI_Datatype datatype, MPI_Status *status);
int mca_io_romio_MPI_File_write_shared(mca_io_romio_MPI_File fh, void *buf, int count, 
                          MPI_Datatype datatype, MPI_Status *status);
int mca_io_romio_MPI_File_iread_shared(mca_io_romio_MPI_File fh, void *buf, int count, 
                          MPI_Datatype datatype, mca_io_romio_MPIO_Request *request);
int mca_io_romio_MPI_File_iwrite_shared(mca_io_romio_MPI_File fh, void *buf, int count, 
                           MPI_Datatype datatype, mca_io_romio_MPIO_Request *request);
int mca_io_romio_MPI_File_read_ordered(mca_io_romio_MPI_File fh, void *buf, int count, 
                          MPI_Datatype datatype, MPI_Status *status);
int mca_io_romio_MPI_File_write_ordered(mca_io_romio_MPI_File fh, void *buf, int count, 
                           MPI_Datatype datatype, MPI_Status *status);
int mca_io_romio_MPI_File_seek_shared(mca_io_romio_MPI_File fh, MPI_Offset offset, int whence);
int mca_io_romio_MPI_File_get_position_shared(mca_io_romio_MPI_File fh, MPI_Offset *offset);

/* Section 9.4.5 */
int mca_io_romio_MPI_File_read_at_all_begin(mca_io_romio_MPI_File fh, MPI_Offset offset, void *buf,
                               int count, MPI_Datatype datatype);
int mca_io_romio_MPI_File_read_at_all_end(mca_io_romio_MPI_File fh, void *buf, MPI_Status *status);
int mca_io_romio_MPI_File_write_at_all_begin(mca_io_romio_MPI_File fh, MPI_Offset offset, void *buf,
                                int count, MPI_Datatype datatype);
int mca_io_romio_MPI_File_write_at_all_end(mca_io_romio_MPI_File fh, void *buf, MPI_Status *status);
int mca_io_romio_MPI_File_read_all_begin(mca_io_romio_MPI_File fh, void *buf, int count, 
                            MPI_Datatype datatype);
int mca_io_romio_MPI_File_read_all_end(mca_io_romio_MPI_File fh, void *buf, MPI_Status *status);
int mca_io_romio_MPI_File_write_all_begin(mca_io_romio_MPI_File fh, void *buf, int count, 
                             MPI_Datatype datatype);
int mca_io_romio_MPI_File_write_all_end(mca_io_romio_MPI_File fh, void *buf, MPI_Status *status);
int mca_io_romio_MPI_File_read_ordered_begin(mca_io_romio_MPI_File fh, void *buf, int count, 
                                MPI_Datatype datatype);
int mca_io_romio_MPI_File_read_ordered_end(mca_io_romio_MPI_File fh, void *buf, MPI_Status *status);
int mca_io_romio_MPI_File_write_ordered_begin(mca_io_romio_MPI_File fh, void *buf, int count, 
                                 MPI_Datatype datatype);
int mca_io_romio_MPI_File_write_ordered_end(mca_io_romio_MPI_File fh, void *buf, MPI_Status *status);

/* Section 9.5.1 */
int mca_io_romio_MPI_File_get_type_extent(mca_io_romio_MPI_File fh, MPI_Datatype datatype, 
                                     MPI_Aint *extent);

/* Section 9.6.1 */
int mca_io_romio_MPI_File_set_atomicity(mca_io_romio_MPI_File fh, int flag);
int mca_io_romio_MPI_File_get_atomicity(mca_io_romio_MPI_File fh, int *flag);
int mca_io_romio_MPI_File_sync(mca_io_romio_MPI_File fh);

/* Section 4.13.3 */
int mca_io_romio_MPI_File_set_errhandler(mca_io_romio_MPI_File, MPI_Errhandler );
int mca_io_romio_MPI_File_get_errhandler(mca_io_romio_MPI_File, MPI_Errhandler * );
/* End Prototypes */


int mca_io_romio_MPIO_Test(mca_io_romio_MPIO_Request *request, int *flag, MPI_Status *status);
int mca_io_romio_MPIO_Wait(mca_io_romio_MPIO_Request *request, MPI_Status *status);
MPI_Fint mca_io_romio_MPIO_Request_c2f(mca_io_romio_MPIO_Request request);
mca_io_romio_MPIO_Request mca_io_romio_MPIO_Request_f2c(MPI_Fint request);




#endif
