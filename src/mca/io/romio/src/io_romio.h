/*
 * $HEADER
 */

#ifndef MCA_IO_ROMIO_H
#define MCA_IO_ROMIO_H

#include "mpi/request/request.h"
#include "mpi/file/file.h"
#include "lam/threads/mutex.h"
#include "romio-1.2.5.1/include/mpio.h" 

/* global variables, instantiated in global.c  */
extern lam_mutex_t mca_io_romio_mutex;
extern mca_io_1_0_0_t romio_actions;


/* 
MPI_Request:  "inherit" the lam_request, and add more stuff:
(romio module will instantiate)
*/
struct mca_io_romio_request_t {
    lam_request_t  super;
    mca_io_romio_MPIO_Request  romio_rq; 
};
typedef struct mca_io_romio_request_t  mca_io_romio_request_t;


/* 
MPI_File:  "inherit" the lam_file_t, and add more stuff
(romio module will instantiate)
*/
struct mca_io_romio_file_t {
    lam_file_t super;
    mca_io_romio_MPI_File romio_fh;
};
typedef struct mca_io_romio_file_t mca_io_romio_file_t;




/* function prototypes 
*
*  ROMIO MCA module routines are named:    mca_io_romio_File_routine
*  which wrap the original Romio routines, which have been renamed
*  mca_io_romio_MPI_File_routine
*
*/
/* Section 9.2 */
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




/* Section 9.3 */
int mca_io_romio_File_set_view(MPI_File fh, MPI_Offset disp, MPI_Datatype etype,
	         MPI_Datatype filetype, char *datarep, MPI_Info info);
int mca_io_romio_File_get_view(MPI_File fh, MPI_Offset *disp, 
                 MPI_Datatype *etype, MPI_Datatype *filetype, char *datarep);

/* Section 9.4.2 */
int mca_io_romio_File_read_at(MPI_File fh, MPI_Offset offset, void *buf,
	      int count, MPI_Datatype datatype, MPI_Status *status);
int mca_io_romio_File_read_at_all(MPI_File fh, MPI_Offset offset, void *buf,
	      int count, MPI_Datatype datatype, MPI_Status *status);
int mca_io_romio_File_write_at(MPI_File fh, MPI_Offset offset, void *buf,
	      int count, MPI_Datatype datatype, MPI_Status *status);
int mca_io_romio_File_write_at_all(MPI_File fh, MPI_Offset offset, void *buf,
	      int count, MPI_Datatype datatype, MPI_Status *status);


int mca_io_romio_File_iread_at(MPI_File fh, MPI_Offset offset, void *buf,
	      int count, MPI_Datatype datatype, MPI_Request *request);
int mca_io_romio_File_iwrite_at(MPI_File fh, MPI_Offset offset, void *buf,
	      int count, MPI_Datatype datatype, MPI_Request *request);


/* Section 9.4.3 */
int mca_io_romio_File_read(MPI_File fh, void *buf, int count, MPI_Datatype
	     datatype, MPI_Status *status); 
int mca_io_romio_File_read_all(MPI_File fh, void *buf, int count, MPI_Datatype
	     datatype, MPI_Status *status); 
int mca_io_romio_File_write(MPI_File fh, void *buf, int count, MPI_Datatype
	      datatype, MPI_Status *status);
int mca_io_romio_File_write_all(MPI_File fh, void *buf, int count, MPI_Datatype
	      datatype, MPI_Status *status);


int mca_io_romio_File_iread(MPI_File fh, void *buf, int count, MPI_Datatype
	     datatype, MPI_Request *request); 
int mca_io_romio_File_iwrite(MPI_File fh, void *buf, int count, 
                             MPI_Datatype datatype, lam_request_t **request);


int mca_io_romio_File_seek(MPI_File fh, MPI_Offset offset, int whence);
int mca_io_romio_File_get_position(MPI_File fh, MPI_Offset *offset);
int mca_io_romio_File_get_byte_offset(MPI_File fh, MPI_Offset offset, 
                                     MPI_Offset *disp);

/* Section 9.4.4 */
int mca_io_romio_File_read_shared(MPI_File fh, void *buf, int count, 
                         MPI_Datatype datatype, MPI_Status *status);
int mca_io_romio_File_write_shared(MPI_File fh, void *buf, int count, 
                          MPI_Datatype datatype, MPI_Status *status);
int mca_io_romio_File_iread_shared(MPI_File fh, void *buf, int count, 
                          MPI_Datatype datatype, MPI_Request *request);
int mca_io_romio_File_iwrite_shared(MPI_File fh, void *buf, int count, 
                           MPI_Datatype datatype, MPI_Request *request);
int mca_io_romio_File_read_ordered(MPI_File fh, void *buf, int count, 
                          MPI_Datatype datatype, MPI_Status *status);
int mca_io_romio_File_write_ordered(MPI_File fh, void *buf, int count, 
                           MPI_Datatype datatype, MPI_Status *status);
int mca_io_romio_File_seek_shared(MPI_File fh, MPI_Offset offset, int whence);
int mca_io_romio_File_get_position_shared(MPI_File fh, MPI_Offset *offset);

/* Section 9.4.5 */
int mca_io_romio_File_read_at_all_begin(MPI_File fh, MPI_Offset offset, void *buf,
                               int count, MPI_Datatype datatype);
int mca_io_romio_File_read_at_all_end(MPI_File fh, void *buf, MPI_Status *status);
int mca_io_romio_File_write_at_all_begin(MPI_File fh, MPI_Offset offset, void *buf,
                                int count, MPI_Datatype datatype);
int mca_io_romio_File_write_at_all_end(MPI_File fh, void *buf, MPI_Status *status);
int mca_io_romio_File_read_all_begin(MPI_File fh, void *buf, int count, 
                            MPI_Datatype datatype);
int mca_io_romio_File_read_all_end(MPI_File fh, void *buf, MPI_Status *status);
int mca_io_romio_File_write_all_begin(MPI_File fh, void *buf, int count, 
                             MPI_Datatype datatype);
int mca_io_romio_File_write_all_end(MPI_File fh, void *buf, MPI_Status *status);
int mca_io_romio_File_read_ordered_begin(MPI_File fh, void *buf, int count, 
                                MPI_Datatype datatype);
int mca_io_romio_File_read_ordered_end(MPI_File fh, void *buf, MPI_Status *status);
int mca_io_romio_File_write_ordered_begin(MPI_File fh, void *buf, int count, 
                                 MPI_Datatype datatype);
int mca_io_romio_File_write_ordered_end(MPI_File fh, void *buf, MPI_Status *status);

/* Section 9.5.1 */
int mca_io_romio_File_get_type_extent(MPI_File fh, MPI_Datatype datatype, 
                                     MPI_Aint *extent);

/* Section 9.6.1 */
int mca_io_romio_File_set_atomicity(MPI_File fh, int flag);
int mca_io_romio_File_get_atomicity(MPI_File fh, int *flag);
int mca_io_romio_File_sync(MPI_File fh);

/* Section 4.13.3 */
int mca_io_romio_File_set_errhandler( MPI_File, MPI_Errhandler );
int mca_io_romio_File_get_errhandler( MPI_File, MPI_Errhandler * );
/* End Prototypes */



/* The funtions will not be called by users, but by LAM's MPI_Test/Wait
 *   functions when they are called with an I/O request. */
int mca_io_romio_Test(MPI_Request *request, int *flag, MPI_Status *status);
int mca_io_romio_Wait(MPI_Request *request, MPI_Status *status);


#endif /* MCA_IO_ROMIO_H */
