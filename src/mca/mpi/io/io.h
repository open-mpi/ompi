/*
 * $HEADER
 */

#ifndef MCA_IO_H
#define MCA_IO_H

#include "mca/mca.h"

/*
 * Macro for use in modules that are of type io v1.0.0
 * 1-1 mapping of all MPI-IO functions
 */
#define MCA_IO_BASE_VERSION_1_0_0 \
  MCA_BASE_VERSION_1_0_0, \
  "io", 1, 0, 0

/*
 * Macro for use in modules that are of type io v2.0.0
 * as of yet undefined, but not 1-1 mapping of all MPI-IO functions
 */
#define MCA_IO_BASE_VERSION_2_0_0 \
  MCA_BASE_VERSION_1_0_0, \
  "io", 2, 0, 0


struct mca_io_1_0_0_t;
typedef struct mca_io_1_0_0_t * (*mca_io_base_module_init_fn_t)(
    int* priority, int* min_thread, int* max_thread);



/* IO module version and interface functions. */
struct mca_io_base_module_1_0_0_t {
   mca_base_module_t version;
   mca_base_module_data_1_0_0_t data;
   mca_io_base_module_init_fn_t init;
};
typedef struct mca_io_base_module_1_0_0_t mca_io_base_module_1_0_0_t;


/*
 * to add functions:  
 * add prototypes for all MPI-IO functions to io_romio.h
 * add typedefs for all MPI-IO functions to io.h
 * add all functions to struct mca_io_1_0_0_t in io.h
 * set all functions in struct in io_romio_module.c
 */


typedef int (*mca_io_base_File_open_t)(MPI_Comm comm, char *filename, int amode,
                          MPI_Info info, MPI_File *fh);
typedef int (*mca_io_base_File_close_t)(MPI_File *fh);
typedef int (*mca_io_base_File_delete_t)(char *filename, MPI_Info info);
typedef int (*mca_io_base_File_set_size_t)(MPI_File fh, MPI_Offset size);
typedef int (*mca_io_base_File_preallocate_t)(MPI_File fh, MPI_Offset size);
typedef int (*mca_io_base_File_get_size_t)(MPI_File fh, MPI_Offset *size);
typedef int (*mca_io_base_File_get_group_t)(MPI_File fh, MPI_Group *group);
typedef int (*mca_io_base_File_get_amode_t)(MPI_File fh, int *amode);
typedef int (*mca_io_base_File_set_info_t)(MPI_File fh, MPI_Info info);
typedef int (*mca_io_base_File_get_info_t)(MPI_File fh, MPI_Info *info_used);

typedef int (*mca_io_base_File_set_view_t)(MPI_File fh, MPI_Offset disp, MPI_Datatype etype,
	         MPI_Datatype filetype, char *datarep, MPI_Info info);
typedef int (*mca_io_base_File_get_view_t)(MPI_File fh, MPI_Offset *disp, 
                 MPI_Datatype *etype, MPI_Datatype *filetype, char *datarep);

typedef int (*mca_io_base_File_read_at_t)(MPI_File fh, MPI_Offset offset, void *buf,
	      int count, MPI_Datatype datatype, MPI_Status *status);
typedef int (*mca_io_base_File_read_at_all_t)(MPI_File fh, MPI_Offset offset, void *buf,
	      int count, MPI_Datatype datatype, MPI_Status *status);
typedef int (*mca_io_base_File_write_at_t)(MPI_File fh, MPI_Offset offset, void *buf,
	      int count, MPI_Datatype datatype, MPI_Status *status);
typedef int (*mca_io_base_File_write_at_all_t)(MPI_File fh, MPI_Offset offset, void *buf,
	      int count, MPI_Datatype datatype, MPI_Status *status);


typedef int (*mca_io_base_File_iread_at_t)(MPI_File fh, MPI_Offset offset, void *buf,
	      int count, MPI_Datatype datatype, MPI_Request *request);
typedef int (*mca_io_base_File_iwrite_at_t)(MPI_File fh, MPI_Offset offset, void *buf,
	      int count, MPI_Datatype datatype, MPI_Request *request);


typedef int (*mca_io_base_File_read_t)(MPI_File fh, void *buf, int count, MPI_Datatype
	     datatype, MPI_Status *status); 
typedef int (*mca_io_base_File_read_all_t)(MPI_File fh, void *buf, int count, MPI_Datatype
	     datatype, MPI_Status *status); 
typedef int (*mca_io_base_File_write_t)(MPI_File fh, void *buf, int count, MPI_Datatype
	      datatype, MPI_Status *status);
typedef int (*mca_io_base_File_write_all_t)(MPI_File fh, void *buf, int count, MPI_Datatype
	      datatype, MPI_Status *status);


typedef int (*mca_io_base_File_iread_t)(MPI_File fh, void *buf, int count, MPI_Datatype
	     datatype, MPI_Request *request); 
typedef int (*mca_io_base_File_iwrite_t)(MPI_File fh, void *buf, int count, 
                             MPI_Datatype datatype, MPI_Request *request);


typedef int (*mca_io_base_File_seek_t)(MPI_File fh, MPI_Offset offset, int whence);
typedef int (*mca_io_base_File_get_position_t)(MPI_File fh, MPI_Offset *offset);
typedef int (*mca_io_base_File_get_byte_offset_t)(MPI_File fh, MPI_Offset offset, 
                                     MPI_Offset *disp);

typedef int (*mca_io_base_File_read_shared_t)(MPI_File fh, void *buf, int count, 
                         MPI_Datatype datatype, MPI_Status *status);
typedef int (*mca_io_base_File_write_shared_t)(MPI_File fh, void *buf, int count, 
                          MPI_Datatype datatype, MPI_Status *status);
typedef int (*mca_io_base_File_iread_shared_t)(MPI_File fh, void *buf, int count, 
                          MPI_Datatype datatype, MPI_Request *request);
typedef int (*mca_io_base_File_iwrite_shared_t)(MPI_File fh, void *buf, int count, 
                           MPI_Datatype datatype, MPI_Request *request);
typedef int (*mca_io_base_File_read_ordered_t)(MPI_File fh, void *buf, int count, 
                          MPI_Datatype datatype, MPI_Status *status);
typedef int (*mca_io_base_File_write_ordered_t)(MPI_File fh, void *buf, int count, 
                           MPI_Datatype datatype, MPI_Status *status);
typedef int (*mca_io_base_File_seek_shared_t)(MPI_File fh, MPI_Offset offset, int whence);
typedef int (*mca_io_base_File_get_position_shared_t)(MPI_File fh, MPI_Offset *offset);

typedef int (*mca_io_base_File_read_at_all_begin_t)(MPI_File fh, MPI_Offset offset, void *buf,
                               int count, MPI_Datatype datatype);
typedef int (*mca_io_base_File_read_at_all_end_t)(MPI_File fh, void *buf, MPI_Status *status);
typedef int (*mca_io_base_File_write_at_all_begin_t)(MPI_File fh, MPI_Offset offset, void *buf,
                                int count, MPI_Datatype datatype);
typedef int (*mca_io_base_File_write_at_all_end_t)(MPI_File fh, void *buf, MPI_Status *status);
typedef int (*mca_io_base_File_read_all_begin_t)(MPI_File fh, void *buf, int count, 
                            MPI_Datatype datatype);
typedef int (*mca_io_base_File_read_all_end_t)(MPI_File fh, void *buf, MPI_Status *status);
typedef int (*mca_io_base_File_write_all_begin_t)(MPI_File fh, void *buf, int count, 
                             MPI_Datatype datatype);
typedef int (*mca_io_base_File_write_all_end_t)(MPI_File fh, void *buf, MPI_Status *status);
typedef int (*mca_io_base_File_read_ordered_begin_t)(MPI_File fh, void *buf, int count, 
                                MPI_Datatype datatype);
typedef int (*mca_io_base_File_read_ordered_end_t)(MPI_File fh, void *buf, MPI_Status *status);
typedef int (*mca_io_base_File_write_ordered_begin_t)(MPI_File fh, void *buf, int count, 
                                 MPI_Datatype datatype);
typedef int (*mca_io_base_File_write_ordered_end_t)(MPI_File fh, void *buf, MPI_Status *status);

typedef int (*mca_io_base_File_get_type_extent_t)(MPI_File fh, MPI_Datatype datatype, 
                                     MPI_Aint *extent);

typedef int (*mca_io_base_File_set_atomicity_t)(MPI_File fh, int flag);
typedef int (*mca_io_base_File_get_atomicity_t)(MPI_File fh, int *flag);
typedef int (*mca_io_base_File_sync_t)(MPI_File fh);

typedef int (*mca_io_base_File_set_errhandler_t)(MPI_File fh, MPI_Errhandler e);
typedef int (*mca_io_base_File_get_errhandler_t)(MPI_File fh, MPI_Errhandler *e );

typedef int (*mca_io_base_Test_t)(MPI_Request *request, int *flag, MPI_Status *status);
typedef int (*mca_io_base_Wait_t)(MPI_Request *request, MPI_Status *status);





struct mca_io_1_0_0_t {
    mca_io_base_File_open_t        mca_io_File_open;
    mca_io_base_File_close_t       mca_io_File_close;
    mca_io_base_File_delete_t      mca_io_File_delete;
    mca_io_base_File_set_size_t    mca_io_File_set_size;
    mca_io_base_File_preallocate_t mca_io_File_preallocate;
    mca_io_base_File_get_size_t    mca_io_File_get_size;
    mca_io_base_File_get_group_t   mca_io_File_get_group;
    mca_io_base_File_get_amode_t   mca_io_File_get_amode; 
    mca_io_base_File_set_info_t    mca_io_File_set_info;
    mca_io_base_File_get_info_t    mca_io_File_get_info;

    mca_io_base_File_set_view_t    mca_io_File_set_view;
    mca_io_base_File_get_view_t    mca_io_File_get_view;

    mca_io_base_File_read_at_t     mca_io_File_read_at;
    mca_io_base_File_read_at_all_t mca_io_File_read_at_all;
    mca_io_base_File_write_at_t    mca_io_File_write_at;
    mca_io_base_File_write_at_all_t  mca_io_File_write_at_all;

    mca_io_base_File_iread_at_t    mca_io_File_iread_at;
    mca_io_base_File_iwrite_at_t   mca_io_File_iwrite_at;

    mca_io_base_File_read_t        mca_io_File_read;
    mca_io_base_File_read_all_t    mca_io_File_read_all;
    mca_io_base_File_write_t       mca_io_File_write;
    mca_io_base_File_write_all_t   mca_io_File_write_all;

    mca_io_base_File_iread_t       mca_io_File_iread;
    mca_io_base_File_iwrite_t      mca_io_File_iwrite;

    mca_io_base_File_seek_t        mca_io_File_seek;
    mca_io_base_File_get_position_t mca_io_File_get_position;
    mca_io_base_File_get_byte_offset_t mca_io_File_get_byte_offset;

    mca_io_base_File_read_shared_t   mca_io_File_read_shared;
    mca_io_base_File_write_shared_t  mca_io_File_write_shared;
    mca_io_base_File_iread_shared_t  mca_io_File_iread_shared;
    mca_io_base_File_iwrite_shared_t mca_io_File_iwrite_shared;
    mca_io_base_File_read_ordered_t  mca_io_File_read_ordered;
    mca_io_base_File_write_ordered_t mca_io_File_write_ordered;
    mca_io_base_File_seek_shared_t   mca_io_File_seek_shared;
    mca_io_base_File_get_position_shared_t  mca_io_File_get_position_shared;

    mca_io_base_File_read_at_all_begin_t    mca_io_File_read_at_all_begin;
    mca_io_base_File_read_at_all_end_t      mca_io_File_read_at_all_end;
    mca_io_base_File_write_at_all_begin_t   mca_io_File_write_at_all_begin;
    mca_io_base_File_write_at_all_end_t     mca_io_File_write_at_all_end;
    mca_io_base_File_read_all_begin_t       mca_io_File_read_all_begin;
    mca_io_base_File_read_all_end_t         mca_io_File_read_all_end;
    mca_io_base_File_write_all_begin_t      mca_io_File_write_all_begin;
    mca_io_base_File_write_all_end_t        mca_io_File_write_all_end;
    mca_io_base_File_read_ordered_begin_t   mca_io_File_read_ordered_begin;
    mca_io_base_File_read_ordered_end_t     mca_io_File_read_ordered_end;
    mca_io_base_File_write_ordered_begin_t  mca_io_File_write_ordered_begin;
    mca_io_base_File_write_ordered_end_t    mca_io_File_write_ordered_end;

    mca_io_base_File_get_type_extent_t      mca_io_File_get_type_extent;

    mca_io_base_File_set_atomicity_t        mca_io_File_set_atomicity;
    mca_io_base_File_get_atomicity_t        mca_io_File_get_atomicity;
    mca_io_base_File_sync_t                 mca_io_File_sync;

    mca_io_base_File_set_errhandler_t       mca_io_File_set_errhandler;
    mca_io_base_File_get_errhandler_t       mca_io_File_get_errhandler;

    mca_io_base_Test_t                      mca_io_Test;
    mca_io_base_Wait_t                      mca_io_Wait;

};
typedef struct mca_io_1_0_0_t mca_io_1_0_0_t;







#endif /* MCA_IO_H */



