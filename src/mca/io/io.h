/*
 * $HEADER
 */

#ifndef MCA_IO_H
#define MCA_IO_H

#include "mca/mca.h"

/*
 * Macro for use in components that are of type io v1.0.0
 * 1-1 mapping of all MPI-IO functions
 */
#define MCA_IO_BASE_VERSION_1_0_0 \
  MCA_BASE_VERSION_1_0_0, \
  "io", 1, 0, 0

/*
 * Macro for use in components that are of type io v2.0.0
 * as of yet undefined, but not 1-1 mapping of all MPI-IO functions
 */
#define MCA_IO_BASE_VERSION_2_0_0 \
  MCA_BASE_VERSION_1_0_0, \
  "io", 2, 0, 0

struct mca_io_base_module_1_0_0_t;
typedef struct mca_io_base_module_1_0_0_t * (*mca_io_base_component_init_fn_t)
  (int *priority, int* min_thread, int* max_thread);

/* IO component version and interface functions. */
struct mca_io_base_component_1_0_0_t {
   mca_base_component_t io_version;
   mca_base_component_data_1_0_0_t io_data;
   mca_io_base_component_init_fn_t io_init;
};
typedef struct mca_io_base_component_1_0_0_t mca_io_base_component_1_0_0_t;


/*
 * to add functions:  
 * add prototypes for all MPI-IO functions to io_romio.h
 * add typedefs for all MPI-IO functions to io.h
 * add all functions to struct mca_io_1_0_0_t in io.h
 * set all functions in struct in io_romio_component.c
 */


typedef int (*mca_io_base_module_File_open_t)
  (struct ompi_communicator_t *comm, char *filename, int amode,
   struct ompi_info_t *info, struct ompi_file_t **fh);
typedef int (*mca_io_base_module_File_close_t)(struct ompi_file_t **fh);
typedef int (*mca_io_base_module_File_delete_t)
  (char *filename, struct ompi_info_t *info);
typedef int (*mca_io_base_module_File_set_size_t)
  (struct ompi_file_t *fh, MPI_Offset size);
typedef int (*mca_io_base_module_File_preallocate_t)
  (struct ompi_file_t *fh, MPI_Offset size);
typedef int (*mca_io_base_module_File_get_size_t)
  (struct ompi_file_t *fh, MPI_Offset *size);
typedef int (*mca_io_base_module_File_get_group_t)
  (struct ompi_file_t *fh, struct ompi_group_t **group);
typedef int (*mca_io_base_module_File_get_amode_t)
  (struct ompi_file_t *fh, int *amode);
typedef int (*mca_io_base_module_File_set_info_t)
  (struct ompi_file_t *fh, struct ompi_info_t *info);
typedef int (*mca_io_base_module_File_get_info_t)
  (struct ompi_file_t *fh, struct ompi_info_t **info_used);

typedef int (*mca_io_base_module_File_set_view_t)
  (struct ompi_file_t *fh, MPI_Offset disp, struct ompi_datatype_t *etype,
   struct ompi_datatype_t *filetype, char *datarep, struct ompi_info_t *info);
typedef int (*mca_io_base_module_File_get_view_t)
  (struct ompi_file_t *fh, MPI_Offset *disp, 
   struct ompi_datatype_t **etype, struct ompi_datatype_t **filetype,
   char *datarep);

typedef int (*mca_io_base_module_File_read_at_t)
  (struct ompi_file_t *fh, MPI_Offset offset, void *buf,
   int count, struct ompi_datatype_t *datatype, 
   struct ompi_status_public_t *status);
typedef int (*mca_io_base_module_File_read_at_all_t)
  (struct ompi_file_t *fh, MPI_Offset offset, void *buf,
   int count, struct ompi_datatype_t *datatype, 
   struct ompi_status_public_t *status);
typedef int (*mca_io_base_module_File_write_at_t)
  (struct ompi_file_t *fh, MPI_Offset offset, void *buf,
   int count, struct ompi_datatype_t *datatype, 
   struct ompi_status_public_t *status);
typedef int (*mca_io_base_module_File_write_at_all_t)
  (struct ompi_file_t *fh, MPI_Offset offset, void *buf,
   int count, struct ompi_datatype_t *datatype, 
   struct ompi_status_public_t *status);

typedef int (*mca_io_base_module_File_iread_at_t)
  (struct ompi_file_t *fh, MPI_Offset offset, void *buf,
   int count, struct ompi_datatype_t *datatype, 
   struct ompi_request_t **request);
typedef int (*mca_io_base_module_File_iwrite_at_t)
  (struct ompi_file_t *fh, MPI_Offset offset, void *buf,
   int count, struct ompi_datatype_t *datatype, 
   struct ompi_request_t **request);


typedef int (*mca_io_base_module_File_read_t)
  (struct ompi_file_t *fh, void *buf, int count, MPI_Datatype
   datatype, struct ompi_status_public_t *status); 
typedef int (*mca_io_base_module_File_read_all_t)
  (struct ompi_file_t *fh, void *buf, int count, MPI_Datatype
   datatype, struct ompi_status_public_t *status); 
typedef int (*mca_io_base_module_File_write_t)
  (struct ompi_file_t *fh, void *buf, int count, MPI_Datatype
   datatype, struct ompi_status_public_t *status);
typedef int (*mca_io_base_module_File_write_all_t)
  (struct ompi_file_t *fh, void *buf, int count, MPI_Datatype
   datatype, struct ompi_status_public_t *status);

typedef int (*mca_io_base_module_File_iread_t)
  (struct ompi_file_t *fh, void *buf, int count, MPI_Datatype
   datatype, struct ompi_request_t **request); 
typedef int (*mca_io_base_module_File_iwrite_t)
  (struct ompi_file_t *fh, void *buf, int count, 
   struct ompi_datatype_t *datatype, struct ompi_request_t **request);

typedef int (*mca_io_base_module_File_seek_t)
  (struct ompi_file_t *fh, MPI_Offset offset, int whence);
typedef int (*mca_io_base_module_File_get_position_t)
  (struct ompi_file_t *fh, MPI_Offset *offset);
typedef int (*mca_io_base_module_File_get_byte_offset_t)
  (struct ompi_file_t *fh, MPI_Offset offset, MPI_Offset *disp);

typedef int (*mca_io_base_module_File_read_shared_t)
  (struct ompi_file_t *fh, void *buf, int count, 
   struct ompi_datatype_t *datatype, struct ompi_status_public_t *status);
typedef int (*mca_io_base_module_File_write_shared_t)
  (struct ompi_file_t *fh, void *buf, int count, 
   struct ompi_datatype_t *datatype, struct ompi_status_public_t *status);
typedef int (*mca_io_base_module_File_iread_shared_t)
  (struct ompi_file_t *fh, void *buf, int count, 
   struct ompi_datatype_t *datatype, struct ompi_request_t **request);
typedef int (*mca_io_base_module_File_iwrite_shared_t)
  (struct ompi_file_t *fh, void *buf, int count, 
   struct ompi_datatype_t *datatype, struct ompi_request_t **request);
typedef int (*mca_io_base_module_File_read_ordered_t)
  (struct ompi_file_t *fh, void *buf, int count, 
   struct ompi_datatype_t *datatype, struct ompi_status_public_t *status);
typedef int (*mca_io_base_module_File_write_ordered_t)
  (struct ompi_file_t *fh, void *buf, int count, 
   struct ompi_datatype_t *datatype, struct ompi_status_public_t *status);
typedef int (*mca_io_base_module_File_seek_shared_t)
  (struct ompi_file_t *fh, MPI_Offset offset, int whence);
typedef int (*mca_io_base_module_File_get_position_shared_t)
  (struct ompi_file_t *fh, MPI_Offset *offset);

typedef int (*mca_io_base_module_File_read_at_all_begin_t)
  (struct ompi_file_t *fh, MPI_Offset offset, void *buf,
   int count, struct ompi_datatype_t *datatype);
typedef int (*mca_io_base_module_File_read_at_all_end_t)
  (struct ompi_file_t *fh, void *buf, struct ompi_status_public_t *status);
typedef int (*mca_io_base_module_File_write_at_all_begin_t)
  (struct ompi_file_t *fh, MPI_Offset offset, void *buf,
   int count, struct ompi_datatype_t *datatype);
typedef int (*mca_io_base_module_File_write_at_all_end_t)
  (struct ompi_file_t *fh, void *buf, struct ompi_status_public_t *status);
typedef int (*mca_io_base_module_File_read_all_begin_t)
  (struct ompi_file_t *fh, void *buf, int count, 
   struct ompi_datatype_t *datatype);
typedef int (*mca_io_base_module_File_read_all_end_t)
  (struct ompi_file_t *fh, void *buf, struct ompi_status_public_t *status);
typedef int (*mca_io_base_module_File_write_all_begin_t)
  (struct ompi_file_t *fh, void *buf, int count, 
   struct ompi_datatype_t *datatype);
typedef int (*mca_io_base_module_File_write_all_end_t)
  (struct ompi_file_t *fh, void *buf, struct ompi_status_public_t *status);
typedef int (*mca_io_base_module_File_read_ordered_begin_t)
  (struct ompi_file_t *fh, void *buf, int count, 
   struct ompi_datatype_t *datatype);
typedef int (*mca_io_base_module_File_read_ordered_end_t)
  (struct ompi_file_t *fh, void *buf, struct ompi_status_public_t *status);
typedef int (*mca_io_base_module_File_write_ordered_begin_t)
  (struct ompi_file_t *fh, void *buf, int count, 
   struct ompi_datatype_t *datatype);
typedef int (*mca_io_base_module_File_write_ordered_end_t)
  (struct ompi_file_t *fh, void *buf, struct ompi_status_public_t *status);

typedef int (*mca_io_base_module_File_get_type_extent_t)
  (struct ompi_file_t *fh, struct ompi_datatype_t *datatype, 
   MPI_Aint *extent);

typedef int (*mca_io_base_module_File_set_atomicity_t)
  (struct ompi_file_t *fh, int flag);
typedef int (*mca_io_base_module_File_get_atomicity_t)
  (struct ompi_file_t *fh, int *flag);
typedef int (*mca_io_base_module_File_sync_t)(struct ompi_file_t *fh);

typedef int (*mca_io_base_module_File_set_errhandler_t)
  (struct ompi_file_t *fh, MPI_Errhandler e);
typedef int (*mca_io_base_module_File_get_errhandler_t)
  (struct ompi_file_t *fh, MPI_Errhandler *e );

typedef int (*mca_io_base_module_Test_t)
  (struct ompi_request_t **request, int *flag, 
   struct ompi_status_public_t *status);
typedef int (*mca_io_base_module_Wait_t)
  (struct ompi_request_t **request, struct ompi_status_public_t *status);

struct mca_io_base_module_1_0_0_t {
    mca_io_base_module_File_open_t        mca_io_File_open;
    mca_io_base_module_File_close_t       mca_io_File_close;
    mca_io_base_module_File_delete_t      mca_io_File_delete;
    mca_io_base_module_File_set_size_t    mca_io_File_set_size;
    mca_io_base_module_File_preallocate_t mca_io_File_preallocate;
    mca_io_base_module_File_get_size_t    mca_io_File_get_size;
    mca_io_base_module_File_get_group_t   mca_io_File_get_group;
    mca_io_base_module_File_get_amode_t   mca_io_File_get_amode; 
    mca_io_base_module_File_set_info_t    mca_io_File_set_info;
    mca_io_base_module_File_get_info_t    mca_io_File_get_info;

    mca_io_base_module_File_set_view_t    mca_io_File_set_view;
    mca_io_base_module_File_get_view_t    mca_io_File_get_view;

    mca_io_base_module_File_read_at_t     mca_io_File_read_at;
    mca_io_base_module_File_read_at_all_t mca_io_File_read_at_all;
    mca_io_base_module_File_write_at_t    mca_io_File_write_at;
    mca_io_base_module_File_write_at_all_t  mca_io_File_write_at_all;

    mca_io_base_module_File_iread_at_t    mca_io_File_iread_at;
    mca_io_base_module_File_iwrite_at_t   mca_io_File_iwrite_at;

    mca_io_base_module_File_read_t        mca_io_File_read;
    mca_io_base_module_File_read_all_t    mca_io_File_read_all;
    mca_io_base_module_File_write_t       mca_io_File_write;
    mca_io_base_module_File_write_all_t   mca_io_File_write_all;

    mca_io_base_module_File_iread_t       mca_io_File_iread;
    mca_io_base_module_File_iwrite_t      mca_io_File_iwrite;

    mca_io_base_module_File_seek_t        mca_io_File_seek;
    mca_io_base_module_File_get_position_t mca_io_File_get_position;
    mca_io_base_module_File_get_byte_offset_t mca_io_File_get_byte_offset;

    mca_io_base_module_File_read_shared_t   mca_io_File_read_shared;
    mca_io_base_module_File_write_shared_t  mca_io_File_write_shared;
    mca_io_base_module_File_iread_shared_t  mca_io_File_iread_shared;
    mca_io_base_module_File_iwrite_shared_t mca_io_File_iwrite_shared;
    mca_io_base_module_File_read_ordered_t  mca_io_File_read_ordered;
    mca_io_base_module_File_write_ordered_t mca_io_File_write_ordered;
    mca_io_base_module_File_seek_shared_t   mca_io_File_seek_shared;
    mca_io_base_module_File_get_position_shared_t  mca_io_File_get_position_shared;

    mca_io_base_module_File_read_at_all_begin_t    mca_io_File_read_at_all_begin;
    mca_io_base_module_File_read_at_all_end_t      mca_io_File_read_at_all_end;
    mca_io_base_module_File_write_at_all_begin_t   mca_io_File_write_at_all_begin;
    mca_io_base_module_File_write_at_all_end_t     mca_io_File_write_at_all_end;
    mca_io_base_module_File_read_all_begin_t       mca_io_File_read_all_begin;
    mca_io_base_module_File_read_all_end_t         mca_io_File_read_all_end;
    mca_io_base_module_File_write_all_begin_t      mca_io_File_write_all_begin;
    mca_io_base_module_File_write_all_end_t        mca_io_File_write_all_end;
    mca_io_base_module_File_read_ordered_begin_t   mca_io_File_read_ordered_begin;
    mca_io_base_module_File_read_ordered_end_t     mca_io_File_read_ordered_end;
    mca_io_base_module_File_write_ordered_begin_t  mca_io_File_write_ordered_begin;
    mca_io_base_module_File_write_ordered_end_t    mca_io_File_write_ordered_end;

    mca_io_base_module_File_get_type_extent_t      mca_io_File_get_type_extent;

    mca_io_base_module_File_set_atomicity_t        mca_io_File_set_atomicity;
    mca_io_base_module_File_get_atomicity_t        mca_io_File_get_atomicity;
    mca_io_base_module_File_sync_t                 mca_io_File_sync;

    mca_io_base_module_File_set_errhandler_t       mca_io_File_set_errhandler;
    mca_io_base_module_File_get_errhandler_t       mca_io_File_get_errhandler;

    mca_io_base_module_Test_t                      mca_io_Test;
    mca_io_base_module_Wait_t                      mca_io_Wait;
};
typedef struct mca_io_base_module_1_0_0_t mca_io_base_module_1_0_0_t;


#endif /* MCA_IO_H */
