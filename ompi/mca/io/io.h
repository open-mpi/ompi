/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef MCA_IO_H
#define MCA_IO_H

#include "mpi.h"
#include "opal/mca/mca.h"
#include "ompi/request/request.h"

/*
 * Forward declaration for private data on io components and modules.
 */
struct ompi_file_t;
struct mca_io_base_file_t;
struct mca_io_base_delete_t;


/*
 * Forward declaration so that we don't create an include file loop.
 */
struct mca_io_base_request_t;


/*
 * Forward declarations of things declared in this file
 */
struct mca_io_base_module_1_0_0_t;
union mca_io_base_modules_t;


/**
 * Version of IO component interface that we're using.  
 *
 * The IO component is being designed to ensure that it can
 * simultaneously support multiple component versions in a single
 * executable.  This is because ROMIO will always be v1.x that
 * supports pretty much a 1-to-1 MPI-API-to-module-function mapping,
 * but we plan to have a v2.x series that will be "something
 * different" (as yet undefined).
 */
enum mca_io_base_version_t {
    MCA_IO_BASE_V_NONE,
    MCA_IO_BASE_V_1_0_0,

    MCA_IO_BASE_V_MAX
};
/**
 * Convenience typedef
 */
typedef enum mca_io_base_version_t mca_io_base_version_t;


/*
 * Macro for use in components that are of type io v1.0.0
 * 1-1 mapping of all MPI-IO functions
 */
#define MCA_IO_BASE_VERSION_1_0_0 \
  MCA_BASE_VERSION_1_0_0, \
  "io", 1, 0, 0

/*
 * Component v1.0.0
 */

struct mca_io_base_module_1_0_0_t;
typedef int (*mca_io_base_component_init_query_fn_t)
    (bool enable_progress_threads, bool enable_mpi_threads);
typedef const struct mca_io_base_module_1_0_0_t *
    (*mca_io_base_component_file_query_1_0_0_fn_t)
    (struct ompi_file_t *file, struct mca_io_base_file_t **private_data, 
     int *priority);
typedef int (*mca_io_base_component_file_unquery_fn_t)
    (struct ompi_file_t *file, struct mca_io_base_file_t *private_data);

typedef int (*mca_io_base_component_file_delete_query_fn_t)
    (char *filename, struct ompi_info_t *info, 
     struct mca_io_base_delete_t **private_data,
     bool *usable, int *priority);
typedef int (*mca_io_base_component_file_delete_select_fn_t)
    (char *filename, struct ompi_info_t *info, 
     struct mca_io_base_delete_t *private_data);
typedef int (*mca_io_base_component_file_delete_unselect_fn_t)
    (char *filename, struct ompi_info_t *info,
     struct mca_io_base_delete_t *private_data);

typedef int (*mca_io_base_component_progress_fn_t)(void);

typedef int (*mca_io_base_component_register_datarep_fn_t)(
                                              char *,
                                              MPI_Datarep_conversion_function*,
                                              MPI_Datarep_conversion_function*,
                                              MPI_Datarep_extent_function*,
                                              void*);


/* IO component version and interface functions. */
struct mca_io_base_component_1_0_0_t {
    mca_base_component_t io_version;
    mca_base_component_data_1_0_0_t io_data;

    /** Additional bytes that this module needs to be allocated when
        an MPI_Request (ompi_request_t) is allocated. */

    size_t io_request_bytes;

    mca_io_base_component_init_query_fn_t io_init_query;
    mca_io_base_component_file_query_1_0_0_fn_t io_file_query;
    mca_io_base_component_file_unquery_fn_t io_file_unquery;

    mca_io_base_component_file_delete_query_fn_t io_delete_query;
    mca_io_base_component_file_delete_unselect_fn_t io_delete_unquery;
    mca_io_base_component_file_delete_select_fn_t io_delete_select;

    mca_io_base_component_progress_fn_t io_progress;

    mca_io_base_component_register_datarep_fn_t io_register_datarep;
};
typedef struct mca_io_base_component_1_0_0_t mca_io_base_component_1_0_0_t;


/*
 * All component versions
 */
union mca_io_base_components_t {
    mca_io_base_component_1_0_0_t v1_0_0;
};
typedef union mca_io_base_components_t mca_io_base_components_t;


/*
 * Module v1.0.0
 */

/**
 * Initialize a request for use by the io module.
 *
 * @param module (IN)    IO module
 * @param request (IN)   Pointer to allocated request.
 *
 * To reduce latency (number of required allocations), the io base
 * allocates additional space along with each request that may be used
 * by the io module for additional control information. If the io
 * module intends to use this space, the io_request_bytes attribute
 * should be set to reflect the number of bytes above
 * sizeof(mca_io_base_request_t) are needed by the io component. This
 * space is allocated contiguously along with the
 * mca_io_base_request_t with the space immediately following the base
 * request available to the io module.
 * 
 * This init function is called the first time the request is created
 * by the io base. On completion of the request, the io base will
 * cache the request for later use by the same io module.  When the
 * request is re-used from the cache, this init function is NOT called
 * again.
 */
typedef int (*mca_io_base_module_request_once_init_fn_t)(
    union mca_io_base_modules_t* module_union,
    struct mca_io_base_request_t* request);


/**
 * Cleanup any resources that may have been associated with the
 * request by the io module.
 *
 * @param module (IN)    io module
 * @param request (IN)   Pointer to allocated request.
 *
 * This function is called when the io base removes a request from the
 * io module's cache (due to resource constraints) or the cache limit
 * has been reached, prior to re-using the request for another io
 * module. This provides the io module the chance to cleanup/release
 * any resources cached on the request by the io module.
 */
typedef int (*mca_io_base_module_request_once_finalize_fn_t)(
    union mca_io_base_modules_t* module_union,
    struct mca_io_base_request_t* request);

typedef int (*mca_io_base_module_request_fini_fn_t)(
    struct ompi_file_t *file,
    union mca_io_base_modules_t* module_union,
    struct mca_io_base_request_t* request);

typedef int (*mca_io_base_module_file_open_fn_t)
    (struct ompi_communicator_t *comm, char *filename, int amode,
     struct ompi_info_t *info, struct ompi_file_t *fh);
typedef int (*mca_io_base_module_file_close_fn_t)(struct ompi_file_t *fh);

typedef int (*mca_io_base_module_file_set_size_fn_t)
    (struct ompi_file_t *fh, MPI_Offset size);
typedef int (*mca_io_base_module_file_preallocate_fn_t)
    (struct ompi_file_t *fh, MPI_Offset size);
typedef int (*mca_io_base_module_file_get_size_fn_t)
    (struct ompi_file_t *fh, MPI_Offset *size);
typedef int (*mca_io_base_module_file_get_amode_fn_t)
    (struct ompi_file_t *fh, int *amode);
typedef int (*mca_io_base_module_file_set_info_fn_t)
    (struct ompi_file_t *fh, struct ompi_info_t *info);
typedef int (*mca_io_base_module_file_get_info_fn_t)
    (struct ompi_file_t *fh, struct ompi_info_t **info_used);

typedef int (*mca_io_base_module_file_set_view_fn_t)
    (struct ompi_file_t *fh, MPI_Offset disp, struct ompi_datatype_t *etype,
     struct ompi_datatype_t *filetype, char *datarep, 
     struct ompi_info_t *info);
typedef int (*mca_io_base_module_file_get_view_fn_t)
    (struct ompi_file_t *fh, MPI_Offset *disp, 
     struct ompi_datatype_t **etype, struct ompi_datatype_t **filetype,
     char *datarep);

typedef int (*mca_io_base_module_file_read_at_fn_t)
    (struct ompi_file_t *fh, MPI_Offset offset, void *buf,
     int count, struct ompi_datatype_t *datatype, 
     struct ompi_status_public_t *status);
typedef int (*mca_io_base_module_file_read_at_all_fn_t)
    (struct ompi_file_t *fh, MPI_Offset offset, void *buf,
     int count, struct ompi_datatype_t *datatype, 
     struct ompi_status_public_t *status);
typedef int (*mca_io_base_module_file_write_at_fn_t)
    (struct ompi_file_t *fh, MPI_Offset offset, void *buf,
     int count, struct ompi_datatype_t *datatype, 
     struct ompi_status_public_t *status);
typedef int (*mca_io_base_module_file_write_at_all_fn_t)
    (struct ompi_file_t *fh, MPI_Offset offset, void *buf,
     int count, struct ompi_datatype_t *datatype, 
     struct ompi_status_public_t *status);

typedef int (*mca_io_base_module_file_iread_at_fn_t)
    (struct ompi_file_t *fh, MPI_Offset offset, void *buf,
     int count, struct ompi_datatype_t *datatype, 
     struct mca_io_base_request_t *request);
typedef int (*mca_io_base_module_file_iwrite_at_fn_t)
    (struct ompi_file_t *fh, MPI_Offset offset, void *buf,
     int count, struct ompi_datatype_t *datatype, 
     struct mca_io_base_request_t *request);

typedef int (*mca_io_base_module_file_read_fn_t)
    (struct ompi_file_t *fh, void *buf, int count, struct ompi_datatype_t *
     datatype, struct ompi_status_public_t *status); 
typedef int (*mca_io_base_module_file_read_all_fn_t)
    (struct ompi_file_t *fh, void *buf, int count, struct ompi_datatype_t *
     datatype, struct ompi_status_public_t *status); 
typedef int (*mca_io_base_module_file_write_fn_t)
    (struct ompi_file_t *fh, void *buf, int count, struct ompi_datatype_t *
     datatype, struct ompi_status_public_t *status);
typedef int (*mca_io_base_module_file_write_all_fn_t)
    (struct ompi_file_t *fh, void *buf, int count, struct ompi_datatype_t *
     datatype, struct ompi_status_public_t *status);

typedef int (*mca_io_base_module_file_iread_fn_t)
    (struct ompi_file_t *fh, void *buf, int count, struct ompi_datatype_t *
     datatype, struct mca_io_base_request_t *request); 
typedef int (*mca_io_base_module_file_iwrite_fn_t)
    (struct ompi_file_t *fh, void *buf, int count, 
     struct ompi_datatype_t *datatype, struct mca_io_base_request_t *request);

typedef int (*mca_io_base_module_file_seek_fn_t)
    (struct ompi_file_t *fh, MPI_Offset offset, int whence);
typedef int (*mca_io_base_module_file_get_position_fn_t)
    (struct ompi_file_t *fh, MPI_Offset *offset);
typedef int (*mca_io_base_module_file_get_byte_offset_fn_t)
    (struct ompi_file_t *fh, MPI_Offset offset, MPI_Offset *disp);

typedef int (*mca_io_base_module_file_read_shared_fn_t)
    (struct ompi_file_t *fh, void *buf, int count, 
     struct ompi_datatype_t *datatype, struct ompi_status_public_t *status);
typedef int (*mca_io_base_module_file_write_shared_fn_t)
    (struct ompi_file_t *fh, void *buf, int count, 
     struct ompi_datatype_t *datatype, struct ompi_status_public_t *status);
typedef int (*mca_io_base_module_file_iread_shared_fn_t)
    (struct ompi_file_t *fh, void *buf, int count, 
     struct ompi_datatype_t *datatype, struct mca_io_base_request_t *request);
typedef int (*mca_io_base_module_file_iwrite_shared_fn_t)
    (struct ompi_file_t *fh, void *buf, int count, 
     struct ompi_datatype_t *datatype, struct mca_io_base_request_t *request);
typedef int (*mca_io_base_module_file_read_ordered_fn_t)
    (struct ompi_file_t *fh, void *buf, int count, 
     struct ompi_datatype_t *datatype, struct ompi_status_public_t *status);
typedef int (*mca_io_base_module_file_write_ordered_fn_t)
    (struct ompi_file_t *fh, void *buf, int count, 
     struct ompi_datatype_t *datatype, struct ompi_status_public_t *status);
typedef int (*mca_io_base_module_file_seek_shared_fn_t)
    (struct ompi_file_t *fh, MPI_Offset offset, int whence);
typedef int (*mca_io_base_module_file_get_position_shared_fn_t)
    (struct ompi_file_t *fh, MPI_Offset *offset);

typedef int (*mca_io_base_module_file_read_at_all_begin_fn_t)
    (struct ompi_file_t *fh, MPI_Offset offset, void *buf,
     int count, struct ompi_datatype_t *datatype);
typedef int (*mca_io_base_module_file_read_at_all_end_fn_t)
    (struct ompi_file_t *fh, void *buf, struct ompi_status_public_t *status);
typedef int (*mca_io_base_module_file_write_at_all_begin_fn_t)
    (struct ompi_file_t *fh, MPI_Offset offset, void *buf,
     int count, struct ompi_datatype_t *datatype);
typedef int (*mca_io_base_module_file_write_at_all_end_fn_t)
    (struct ompi_file_t *fh, void *buf, struct ompi_status_public_t *status);
typedef int (*mca_io_base_module_file_read_all_begin_fn_t)
    (struct ompi_file_t *fh, void *buf, int count, 
     struct ompi_datatype_t *datatype);
typedef int (*mca_io_base_module_file_read_all_end_fn_t)
    (struct ompi_file_t *fh, void *buf, struct ompi_status_public_t *status);
typedef int (*mca_io_base_module_file_write_all_begin_fn_t)
    (struct ompi_file_t *fh, void *buf, int count, 
     struct ompi_datatype_t *datatype);
typedef int (*mca_io_base_module_file_write_all_end_fn_t)
    (struct ompi_file_t *fh, void *buf, struct ompi_status_public_t *status);
typedef int (*mca_io_base_module_file_read_ordered_begin_fn_t)
    (struct ompi_file_t *fh, void *buf, int count, 
     struct ompi_datatype_t *datatype);
typedef int (*mca_io_base_module_file_read_ordered_end_fn_t)
    (struct ompi_file_t *fh, void *buf, struct ompi_status_public_t *status);
typedef int (*mca_io_base_module_file_write_ordered_begin_fn_t)
    (struct ompi_file_t *fh, void *buf, int count, 
     struct ompi_datatype_t *datatype);
typedef int (*mca_io_base_module_file_write_ordered_end_fn_t)
    (struct ompi_file_t *fh, void *buf, struct ompi_status_public_t *status);

typedef int (*mca_io_base_module_file_get_type_extent_fn_t)
    (struct ompi_file_t *fh, struct ompi_datatype_t *datatype, 
     MPI_Aint *extent);

typedef int (*mca_io_base_module_file_set_atomicity_fn_t)
    (struct ompi_file_t *fh, int flag);
typedef int (*mca_io_base_module_file_get_atomicity_fn_t)
    (struct ompi_file_t *fh, int *flag);
typedef int (*mca_io_base_module_file_sync_fn_t)(struct ompi_file_t *fh);

struct mca_io_base_module_1_0_0_t {

    /** Once-per-process request initializtion function */

    mca_io_base_module_request_once_init_fn_t io_module_request_once_init;

    /** Once-per-process request finalization function */

    mca_io_base_module_request_once_finalize_fn_t io_module_request_once_finalize;

    /** Free a request (per usage) */

    ompi_request_free_fn_t io_module_request_free;

    /** Cancel a request (per usage) */

    ompi_request_cancel_fn_t io_module_request_cancel;

    /* Back-ends to MPI API calls (pretty much a 1-to-1 mapping) */

    mca_io_base_module_file_open_fn_t        io_module_file_open;
    mca_io_base_module_file_close_fn_t       io_module_file_close;
  
    mca_io_base_module_file_set_size_fn_t    io_module_file_set_size;
    mca_io_base_module_file_preallocate_fn_t io_module_file_preallocate;
    mca_io_base_module_file_get_size_fn_t    io_module_file_get_size;
    mca_io_base_module_file_get_amode_fn_t   io_module_file_get_amode; 
    mca_io_base_module_file_set_info_fn_t    io_module_file_set_info;
    mca_io_base_module_file_get_info_fn_t    io_module_file_get_info;
    
    mca_io_base_module_file_set_view_fn_t    io_module_file_set_view;
    mca_io_base_module_file_get_view_fn_t    io_module_file_get_view;

    mca_io_base_module_file_read_at_fn_t     io_module_file_read_at;
    mca_io_base_module_file_read_at_all_fn_t io_module_file_read_at_all;
    mca_io_base_module_file_write_at_fn_t    io_module_file_write_at;
    mca_io_base_module_file_write_at_all_fn_t  io_module_file_write_at_all;
    
    mca_io_base_module_file_iread_at_fn_t    io_module_file_iread_at;
    mca_io_base_module_file_iwrite_at_fn_t   io_module_file_iwrite_at;
    
    mca_io_base_module_file_read_fn_t        io_module_file_read;
    mca_io_base_module_file_read_all_fn_t    io_module_file_read_all;
    mca_io_base_module_file_write_fn_t       io_module_file_write;
    mca_io_base_module_file_write_all_fn_t   io_module_file_write_all;
    
    mca_io_base_module_file_iread_fn_t       io_module_file_iread;
    mca_io_base_module_file_iwrite_fn_t      io_module_file_iwrite;
    
    mca_io_base_module_file_seek_fn_t        io_module_file_seek;
    mca_io_base_module_file_get_position_fn_t io_module_file_get_position;
    mca_io_base_module_file_get_byte_offset_fn_t io_module_file_get_byte_offset;
    
    mca_io_base_module_file_read_shared_fn_t   io_module_file_read_shared;
    mca_io_base_module_file_write_shared_fn_t  io_module_file_write_shared;
    mca_io_base_module_file_iread_shared_fn_t  io_module_file_iread_shared;
    mca_io_base_module_file_iwrite_shared_fn_t io_module_file_iwrite_shared;
    mca_io_base_module_file_read_ordered_fn_t  io_module_file_read_ordered;
    mca_io_base_module_file_write_ordered_fn_t io_module_file_write_ordered;
    mca_io_base_module_file_seek_shared_fn_t   io_module_file_seek_shared;
    mca_io_base_module_file_get_position_shared_fn_t  io_module_file_get_position_shared;
    
    mca_io_base_module_file_read_at_all_begin_fn_t    io_module_file_read_at_all_begin;
    mca_io_base_module_file_read_at_all_end_fn_t      io_module_file_read_at_all_end;
    mca_io_base_module_file_write_at_all_begin_fn_t   io_module_file_write_at_all_begin;
    mca_io_base_module_file_write_at_all_end_fn_t     io_module_file_write_at_all_end;
    mca_io_base_module_file_read_all_begin_fn_t       io_module_file_read_all_begin;
    mca_io_base_module_file_read_all_end_fn_t         io_module_file_read_all_end;
    mca_io_base_module_file_write_all_begin_fn_t      io_module_file_write_all_begin;
    mca_io_base_module_file_write_all_end_fn_t        io_module_file_write_all_end;
    mca_io_base_module_file_read_ordered_begin_fn_t   io_module_file_read_ordered_begin;
    mca_io_base_module_file_read_ordered_end_fn_t     io_module_file_read_ordered_end;
    mca_io_base_module_file_write_ordered_begin_fn_t  io_module_file_write_ordered_begin;
    mca_io_base_module_file_write_ordered_end_fn_t    io_module_file_write_ordered_end;
    
    mca_io_base_module_file_get_type_extent_fn_t      io_module_file_get_type_extent;
    
    mca_io_base_module_file_set_atomicity_fn_t        io_module_file_set_atomicity;
    mca_io_base_module_file_get_atomicity_fn_t        io_module_file_get_atomicity;
    mca_io_base_module_file_sync_fn_t                 io_module_file_sync;
};
typedef struct mca_io_base_module_1_0_0_t mca_io_base_module_1_0_0_t;


/*
 * All module versions
 */
union mca_io_base_modules_t {
    mca_io_base_module_1_0_0_t v1_0_0;
};
typedef union mca_io_base_modules_t mca_io_base_modules_t;

#endif /* MCA_IO_H */
