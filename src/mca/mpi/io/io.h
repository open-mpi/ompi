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


//
// add typedefs for all MPI-IO functions to io.h
// add prototypes for all MPI-IO functions to io_romio.h
// add all functions to struct mca_io_1_0_0_t
// set all functions in struct in io_romio_module.c
//


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


struct mca_io_1_0_0_t {
    mca_io_base_File_open_t        io_File_open;
    mca_io_base_File_close_t       io_File_close;
    mca_io_base_File_delete_t      io_File_delete;
    mca_io_base_File_set_size_t    io_File_set_size;
    mca_io_base_File_preallocate_t io_File_preallocate;
    mca_io_base_File_get_size_t    io_File_get_size;
    mca_io_base_File_get_group_t   io_File_get_group;
    mca_io_base_File_get_amode_t   io_File_get_amode; 
    mca_io_base_File_set_info_t    io_File_set_info;
    mca_io_base_File_get_info_t    io_File_get_info;
};
typedef struct mca_io_1_0_0_t mca_io_1_0_0_t;







#endif /* MCA_IO_H */




