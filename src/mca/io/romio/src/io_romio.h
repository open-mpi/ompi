/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef MCA_IO_ROMIO_H
#define MCA_IO_ROMIO_H

#include "request/request.h"
#include "file/file.h"
#include "threads/mutex.h"
#include "romio-dist/adio/include/romioconf.h"
#include "romio-dist/include/mpio.h"
#include "mca/io/io.h"
#include "mca/io/base/io_base_request.h"


#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
/*
 * global variables, instantiated in module.c  
 */
extern ompi_mutex_t mca_io_romio_mutex;
extern mca_io_base_module_1_0_0_t mca_io_romio_module;


/* 
 * The romio component will "inherit" from the mca_io_base_request_t,
 * adding its own stuff on to the end.
 */
struct mca_io_romio_request_t {
    mca_io_base_request_t super;
    ROMIO_PREFIX (MPIO_Request) romio_rq;
};
typedef struct mca_io_romio_request_t mca_io_romio_request_t;


/* 
 * Private data for ROMIO modules
 */
struct mca_io_romio_data_t {
    ROMIO_PREFIX (MPI_File) romio_fh;
};
typedef struct mca_io_romio_data_t mca_io_romio_data_t;


/*
 * Module functions
 *
 *  mca->ROMIO module routines:    
 *    ROMIO_PREFIX(file_XXX)
 *  ROMIO operations names:
 *    ROMIO_PREFIX(MPI_File_XXX)
 */
/* Section 9.2 */
int         mca_io_romio_file_open (struct ompi_communicator_t *comm,
                                    char *filename,
                                    int amode,
                                    struct ompi_info_t *info,
                                    ompi_file_t *fh);
int         mca_io_romio_file_close (struct ompi_file_t *fh);
int         mca_io_romio_file_delete (char *filename,
                                      struct ompi_info_t *info);
int         mca_io_romio_file_set_size (struct ompi_file_t *fh,
                                        MPI_Offset size);
int         mca_io_romio_file_preallocate (struct ompi_file_t *fh,
                                           MPI_Offset size);
int         mca_io_romio_file_get_size (struct ompi_file_t *fh,
                                        MPI_Offset * size);
int         mca_io_romio_file_get_amode (struct ompi_file_t *fh,
                                         int *amode);
int         mca_io_romio_file_set_info (struct ompi_file_t *fh,
                                        struct ompi_info_t *info);
int         mca_io_romio_file_get_info (struct ompi_file_t *fh,
                                        struct ompi_info_t ** info_used);

/* Section 9.3 */
int         mca_io_romio_file_set_view (struct ompi_file_t *fh,
                                        MPI_Offset disp,
                                        struct ompi_datatype_t *etype,
                                        struct ompi_datatype_t *filetype,
                                        char *datarep,
                                        struct ompi_info_t *info);
int         mca_io_romio_file_get_view (struct ompi_file_t *fh,
                                        MPI_Offset * disp,
                                        struct ompi_datatype_t ** etype,
                                        struct ompi_datatype_t ** filetype,
                                        char *datarep);

/* Section 9.4.2 */
int         mca_io_romio_file_read_at (struct ompi_file_t *fh,
                                       MPI_Offset offset,
                                       void *buf,
                                       int count,
                                       struct ompi_datatype_t *datatype,
                                       ompi_status_public_t * status);
int         mca_io_romio_file_read_at_all (struct ompi_file_t *fh,
                                           MPI_Offset offset,
                                           void *buf,
                                           int count,
                                           struct ompi_datatype_t *datatype,
                                           ompi_status_public_t * status);
int         mca_io_romio_file_write_at (struct ompi_file_t *fh,
                                        MPI_Offset offset,
                                        void *buf,
                                        int count,
                                        struct ompi_datatype_t *datatype,
                                        ompi_status_public_t * status);
int         mca_io_romio_file_write_at_all (struct ompi_file_t *fh,
                                            MPI_Offset offset,
                                            void *buf,
                                            int count,
                                            struct ompi_datatype_t *datatype,
                                            ompi_status_public_t * status);
int         mca_io_romio_file_iread_at (struct ompi_file_t *fh,
                                        MPI_Offset offset,
                                        void *buf,
                                        int count,
                                        struct ompi_datatype_t *datatype,
                                        mca_io_base_request_t * request);
int         mca_io_romio_file_iwrite_at (struct ompi_file_t *fh,
                                         MPI_Offset offset,
                                         void *buf,
                                         int count,
                                         struct ompi_datatype_t *datatype,
                                         mca_io_base_request_t * request);

/* Section 9.4.3 */
int         mca_io_romio_file_read (struct ompi_file_t *fh,
                                    void *buf,
                                    int count,
                                    struct ompi_datatype_t *datatype,
                                    ompi_status_public_t * status);
int         mca_io_romio_file_read_all (struct ompi_file_t *fh,
                                        void *buf,
                                        int count,
                                        struct ompi_datatype_t *datatype,
                                        ompi_status_public_t * status);
int         mca_io_romio_file_write (struct ompi_file_t *fh,
                                     void *buf,
                                     int count,
                                     struct ompi_datatype_t *datatype,
                                     ompi_status_public_t * status);
int         mca_io_romio_file_write_all (struct ompi_file_t *fh,
                                         void *buf,
                                         int count,
                                         struct ompi_datatype_t *datatype,
                                         ompi_status_public_t * status);
int         mca_io_romio_file_iread (struct ompi_file_t *fh,
                                     void *buf,
                                     int count,
                                     struct ompi_datatype_t *datatype,
                                     mca_io_base_request_t * request);
int         mca_io_romio_file_iwrite (struct ompi_file_t *fh,
                                      void *buf,
                                      int count,
                                      struct ompi_datatype_t *datatype,
                                      mca_io_base_request_t * request);
int         mca_io_romio_file_seek (struct ompi_file_t *fh,
                                    MPI_Offset offset,
                                    int whence);
int         mca_io_romio_file_get_position (struct ompi_file_t *fh,
                                            MPI_Offset * offset);
int         mca_io_romio_file_get_byte_offset (struct ompi_file_t *fh,
                                               MPI_Offset offset,
                                               MPI_Offset * disp);

/* Section 9.4.4 */
int         mca_io_romio_file_read_shared (struct ompi_file_t *fh,
                                           void *buf,
                                           int count,
                                           struct ompi_datatype_t *datatype,
                                           ompi_status_public_t * status);
int         mca_io_romio_file_write_shared (struct ompi_file_t *fh,
                                            void *buf,
                                            int count,
                                            struct ompi_datatype_t *datatype,
                                            ompi_status_public_t * status);
int         mca_io_romio_file_iread_shared (struct ompi_file_t *fh,
                                            void *buf,
                                            int count,
                                            struct ompi_datatype_t *datatype,
                                            mca_io_base_request_t * request);
int         mca_io_romio_file_iwrite_shared (struct ompi_file_t *fh,
                                             void *buf,
                                             int count,
                                             struct ompi_datatype_t *datatype,
                                             mca_io_base_request_t * request);
int         mca_io_romio_file_read_ordered (struct ompi_file_t *fh,
                                            void *buf,
                                            int count,
                                            struct ompi_datatype_t *datatype,
                                            ompi_status_public_t * status);
int         mca_io_romio_file_write_ordered (struct ompi_file_t *fh,
                                             void *buf,
                                             int count,
                                             struct ompi_datatype_t *datatype,
                                             ompi_status_public_t * status);
int         mca_io_romio_file_seek_shared (struct ompi_file_t *fh,
                                           MPI_Offset offset,
                                           int whence);
int         mca_io_romio_file_get_position_shared (struct ompi_file_t *fh,
                                                   MPI_Offset * offset);

/* Section 9.4.5 */
int         mca_io_romio_file_read_at_all_begin (struct ompi_file_t *fh,
                                                 MPI_Offset offset,
                                                 void *buf,
                                                 int count,
                                                 struct ompi_datatype_t *datatype);
int         mca_io_romio_file_read_at_all_end (struct ompi_file_t *fh,
                                               void *buf,
                                               ompi_status_public_t * status);
int         mca_io_romio_file_write_at_all_begin (struct ompi_file_t *fh,
                                                  MPI_Offset offset,
                                                  void *buf,
                                                  int count,
                                                  struct ompi_datatype_t *datatype);
int         mca_io_romio_file_write_at_all_end (struct ompi_file_t *fh,
                                                void *buf,
                                                ompi_status_public_t * status);
int         mca_io_romio_file_read_all_begin (struct ompi_file_t *fh,
                                              void *buf,
                                              int count,
                                              struct ompi_datatype_t *datatype);
int         mca_io_romio_file_read_all_end (struct ompi_file_t *fh,
                                            void *buf,
                                            ompi_status_public_t * status);
int         mca_io_romio_file_write_all_begin (struct ompi_file_t *fh,
                                               void *buf,
                                               int count,
                                               struct ompi_datatype_t *datatype);
int         mca_io_romio_file_write_all_end (struct ompi_file_t *fh,
                                             void *buf,
                                             ompi_status_public_t * status);
int         mca_io_romio_file_read_ordered_begin (struct ompi_file_t *fh,
                                                  void *buf,
                                                  int count,
                                                  struct ompi_datatype_t *datatype);
int         mca_io_romio_file_read_ordered_end (struct ompi_file_t *fh,
                                                void *buf,
                                                ompi_status_public_t * status);
int         mca_io_romio_file_write_ordered_begin (struct ompi_file_t *fh,
                                                   void *buf,
                                                   int count,
                                                   struct ompi_datatype_t *datatype);
int         mca_io_romio_file_write_ordered_end (struct ompi_file_t *fh,
                                                 void *buf,
                                                 struct ompi_status_public_t * status);

/* Section 9.5.1 */
int         mca_io_romio_file_get_type_extent (struct ompi_file_t *fh,
                                               struct ompi_datatype_t *datatype,
                                               MPI_Aint * extent);

/* Section 9.6.1 */
int         mca_io_romio_file_set_atomicity (struct ompi_file_t *fh,
                                             int flag);
int         mca_io_romio_file_get_atomicity (struct ompi_file_t *fh,
                                             int *flag);
int         mca_io_romio_file_sync (struct ompi_file_t *fh);

/* End Prototypes */

/* The funtions will not be called by users, but by OMPI's 
 * MPI_Test/Wait functions when they are called with an I/O request. 
 */
int         mca_io_romio_test (mca_io_base_request_t * request,
                               int *flag,
                               ompi_status_public_t * status);
int         mca_io_romio_wait (mca_io_base_request_t * request,
                               ompi_status_public_t * status);
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif                          /* MCA_IO_ROMIO_H */
