/*
 * $HEADER
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
int         mca_io_romio_file_open (ompi_communicator_t *comm,
                                    char *filename,
                                    int amode,
                                    ompi_info_t *info,
                                    ompi_file_t *fh);
int         mca_io_romio_file_close (ompi_file_t *fh);
int         mca_io_romio_file_delete (char *filename,
                                      ompi_info_t *info);
int         mca_io_romio_file_set_size (ompi_file_t *fh,
                                        MPI_Offset size);
int         mca_io_romio_file_preallocate (ompi_file_t *fh,
                                           MPI_Offset size);
int         mca_io_romio_file_get_size (ompi_file_t *fh,
                                        MPI_Offset * size);
int         mca_io_romio_file_get_amode (ompi_file_t *fh,
                                         int *amode);
int         mca_io_romio_file_set_info (ompi_file_t *fh,
                                        ompi_info_t *info);
int         mca_io_romio_file_get_info (ompi_file_t *fh,
                                        ompi_info_t ** info_used);

/* Section 9.3 */
int         mca_io_romio_file_set_view (ompi_file_t *fh,
                                        MPI_Offset disp,
                                        ompi_datatype_t *etype,
                                        ompi_datatype_t *filetype,
                                        char *datarep,
                                        ompi_info_t *info);
int         mca_io_romio_file_get_view (ompi_file_t *fh,
                                        MPI_Offset * disp,
                                        ompi_datatype_t ** etype,
                                        ompi_datatype_t ** filetype,
                                        char *datarep);

/* Section 9.4.2 */
int         mca_io_romio_file_read_at (ompi_file_t *fh,
                                       MPI_Offset offset,
                                       void *buf,
                                       int count,
                                       ompi_datatype_t *datatype,
                                       ompi_status_public_t * status);
int         mca_io_romio_file_read_at_all (ompi_file_t *fh,
                                           MPI_Offset offset,
                                           void *buf,
                                           int count,
                                           ompi_datatype_t *datatype,
                                           ompi_status_public_t * status);
int         mca_io_romio_file_write_at (ompi_file_t *fh,
                                        MPI_Offset offset,
                                        void *buf,
                                        int count,
                                        ompi_datatype_t *datatype,
                                        ompi_status_public_t * status);
int         mca_io_romio_file_write_at_all (ompi_file_t *fh,
                                            MPI_Offset offset,
                                            void *buf,
                                            int count,
                                            ompi_datatype_t *datatype,
                                            ompi_status_public_t * status);
int         mca_io_romio_file_iread_at (ompi_file_t *fh,
                                        MPI_Offset offset,
                                        void *buf,
                                        int count,
                                        ompi_datatype_t *datatype,
                                        mca_io_base_request_t * request);
int         mca_io_romio_file_iwrite_at (ompi_file_t *fh,
                                         MPI_Offset offset,
                                         void *buf,
                                         int count,
                                         ompi_datatype_t *datatype,
                                         mca_io_base_request_t * request);

/* Section 9.4.3 */
int         mca_io_romio_file_read (ompi_file_t *fh,
                                    void *buf,
                                    int count,
                                    ompi_datatype_t *datatype,
                                    ompi_status_public_t * status);
int         mca_io_romio_file_read_all (ompi_file_t *fh,
                                        void *buf,
                                        int count,
                                        ompi_datatype_t *datatype,
                                        ompi_status_public_t * status);
int         mca_io_romio_file_write (ompi_file_t *fh,
                                     void *buf,
                                     int count,
                                     ompi_datatype_t *datatype,
                                     ompi_status_public_t * status);
int         mca_io_romio_file_write_all (ompi_file_t *fh,
                                         void *buf,
                                         int count,
                                         ompi_datatype_t *datatype,
                                         ompi_status_public_t * status);
int         mca_io_romio_file_iread (ompi_file_t *fh,
                                     void *buf,
                                     int count,
                                     ompi_datatype_t *datatype,
                                     mca_io_base_request_t * request);
int         mca_io_romio_file_iwrite (ompi_file_t *fh,
                                      void *buf,
                                      int count,
                                      ompi_datatype_t *datatype,
                                      mca_io_base_request_t * request);
int         mca_io_romio_file_seek (ompi_file_t *fh,
                                    MPI_Offset offset,
                                    int whence);
int         mca_io_romio_file_get_position (ompi_file_t *fh,
                                            MPI_Offset * offset);
int         mca_io_romio_file_get_byte_offset (ompi_file_t *fh,
                                               MPI_Offset offset,
                                               MPI_Offset * disp);

/* Section 9.4.4 */
int         mca_io_romio_file_read_shared (ompi_file_t *fh,
                                           void *buf,
                                           int count,
                                           ompi_datatype_t *datatype,
                                           ompi_status_public_t * status);
int         mca_io_romio_file_write_shared (ompi_file_t *fh,
                                            void *buf,
                                            int count,
                                            ompi_datatype_t *datatype,
                                            ompi_status_public_t * status);
int         mca_io_romio_file_iread_shared (ompi_file_t *fh,
                                            void *buf,
                                            int count,
                                            ompi_datatype_t *datatype,
                                            mca_io_base_request_t * request);
int         mca_io_romio_file_iwrite_shared (ompi_file_t *fh,
                                             void *buf,
                                             int count,
                                             ompi_datatype_t *datatype,
                                             mca_io_base_request_t * request);
int         mca_io_romio_file_read_ordered (ompi_file_t *fh,
                                            void *buf,
                                            int count,
                                            ompi_datatype_t *datatype,
                                            ompi_status_public_t * status);
int         mca_io_romio_file_write_ordered (ompi_file_t *fh,
                                             void *buf,
                                             int count,
                                             ompi_datatype_t *datatype,
                                             ompi_status_public_t * status);
int         mca_io_romio_file_seek_shared (ompi_file_t *fh,
                                           MPI_Offset offset,
                                           int whence);
int         mca_io_romio_file_get_position_shared (ompi_file_t *fh,
                                                   MPI_Offset * offset);

/* Section 9.4.5 */
int         mca_io_romio_file_read_at_all_begin (ompi_file_t *fh,
                                                 MPI_Offset offset,
                                                 void *buf,
                                                 int count,
                                                 ompi_datatype_t *datatype);
int         mca_io_romio_file_read_at_all_end (ompi_file_t *fh,
                                               void *buf,
                                               ompi_status_public_t * status);
int         mca_io_romio_file_write_at_all_begin (ompi_file_t *fh,
                                                  MPI_Offset offset,
                                                  void *buf,
                                                  int count,
                                                  ompi_datatype_t *datatype);
int         mca_io_romio_file_write_at_all_end (ompi_file_t *fh,
                                                void *buf,
                                                ompi_status_public_t * status);
int         mca_io_romio_file_read_all_begin (ompi_file_t *fh,
                                              void *buf,
                                              int count,
                                              ompi_datatype_t *datatype);
int         mca_io_romio_file_read_all_end (ompi_file_t *fh,
                                            void *buf,
                                            ompi_status_public_t * status);
int         mca_io_romio_file_write_all_begin (ompi_file_t *fh,
                                               void *buf,
                                               int count,
                                               ompi_datatype_t *datatype);
int         mca_io_romio_file_write_all_end (ompi_file_t *fh,
                                             void *buf,
                                             ompi_status_public_t * status);
int         mca_io_romio_file_read_ordered_begin (ompi_file_t *fh,
                                                  void *buf,
                                                  int count,
                                                  ompi_datatype_t *datatype);
int         mca_io_romio_file_read_ordered_end (ompi_file_t *fh,
                                                void *buf,
                                                ompi_status_public_t * status);
int         mca_io_romio_file_write_ordered_begin (ompi_file_t *fh,
                                                   void *buf,
                                                   int count,
                                                   ompi_datatype_t *datatype);
int         mca_io_romio_file_write_ordered_end (ompi_file_t *fh,
                                                 void *buf,
                                                 ompi_status_public_t * status);

/* Section 9.5.1 */
int         mca_io_romio_file_get_type_extent (ompi_file_t *fh,
                                               ompi_datatype_t *datatype,
                                               MPI_Aint * extent);

/* Section 9.6.1 */
int         mca_io_romio_file_set_atomicity (ompi_file_t *fh,
                                             int flag);
int         mca_io_romio_file_get_atomicity (ompi_file_t *fh,
                                             int *flag);
int         mca_io_romio_file_sync (ompi_file_t *fh);

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
