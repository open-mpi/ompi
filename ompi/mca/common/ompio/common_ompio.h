/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008-2016 University of Houston. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_COMMON_OMPIO_H
#define MCA_COMMON_OMPIO_H

#include "ompi/mca/common/ompio/common_ompio_print_queue.h"
#include "common_ompio_aggregators.h"
#include "ompi/mca/io/ompio/io_ompio.h"

#define OMPIO_MCA_GET(fh, name) ((fh)->f_get_mca_parameter_value(#name, strlen(#name)+1))


OMPI_DECLSPEC int mca_common_ompio_file_write (mca_io_ompio_file_t *fh, const void *buf,  int count,
                                               struct ompi_datatype_t *datatype, 
                                               ompi_status_public_t *status);

OMPI_DECLSPEC int mca_common_ompio_file_write_at (mca_io_ompio_file_t *fh, OMPI_MPI_OFFSET_TYPE offset,  const void *buf,
                                                  int count,  struct ompi_datatype_t *datatype, 
                                                  ompi_status_public_t *status);

OMPI_DECLSPEC int mca_common_ompio_file_iwrite (mca_io_ompio_file_t *fh, const void *buf, int count,
                                                struct ompi_datatype_t *datatype, ompi_request_t **request);

OMPI_DECLSPEC int mca_common_ompio_file_iwrite_at (mca_io_ompio_file_t *fh,  OMPI_MPI_OFFSET_TYPE offset,
                                                   const void *buf,  int count,  struct ompi_datatype_t *datatype,
                                                   ompi_request_t **request);

OMPI_DECLSPEC int mca_common_ompio_file_write_at_all (mca_io_ompio_file_t *fh, OMPI_MPI_OFFSET_TYPE offset, const void *buf,
                                                      int count, struct ompi_datatype_t *datatype, 
                                                      ompi_status_public_t *status);


OMPI_DECLSPEC int mca_common_ompio_file_iwrite_at_all (mca_io_ompio_file_t *fp, OMPI_MPI_OFFSET_TYPE offset, const void *buf,
                                                       int count, struct ompi_datatype_t *datatype, ompi_request_t **request);

OMPI_DECLSPEC int mca_common_ompio_build_io_array ( mca_io_ompio_file_t *fh, int index, int cycles,
                                                    size_t bytes_per_cycle, size_t max_data, uint32_t iov_count,
                                                    struct iovec *decoded_iov, int *ii, int *jj, size_t *tbw,
                                                    size_t *spc );


OMPI_DECLSPEC int mca_common_ompio_file_read (mca_io_ompio_file_t *fh,  void *buf,  int count,
                                              struct ompi_datatype_t *datatype, ompi_status_public_t *status);

OMPI_DECLSPEC int mca_common_ompio_file_read_at (mca_io_ompio_file_t *fh, OMPI_MPI_OFFSET_TYPE offset,  void *buf,
                                                 int count, struct ompi_datatype_t *datatype, 
                                                 ompi_status_public_t * status);

OMPI_DECLSPEC int mca_common_ompio_file_iread (mca_io_ompio_file_t *fh, void *buf, int count,
                                               struct ompi_datatype_t *datatype, ompi_request_t **request);

OMPI_DECLSPEC int mca_common_ompio_file_iread_at (mca_io_ompio_file_t *fh, OMPI_MPI_OFFSET_TYPE offset,
                                                  void *buf, int count, struct ompi_datatype_t *datatype,
                                                  ompi_request_t **request);

OMPI_DECLSPEC int mca_common_ompio_file_read_at_all (mca_io_ompio_file_t *fh, OMPI_MPI_OFFSET_TYPE offset,
                                                     void *buf, int count, struct ompi_datatype_t *datatype,
                                                     ompi_status_public_t * status);

OMPI_DECLSPEC int mca_common_ompio_file_iread_at_all (mca_io_ompio_file_t *fp, OMPI_MPI_OFFSET_TYPE offset,
                                                      void *buf, int count, struct ompi_datatype_t *datatype,
                                                      ompi_request_t **request);

OMPI_DECLSPEC int mca_common_ompio_file_open (ompi_communicator_t *comm, const char *filename,
                                              int amode, ompi_info_t *info,
                                              mca_io_ompio_file_t *ompio_fh, bool use_sharedfp);

OMPI_DECLSPEC int mca_common_ompio_file_close (mca_io_ompio_file_t *ompio_fh);
OMPI_DECLSPEC int mca_common_ompio_file_get_size (mca_io_ompio_file_t *ompio_fh, OMPI_MPI_OFFSET_TYPE *size);
OMPI_DECLSPEC int mca_common_ompio_file_get_position (mca_io_ompio_file_t *fh,OMPI_MPI_OFFSET_TYPE *offset);
OMPI_DECLSPEC int mca_common_ompio_set_explicit_offset (mca_io_ompio_file_t *fh, OMPI_MPI_OFFSET_TYPE offset);
OMPI_DECLSPEC int mca_common_ompio_set_file_defaults (mca_io_ompio_file_t *fh);
OMPI_DECLSPEC int mca_common_ompio_set_view (mca_io_ompio_file_t *fh,  OMPI_MPI_OFFSET_TYPE disp,
                                             ompi_datatype_t *etype,  ompi_datatype_t *filetype, const char *datarep,
                                             ompi_info_t *info);
 
OMPI_DECLSPEC int ompi_common_ompio_decode_datatype (struct mca_io_ompio_file_t *fh, ompi_datatype_t *datatype,
                                                     int count, const void *buf,  size_t *max_data,  struct iovec **iov,
                                                     uint32_t *iovec_count);

OMPI_DECLSPEC int mca_common_ompio_file_delete (const char *filename,
                                                struct ompi_info_t *info);
#endif /* MCA_COMMON_OMPIO_H */
