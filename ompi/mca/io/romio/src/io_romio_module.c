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
#include "ompi_config.h"

#include "mpi.h"
#include "opal/threads/mutex.h"
#include "ompi/mca/io/io.h"
#include "io_romio.h"


/*
 * Global function that does not need to be prototyped in a header
 * because ROMIO just expects this function to exist
 */
int MPIR_Status_set_bytes(ompi_status_public_t *status, 
                          struct ompi_datatype_t *datatype, int size);


/*
 * The ROMIO module operations
 */
mca_io_base_module_1_0_0_t mca_io_romio_module = {

    /* Once-per-process request init / finalize functions */

    NULL,
    NULL,

    /* Finalize, free, cancel */

    mca_io_romio_request_free,
    mca_io_romio_request_cancel,

    /* Back end to MPI API calls (pretty much a 1-to-1 mapping) */

    mca_io_romio_file_open,
    mca_io_romio_file_close,

    mca_io_romio_file_set_size,
    mca_io_romio_file_preallocate,
    mca_io_romio_file_get_size,
    mca_io_romio_file_get_amode,
    mca_io_romio_file_set_info,
    mca_io_romio_file_get_info,
    mca_io_romio_file_set_view,
    mca_io_romio_file_get_view,

    /* Index IO operations */
    mca_io_romio_file_read_at,
    mca_io_romio_file_read_at_all,
    mca_io_romio_file_write_at,
    mca_io_romio_file_write_at_all,
    mca_io_romio_file_iread_at,
    mca_io_romio_file_iwrite_at,

    /* non-indexed IO operations */
    mca_io_romio_file_read,
    mca_io_romio_file_read_all,
    mca_io_romio_file_write,
    mca_io_romio_file_write_all,
    mca_io_romio_file_iread,
    mca_io_romio_file_iwrite,

    mca_io_romio_file_seek,
    mca_io_romio_file_get_position,
    mca_io_romio_file_get_byte_offset,

    mca_io_romio_file_read_shared,
    mca_io_romio_file_write_shared,
    mca_io_romio_file_iread_shared,
    mca_io_romio_file_iwrite_shared,
    mca_io_romio_file_read_ordered,
    mca_io_romio_file_write_ordered,
    mca_io_romio_file_seek_shared,
    mca_io_romio_file_get_position_shared,

    /* Split IO operations */
    mca_io_romio_file_read_at_all_begin,
    mca_io_romio_file_read_at_all_end,
    mca_io_romio_file_write_at_all_begin,
    mca_io_romio_file_write_at_all_end,
    mca_io_romio_file_read_all_begin,
    mca_io_romio_file_read_all_end,
    mca_io_romio_file_write_all_begin,
    mca_io_romio_file_write_all_end,
    mca_io_romio_file_read_ordered_begin,
    mca_io_romio_file_read_ordered_end,
    mca_io_romio_file_write_ordered_begin,
    mca_io_romio_file_write_ordered_end,

    mca_io_romio_file_get_type_extent,

    /* Sync/atomic IO operations */
    mca_io_romio_file_set_atomicity,
    mca_io_romio_file_get_atomicity,
    mca_io_romio_file_sync
};


/*
 * This function is required by ROMIO to set information on an Open
 * MPI status.  Conveniently, it maps directly to
 * MPI_Status_set_elements (almost like they planned that... hmmm...).
 */
int MPIR_Status_set_bytes(ompi_status_public_t *status, 
                          struct ompi_datatype_t *datatype, int nbytes)
{
    /* Note that ROMIO is going to give a number of *bytes* here, but
       MPI_STATUS_SET_ELEMENTS requires a number of *elements*.  So
       rather than try to do a conversion up here, just set the number
       of bytes with MPI_CHAR as the datatype.  If someone does a
       GET_STATUS later, they have to supply their own datatype, and
       we do the right calculations there.  This prevents roundoff
       errors here, potentially "losing" bytes in the process. */

    MPI_Status_set_elements(status, MPI_CHAR, nbytes);
    return MPI_SUCCESS;
}
