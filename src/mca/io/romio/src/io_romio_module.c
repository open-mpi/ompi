/*
 * $HEADER
 */
#include "ompi_config.h"

#include "mpi.h"
#include "threads/mutex.h"
#include "mca/io/io.h"
#include "io_romio.h"


/*
 * Global ROMIO mutex because ROMIO is not thread safe
 */
ompi_mutex_t mca_io_romio_mutex;


/*
 * Global function that does not need to be prototyped in a header
 * because ROMIO just expects this function to exist
 */
int MPIR_Status_set_bytes(ompi_status_public_t *status, 
                          ompi_datatype_t *datatype, int size);


/*
 * Private functions
 */
static int io_romio_request_init(mca_io_base_modules_t *module_union,
                                 mca_io_base_request_t *request);


/*
 * The ROMIO module operations
 */
mca_io_base_module_1_0_0_t mca_io_romio_module = {

    /* Max number of requests in the request cache */

    32,

    /* Additional number of bytes required for this component's
       requests */

    sizeof(mca_io_romio_request_t) - sizeof(mca_io_base_request_t),

    /* Request init / finalize functions */

    io_romio_request_init,
    NULL,

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
    mca_io_romio_file_sync,

    /* The following two are not add-on for MPI-IO implementations */
    mca_io_romio_test,
    mca_io_romio_wait
};


/*
 * This function is required by ROMIO to set information on an Open
 * MPI status.  Conveniently, it maps directly to
 * MPI_Status_set_elements (almost like they planned that... hmmm...).
 */
int MPIR_Status_set_bytes(ompi_status_public_t *status, 
                          ompi_datatype_t *datatype, int nbytes)
{
    MPI_Status_set_elements(status, datatype, nbytes);
    return MPI_SUCCESS;
}


/*
 * One-time initialization of an MPI_Request for this module
 */
static int io_romio_request_init(mca_io_base_modules_t *module_union,
                                 mca_io_base_request_t *request)
{
    request->req_ver = MCA_IO_BASE_V_1_0_0;
    return OMPI_SUCCESS;
}
