/*
 * $HEADER
 */
#include "threads/mutex.h"
#include "io_romio.h"

ompi_mutex_t mca_io_romio_mutex;

mca_io_1_0_0_t mca_io_romio_ops = {

    /* manage functions */
    ROMIO_PREFIX (File_open),
    ROMIO_PREFIX (File_close),

    ROMIO_PREFIX (File_delete),
    ROMIO_PREFIX (File_set_size),
    ROMIO_PREFIX (File_preallocate),
    ROMIO_PREFIX (File_get_size),
    ROMIO_PREFIX (File_get_group),
    ROMIO_PREFIX (File_get_amode),
    ROMIO_PREFIX (File_set_info),
    ROMIO_PREFIX (File_get_info),
    ROMIO_PREFIX (File_set_view),
    ROMIO_PREFIX (File_get_view),

    /* Index IO operations */
    ROMIO_PREFIX (File_read_at),
    ROMIO_PREFIX (File_read_at_all),
    ROMIO_PREFIX (File_write_at),
    ROMIO_PREFIX (File_write_at_all),
    ROMIO_PREFIX (File_iread_at),
    ROMIO_PREFIX (File_iwrite_at),

    /* non-indexed IO operations */
    ROMIO_PREFIX (File_read),
    ROMIO_PREFIX (File_read_all),
    ROMIO_PREFIX (File_write),
    ROMIO_PREFIX (File_write_all),
    ROMIO_PREFIX (File_iread),
    ROMIO_PREFIX (File_iwrite),

    ROMIO_PREFIX (File_seek),
    ROMIO_PREFIX (File_get_position),
    ROMIO_PREFIX (File_get_byte_offset),

    ROMIO_PREFIX (File_read_shared),
    ROMIO_PREFIX (File_write_shared),
    ROMIO_PREFIX (File_iread_shared),
    ROMIO_PREFIX (File_iwrite_shared),
    ROMIO_PREFIX (File_read_ordered),
    ROMIO_PREFIX (File_write_ordered),
    ROMIO_PREFIX (File_seek_shared),
    ROMIO_PREFIX (File_get_position_shared),

    /* Split IO operations */
    ROMIO_PREFIX (File_read_at_all_begin),
    ROMIO_PREFIX (File_read_at_all_end),
    ROMIO_PREFIX (File_write_at_all_begin),
    ROMIO_PREFIX (File_write_at_all_end),
    ROMIO_PREFIX (File_read_all_begin),
    ROMIO_PREFIX (File_read_all_end),
    ROMIO_PREFIX (File_write_all_begin),
    ROMIO_PREFIX (File_write_all_end),
    ROMIO_PREFIX (File_read_ordered_begin),
    ROMIO_PREFIX (File_read_ordered_end),
    ROMIO_PREFIX (File_write_ordered_begin),
    ROMIO_PREFIX (File_write_ordered_end),

    ROMIO_PREFIX (File_get_type_extent),

    /* Sync/atomic IO operations */
    ROMIO_PREFIX (File_set_atomicity),
    ROMIO_PREFIX (File_get_atomicity),
    ROMIO_PREFIX (File_sync),

    ROMIO_PREFIX (File_set_errhandler),
    ROMIO_PREFIX (File_get_errhandler),

    /* The following two are not add-on for MPI-IO implementations */
    ROMIO_PREFIX (Test),
    ROMIO_PREFIX (Wait)
};
