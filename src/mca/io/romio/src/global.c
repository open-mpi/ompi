/*
 * $HEADER
 */
#include "lam/threads/mutex.h"
#include "io_romio.h"

lam_mutex_t mca_io_romio_mutex;

mca_io_1_0_0_t romio_actions = {
 mca_io_romio_File_open,
 mca_io_romio_File_close,
 mca_io_romio_File_delete,
 mca_io_romio_File_set_size,
 mca_io_romio_File_preallocate,
 mca_io_romio_File_get_size,
 mca_io_romio_File_get_group,
 mca_io_romio_File_get_amode,    
 mca_io_romio_File_set_info,
 mca_io_romio_File_get_info,
 mca_io_romio_File_set_view,
 mca_io_romio_File_get_view,
 mca_io_romio_File_read_at,
 mca_io_romio_File_read_at_all,
 mca_io_romio_File_write_at,
 mca_io_romio_File_write_at_all,
 mca_io_romio_File_iread_at,
 mca_io_romio_File_iwrite_at,
 mca_io_romio_File_read,
 mca_io_romio_File_read_all,
 mca_io_romio_File_write,
 mca_io_romio_File_write_all,
 mca_io_romio_File_iread,
 mca_io_romio_File_iwrite,
 
 mca_io_romio_File_seek,
 mca_io_romio_File_get_position,
 mca_io_romio_File_get_byte_offset,
 
 mca_io_romio_File_read_shared,
 mca_io_romio_File_write_shared,
 mca_io_romio_File_iread_shared,
 mca_io_romio_File_iwrite_shared,
 mca_io_romio_File_read_ordered,
 mca_io_romio_File_write_ordered,
 mca_io_romio_File_seek_shared,
 mca_io_romio_File_get_position_shared,
 
 mca_io_romio_File_read_at_all_begin,
 mca_io_romio_File_read_at_all_end,
 mca_io_romio_File_write_at_all_begin,
 mca_io_romio_File_write_at_all_end,
 mca_io_romio_File_read_all_begin,
 mca_io_romio_File_read_all_end,
 mca_io_romio_File_write_all_begin,
 mca_io_romio_File_write_all_end,
 mca_io_romio_File_read_ordered_begin,
 mca_io_romio_File_read_ordered_end,
 mca_io_romio_File_write_ordered_begin,
 mca_io_romio_File_write_ordered_end,
 
 mca_io_romio_File_get_type_extent,
 
 mca_io_romio_File_set_atomicity,
 mca_io_romio_File_get_atomicity,
 mca_io_romio_File_sync,
 
 mca_io_romio_File_set_errhandler,
 mca_io_romio_File_get_errhandler,
 
 mca_io_romio_Test,
 mca_io_romio_Wait
};
