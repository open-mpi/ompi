#ifndef MCA_ROMIO_IO_H
#define MCA_ROMIO_IO_H

/* Section 9.2 */
/* Begin Prototypes */
#define MPI_File_open ROMIO_PREFIX(MPI_File_open)
#define MPI_File_close ROMIO_PREFIX(MPI_File_close)
#define MPI_File_delete ROMIO_PREFIX(MPI_File_delete)
#define MPI_File_set_size ROMIO_PREFIX(MPI_File_set_size)
#define MPI_File_preallocate ROMIO_PREFIX(MPI_File_preallocate)
#define MPI_File_get_size ROMIO_PREFIX(MPI_File_get_size)
#define MPI_File_get_group ROMIO_PREFIX(MPI_File_get_group)
#define MPI_File_get_amode ROMIO_PREFIX(MPI_File_get_amode)
#define MPI_File_set_info ROMIO_PREFIX(MPI_File_set_info)
#define MPI_File_get_info ROMIO_PREFIX(MPI_File_get_info)

/* Section 9.3 */
#define MPI_File_set_view ROMIO_PREFIX(MPI_File_set_view)
#define MPI_File_get_view ROMIO_PREFIX(MPI_File_get_view)

/* Section 9.4.2 */
#define MPI_File_read_at ROMIO_PREFIX(MPI_File_read_at)
#define MPI_File_read_at_all ROMIO_PREFIX(MPI_File_read_at_all)
#define MPI_File_write_at ROMIO_PREFIX(MPI_File_write_at)
#define MPI_File_write_at_all ROMIO_PREFIX(MPI_File_write_at_all)
#define MPI_File_iread_at ROMIO_PREFIX(MPI_File_iread_at)
#define MPI_File_iwrite_at ROMIO_PREFIX(MPI_File_iwrite_at)

/* Section 9.4.3 */
#define MPI_File_read ROMIO_PREFIX(MPI_File_read)
#define MPI_File_read_all ROMIO_PREFIX(MPI_File_read_all)
#define MPI_File_write ROMIO_PREFIX(MPI_File_write)
#define MPI_File_write_all ROMIO_PREFIX(MPI_File_write_all)

#define MPI_File_iread ROMIO_PREFIX(MPI_File_iread)
#define MPI_File_iwrite ROMIO_PREFIX(MPI_File_iwrite)

#define MPI_File_seek ROMIO_PREFIX(MPI_File_seek)
#define MPI_File_get_position ROMIO_PREFIX(MPI_File_get_position)
#define MPI_File_get_byte_offset ROMIO_PREFIX(MPI_File_get_byte_offset)

/* Section 9.4.4 */
#define MPI_File_read_shared ROMIO_PREFIX(MPI_File_read_shared)
#define MPI_File_write_shared ROMIO_PREFIX(MPI_File_write_shared)
#define MPI_File_iread_shared ROMIO_PREFIX(MPI_File_iread_shared)
#define MPI_File_iwrite_shared ROMIO_PREFIX(MPI_File_iwrite_shared)
#define MPI_File_read_ordered ROMIO_PREFIX(MPI_File_read_ordered)
#define MPI_File_write_ordered ROMIO_PREFIX(MPI_File_write_ordered)
#define MPI_File_seek_shared ROMIO_PREFIX(MPI_File_seek_shared)
#define MPI_File_get_position_shared ROMIO_PREFIX(MPI_File_get_position_shared)

/* Section 9.4.5 */
#define MPI_File_read_at_all_begin ROMIO_PREFIX(MPI_File_read_at_all_begin)
#define MPI_File_read_at_all_end ROMIO_PREFIX(MPI_File_read_at_all_end)
#define MPI_File_write_at_all_begin ROMIO_PREFIX(MPI_File_write_at_all_begin)
#define MPI_File_write_at_all_end ROMIO_PREFIX(MPI_File_write_at_all_end)
#define MPI_File_read_all_begin ROMIO_PREFIX(MPI_File_read_all_begin)
#define MPI_File_read_all_end ROMIO_PREFIX(MPI_File_read_all_end)
#define MPI_File_write_all_begin ROMIO_PREFIX(MPI_File_write_all_begin)
#define MPI_File_write_all_end ROMIO_PREFIX(MPI_File_write_all_end)
#define MPI_File_read_ordered_begin ROMIO_PREFIX(MPI_File_read_ordered_begin)
#define MPI_File_read_ordered_end ROMIO_PREFIX(MPI_File_read_ordered_end)
#define MPI_File_write_ordered_begin ROMIO_PREFIX(MPI_File_write_ordered_begin)
#define MPI_File_write_ordered_end ROMIO_PREFIX(MPI_File_write_ordered_end)

/* Section 9.5.1 */
#define MPI_File_get_type_extent ROMIO_PREFIX(MPI_File_get_type_extent)

/* Section 9.6.1 */
#define MPI_File_set_atomicity ROMIO_PREFIX(MPI_File_set_atomicity)
#define MPI_File_get_atomicity ROMIO_PREFIX(MPI_File_get_atomicity)
#define MPI_File_sync ROMIO_PREFIX(MPI_File_sync)

/* Section 4.13.3 */
#define MPI_File_set_errhandler ROMIO_PREFIX(MPI_File_set_errhandler)
#define MPI_File_get_errhandler ROMIO_PREFIX(MPI_File_get_errhandler)
/* End Prototypes */

#if 0
/* Section 4.14.4 */
#define MPI_Type_create_subarray ROMIO_PREFIX(MPI_Type_create_subarray)

/* Section 4.14.5 */
#define MPI_Type_create_darray ROMIO_PREFIX(MPI_Type_create_darray)
#endif

#define MPI_File_f2c ROMIO_PREFIX(MPI_File_f2c)
#define MPI_File_c2f ROMIO_PREFIX(MPI_File_c2f)

#define MPIO_Test ROMIO_PREFIX(MPIO_Test)
#define MPIO_Wait ROMIO_PREFIX(MPIO_Wait)

#define MPIO_Request_c2f ROMIO_PREFIX(MPIO_Request_c2f)
#define MPIO_Request_f2c ROMIO_PREFIX(MPIO_Request_f2c)

/* Conversion of MPI_File and MPIO_Request */
#define MPI_File ROMIO_PREFIX(MPI_File)
#define MPIO_Request ROMIO_PREFIX(MPIO_Request)

/* info functions if not defined in the MPI implementation */
#if 0
#ifndef HAVE_MPI_INFO
int         MPI_Info_create (MPI_Info * info);
int         MPI_Info_set (MPI_Info info,
                          char *key,
                          char *value);
int         MPI_Info_delete (MPI_Info info,
                             char *key);
int         MPI_Info_get (MPI_Info info,
                          char *key,
                          int valuelen,
                          char *value,
                          int *flag);
int         MPI_Info_get_valuelen (MPI_Info info,
                                   char *key,
                                   int *valuelen,
                                   int *flag);
int         MPI_Info_get_nkeys (MPI_Info info,
                                int *nkeys);
int         MPI_Info_get_nthkey (MPI_Info info,
                                 int n,
                                 char *key);
int         MPI_Info_dup (MPI_Info info,
                          MPI_Info * newinfo);
int         MPI_Info_free (MPI_Info * info);

MPI_Fint    MPI_Info_c2f (MPI_Info info);
MPI_Info    MPI_Info_f2c (MPI_Fint info);
#endif
#endif

#endif
