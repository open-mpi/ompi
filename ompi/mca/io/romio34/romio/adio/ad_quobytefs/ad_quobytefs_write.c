/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */


#include "adio.h"

#ifdef AGGREGATION_PROFILE
#include "mpe.h"
#endif

#include "ad_quobytefs.h"

void ADIOI_QUOBYTEFS_WriteContig(ADIO_File fd, const void *buf, int count,
                                 MPI_Datatype datatype, int file_ptr_type,
                                 ADIO_Offset offset, ADIO_Status * status, int *error_code)
{
    MPI_Count datatype_size;
    ADIO_Offset bytes_transfered;
    char *buffer_pointer = (char *) buf;
    static char myname[] = "ADIOI_QUOBYTEFS_WRITECONTIG";
#ifdef AGGREGATION_PROFILE
    MPE_Log_event(5036, 0, NULL);
#endif
    MPI_Type_size_x(datatype, &datatype_size);
    bytes_transfered = datatype_size * (ADIO_Offset) count;

    if (file_ptr_type == ADIO_INDIVIDUAL) {
        offset = fd->fp_ind;
    }
#ifdef ADIOI_MPE_LOGGING
    MPE_Log_event(ADIOI_MPE_write_a, 0, NULL);
#endif
    if (quobyte_write(fd->file_handle, buffer_pointer, offset, bytes_transfered,
                      false /* sync write */) != bytes_transfered) {
        *error_code = MPIO_Err_create_code(MPI_SUCCESS, MPIR_ERR_FATAL, myname,
                                           __LINE__, MPI_ERR_IO, "Quobyte failed to write", 0);
        return;
    }
#ifdef ADIOI_MPE_LOGGING
    MPE_Log_event(ADIOI_MPE_write_b, 0, NULL);
#endif
    fd->fp_sys_posn = offset + bytes_transfered;

    if (file_ptr_type == ADIO_INDIVIDUAL) {
        fd->fp_ind += bytes_transfered;
    }
#ifdef HAVE_STATUS_SET_BYTES
    MPIR_Status_set_bytes(status, datatype, bytes_transfered);
#endif
    *error_code = MPI_SUCCESS;
#ifdef AGGREGATION_PROFILE
    MPE_Log_event(5037, 0, NULL);
#endif
}
