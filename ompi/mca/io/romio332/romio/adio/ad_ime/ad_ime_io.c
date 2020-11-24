/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*-
 *
 *
 *   Copyright (C) 1997 University of Chicago.
 *   Copyright (C) 2017 DataDirect Networks.
 *   See COPYRIGHT notice in top-level directory.
 */

#include "adio.h"
#include "adio_extern.h"
#include "ad_ime.h"

#include "ad_ime_common.h"

#include <stdint.h>

#define IME_READ 0
#define IME_WRITE 1

static void IME_IOContig(ADIO_File fd,
                         void *buf,
                         int count,
                         MPI_Datatype datatype,
                         int file_ptr_type,
                         ADIO_Offset offset, ADIO_Status * status, int io_flag, int *error_code)
{
    ssize_t ret;
    MPI_Count datatype_size;
    size_t mem_len;
    uint64_t file_offset = offset;
    static char myname[] = "ADIOI_IME_IOCONTIG";

    MPI_Type_size_x(datatype, &datatype_size);
    mem_len = datatype_size * count;

    if (file_ptr_type == ADIO_INDIVIDUAL)
        file_offset = fd->fp_ind;

    switch (io_flag) {
        case IME_READ:
            ret = ime_native_pread(fd->fd_sys, buf, mem_len, offset);
            break;
        case IME_WRITE:
            ret = ime_native_pwrite(fd->fd_sys, buf, mem_len, offset);
            break;
        default:
            *error_code = MPIO_Err_create_code(MPI_SUCCESS,
                                               MPIR_ERR_RECOVERABLE,
                                               myname, __LINE__, MPI_ERR_IO, "Unknown flag", 0);
            goto exit;

            break;
    };

    /* Let the application decide how to fail */
    if (ret < 0) {
        *error_code = MPI_SUCCESS;
        goto exit;
    }

    if (file_ptr_type == ADIO_INDIVIDUAL)
        fd->fp_ind += ret;
    fd->fp_sys_posn = file_offset + ret;

#ifdef HAVE_STATUS_SET_BYTES
    MPIR_Status_set_bytes(status, datatype, ret);
#endif

    *error_code = MPI_SUCCESS;

  exit:
    return;
}

void ADIOI_IME_ReadContig(ADIO_File fd,
                          void *buf,
                          int count,
                          MPI_Datatype datatype,
                          int file_ptr_type,
                          ADIO_Offset offset, ADIO_Status * status, int *error_code)
{
    IME_IOContig(fd, buf, count, datatype, file_ptr_type, offset, status, IME_READ, error_code);
}

void ADIOI_IME_WriteContig(ADIO_File fd,
                           const void *buf,
                           int count,
                           MPI_Datatype datatype,
                           int file_ptr_type,
                           ADIO_Offset offset, ADIO_Status * status, int *error_code)
{
    IME_IOContig(fd,
                 (void *) buf,
                 count, datatype, file_ptr_type, offset, status, IME_WRITE, error_code);
}
