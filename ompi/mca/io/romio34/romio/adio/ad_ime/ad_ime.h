/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#ifndef AD_IME_H_INCLUDED
#define AD_IME_H_INCLUDED

#include "adio.h"
#ifdef HAVE_IME_NATIVE_H
#include "ime_native.h"
#endif

#define ADIOI_IME_PREFIX        "ime:"
#define ADIOI_IME_PREFIX_LEN    (sizeof(ADIOI_IME_PREFIX) - 1)

void ADIOI_IME_Open(ADIO_File fd, int *error_code);

void ADIOI_IME_Close(ADIO_File fd, int *error_code);

void ADIOI_IME_ReadContig(ADIO_File fd,
                          void *buf,
                          int count,
                          MPI_Datatype datatype,
                          int file_ptr_type,
                          ADIO_Offset offset, ADIO_Status * status, int *error_code);

void ADIOI_IME_WriteContig(ADIO_File fd,
                           const void *buf,
                           int count,
                           MPI_Datatype datatype,
                           int file_ptr_type,
                           ADIO_Offset offset, ADIO_Status * status, int *error_code);

void ADIOI_IME_Fcntl(ADIO_File fd, int flag, ADIO_Fcntl_t * fcntl_struct, int *error_code);

void ADIOI_IME_Flush(ADIO_File fd, int *error_code);

void ADIOI_IME_Delete(const char *filename, int *error_code);

void ADIOI_IME_Resize(ADIO_File fd, ADIO_Offset size, int *error_code);

void ADIOI_IME_SetInfo(ADIO_File fd, MPI_Info users_info, int *error_code);

int ADIOI_IME_Feature(ADIO_File fd, int flag);
#endif /* AD_IME_H_INCLUDED */
