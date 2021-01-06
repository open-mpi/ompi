/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#ifndef AD_UFS_H_INCLUDED
#define AD_UFS_H_INCLUDED

#include "adio.h"
#include <unistd.h>
#include <sys/types.h>
#include <fcntl.h>

#ifdef HAVE_SIGNAL_H
#include <signal.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#ifdef HAVE_AIO_LITE_H
#include <aio-lite.h>
#else
#ifdef  HAVE_AIO_H
#include <aio.h>
#endif
#ifdef HAVE_SYS_AIO_H
#include <sys/aio.h>
#endif
#endif

/* Workaround for incomplete set of definitions if __REDIRECT is not
   defined and large file support is used in aio.h */
#if !defined(__REDIRECT) && defined(__USE_FILE_OFFSET64)
#define aiocb aiocb64
#endif

int ADIOI_UFS_aio(ADIO_File fd, void *buf, int len, ADIO_Offset offset, int wr, void *handle);

void ADIOI_UFS_Open(ADIO_File fd, int *error_code);
void ADIOI_UFS_IwriteContig(ADIO_File fd, void *buf, int count,
                            MPI_Datatype datatype, int file_ptr_type,
                            ADIO_Offset offset, ADIO_Request * request, int
                            *error_code);
void ADIOI_UFS_IreadContig(ADIO_File fd, void *buf, int count,
                           MPI_Datatype datatype, int file_ptr_type,
                           ADIO_Offset offset, ADIO_Request * request, int
                           *error_code);
int ADIOI_UFS_ReadDone(ADIO_Request * request, ADIO_Status * status, int
                       *error_code);
int ADIOI_UFS_WriteDone(ADIO_Request * request, ADIO_Status * status, int
                        *error_code);
void ADIOI_UFS_ReadComplete(ADIO_Request * request, ADIO_Status * status, int
                            *error_code);
void ADIOI_UFS_WriteComplete(ADIO_Request * request, ADIO_Status * status, int *error_code);
void ADIOI_UFS_Fcntl(ADIO_File fd, int flag, ADIO_Fcntl_t * fcntl_struct, int
                     *error_code);

#endif /* AD_UFS_H_INCLUDED */
