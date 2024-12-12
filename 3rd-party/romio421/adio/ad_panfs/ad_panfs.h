/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#ifndef AD_PANFS_H_INCLUDED
#define AD_PANFS_H_INCLUDED

#include "adio.h"
#include <unistd.h>
#include <sys/types.h>
#include <fcntl.h>

#ifndef NO_AIO
#ifdef AIO_SUN
#include <sys/asynch.h>
#else
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
#endif
#endif

void ADIOI_PANFS_Open(ADIO_File fd, int *error_code);
/* Panasas 6 introduced some new features */
void ADIOI_PANFS_Open6(ADIO_File fd, int *error_code);
void ADIOI_PANFS_SetInfo(ADIO_File fd, MPI_Info users_info, int *error_code);
void ADIOI_PANFS_ReadContig(ADIO_File fd, void *buf, MPI_Aint count,
                            MPI_Datatype datatype, int file_ptr_type,
                            ADIO_Offset offset, ADIO_Status * status, int *error_code);
void ADIOI_PANFS_Resize(ADIO_File fd, ADIO_Offset size, int *error_code);
void ADIOI_PANFS_WriteContig(ADIO_File fd, const void *buf, MPI_Aint count,
                             MPI_Datatype datatype, int file_ptr_type,
                             ADIO_Offset offset, ADIO_Status * status, int *error_code);

/* TODO: move this to common code and have all routines retry. */
/* TODO: also check for EWOULDBLOCK */
#if defined(NEEDS_USLEEP_DECL)
int usleep(useconds_t usec);
#endif

/* Delay 1 ms */
#define AD_PANFS_RETRY_DELAY 1000

#define AD_PANFS_RETRY(_op_,_rc_) \
{ \
    _rc_ = (_op_); \
    while (_rc_ == -1 && errno == EAGAIN) \
    { \
        if (usleep(AD_PANFS_RETRY_DELAY) == -1) \
        { \
            break; \
        } \
        _rc_ = (_op_); \
    } \
}

#endif /* AD_PANFS_H_INCLUDED */
