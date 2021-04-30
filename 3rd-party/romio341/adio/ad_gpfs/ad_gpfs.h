/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

/**
 * \file ad_gpfs.h
 * \brief ???
 */

#ifndef AD_GPFS_H_INCLUDED
#define AD_GPFS_H_INCLUDED

#include "adio.h"
#include <unistd.h>
#include <stdlib.h>
#include <sys/types.h>
#include <fcntl.h>

#ifdef HAVE_SIGNAL_H
#include <signal.h>
#endif
#ifdef HAVE_AIO_LITE_H
#include <aio-lite.h>
#elif defined  HAVE_AIO_H
#include <aio.h>
#endif


void ADIOI_GPFS_Open(ADIO_File fd, int *error_code);

void ADIOI_GPFS_Close(ADIO_File fd, int *error_code);

void ADIOI_GPFS_ReadContig(ADIO_File fd, void *buf, int count,
                           MPI_Datatype datatype, int file_ptr_type,
                           ADIO_Offset offset, ADIO_Status * status, int
                           *error_code);
void ADIOI_GPFS_WriteContig(ADIO_File fd, const void *buf, int count,
                            MPI_Datatype datatype, int file_ptr_type,
                            ADIO_Offset offset, ADIO_Status * status, int
                            *error_code);

void ADIOI_GPFS_SetInfo(ADIO_File fd, MPI_Info users_info, int *error_code);

void ADIOI_GPFS_WriteStrided(ADIO_File fd, const void *buf, int count,
                             MPI_Datatype datatype, int file_ptr_type,
                             ADIO_Offset offset, ADIO_Status * status, int
                             *error_code);
void ADIOI_GPFS_ReadStrided(ADIO_File fd, void *buf, int count,
                            MPI_Datatype datatype, int file_ptr_type,
                            ADIO_Offset offset, ADIO_Status * status, int
                            *error_code);

void ADIOI_GPFS_ReadStridedColl(ADIO_File fd, void *buf, int count,
                                MPI_Datatype datatype, int file_ptr_type,
                                ADIO_Offset offset, ADIO_Status * status, int
                                *error_code);

void ADIOI_GPFS_WriteStridedColl(ADIO_File fd, const void *buf, int count,
                                 MPI_Datatype datatype, int file_ptr_type,
                                 ADIO_Offset offset, ADIO_Status * status, int
                                 *error_code);

void ADIOI_GPFS_Flush(ADIO_File fd, int *error_code);

#include "ad_tuning.h"
#include "ad_gpfs_tuning.h"


#endif /* AD_GPFS_H_INCLUDED */
