/* ---------------------------------------------------------------- */
/* (C)Copyright IBM Corp.  2007, 2008                               */
/* ---------------------------------------------------------------- */
/**
 * \file ad_bg.h
 * \brief ???
 */

/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 *   Copyright (C) 1997 University of Chicago.
 *   See COPYRIGHT notice in top-level directory.
 */

#ifndef AD_BG_INCLUDE
#define AD_BG_INCLUDE

#include <unistd.h>
#include <stdlib.h>
#include <sys/types.h>
#include <fcntl.h>
#include "adio.h"

#ifdef HAVE_SIGNAL_H
#include <signal.h>
#endif
#ifdef HAVE_AIO_H
#include <aio.h>
#endif

#if 0
int ADIOI_BG_aio(ADIO_File fd, void *buf, int len, ADIO_Offset offset,
		  int wr, void *handle);
#endif

void ADIOI_BG_Open(ADIO_File fd, int *error_code);

void ADIOI_BG_Close(ADIO_File fd, int *error_code);

void ADIOI_BG_ReadContig(ADIO_File fd, void *buf, int count,
                      MPI_Datatype datatype, int file_ptr_type,
                     ADIO_Offset offset, ADIO_Status *status, int
		     *error_code);
void ADIOI_BG_WriteContig(ADIO_File fd, const void *buf, int count,
                      MPI_Datatype datatype, int file_ptr_type,
                      ADIO_Offset offset, ADIO_Status *status, int
		      *error_code);
#if 0
void ADIOI_BG_IwriteContig(ADIO_File fd, void *buf, int count,
                      MPI_Datatype datatype, int file_ptr_type,
                      ADIO_Offset offset, ADIO_Request *request, int
		      *error_code);
void ADIOI_BG_IreadContig(ADIO_File fd, void *buf, int count,
                      MPI_Datatype datatype, int file_ptr_type,
                      ADIO_Offset offset, ADIO_Request *request, int
		      *error_code);
int ADIOI_BG_ReadDone(ADIO_Request *request, ADIO_Status *status, int
		       *error_code);
int ADIOI_BG_WriteDone(ADIO_Request *request, ADIO_Status *status, int
		       *error_code);
void ADIOI_BG_ReadComplete(ADIO_Request *request, ADIO_Status *status, int
		       *error_code);
void ADIOI_BG_WriteComplete(ADIO_Request *request, ADIO_Status *status,
			int *error_code);
#endif
void ADIOI_BG_Fcntl(ADIO_File fd, int flag, ADIO_Fcntl_t *fcntl_struct, int
		*error_code);
void ADIOI_BG_SetInfo(ADIO_File fd, MPI_Info users_info, int *error_code);

void ADIOI_BG_WriteStrided(ADIO_File fd, const void *buf, int count,
		       MPI_Datatype datatype, int file_ptr_type,
		       ADIO_Offset offset, ADIO_Status *status, int
		       *error_code);
void ADIOI_BG_ReadStrided(ADIO_File fd, void *buf, int count,
		       MPI_Datatype datatype, int file_ptr_type,
		       ADIO_Offset offset, ADIO_Status *status, int
		       *error_code);

void ADIOI_BG_ReadStridedColl(ADIO_File fd, void *buf, int count,
                               MPI_Datatype datatype, int file_ptr_type,
                               ADIO_Offset offset, ADIO_Status *status, int
                               *error_code);

void ADIOI_BG_WriteStridedColl(ADIO_File fd, const void *buf, int count,
                       MPI_Datatype datatype, int file_ptr_type,
                       ADIO_Offset offset, ADIO_Status *status, int
                       *error_code);

void ADIOI_BG_Get_shared_fp(ADIO_File fd, int size, ADIO_Offset *shared_fp, int *error_code);
void ADIOI_BG_Set_shared_fp(ADIO_File fd, ADIO_Offset offset, int *error_code);

void ADIOI_BG_Flush(ADIO_File fd, int *error_code);

#include "ad_bg_tuning.h"


#endif
