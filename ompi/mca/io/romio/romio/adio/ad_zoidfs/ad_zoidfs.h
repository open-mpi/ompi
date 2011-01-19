/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#ifndef AD_ZOIDFS_INCLUDE
#define AD_ZOIDFS_INCLUDE

#include "adio.h"
#ifdef HAVE_ZOIDFS_H
#include "zoidfs.h"
#endif


typedef zoidfs_handle_t ADIOI_ZOIDFS_object;

void ADIOI_ZOIDFS_Open(ADIO_File fd, int *error_code);
void ADIOI_ZOIDFS_Close(ADIO_File fd, int *error_code);
void ADIOI_ZOIDFS_ReadContig(ADIO_File fd, void *buf, int count, 
                      MPI_Datatype datatype, int file_ptr_type,
                     ADIO_Offset offset, ADIO_Status *status, int
		     *error_code);
void ADIOI_ZOIDFS_WriteContig(ADIO_File fd, void *buf, int count, 
                      MPI_Datatype datatype, int file_ptr_type,
                      ADIO_Offset offset, ADIO_Status *status, int
		      *error_code);   
void ADIOI_ZOIDFS_Fcntl(ADIO_File fd, int flag, ADIO_Fcntl_t *fcntl_struct, int
		*error_code); 
void ADIOI_ZOIDFS_WriteStrided(ADIO_File fd, void *buf, int count,
		       MPI_Datatype datatype, int file_ptr_type,
		       ADIO_Offset offset, ADIO_Status *status, int
		       *error_code);
void ADIOI_ZOIDFS_ReadStrided(ADIO_File fd, void *buf, int count,
		       MPI_Datatype datatype, int file_ptr_type,
		       ADIO_Offset offset, ADIO_Status *status, int
		       *error_code);
void ADIOI_ZOIDFS_Flush(ADIO_File fd, int *error_code);
void ADIOI_ZOIDFS_Delete(char *filename, int *error_code);
void ADIOI_ZOIDFS_Resize(ADIO_File fd, ADIO_Offset size, int *error_code);
void ADIOI_ZOIDFS_SetInfo(ADIO_File fd, MPI_Info users_info, int *error_code);
int  ADIOI_ZOIDFS_Feature(ADIO_File fd, int flag);
#endif
