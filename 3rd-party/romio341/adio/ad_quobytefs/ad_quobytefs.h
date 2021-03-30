/**
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 *
 *   The main purpose of implementing the ADIO interface for Quobyte is to
 *   avoid calling the kernel for I/O operations. Using default MPI, a user
 *   would mount a quobyte volume in userspace and then use regular unix file
 *   paths as input for MPI-IO.
 *
 *   This results in the following calls:
 *   MPI-IO <-> UFS (kernel) <-> quobyte client (userspace)
 *
 *   Using Quobyte ADIO interface we will call the quobyte client directly
 *   through our API library:
 *   MPI-IO <-> quobyte client (userspace)
 *
 *   This enables Quobyte users to take full advantage of the Quobyte
 *   distributed filesystem.
 *
 *   This implementation of the ADIO interface is based on the available UFS
 *   ADIO implementation, committing minimal changes necessary for the
 *   compatibility with the Quobyte filesystem.
 */

#ifndef AD_QUOBYTEFS_H_INCLUDED
#define AD_QUOBYTEFS_H_INCLUDED

#include "adio.h"
#include <unistd.h>
#include <sys/types.h>
#include <fcntl.h>

#ifdef HAVE_SIGNAL_H
#include <signal.h>
#endif

extern int global_quobyte_io_context;

void ADIOI_QUOBYTEFS_CreateAdapter(const char *registry, int *error_code);
void ADIOI_QUOBYTEFS_DestroyAdapter(void) __attribute__ ((destructor));

void ADIOI_QUOBYTEFS_Open(ADIO_File fd, int *error_code);
void ADIOI_QUOBYTEFS_ReadContig(ADIO_File fd, void *buf, int count,
                                MPI_Datatype datatype, int file_ptr_type,
                                ADIO_Offset offset, ADIO_Status * status, int *error_code);
void ADIOI_QUOBYTEFS_WriteContig(ADIO_File fd, const void *buf, int count,
                                 MPI_Datatype datatype, int file_ptr_type,
                                 ADIO_Offset offset, ADIO_Status * status, int *error_code);
void ADIOI_QUOBYTEFS_Fcntl(ADIO_File fd, int flag, ADIO_Fcntl_t * fcntl_struct, int
                           *error_code);
void ADIOI_QUOBYTEFS_Close(ADIO_File fd, int *error_code);
void ADIOI_QUOBYTEFS_Flush(ADIO_File fd, int *error_code);
void ADIOI_QUOBYTEFS_Resize(ADIO_File fd, ADIO_Offset size, int *error_code);
void ADIOI_QUOBYTEFS_Delete(const char *path, int *error_code);
int ADIOI_QUOBYTEFS_SetLock(ADIO_File fd, int cmd, int type, ADIO_Offset offset, int whence,
                            ADIO_Offset len);
int ADIOI_QUOBYTEFS_aio(ADIO_File fd, void *buf, int count, MPI_Datatype type,
                        ADIO_Offset offset, int wr, MPI_Request * request);
int ADIOI_QUOBYTEFS_aio_free_fn(void *extra_state);
int ADIOI_QUOBYTEFS_aio_poll_fn(void *extra_state, MPI_Status * status);
int ADIOI_QUOBYTEFS_aio_wait_fn(int count, void **array_of_states, double timeout,
                                MPI_Status * status);
void ADIOI_QUOBYTEFS_IreadContig(ADIO_File fd, void *buf, int count, MPI_Datatype datatype,
                                 int file_ptr_type, ADIO_Offset offset, MPI_Request * request,
                                 int *error_code);
void ADIOI_QUOBYTEFS_IwriteContig(ADIO_File fd, const void *buf, int count, MPI_Datatype datatype,
                                  int file_ptr_type, ADIO_Offset offset, ADIO_Request * request,
                                  int *error_code);
#endif /* AD_QUOBYTEFS_H_INCLUDED */
