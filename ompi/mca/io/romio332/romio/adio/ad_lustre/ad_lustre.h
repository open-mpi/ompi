/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/*
 *   Copyright (C) 1997 University of Chicago.
 *   See COPYRIGHT notice in top-level directory.
 *
 *   Copyright (C) 2007 Oak Ridge National Laboratory
 *
 *   Copyright (C) 2008 Sun Microsystems, Lustre group
 */

#ifndef AD_LUSTRE_H_INCLUDED
#define AD_LUSTRE_H_INCLUDED

/* temp*/
#define HAVE_ASM_TYPES_H 1

#include <unistd.h>
#include <linux/types.h>

#ifdef __linux__
#include <sys/ioctl.h>  /* necessary for: */
#include <time.h>
#ifndef __USE_GNU
#define __USE_GNU 1     /* O_DIRECT and */
#include <fcntl.h>      /* IO operations */
#endif
#undef __USE_GNU
#endif /* __linux__ */

#include <sys/ioctl.h>

#include "adio.h"
#include "ad_tuning.h"

#ifdef HAVE_LUSTRE_LUSTRE_USER_H
#include <lustre/lustre_user.h>
#endif
#ifdef HAVE_LINUX_LUSTRE_LUSTRE_USER_H
#include <linux/lustre/lustre_user.h>
#endif


#ifdef HAVE_SIGNAL_H
#include <signal.h>
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
#endif /* End of HAVE_AIO_LITE_H */

void ADIOI_LUSTRE_Open(ADIO_File fd, int *error_code);
void ADIOI_LUSTRE_Close(ADIO_File fd, int *error_code);
void ADIOI_LUSTRE_ReadContig(ADIO_File fd, void *buf, int count,
                             MPI_Datatype datatype, int file_ptr_type,
                             ADIO_Offset offset, ADIO_Status * status, int *error_code);
void ADIOI_LUSTRE_WriteContig(ADIO_File fd, const void *buf, int count,
                              MPI_Datatype datatype, int file_ptr_type,
                              ADIO_Offset offset, ADIO_Status * status, int *error_code);
void ADIOI_LUSTRE_WriteStrided(ADIO_File fd, const void *buf, int count,
                               MPI_Datatype datatype, int file_ptr_type,
                               ADIO_Offset offset, ADIO_Status * status, int *error_code);
void ADIOI_LUSTRE_WriteStridedColl(ADIO_File fd, const void *buf, int count,
                                   MPI_Datatype datatype, int file_ptr_type,
                                   ADIO_Offset offset, ADIO_Status * status, int *error_code);
void ADIOI_LUSTRE_ReadStridedColl(ADIO_File fd, void *buf, int count,
                                  MPI_Datatype datatype, int file_ptr_type,
                                  ADIO_Offset offset, ADIO_Status * status, int *error_code);
void ADIOI_LUSTRE_ReadStrided(ADIO_File fd, void *buf, int count,
                              MPI_Datatype datatype, int file_ptr_type,
                              ADIO_Offset offset, ADIO_Status * status, int *error_code);
void ADIOI_LUSTRE_Fcntl(ADIO_File fd, int flag, ADIO_Fcntl_t * fcntl_struct, int *error_code);
void ADIOI_LUSTRE_SetInfo(ADIO_File fd, MPI_Info users_info, int *error_code);

/* the lustre utilities: */
int ADIOI_LUSTRE_Docollect(ADIO_File fd, int contig_access_count,
                           ADIO_Offset * len_list, int nprocs);

void ADIOI_LUSTRE_Get_striping_info(ADIO_File fd, int **striping_info_ptr, int mode);
void ADIOI_LUSTRE_Calc_my_req(ADIO_File fd, ADIO_Offset * offset_list,
                              ADIO_Offset * len_list, int contig_access_count,
                              int *striping_info, int nprocs,
                              int *count_my_req_procs_ptr,
                              int **count_my_req_per_proc_ptr,
                              ADIOI_Access ** my_req_ptr, ADIO_Offset *** buf_idx_ptr);

int ADIOI_LUSTRE_Calc_aggregator(ADIO_File fd, ADIO_Offset off,
                                 ADIO_Offset * len, int *striping_info);
#endif /* AD_LUSTRE_H_INCLUDED */
