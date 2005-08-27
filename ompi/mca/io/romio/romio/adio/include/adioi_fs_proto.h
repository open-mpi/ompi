/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   $Id: adioi_fs_proto.h,v 1.11 2002/10/24 17:01:16 gropp Exp $    
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */


#ifndef ADIO_PROTO
#define ADIO_PROTO

#ifdef NFS
extern struct ADIOI_Fns_struct ADIO_NFS_operations;

void ADIOI_NFS_Open(ADIO_File fd, int *error_code);
void ADIOI_NFS_Close(ADIO_File fd, int *error_code);
void ADIOI_NFS_ReadContig(ADIO_File fd, void *buf, int count, 
                      MPI_Datatype datatype, int file_ptr_type,
                     ADIO_Offset offset, ADIO_Status *status, int
		     *error_code);
void ADIOI_NFS_WriteContig(ADIO_File fd, void *buf, int count, 
                      MPI_Datatype datatype, int file_ptr_type,
                      ADIO_Offset offset, ADIO_Status *status, int
		      *error_code);   
void ADIOI_NFS_IwriteContig(ADIO_File fd, void *buf, int count, 
                      MPI_Datatype datatype, int file_ptr_type,
                      ADIO_Offset offset, ADIO_Request *request, int
		      *error_code);   
void ADIOI_NFS_IreadContig(ADIO_File fd, void *buf, int count, 
                      MPI_Datatype datatype, int file_ptr_type,
                      ADIO_Offset offset, ADIO_Request *request, int
		      *error_code);   
int ADIOI_NFS_ReadDone(ADIO_Request *request, ADIO_Status *status, int
		       *error_code);
int ADIOI_NFS_WriteDone(ADIO_Request *request, ADIO_Status *status, int
		       *error_code);
void ADIOI_NFS_ReadComplete(ADIO_Request *request, ADIO_Status *status, int
		       *error_code); 
void ADIOI_NFS_WriteComplete(ADIO_Request *request, ADIO_Status *status,
			int *error_code); 
void ADIOI_NFS_Fcntl(ADIO_File fd, int flag, ADIO_Fcntl_t *fcntl_struct, int
		*error_code); 
void ADIOI_NFS_WriteStrided(ADIO_File fd, void *buf, int count,
		       MPI_Datatype datatype, int file_ptr_type,
		       ADIO_Offset offset, ADIO_Status *status, int
		       *error_code);
void ADIOI_NFS_ReadStrided(ADIO_File fd, void *buf, int count,
		       MPI_Datatype datatype, int file_ptr_type,
		       ADIO_Offset offset, ADIO_Status *status, int
		       *error_code);
void ADIOI_NFS_WriteStridedColl(ADIO_File fd, void *buf, int count,
		       MPI_Datatype datatype, int file_ptr_type,
		       ADIO_Offset offset, ADIO_Status *status, int
		       *error_code);
void ADIOI_NFS_ReadStridedColl(ADIO_File fd, void *buf, int count,
		       MPI_Datatype datatype, int file_ptr_type,
		       ADIO_Offset offset, ADIO_Status *status, int
		       *error_code);
void ADIOI_NFS_IreadStrided(ADIO_File fd, void *buf, int count,
		       MPI_Datatype datatype, int file_ptr_type,
		       ADIO_Offset offset, ADIO_Request *request, int
		       *error_code);
void ADIOI_NFS_IwriteStrided(ADIO_File fd, void *buf, int count,
		       MPI_Datatype datatype, int file_ptr_type,
		       ADIO_Offset offset, ADIO_Request *request, int
		       *error_code);
void ADIOI_NFS_Flush(ADIO_File fd, int *error_code);
void ADIOI_NFS_Resize(ADIO_File fd, ADIO_Offset size, int *error_code);
ADIO_Offset ADIOI_NFS_SeekIndividual(ADIO_File fd, ADIO_Offset offset, 
                       int whence, int *error_code);
void ADIOI_NFS_SetInfo(ADIO_File fd, MPI_Info users_info, int *error_code);
void ADIOI_NFS_Get_shared_fp(ADIO_File fd, int size, ADIO_Offset *shared_fp, 
			 int *error_code);
void ADIOI_NFS_Set_shared_fp(ADIO_File fd, ADIO_Offset offset, int *error_code);
#endif

#ifdef PFS
extern struct ADIOI_Fns_struct ADIO_PFS_operations;

void ADIOI_PFS_Open(ADIO_File fd, int *error_code);
void ADIOI_PFS_Close(ADIO_File fd, int *error_code);
void ADIOI_PFS_ReadContig(ADIO_File fd, void *buf, int count, 
                      MPI_Datatype datatype, int file_ptr_type,
                     ADIO_Offset offset, ADIO_Status *status, int
		     *error_code);
void ADIOI_PFS_WriteContig(ADIO_File fd, void *buf, int count, 
                      MPI_Datatype datatype, int file_ptr_type,
                      ADIO_Offset offset, ADIO_Status *status, int
		      *error_code);   
void ADIOI_PFS_IwriteContig(ADIO_File fd, void *buf, int count, 
                      MPI_Datatype datatype, int file_ptr_type,
                      ADIO_Offset offset, ADIO_Request *request, int
		      *error_code);   
void ADIOI_PFS_IreadContig(ADIO_File fd, void *buf, int count, 
                      MPI_Datatype datatype, int file_ptr_type,
                      ADIO_Offset offset, ADIO_Request *request, int
		      *error_code);   
int ADIOI_PFS_ReadDone(ADIO_Request *request, ADIO_Status *status, int
		       *error_code);
int ADIOI_PFS_WriteDone(ADIO_Request *request, ADIO_Status *status, int
		       *error_code);
void ADIOI_PFS_ReadComplete(ADIO_Request *request, ADIO_Status *status, int
		       *error_code); 
void ADIOI_PFS_WriteComplete(ADIO_Request *request, ADIO_Status *status,
			int *error_code); 
void ADIOI_PFS_Fcntl(ADIO_File fd, int flag, ADIO_Fcntl_t *fcntl_struct, int
		*error_code); 
void ADIOI_PFS_WriteStrided(ADIO_File fd, void *buf, int count,
		       MPI_Datatype datatype, int file_ptr_type,
		       ADIO_Offset offset, ADIO_Status *status, int
		       *error_code);
void ADIOI_PFS_ReadStrided(ADIO_File fd, void *buf, int count,
		       MPI_Datatype datatype, int file_ptr_type,
		       ADIO_Offset offset, ADIO_Status *status, int
		       *error_code);
void ADIOI_PFS_WriteStridedColl(ADIO_File fd, void *buf, int count,
		       MPI_Datatype datatype, int file_ptr_type,
		       ADIO_Offset offset, ADIO_Status *status, int
		       *error_code);
void ADIOI_PFS_ReadStridedColl(ADIO_File fd, void *buf, int count,
		       MPI_Datatype datatype, int file_ptr_type,
		       ADIO_Offset offset, ADIO_Status *status, int
		       *error_code);
void ADIOI_PFS_IreadStrided(ADIO_File fd, void *buf, int count,
		       MPI_Datatype datatype, int file_ptr_type,
		       ADIO_Offset offset, ADIO_Request *request, int
		       *error_code);
void ADIOI_PFS_IwriteStrided(ADIO_File fd, void *buf, int count,
		       MPI_Datatype datatype, int file_ptr_type,
		       ADIO_Offset offset, ADIO_Request *request, int
		       *error_code);
void ADIOI_PFS_Flush(ADIO_File fd, int *error_code);
void ADIOI_PFS_Resize(ADIO_File fd, ADIO_Offset size, int *error_code);
ADIO_Offset ADIOI_PFS_SeekIndividual(ADIO_File fd, ADIO_Offset offset, 
                       int whence, int *error_code);
void ADIOI_PFS_SetInfo(ADIO_File fd, MPI_Info users_info, int *error_code);
#endif

#ifdef PIOFS
extern struct ADIOI_Fns_struct ADIO_PIOFS_operations;

void ADIOI_PIOFS_Open(ADIO_File fd, int *error_code);
void ADIOI_PIOFS_Close(ADIO_File fd, int *error_code);
void ADIOI_PIOFS_ReadContig(ADIO_File fd, void *buf, int count, 
                      MPI_Datatype datatype, int file_ptr_type,
                     ADIO_Offset offset, ADIO_Status *status, int
		     *error_code);
void ADIOI_PIOFS_WriteContig(ADIO_File fd, void *buf, int count, 
                      MPI_Datatype datatype, int file_ptr_type,
                      ADIO_Offset offset, ADIO_Status *status, int
		      *error_code);   
void ADIOI_PIOFS_IwriteContig(ADIO_File fd, void *buf, int count, 
                      MPI_Datatype datatype, int file_ptr_type,
                      ADIO_Offset offset, ADIO_Request *request, int
		      *error_code);   
void ADIOI_PIOFS_IreadContig(ADIO_File fd, void *buf, int count, 
                      MPI_Datatype datatype, int file_ptr_type,
                      ADIO_Offset offset, ADIO_Request *request, int
		      *error_code);   
int ADIOI_PIOFS_ReadDone(ADIO_Request *request, ADIO_Status *status, int
		       *error_code);
int ADIOI_PIOFS_WriteDone(ADIO_Request *request, ADIO_Status *status, int
		       *error_code);
void ADIOI_PIOFS_ReadComplete(ADIO_Request *request, ADIO_Status *status, int
		       *error_code); 
void ADIOI_PIOFS_WriteComplete(ADIO_Request *request, ADIO_Status *status,
			int *error_code); 
void ADIOI_PIOFS_Fcntl(ADIO_File fd, int flag, ADIO_Fcntl_t *fcntl_struct, int
		*error_code); 
void ADIOI_PIOFS_WriteStrided(ADIO_File fd, void *buf, int count,
		       MPI_Datatype datatype, int file_ptr_type,
		       ADIO_Offset offset, ADIO_Status *status, int
		       *error_code);
void ADIOI_PIOFS_ReadStrided(ADIO_File fd, void *buf, int count,
		       MPI_Datatype datatype, int file_ptr_type,
		       ADIO_Offset offset, ADIO_Status *status, int
		       *error_code);
void ADIOI_PIOFS_WriteStridedColl(ADIO_File fd, void *buf, int count,
		       MPI_Datatype datatype, int file_ptr_type,
		       ADIO_Offset offset, ADIO_Status *status, int
		       *error_code);
void ADIOI_PIOFS_ReadStridedColl(ADIO_File fd, void *buf, int count,
		       MPI_Datatype datatype, int file_ptr_type,
		       ADIO_Offset offset, ADIO_Status *status, int
		       *error_code);
void ADIOI_PIOFS_IreadStrided(ADIO_File fd, void *buf, int count,
		       MPI_Datatype datatype, int file_ptr_type,
		       ADIO_Offset offset, ADIO_Request *request, int
		       *error_code);
void ADIOI_PIOFS_IwriteStrided(ADIO_File fd, void *buf, int count,
		       MPI_Datatype datatype, int file_ptr_type,
		       ADIO_Offset offset, ADIO_Request *request, int
		       *error_code);
void ADIOI_PIOFS_Flush(ADIO_File fd, int *error_code);
void ADIOI_PIOFS_Resize(ADIO_File fd, ADIO_Offset size, int *error_code);
ADIO_Offset ADIOI_PIOFS_SeekIndividual(ADIO_File fd, ADIO_Offset offset, 
                       int whence, int *error_code);
void ADIOI_PIOFS_SetInfo(ADIO_File fd, MPI_Info users_info, int *error_code);
#endif

#ifdef UFS
extern struct ADIOI_Fns_struct ADIO_UFS_operations;

void ADIOI_UFS_Open(ADIO_File fd, int *error_code);
void ADIOI_UFS_Close(ADIO_File fd, int *error_code);
void ADIOI_UFS_ReadContig(ADIO_File fd, void *buf, int count, 
                      MPI_Datatype datatype, int file_ptr_type,
                     ADIO_Offset offset, ADIO_Status *status, int
		     *error_code);
void ADIOI_UFS_WriteContig(ADIO_File fd, void *buf, int count, 
                      MPI_Datatype datatype, int file_ptr_type,
                      ADIO_Offset offset, ADIO_Status *status, int
		      *error_code);   
void ADIOI_UFS_IwriteContig(ADIO_File fd, void *buf, int count, 
                      MPI_Datatype datatype, int file_ptr_type,
                      ADIO_Offset offset, ADIO_Request *request, int
		      *error_code);   
void ADIOI_UFS_IreadContig(ADIO_File fd, void *buf, int count, 
                      MPI_Datatype datatype, int file_ptr_type,
                      ADIO_Offset offset, ADIO_Request *request, int
		      *error_code);   
int ADIOI_UFS_ReadDone(ADIO_Request *request, ADIO_Status *status, int
		       *error_code);
int ADIOI_UFS_WriteDone(ADIO_Request *request, ADIO_Status *status, int
		       *error_code);
void ADIOI_UFS_ReadComplete(ADIO_Request *request, ADIO_Status *status, int
		       *error_code); 
void ADIOI_UFS_WriteComplete(ADIO_Request *request, ADIO_Status *status,
			int *error_code); 
void ADIOI_UFS_Fcntl(ADIO_File fd, int flag, ADIO_Fcntl_t *fcntl_struct, int
		*error_code); 
void ADIOI_UFS_WriteStrided(ADIO_File fd, void *buf, int count,
		       MPI_Datatype datatype, int file_ptr_type,
		       ADIO_Offset offset, ADIO_Status *status, int
		       *error_code);
void ADIOI_UFS_ReadStrided(ADIO_File fd, void *buf, int count,
		       MPI_Datatype datatype, int file_ptr_type,
		       ADIO_Offset offset, ADIO_Status *status, int
		       *error_code);
void ADIOI_UFS_WriteStridedColl(ADIO_File fd, void *buf, int count,
		       MPI_Datatype datatype, int file_ptr_type,
		       ADIO_Offset offset, ADIO_Status *status, int
		       *error_code);
void ADIOI_UFS_ReadStridedColl(ADIO_File fd, void *buf, int count,
		       MPI_Datatype datatype, int file_ptr_type,
		       ADIO_Offset offset, ADIO_Status *status, int
		       *error_code);
void ADIOI_UFS_IreadStrided(ADIO_File fd, void *buf, int count,
		       MPI_Datatype datatype, int file_ptr_type,
		       ADIO_Offset offset, ADIO_Request *request, int
		       *error_code);
void ADIOI_UFS_IwriteStrided(ADIO_File fd, void *buf, int count,
		       MPI_Datatype datatype, int file_ptr_type,
		       ADIO_Offset offset, ADIO_Request *request, int
		       *error_code);
void ADIOI_UFS_Flush(ADIO_File fd, int *error_code);
void ADIOI_UFS_Resize(ADIO_File fd, ADIO_Offset size, int *error_code);
ADIO_Offset ADIOI_UFS_SeekIndividual(ADIO_File fd, ADIO_Offset offset, 
                       int whence, int *error_code);
void ADIOI_UFS_SetInfo(ADIO_File fd, MPI_Info users_info, int *error_code);
#endif

#ifdef ROMIO_NTFS
extern struct ADIOI_Fns_struct ADIO_NTFS_operations;

void ADIOI_NTFS_Open(ADIO_File fd, int *error_code);
void ADIOI_NTFS_Close(ADIO_File fd, int *error_code);
void ADIOI_NTFS_ReadContig(ADIO_File fd, void *buf, int count, 
                      MPI_Datatype datatype, int file_ptr_type,
                     ADIO_Offset offset, ADIO_Status *status, int
		     *error_code);
void ADIOI_NTFS_WriteContig(ADIO_File fd, void *buf, int count, 
                      MPI_Datatype datatype, int file_ptr_type,
                      ADIO_Offset offset, ADIO_Status *status, int
		      *error_code);   
void ADIOI_NTFS_IwriteContig(ADIO_File fd, void *buf, int count, 
                      MPI_Datatype datatype, int file_ptr_type,
                      ADIO_Offset offset, ADIO_Request *request, int
		      *error_code);   
void ADIOI_NTFS_IreadContig(ADIO_File fd, void *buf, int count, 
                      MPI_Datatype datatype, int file_ptr_type,
                      ADIO_Offset offset, ADIO_Request *request, int
		      *error_code);   
int ADIOI_NTFS_ReadDone(ADIO_Request *request, ADIO_Status *status, int
		       *error_code);
int ADIOI_NTFS_WriteDone(ADIO_Request *request, ADIO_Status *status, int
		       *error_code);
void ADIOI_NTFS_ReadComplete(ADIO_Request *request, ADIO_Status *status, int
		       *error_code); 
void ADIOI_NTFS_WriteComplete(ADIO_Request *request, ADIO_Status *status,
			int *error_code); 
void ADIOI_NTFS_Fcntl(ADIO_File fd, int flag, ADIO_Fcntl_t *fcntl_struct, int
		*error_code); 
void ADIOI_NTFS_WriteStrided(ADIO_File fd, void *buf, int count,
		       MPI_Datatype datatype, int file_ptr_type,
		       ADIO_Offset offset, ADIO_Status *status, int
		       *error_code);
void ADIOI_NTFS_ReadStrided(ADIO_File fd, void *buf, int count,
		       MPI_Datatype datatype, int file_ptr_type,
		       ADIO_Offset offset, ADIO_Status *status, int
		       *error_code);
void ADIOI_NTFS_WriteStridedColl(ADIO_File fd, void *buf, int count,
		       MPI_Datatype datatype, int file_ptr_type,
		       ADIO_Offset offset, ADIO_Status *status, int
		       *error_code);
void ADIOI_NTFS_ReadStridedColl(ADIO_File fd, void *buf, int count,
		       MPI_Datatype datatype, int file_ptr_type,
		       ADIO_Offset offset, ADIO_Status *status, int
		       *error_code);
void ADIOI_NTFS_IreadStrided(ADIO_File fd, void *buf, int count,
		       MPI_Datatype datatype, int file_ptr_type,
		       ADIO_Offset offset, ADIO_Request *request, int
		       *error_code);
void ADIOI_NTFS_IwriteStrided(ADIO_File fd, void *buf, int count,
		       MPI_Datatype datatype, int file_ptr_type,
		       ADIO_Offset offset, ADIO_Request *request, int
		       *error_code);
void ADIOI_NTFS_Flush(ADIO_File fd, int *error_code);
void ADIOI_NTFS_Resize(ADIO_File fd, ADIO_Offset size, int *error_code);
ADIO_Offset ADIOI_NTFS_SeekIndividual(ADIO_File fd, ADIO_Offset offset, 
                       int whence, int *error_code);
void ADIOI_NTFS_SetInfo(ADIO_File fd, MPI_Info users_info, int *error_code);
#endif


#ifdef HFS
extern struct ADIOI_Fns_struct ADIO_HFS_operations;

void ADIOI_HFS_Open(ADIO_File fd, int *error_code);
void ADIOI_HFS_Close(ADIO_File fd, int *error_code);
void ADIOI_HFS_ReadContig(ADIO_File fd, void *buf, int count, 
                      MPI_Datatype datatype, int file_ptr_type,
                     ADIO_Offset offset, ADIO_Status *status, int
		     *error_code);
void ADIOI_HFS_WriteContig(ADIO_File fd, void *buf, int count, 
                      MPI_Datatype datatype, int file_ptr_type,
                      ADIO_Offset offset, ADIO_Status *status, int
		      *error_code);   
void ADIOI_HFS_IwriteContig(ADIO_File fd, void *buf, int count, 
                      MPI_Datatype datatype, int file_ptr_type,
                      ADIO_Offset offset, ADIO_Request *request, int
		      *error_code);   
void ADIOI_HFS_IreadContig(ADIO_File fd, void *buf, int count, 
                      MPI_Datatype datatype, int file_ptr_type,
                      ADIO_Offset offset, ADIO_Request *request, int
		      *error_code);   
int ADIOI_HFS_ReadDone(ADIO_Request *request, ADIO_Status *status, int
		       *error_code);
int ADIOI_HFS_WriteDone(ADIO_Request *request, ADIO_Status *status, int
		       *error_code);
void ADIOI_HFS_ReadComplete(ADIO_Request *request, ADIO_Status *status, int
		       *error_code); 
void ADIOI_HFS_WriteComplete(ADIO_Request *request, ADIO_Status *status,
			int *error_code); 
void ADIOI_HFS_Fcntl(ADIO_File fd, int flag, ADIO_Fcntl_t *fcntl_struct, int
		*error_code); 
void ADIOI_HFS_WriteStrided(ADIO_File fd, void *buf, int count,
		       MPI_Datatype datatype, int file_ptr_type,
		       ADIO_Offset offset, ADIO_Status *status, int
		       *error_code);
void ADIOI_HFS_ReadStrided(ADIO_File fd, void *buf, int count,
		       MPI_Datatype datatype, int file_ptr_type,
		       ADIO_Offset offset, ADIO_Status *status, int
		       *error_code);
void ADIOI_HFS_WriteStridedColl(ADIO_File fd, void *buf, int count,
		       MPI_Datatype datatype, int file_ptr_type,
		       ADIO_Offset offset, ADIO_Status *status, int
		       *error_code);
void ADIOI_HFS_ReadStridedColl(ADIO_File fd, void *buf, int count,
		       MPI_Datatype datatype, int file_ptr_type,
		       ADIO_Offset offset, ADIO_Status *status, int
		       *error_code);
void ADIOI_HFS_IreadStrided(ADIO_File fd, void *buf, int count,
		       MPI_Datatype datatype, int file_ptr_type,
		       ADIO_Offset offset, ADIO_Request *request, int
		       *error_code);
void ADIOI_HFS_IwriteStrided(ADIO_File fd, void *buf, int count,
		       MPI_Datatype datatype, int file_ptr_type,
		       ADIO_Offset offset, ADIO_Request *request, int
		       *error_code);
void ADIOI_HFS_Flush(ADIO_File fd, int *error_code);
void ADIOI_HFS_Resize(ADIO_File fd, ADIO_Offset size, int *error_code);
ADIO_Offset ADIOI_HFS_SeekIndividual(ADIO_File fd, ADIO_Offset offset, 
                       int whence, int *error_code);
void ADIOI_HFS_SetInfo(ADIO_File fd, MPI_Info users_info, int *error_code);
#endif

#ifdef XFS
extern struct ADIOI_Fns_struct ADIO_XFS_operations;

void ADIOI_XFS_Open(ADIO_File fd, int *error_code);
void ADIOI_XFS_Close(ADIO_File fd, int *error_code);
void ADIOI_XFS_ReadContig(ADIO_File fd, void *buf, int count, 
                      MPI_Datatype datatype, int file_ptr_type,
                     ADIO_Offset offset, ADIO_Status *status, int
		     *error_code);
void ADIOI_XFS_WriteContig(ADIO_File fd, void *buf, int count, 
                      MPI_Datatype datatype, int file_ptr_type,
                      ADIO_Offset offset, ADIO_Status *status, int
		      *error_code);   
void ADIOI_XFS_IwriteContig(ADIO_File fd, void *buf, int count, 
                      MPI_Datatype datatype, int file_ptr_type,
                      ADIO_Offset offset, ADIO_Request *request, int
		      *error_code);   
void ADIOI_XFS_IreadContig(ADIO_File fd, void *buf, int count, 
                      MPI_Datatype datatype, int file_ptr_type,
                      ADIO_Offset offset, ADIO_Request *request, int
		      *error_code);   
int ADIOI_XFS_ReadDone(ADIO_Request *request, ADIO_Status *status, int
		       *error_code);
int ADIOI_XFS_WriteDone(ADIO_Request *request, ADIO_Status *status, int
		       *error_code);
void ADIOI_XFS_ReadComplete(ADIO_Request *request, ADIO_Status *status, int
		       *error_code); 
void ADIOI_XFS_WriteComplete(ADIO_Request *request, ADIO_Status *status,
			int *error_code); 
void ADIOI_XFS_Fcntl(ADIO_File fd, int flag, ADIO_Fcntl_t *fcntl_struct, int
		*error_code); 
void ADIOI_XFS_WriteStrided(ADIO_File fd, void *buf, int count,
		       MPI_Datatype datatype, int file_ptr_type,
		       ADIO_Offset offset, ADIO_Status *status, int
		       *error_code);
void ADIOI_XFS_ReadStrided(ADIO_File fd, void *buf, int count,
		       MPI_Datatype datatype, int file_ptr_type,
		       ADIO_Offset offset, ADIO_Status *status, int
		       *error_code);
void ADIOI_XFS_WriteStridedColl(ADIO_File fd, void *buf, int count,
		       MPI_Datatype datatype, int file_ptr_type,
		       ADIO_Offset offset, ADIO_Status *status, int
		       *error_code);
void ADIOI_XFS_ReadStridedColl(ADIO_File fd, void *buf, int count,
		       MPI_Datatype datatype, int file_ptr_type,
		       ADIO_Offset offset, ADIO_Status *status, int
		       *error_code);
void ADIOI_XFS_IreadStrided(ADIO_File fd, void *buf, int count,
		       MPI_Datatype datatype, int file_ptr_type,
		       ADIO_Offset offset, ADIO_Request *request, int
		       *error_code);
void ADIOI_XFS_IwriteStrided(ADIO_File fd, void *buf, int count,
		       MPI_Datatype datatype, int file_ptr_type,
		       ADIO_Offset offset, ADIO_Request *request, int
		       *error_code);
void ADIOI_XFS_Flush(ADIO_File fd, int *error_code);
void ADIOI_XFS_Resize(ADIO_File fd, ADIO_Offset size, int *error_code);
ADIO_Offset ADIOI_XFS_SeekIndividual(ADIO_File fd, ADIO_Offset offset, 
                       int whence, int *error_code);
void ADIOI_XFS_SetInfo(ADIO_File fd, MPI_Info users_info, int *error_code);
#endif

#ifdef SFS
extern struct ADIOI_Fns_struct ADIO_SFS_operations;

void ADIOI_SFS_Open(ADIO_File fd, int *error_code);
void ADIOI_SFS_Close(ADIO_File fd, int *error_code);
void ADIOI_SFS_ReadContig(ADIO_File fd, void *buf, int count, 
                      MPI_Datatype datatype, int file_ptr_type,
                     ADIO_Offset offset, ADIO_Status *status, int
		     *error_code);
void ADIOI_SFS_WriteContig(ADIO_File fd, void *buf, int count, 
                      MPI_Datatype datatype, int file_ptr_type,
                      ADIO_Offset offset, ADIO_Status *status, int
		      *error_code);   
void ADIOI_SFS_IwriteContig(ADIO_File fd, void *buf, int count, 
                      MPI_Datatype datatype, int file_ptr_type,
                      ADIO_Offset offset, ADIO_Request *request, int
		      *error_code);   
void ADIOI_SFS_IreadContig(ADIO_File fd, void *buf, int count, 
                      MPI_Datatype datatype, int file_ptr_type,
                      ADIO_Offset offset, ADIO_Request *request, int
		      *error_code);   
int ADIOI_SFS_ReadDone(ADIO_Request *request, ADIO_Status *status, int
		       *error_code);
int ADIOI_SFS_WriteDone(ADIO_Request *request, ADIO_Status *status, int
		       *error_code);
void ADIOI_SFS_ReadComplete(ADIO_Request *request, ADIO_Status *status, int
		       *error_code); 
void ADIOI_SFS_WriteComplete(ADIO_Request *request, ADIO_Status *status,
			int *error_code); 
void ADIOI_SFS_Fcntl(ADIO_File fd, int flag, ADIO_Fcntl_t *fcntl_struct, int
		*error_code); 
void ADIOI_SFS_WriteStrided(ADIO_File fd, void *buf, int count,
		       MPI_Datatype datatype, int file_ptr_type,
		       ADIO_Offset offset, ADIO_Status *status, int
		       *error_code);
void ADIOI_SFS_ReadStrided(ADIO_File fd, void *buf, int count,
		       MPI_Datatype datatype, int file_ptr_type,
		       ADIO_Offset offset, ADIO_Status *status, int
		       *error_code);
void ADIOI_SFS_WriteStridedColl(ADIO_File fd, void *buf, int count,
		       MPI_Datatype datatype, int file_ptr_type,
		       ADIO_Offset offset, ADIO_Status *status, int
		       *error_code);
void ADIOI_SFS_ReadStridedColl(ADIO_File fd, void *buf, int count,
		       MPI_Datatype datatype, int file_ptr_type,
		       ADIO_Offset offset, ADIO_Status *status, int
		       *error_code);
void ADIOI_SFS_IreadStrided(ADIO_File fd, void *buf, int count,
		       MPI_Datatype datatype, int file_ptr_type,
		       ADIO_Offset offset, ADIO_Request *request, int
		       *error_code);
void ADIOI_SFS_IwriteStrided(ADIO_File fd, void *buf, int count,
		       MPI_Datatype datatype, int file_ptr_type,
		       ADIO_Offset offset, ADIO_Request *request, int
		       *error_code);
void ADIOI_SFS_Flush(ADIO_File fd, int *error_code);
void ADIOI_SFS_Resize(ADIO_File fd, ADIO_Offset size, int *error_code);
ADIO_Offset ADIOI_SFS_SeekIndividual(ADIO_File fd, ADIO_Offset offset, 
                       int whence, int *error_code);
void ADIOI_SFS_SetInfo(ADIO_File fd, MPI_Info users_info, int *error_code);
#endif

#ifdef ROMIO_PVFS
extern struct ADIOI_Fns_struct ADIO_PVFS_operations;

void ADIOI_PVFS_Open(ADIO_File fd, int *error_code);
void ADIOI_PVFS_Close(ADIO_File fd, int *error_code);
void ADIOI_PVFS_ReadContig(ADIO_File fd, void *buf, int count, 
                      MPI_Datatype datatype, int file_ptr_type,
                     ADIO_Offset offset, ADIO_Status *status, int
		     *error_code);
void ADIOI_PVFS_WriteContig(ADIO_File fd, void *buf, int count, 
                      MPI_Datatype datatype, int file_ptr_type,
                      ADIO_Offset offset, ADIO_Status *status, int
		      *error_code);   
void ADIOI_PVFS_IwriteContig(ADIO_File fd, void *buf, int count, 
                      MPI_Datatype datatype, int file_ptr_type,
                      ADIO_Offset offset, ADIO_Request *request, int
		      *error_code);   
void ADIOI_PVFS_IreadContig(ADIO_File fd, void *buf, int count, 
                      MPI_Datatype datatype, int file_ptr_type,
                      ADIO_Offset offset, ADIO_Request *request, int
		      *error_code);   
int ADIOI_PVFS_ReadDone(ADIO_Request *request, ADIO_Status *status, int
		       *error_code);
int ADIOI_PVFS_WriteDone(ADIO_Request *request, ADIO_Status *status, int
		       *error_code);
void ADIOI_PVFS_ReadComplete(ADIO_Request *request, ADIO_Status *status, int
		       *error_code); 
void ADIOI_PVFS_WriteComplete(ADIO_Request *request, ADIO_Status *status,
			int *error_code); 
void ADIOI_PVFS_Fcntl(ADIO_File fd, int flag, ADIO_Fcntl_t *fcntl_struct, int
		*error_code); 
void ADIOI_PVFS_WriteStrided(ADIO_File fd, void *buf, int count,
		       MPI_Datatype datatype, int file_ptr_type,
		       ADIO_Offset offset, ADIO_Status *status, int
		       *error_code);
void ADIOI_PVFS_ReadStrided(ADIO_File fd, void *buf, int count,
		       MPI_Datatype datatype, int file_ptr_type,
		       ADIO_Offset offset, ADIO_Status *status, int
		       *error_code);
void ADIOI_PVFS_WriteStridedColl(ADIO_File fd, void *buf, int count,
		       MPI_Datatype datatype, int file_ptr_type,
		       ADIO_Offset offset, ADIO_Status *status, int
		       *error_code);
void ADIOI_PVFS_ReadStridedColl(ADIO_File fd, void *buf, int count,
		       MPI_Datatype datatype, int file_ptr_type,
		       ADIO_Offset offset, ADIO_Status *status, int
		       *error_code);
void ADIOI_PVFS_IreadStrided(ADIO_File fd, void *buf, int count,
		       MPI_Datatype datatype, int file_ptr_type,
		       ADIO_Offset offset, ADIO_Request *request, int
		       *error_code);
void ADIOI_PVFS_IwriteStrided(ADIO_File fd, void *buf, int count,
		       MPI_Datatype datatype, int file_ptr_type,
		       ADIO_Offset offset, ADIO_Request *request, int
		       *error_code);
void ADIOI_PVFS_Flush(ADIO_File fd, int *error_code);
void ADIOI_PVFS_Delete(char *filename, int *error_code);
void ADIOI_PVFS_Resize(ADIO_File fd, ADIO_Offset size, int *error_code);
ADIO_Offset ADIOI_PVFS_SeekIndividual(ADIO_File fd, ADIO_Offset offset, 
                       int whence, int *error_code);
void ADIOI_PVFS_SetInfo(ADIO_File fd, MPI_Info users_info, int *error_code);
#endif

#ifdef ROMIO_TESTFS
extern struct ADIOI_Fns_struct ADIO_TESTFS_operations;

/* prototypes are in adio/ad_testfs/ad_testfs.h */
#endif

#endif
