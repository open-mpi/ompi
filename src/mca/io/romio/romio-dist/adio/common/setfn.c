/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   $Id: setfn.c,v 1.12 2002/10/24 17:01:15 gropp Exp $    
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "adio.h"

void ADIOI_SetFunctions(ADIO_File fd)
{
    /* NOTE: soon we want to get rid of this malloc and instead just point
     * straight to the appropriate table
     */
    fd->fns = (ADIOI_Fns *) ADIOI_Malloc(sizeof(ADIOI_Fns));
    switch(fd->file_system) {
    case ADIO_PFS:
#ifdef PFS	
	*(fd->fns) = ADIO_PFS_operations;
#else
	FPRINTF(stderr, "ADIOI_SetFunctions: ROMIO has not been configured to use the PFS file system\n");
        MPI_Abort(MPI_COMM_WORLD, 1);
#endif
	break;

    case ADIO_PIOFS:
#ifdef PIOFS	
	*(fd->fns) = ADIO_PIOFS_operations;
#else
	FPRINTF(stderr, "ADIOI_SetFunctions: ROMIO has not been configured to use the PIOFS file system\n");
        MPI_Abort(MPI_COMM_WORLD, 1);
#endif
	break;

    case ADIO_UFS:
#ifdef UFS	
	*(fd->fns) = ADIO_UFS_operations;
#else
	FPRINTF(stderr, "ADIOI_SetFunctions: ROMIO has not been configured to use the UFS file system\n");
        MPI_Abort(MPI_COMM_WORLD, 1);
#endif
	break;

    case ADIO_NTFS:
#ifdef ROMIO_NTFS
	*(fd->fns) = ADIO_NTFS_operations;
#else
	FPRINTF(stderr, "ADIOI_SetFunctions: ROMIO has not been configured to use the NTFS file system\n");
        MPI_Abort(MPI_COMM_WORLD, 1);
#endif
	break;

    case ADIO_NFS:
#ifdef NFS	
	*(fd->fns) = ADIO_NFS_operations;
#else
	FPRINTF(stderr, "ADIOI_SetFunctions: ROMIO has not been configured to use the NFS file system\n");
        MPI_Abort(MPI_COMM_WORLD, 1);
#endif
	break;

    case ADIO_HFS:
#ifdef HFS	
	*(fd->fns) = ADIO_HFS_operations;
#else
	FPRINTF(stderr, "ADIOI_SetFunctions: ROMIO has not been configured to use the HFS file system\n");
        MPI_Abort(MPI_COMM_WORLD, 1);
#endif
	break;

    case ADIO_XFS:
#ifdef XFS	
	*(fd->fns) = ADIO_XFS_operations;
#else
	FPRINTF(stderr, "ADIOI_SetFunctions: ROMIO has not been configured to use the XFS file system\n");
        MPI_Abort(MPI_COMM_WORLD, 1);
#endif
	break;

    case ADIO_SFS:
#ifdef SFS	
	*(fd->fns) = ADIO_SFS_operations;
#else
	FPRINTF(stderr, "ADIOI_SetFunctions: ROMIO has not been configured to use the SFS file system\n");
        MPI_Abort(MPI_COMM_WORLD, 1);
#endif
	break;

    case ADIO_PVFS:
#ifdef ROMIO_PVFS
	*(fd->fns) = ADIO_PVFS_operations;
#else
	FPRINTF(stderr, "ADIOI_SetFunctions: ROMIO has not been configured to use the PVFS file system\n");
        MPI_Abort(MPI_COMM_WORLD, 1);
#endif
	break;

    case ADIO_TESTFS:
#ifdef ROMIO_TESTFS
	*(fd->fns) = ADIO_TESTFS_operations;
#else
	FPRINTF(stderr, "ADIOI_SetFunctions: ROMIO has not been configured to use the TESTFS file system\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
#endif
	break;

    default:
	FPRINTF(stderr, "ADIOI_SetFunctions: Unsupported file system type\n");
        MPI_Abort(MPI_COMM_WORLD, 1);
	break;
    }
}
