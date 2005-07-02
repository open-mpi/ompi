/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   $Id: ad_pvfs_hints.c,v 1.5 2002/10/24 17:00:57 gropp Exp $    
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "ad_pvfs.h"

void ADIOI_PVFS_SetInfo(ADIO_File fd, MPI_Info users_info, int *error_code)
{
    char *value;
    int flag, tmp_val, str_factor=-1, str_unit=-1, start_iodev=-1; 

    if (!(fd->info)) {
	/* This must be part of the open call. can set striping parameters 
           if necessary. */ 
	MPI_Info_create(&(fd->info));
	MPI_Info_set(fd->info, "romio_pvfs_listio_read", "disable");
	MPI_Info_set(fd->info, "romio_pvfs_listio_write", "disable");
	fd->hints->fs_hints.pvfs.listio_read = ADIOI_HINT_DISABLE;
	fd->hints->fs_hints.pvfs.listio_write = ADIOI_HINT_DISABLE;
	
	/* has user specified any pvfs-specific hints (striping params, listio)
           and do they have the same value on all processes? */
	if (users_info != MPI_INFO_NULL) {
	    value = (char *) ADIOI_Malloc((MPI_MAX_INFO_VAL+1)*sizeof(char));

	    MPI_Info_get(users_info, "striping_factor", MPI_MAX_INFO_VAL, 
			 value, &flag);
	    if (flag) {
		str_factor=atoi(value);
		tmp_val = str_factor;
		MPI_Bcast(&tmp_val, 1, MPI_INT, 0, fd->comm);
		if (tmp_val != str_factor) {
		    FPRINTF(stderr, "ADIOI_PVFS_SetInfo: the value for key \"striping_factor\" must be the same on all processes\n");
		    MPI_Abort(MPI_COMM_WORLD, 1);
		}
		else MPI_Info_set(fd->info, "striping_factor", value);
	    }

	    MPI_Info_get(users_info, "striping_unit", MPI_MAX_INFO_VAL, 
			 value, &flag);
	    if (flag) {
		str_unit=atoi(value);
		tmp_val = str_unit;
		MPI_Bcast(&tmp_val, 1, MPI_INT, 0, fd->comm);
		if (tmp_val != str_unit) {
		    FPRINTF(stderr, "ADIOI_PVFS_SetInfo: the value for key \"striping_unit\" must be the same on all processes\n");
		    MPI_Abort(MPI_COMM_WORLD, 1);
		}
		else MPI_Info_set(fd->info, "striping_unit", value);
	    }

	    MPI_Info_get(users_info, "start_iodevice", MPI_MAX_INFO_VAL, 
			 value, &flag);
	    if (flag) {
		start_iodev=atoi(value);
		tmp_val = start_iodev;
		MPI_Bcast(&tmp_val, 1, MPI_INT, 0, fd->comm);
		if (tmp_val != start_iodev) {
		    FPRINTF(stderr, "ADIOI_PVFS_SetInfo: the value for key \"start_iodevice\" must be the same on all processes\n");
		    MPI_Abort(MPI_COMM_WORLD, 1);
		}
		else MPI_Info_set(fd->info, "start_iodevice", value);
	    }

	    MPI_Info_get(users_info, "romio_pvfs_listio_read", MPI_MAX_INFO_VAL,
			    value, &flag);
	    if (flag) {
		    if ( !strcmp(value, "enable") || !strcmp(value, "ENABLE")) 
		    {
			    MPI_Info_set(fd->info, "romio_pvfs_listio_read", value);
			    fd->hints->fs_hints.pvfs.listio_read = ADIOI_HINT_ENABLE;
		    } else if ( !strcmp(value, "disable") || !strcmp(value, "DISABLE")) 
		    {
			    MPI_Info_set(fd->info , "romio_pvfs_listio_read", value);
			    fd->hints->fs_hints.pvfs.listio_read = ADIOI_HINT_DISABLE;
		    }
		    else if ( !strcmp(value, "automatic") || !strcmp(value, "AUTOMATIC")) 
		    {
			    MPI_Info_set(fd->info, "romio_pvfs_listio_read", value);
			    fd->hints->fs_hints.pvfs.listio_read = ADIOI_HINT_AUTO;
		    }
		    tmp_val = fd->hints->fs_hints.pvfs.listio_read;
		    MPI_Bcast(&tmp_val, 1, MPI_INT, 0, fd->comm);
		    if (tmp_val != fd->hints->fs_hints.pvfs.listio_read) {
			    FPRINTF(stderr, "ADIOI_PVFS_SetInfo: the value for key \"romio_pvfs_listio_read\" must be the same on all processes\n");
			    MPI_Abort(MPI_COMM_WORLD, 1);
		    }
	    }
	    MPI_Info_get(users_info, "romio_pvfs_listio_write", MPI_MAX_INFO_VAL,
			    value, &flag);
	    if (flag) {
		    if ( !strcmp(value, "enable") || !strcmp(value, "ENABLE")) 
		    {
			    MPI_Info_set(fd->info, "romio_pvfs_listio_write", value);
			    fd->hints->fs_hints.pvfs.listio_write = ADIOI_HINT_ENABLE;
		    } else if ( !strcmp(value, "disable") || !strcmp(value, "DISABLE")) 
		    {
			    MPI_Info_set(fd->info, "romio_pvfs_listio_write", value);
			    fd->hints->fs_hints.pvfs.listio_write = ADIOI_HINT_DISABLE;
		    }
		    else if ( !strcmp(value, "automatic") || !strcmp(value, "AUTOMATIC")) 
		    {
			    MPI_Info_set(fd->info, "romio_pvfs_listio_write", value);
			    fd->hints->fs_hints.pvfs.listio_write = ADIOI_HINT_AUTO;
		    }
		    tmp_val = fd->hints->fs_hints.pvfs.listio_write;
		    MPI_Bcast(&tmp_val, 1, MPI_INT, 0, fd->comm);
		    if (tmp_val != fd->hints->fs_hints.pvfs.listio_write) {
			    FPRINTF(stderr, "ADIOI_PVFS_SetInfo: the value for key \"romio_pvfs_listio_write\" must be the same on all processes\n");
			    MPI_Abort(MPI_COMM_WORLD, 1);
		    }
	    }		    
	    ADIOI_Free(value);
	}
    }	

    /* set the values for collective I/O and data sieving parameters */
    ADIOI_GEN_SetInfo(fd, users_info, error_code);

    *error_code = MPI_SUCCESS;
}
