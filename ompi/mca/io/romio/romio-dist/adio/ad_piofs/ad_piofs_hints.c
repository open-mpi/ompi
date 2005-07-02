/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   $Id: ad_piofs_hints.c,v 1.4 2002/10/24 17:00:55 gropp Exp $    
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "ad_piofs.h"

void ADIOI_PIOFS_SetInfo(ADIO_File fd, MPI_Info users_info, int *error_code)
{
    piofs_create_t piofs_create;
    piofs_statfs_t piofs_statfs;
    char *value, *path, *slash;
    int flag, tmp_val, str_factor=-1, str_unit=-1, start_iodev=-1;
    int err, myrank, perm, old_mask, nioservers;

    if (!(fd->info)) {
	/* This must be part of the open call. can set striping parameters 
           if necessary. */ 
	MPI_Info_create(&(fd->info));
	
	/* has user specified striping parameters 
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
		    FPRINTF(stderr, "ADIOI_PIOFS_SetInfo: the value for key \"striping_factor\" must be the same on all processes\n");
		    MPI_Abort(MPI_COMM_WORLD, 1);
		}
	    }

	    MPI_Info_get(users_info, "striping_unit", MPI_MAX_INFO_VAL, 
			 value, &flag);
	    if (flag) {
		str_unit=atoi(value);
		tmp_val = str_unit;
		MPI_Bcast(&tmp_val, 1, MPI_INT, 0, fd->comm);
		if (tmp_val != str_unit) {
		    FPRINTF(stderr, "ADIOI_PIOFS_SetInfo: the value for key \"striping_unit\" must be the same on all processes\n");
		    MPI_Abort(MPI_COMM_WORLD, 1);
		}
	    }

	    MPI_Info_get(users_info, "start_iodevice", MPI_MAX_INFO_VAL, 
			 value, &flag);
	    if (flag) {
		start_iodev=atoi(value);
		tmp_val = start_iodev;
		MPI_Bcast(&tmp_val, 1, MPI_INT, 0, fd->comm);
		if (tmp_val != start_iodev) {
		    FPRINTF(stderr, "ADIOI_PIOFS_SetInfo: the value for key \"start_iodevice\" must be the same on all processes\n");
		    MPI_Abort(MPI_COMM_WORLD, 1);
		}
	    }

	    ADIOI_Free(value);

         /* if user has specified striping info, process 0 tries to set it */
	    if ((str_factor > 0) || (str_unit > 0) || (start_iodev >= 0)) {
		MPI_Comm_rank(fd->comm, &myrank);
		if (!myrank) {
		    if (fd->perm == ADIO_PERM_NULL) {
			old_mask = umask(022);
			umask(old_mask);
			perm = old_mask ^ 0666;
		    }
		    else perm = fd->perm;

		    /* to find out the number of I/O servers, I need
                       the path to the directory containing the file */

		    path = strdup(fd->filename);
		    slash = strrchr(path, '/');
		    if (!slash) strcpy(path, ".");
		    else {
			if (slash == path) *(path + 1) = '\0';
			else *slash = '\0';
		    }
		    strcpy(piofs_statfs.name, path);
		    err = piofsioctl(0, PIOFS_STATFS, &piofs_statfs);
		    nioservers = (err) ? -1 : piofs_statfs.f_nodes;

		    free(path);

		    str_factor = ADIOI_MIN(nioservers, str_factor);
		    if (start_iodev >= nioservers) start_iodev = -1;

		    strcpy(piofs_create.name, fd->filename);
		    piofs_create.bsu = (str_unit > 0) ? str_unit : -1;
		    piofs_create.cells = (str_factor > 0) ? str_factor : -1;
		    piofs_create.permissions = perm;
		    piofs_create.base_node = (start_iodev >= 0) ? 
                                                     start_iodev : -1;
		    piofs_create.flags = 0;

		    err = piofsioctl(0, PIOFS_CREATE, &piofs_create);
		}
		MPI_Barrier(fd->comm);
	    }
	}
    }	
	
    /* set the values for collective I/O and data sieving parameters */
    ADIOI_GEN_SetInfo(fd, users_info, error_code);

    *error_code = MPI_SUCCESS;
}
