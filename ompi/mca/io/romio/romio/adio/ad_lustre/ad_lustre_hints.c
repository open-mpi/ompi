/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 *
 *   Copyright (C) 2007 Oak Ridge National Laboratory
 */

#include "ad_lustre.h"
#include "adio_extern.h"

void ADIOI_LUSTRE_SetInfo(ADIO_File fd, MPI_Info users_info, int *error_code)
{
    char *value, *value_in_fd;
    int flag, tmp_val[3], str_factor=-1, str_unit=0, start_iodev=-1;
    struct lov_user_md lum = { 0 };
    int err, myrank, fd_sys, perm, amode, old_mask;

    value = (char *) ADIOI_Malloc((MPI_MAX_INFO_VAL+1)*sizeof(char));
    if ( (fd->info) == MPI_INFO_NULL) {
	/* This must be part of the open call. can set striping parameters 
           if necessary. */ 
	MPI_Info_create(&(fd->info));

	MPI_Info_set(fd->info, "direct_read", "false");
	MPI_Info_set(fd->info, "direct_write", "false");
	fd->direct_read = fd->direct_write = 0;
	
	/* has user specified striping or server buffering parameters 
           and do they have the same value on all processes? */
	if (users_info != MPI_INFO_NULL) {
	    MPI_Info_get(users_info, "striping_unit", MPI_MAX_INFO_VAL, 
			 value, &flag);
	    if (flag) 
		str_unit=atoi(value);

	    MPI_Info_get(users_info, "striping_factor", MPI_MAX_INFO_VAL, 
			 value, &flag);
	    if (flag) 
		str_factor=atoi(value);

	    MPI_Info_get(users_info, "start_iodevice", MPI_MAX_INFO_VAL, 
			 value, &flag);
	    if (flag) 
		start_iodev=atoi(value);

	    MPI_Info_get(users_info, "direct_read", MPI_MAX_INFO_VAL, 
			     value, &flag);
	    if (flag && (!strcmp(value, "true") || !strcmp(value, "TRUE"))) {
		MPI_Info_set(fd->info, "direct_read", "true");
		fd->direct_read = 1;
	    }

	    MPI_Info_get(users_info, "direct_write", MPI_MAX_INFO_VAL, 
			     value, &flag);
	    if (flag && (!strcmp(value, "true") || !strcmp(value, "TRUE"))) {
		MPI_Info_set(fd->info, "direct_write", "true");
		fd->direct_write = 1;
	    }
	}

	MPI_Comm_rank(fd->comm, &myrank);
	if (myrank == 0) {
	    tmp_val[0] = str_factor;
	    tmp_val[1] = str_unit;
	    tmp_val[2] = start_iodev;
	}
	MPI_Bcast(tmp_val, 3, MPI_INT, 0, fd->comm);

	if (tmp_val[0] != str_factor 
		|| tmp_val[1] != str_unit 
		|| tmp_val[2] != start_iodev) {
	    FPRINTF(stderr, "ADIOI_LUSTRE_SetInfo: All keys"
		    "-striping_factor:striping_unit:start_iodevice "
		    "need to be identical across all processes\n");
	    MPI_Abort(MPI_COMM_WORLD, 1);
       	} else if ((str_factor > 0) || (str_unit > 0) || (start_iodev >= 0)) {
	     /* if user has specified striping info, process 0 tries to set it */
	    if (!myrank) {
		if (fd->perm == ADIO_PERM_NULL) {
		    old_mask = umask(022);
		    umask(old_mask);
		    perm = old_mask ^ 0666;
		}
		else perm = fd->perm;

		amode = 0;
		if (fd->access_mode & ADIO_CREATE)
		    amode = amode | O_CREAT;
		if (fd->access_mode & ADIO_RDONLY)
		    amode = amode | O_RDONLY;
		if (fd->access_mode & ADIO_WRONLY)
		    amode = amode | O_WRONLY;
		if (fd->access_mode & ADIO_RDWR)
		    amode = amode | O_RDWR;
		if (fd->access_mode & ADIO_EXCL)
		    amode = amode | O_EXCL;

		/* we need to create file so ensure this is set */
		amode = amode | O_LOV_DELAY_CREATE | O_CREAT;

		fd_sys = open(fd->filename, amode, perm);
		if (fd_sys == -1) { 
		    if (errno != EEXIST) 
			fprintf(stderr, 
				"Failure to open file %s %d %d\n",strerror(errno), amode, perm);
		} else {
		    lum.lmm_magic = LOV_USER_MAGIC;
		    lum.lmm_pattern = 0;
		    lum.lmm_stripe_size = str_unit;
		    lum.lmm_stripe_count = str_factor;
		    lum.lmm_stripe_offset = start_iodev;

		    err = ioctl(fd_sys, LL_IOC_LOV_SETSTRIPE, &lum);
		    if (err == -1 && errno != EEXIST) { 
			fprintf(stderr, "Failure to set stripe info %s \n", strerror(errno));
		    }
		    close(fd_sys);
	       }
	    } /* End of striping parameters validation */
	}
	
	MPI_Barrier(fd->comm);
	/* set the values for collective I/O and data sieving parameters */
	ADIOI_GEN_SetInfo(fd, users_info, error_code);
    } else {
	/* The file has been opened previously and fd->fd_sys is a valid
           file descriptor. cannot set striping parameters now. */
	
	/* set the values for collective I/O and data sieving parameters */
	ADIOI_GEN_SetInfo(fd, users_info, error_code);
    }
 
    if (ADIOI_Direct_read) fd->direct_read = 1;
    if (ADIOI_Direct_write) fd->direct_write = 1;

    ADIOI_Free(value);

    *error_code = MPI_SUCCESS;
}
