/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/* 
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include <stdlib.h>
#include "ad_pvfs2.h"

#include "hint_fns.h"

void ADIOI_PVFS2_SetInfo(ADIO_File fd, MPI_Info users_info, int *error_code)
{
    char *value;
    int flag, tmp_value;
    static char myname[] = "ADIOI_PVFS_SETINFO";

    if ((fd->info) == MPI_INFO_NULL) {
	/* part of the open call */
	MPI_Info_create(&(fd->info));
	ADIOI_Info_set(fd->info, "romio_pvfs2_debugmask", "0");
	fd->hints->fs_hints.pvfs2.debugmask = 0;

	ADIOI_Info_set(fd->info, "striping_factor", "0");
	fd->hints->striping_factor = 0;

	ADIOI_Info_set(fd->info, "striping_unit", "0");
	fd->hints->striping_unit = 0;

	/* disable the aggressive strided optimizations by default */
        ADIOI_Info_set(fd->info, "romio_pvfs2_posix_read", "disable");
        ADIOI_Info_set(fd->info, "romio_pvfs2_posix_write", "disable");
        fd->hints->fs_hints.pvfs2.posix_read = ADIOI_HINT_DISABLE;
        fd->hints->fs_hints.pvfs2.posix_write = ADIOI_HINT_DISABLE;

        ADIOI_Info_set(fd->info, "romio_pvfs2_dtype_read", "disable");
        ADIOI_Info_set(fd->info, "romio_pvfs2_dtype_write", "disable");
        fd->hints->fs_hints.pvfs2.dtype_read = ADIOI_HINT_DISABLE;
        fd->hints->fs_hints.pvfs2.dtype_write = ADIOI_HINT_DISABLE;

        ADIOI_Info_set(fd->info, "romio_pvfs2_listio_read", "disable");
        ADIOI_Info_set(fd->info, "romio_pvfs2_listio_write", "disable");
        fd->hints->fs_hints.pvfs2.listio_read = ADIOI_HINT_DISABLE;
        fd->hints->fs_hints.pvfs2.listio_write = ADIOI_HINT_DISABLE;

	
	/* any user-provided hints? */
	if (users_info != MPI_INFO_NULL) {
	    value = (char *) ADIOI_Malloc( (MPI_MAX_INFO_VAL+1)*sizeof(char));
	    /* pvfs2 debugging */
	    ADIOI_Info_get(users_info, "romio_pvfs2_debugmask", 
		    MPI_MAX_INFO_VAL, value, &flag);
	    if (flag) {
		tmp_value = fd->hints->fs_hints.pvfs2.debugmask = 
		    PVFS_debug_eventlog_to_mask(value);

		MPI_Bcast(&tmp_value, 1, MPI_INT, 0, fd->comm);
		/* --BEGIN ERROR HANDLING-- */
		if (tmp_value != fd->hints->fs_hints.pvfs2.debugmask) {
		    MPIO_ERR_CREATE_CODE_INFO_NOT_SAME(myname,
						       "romio_pvfs2_debugmask",
						       error_code);
		    return;
		}
		/* --END ERROR HANDLING-- */
		
		ADIOI_Info_set(fd->info, "romio_pvfs2_debugmask", value);
	    }

	    /* the striping factor */
	    ADIOI_Info_check_and_install_int(fd, users_info, "striping_factor",
		    &(fd->hints->striping_factor), myname, error_code);


	    /* the striping unit */
	    ADIOI_Info_check_and_install_int(fd, users_info, "striping_unit",
		    &(fd->hints->striping_unit), myname, error_code);

	    /* distribution name */
	    ADIOI_Info_get(users_info, "romio_pvfs2_distribution_name",
		    MPI_MAX_INFO_VAL, value, &flag);
	    if (flag) {
	    }

	    /* POSIX read */
	    ADIOI_Info_check_and_install_enabled(fd, users_info, "romio_pvfs2_posix_read",
		    &(fd->hints->fs_hints.pvfs2.posix_read), myname, error_code);

            /* POSIX write */
	    ADIOI_Info_check_and_install_enabled(fd, users_info, "romio_pvfs2_posix_write",
		    &(fd->hints->fs_hints.pvfs2.posix_write), myname, error_code);

	    /* Datatype read */
	    ADIOI_Info_check_and_install_enabled(fd, users_info, "romio_pvfs2_dtype_read",
		    &(fd->hints->fs_hints.pvfs2.dtype_read), myname, error_code);

            /* Datatype write */
	    ADIOI_Info_check_and_install_enabled(fd, users_info, "romio_pvfs2_dtype_write",
		    &(fd->hints->fs_hints.pvfs2.dtype_write), myname, error_code);

	    /* Listio read */
	    ADIOI_Info_check_and_install_enabled(fd, users_info, "romio_pvfs2_listio_read",
		    &(fd->hints->fs_hints.pvfs2.listio_read), myname, error_code);

            /* Datatype write */
	    ADIOI_Info_check_and_install_enabled(fd, users_info, "romio_pvfs2_listio_write",
		    &(fd->hints->fs_hints.pvfs2.listio_write), myname, error_code);
            ADIOI_Free(value);
	}
    }
    /* set the values for collective I/O and data sieving parameters */
    ADIOI_GEN_SetInfo(fd, users_info, error_code);

    *error_code = MPI_SUCCESS;
}

/*
 * vim: ts=8 sts=4 sw=4 noexpandtab
 */
