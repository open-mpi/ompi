/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   $Id: ad_open.c,v 1.11 2002/10/24 17:01:12 gropp Exp $    
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "adio.h"
#include "adio_extern.h"
#include "adio_cb_config_list.h"
#ifdef MPISGI
#include "mpisgi2.h"
#endif

ADIO_File ADIO_Open(MPI_Comm orig_comm,
		    MPI_Comm comm, char *filename, int file_system,
		    int access_mode, ADIO_Offset disp, MPI_Datatype etype, 
		    MPI_Datatype filetype, int iomode,
                    MPI_Info info, int perm, int *error_code)
{
    ADIO_File fd;
    ADIO_cb_name_array array;
    int orig_amode, err, rank, procs;
    char *value;
#ifndef PRINT_ERR_MSG
    static char myname[] = "ADIO_OPEN";
#endif

    int rank_ct;
    int *tmp_ranklist;

    *error_code = MPI_SUCCESS;

    fd = (ADIO_File) ADIOI_Malloc(sizeof(struct ADIOI_FileD));
    if (fd == NULL) {
	/* NEED TO HANDLE ENOMEM ERRORS */
    }

    fd->cookie = ADIOI_FILE_COOKIE;
    fd->fp_ind = disp;
    fd->fp_sys_posn = 0;
    fd->comm = comm;       /* dup'ed in MPI_File_open */
    fd->filename = strdup(filename);
    fd->file_system = file_system;
    fd->disp = disp;
    fd->split_coll_count = 0;
    fd->shared_fp_fd = ADIO_FILE_NULL;
    fd->atomicity = 0;

    fd->etype = etype;          /* MPI_BYTE by default */
    fd->filetype = filetype;    /* MPI_BYTE by default */
    fd->etype_size = 1;  /* default etype is MPI_BYTE */

    fd->perm = perm;

    fd->iomode = iomode;
    fd->async_count = 0;

    fd->err_handler = ADIOI_DFLT_ERR_HANDLER;

/* set I/O function pointers */
    ADIOI_SetFunctions(fd);

/* create and initialize info object */
    fd->hints = (ADIOI_Hints *)ADIOI_Malloc(sizeof(struct ADIOI_Hints_struct));
    if (fd->hints == NULL) {
	/* NEED TO HANDLE ENOMEM ERRORS */
    }
    fd->hints->cb_config_list = NULL;
    fd->hints->ranklist = NULL;
    fd->hints->initialized = 0;
    fd->info = MPI_INFO_NULL;
    ADIO_SetInfo(fd, info, &err);

/* gather the processor name array if we don't already have it */

/* this has to be done here so that we can cache the name array in both
 * the dup'd communicator (in case we want it later) and the original
 * communicator
 */
    ADIOI_cb_gather_name_array(orig_comm, comm, &array);

/* parse the cb_config_list and create a rank map on rank 0 */
    MPI_Comm_rank(comm, &rank);
    if (rank == 0) {
	MPI_Comm_size(comm, &procs);
	tmp_ranklist = (int *) ADIOI_Malloc(sizeof(int) * procs);
	if (tmp_ranklist == NULL) {
	    /* NEED TO HANDLE ENOMEM ERRORS */
	}

	rank_ct = ADIOI_cb_config_list_parse(fd->hints->cb_config_list, 
					     array, tmp_ranklist,
					     fd->hints->cb_nodes);

	/* store the ranklist using the minimum amount of memory */
	if (rank_ct > 0) {
	    fd->hints->ranklist = (int *) ADIOI_Malloc(sizeof(int) * rank_ct);
	    memcpy(fd->hints->ranklist, tmp_ranklist, sizeof(int) * rank_ct);
	}
	ADIOI_Free(tmp_ranklist);
	fd->hints->cb_nodes = rank_ct;
	/* TEMPORARY -- REMOVE WHEN NO LONGER UPDATING INFO FOR FS-INDEP. */
	value = (char *) ADIOI_Malloc((MPI_MAX_INFO_VAL+1)*sizeof(char));
	sprintf(value, "%d", rank_ct);
	MPI_Info_set(fd->info, "cb_nodes", value);
	ADIOI_Free(value);
    }
/* bcast the rank map (could do an allgather above and avoid
 * this...would that really be any better?)
 */
    ADIOI_cb_bcast_rank_map(fd);
    if (fd->hints->cb_nodes <= 0) {
#ifdef PRINT_ERR_MSG
	*error_code = MPI_ERR_UNKNOWN;
#else
	*error_code = MPIR_Err_setmsg(MPI_ERR_IO, MPIR_ADIO_ERROR, myname,
				      "Open Error", "%s", 
				      "No aggregators match");
	ADIOI_Error(MPI_FILE_NULL, *error_code, myname);
#endif
	fd = ADIO_FILE_NULL;
	return fd;
    }

/* For writing with data sieving, a read-modify-write is needed. If 
   the file is opened for write_only, the read will fail. Therefore,
   if write_only, open the file as read_write, but record it as write_only
   in fd, so that get_amode returns the right answer. */

    orig_amode = access_mode;
    if (access_mode & ADIO_WRONLY) {
	access_mode = access_mode ^ ADIO_WRONLY;
	access_mode = access_mode | ADIO_RDWR;
    }
    fd->access_mode = access_mode;

    (*(fd->fns->ADIOI_xxx_Open))(fd, error_code);

    fd->access_mode = orig_amode;

    /* if error, may be it was due to the change in amode above. 
       therefore, reopen with access mode provided by the user.*/ 
    if (*error_code != MPI_SUCCESS) 
        (*(fd->fns->ADIOI_xxx_Open))(fd, error_code);

    /* if error, free and set fd to NULL */
    if (*error_code != MPI_SUCCESS) {
	ADIOI_Free(fd->fns);
	MPI_Comm_free(&(fd->comm));
	free(fd->filename);
	MPI_Info_free(&(fd->info));
	ADIOI_Free(fd);
	fd = ADIO_FILE_NULL;
    }

    return fd;
}
