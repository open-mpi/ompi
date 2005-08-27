/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   $Id: ad_pfs_fcntl.c,v 1.5 2002/10/24 17:00:51 gropp Exp $    
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "ad_pfs.h"
#include "adio_extern.h"

void ADIOI_PFS_Fcntl(ADIO_File fd, int flag, ADIO_Fcntl_t *fcntl_struct, int *error_code)
{
    MPI_Datatype copy_etype, copy_filetype;
    int combiner, i, j, k, filetype_is_contig, err;
    ADIOI_Flatlist_node *flat_file;
    int iomod, np_total, np_comm;
#ifndef PRINT_ERR_MSG
    static char myname[] = "ADIOI_PFS_FCNTL";
#endif

    switch(flag) {
    case ADIO_FCNTL_SET_VIEW:
        /* free copies of old etypes and filetypes and delete flattened 
           version of filetype if necessary */

	MPI_Type_get_envelope(fd->etype, &i, &j, &k, &combiner);
	if (combiner != MPI_COMBINER_NAMED) MPI_Type_free(&(fd->etype));

	ADIOI_Datatype_iscontig(fd->filetype, &filetype_is_contig);
	if (!filetype_is_contig) ADIOI_Delete_flattened(fd->filetype);

	MPI_Type_get_envelope(fd->filetype, &i, &j, &k, &combiner);
	if (combiner != MPI_COMBINER_NAMED) MPI_Type_free(&(fd->filetype));

	/* set new info */
	ADIO_SetInfo(fd, fcntl_struct->info, &err);

        /* set new etypes and filetypes */

	MPI_Type_get_envelope(fcntl_struct->etype, &i, &j, &k, &combiner);
	if (combiner == MPI_COMBINER_NAMED) fd->etype = fcntl_struct->etype;
	else {
	    MPI_Type_contiguous(1, fcntl_struct->etype, &copy_etype);
	    MPI_Type_commit(&copy_etype);
	    fd->etype = copy_etype;
	}
	MPI_Type_get_envelope(fcntl_struct->filetype, &i, &j, &k, &combiner);
	if (combiner == MPI_COMBINER_NAMED) 
	    fd->filetype = fcntl_struct->filetype;
	else {
	    MPI_Type_contiguous(1, fcntl_struct->filetype, &copy_filetype);
	    MPI_Type_commit(&copy_filetype);
	    fd->filetype = copy_filetype;
	    ADIOI_Flatten_datatype(fd->filetype);
            /* this function will not flatten the filetype if it turns out
               to be all contiguous. */
	}

	MPI_Type_size(fd->etype, &(fd->etype_size));
	fd->disp = fcntl_struct->disp;

        /* reset MPI-IO file pointer to point to the first byte that can
           be accessed in this view. */

        ADIOI_Datatype_iscontig(fd->filetype, &filetype_is_contig);
	if (filetype_is_contig) fd->fp_ind = fcntl_struct->disp;
	else {
	    flat_file = ADIOI_Flatlist;
	    while (flat_file->type != fd->filetype) 
		flat_file = flat_file->next;
	    for (i=0; i<flat_file->count; i++) {
		if (flat_file->blocklens[i]) {
		    fd->fp_ind = fcntl_struct->disp + flat_file->indices[i];
		    break;
		}
	    }
	}
	*error_code = MPI_SUCCESS;
	break;

    case ADIO_FCNTL_GET_FSIZE:
	if (!(fd->atomicity)) {
          /* in M_ASYNC mode, all processes are not aware of changes 
             in file size (although the manual says otherwise). Therefore, 
             temporarily change to M_UNIX and then change 
             back to M_ASYNC.*/ 
	    MPI_Comm_size(MPI_COMM_WORLD, &np_total);
	    MPI_Comm_size(fd->comm, &np_comm);
	    if (np_total == np_comm) {
		err = _setiomode(fd->fd_sys, M_UNIX);
		err = _setiomode(fd->fd_sys, M_ASYNC);
	    }
            /* else it is M_UNIX anyway, so no problem */
	}
	fcntl_struct->fsize = lseek(fd->fd_sys, 0, SEEK_END);
	if (fd->fp_sys_posn != -1) 
	    lseek(fd->fd_sys, fd->fp_sys_posn, SEEK_SET);
	*error_code = MPI_SUCCESS;
	break;

    case ADIO_FCNTL_SET_DISKSPACE:
	err = _lsize(fd->fd_sys, fcntl_struct->diskspace, SEEK_SET);
#ifdef PRINT_ERR_MSG
	*error_code = (err == -1) ? MPI_ERR_UNKNOWN : MPI_SUCCESS ;
#else
	if (err == -1) {
	    *error_code = MPIR_Err_setmsg(MPI_ERR_IO, MPIR_ADIO_ERROR,
			      myname, "I/O Error", "%s", strerror(errno));
	    ADIOI_Error(fd, *error_code, myname);	    
	}
	else *error_code = MPI_SUCCESS;
#endif
	break;

    case ADIO_FCNTL_SET_IOMODE:
        /* for implementing PFS I/O modes. will not occur in MPI-IO
           implementation.*/
	if (fd->iomode != fcntl_struct->iomode) {
	    fd->iomode = fcntl_struct->iomode;
	    setiomode(fd->fd_sys, iomode);
           /* for some unknown reason, the compiler gives a warning here */
	}
	*error_code = MPI_SUCCESS;
	break;

    case ADIO_FCNTL_SET_ATOMICITY:
	MPI_Comm_size(MPI_COMM_WORLD, &np_total);
	MPI_Comm_size(fd->comm, &np_comm);
	if (np_total == np_comm) {
	    iomod = (fcntl_struct->atomicity == 0) ? M_ASYNC : M_UNIX;
	    err = _setiomode(fd->fd_sys, iomod);
	}
        /* else can't do anything because setiomode is global. but
           the file will have been opened with M_UNIX anyway, because
           gopen is also global. */

	fd->atomicity = (fcntl_struct->atomicity == 0) ? 0 : 1;
#ifdef PRINT_ERR_MSG
	*error_code = (err == -1) ? MPI_ERR_UNKNOWN : MPI_SUCCESS ;
#else
	if (err == -1) {
	    *error_code = MPIR_Err_setmsg(MPI_ERR_IO, MPIR_ADIO_ERROR,
			      myname, "I/O Error", "%s", strerror(errno));
	    ADIOI_Error(fd, *error_code, myname);	    
	}
	else *error_code = MPI_SUCCESS;
#endif
	break;

    default:
	FPRINTF(stderr, "Unknown flag passed to ADIOI_PFS_Fcntl\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
    }
}
