/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2013-2016 University of Houston. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */


#include "ompi_config.h"
#include "sharedfp_lockedfile.h"

#include "mpi.h"
#include "ompi/constants.h"
#include "ompi/mca/sharedfp/sharedfp.h"
#include "ompi/mca/sharedfp/base/base.h"

/*Use fcntl to lock the file which stores the current position*/
#include <fcntl.h>

int
mca_sharedfp_lockedfile_seek (mca_io_ompio_file_t *fh,
                              OMPI_MPI_OFFSET_TYPE offset, int whence)
{
    int rank;
    int ret = OMPI_SUCCESS;
    struct mca_sharedfp_base_data_t *sh = NULL;
    mca_sharedfp_base_module_t * shared_fp_base_module;
    struct mca_sharedfp_lockedfile_data * lockedfile_data;
    int fd_lockedfilehandle;
    /* flock structure that is used to setup the desired fcntl operation */
    struct flock fl;

    if(fh->f_sharedfp_data==NULL){
	opal_output(ompi_sharedfp_base_framework.framework_output,
		    "sharedfp_lockedfile_seek - opening the shared file pointer\n");
        shared_fp_base_module = fh->f_sharedfp;

        ret = shared_fp_base_module->sharedfp_file_open(fh->f_comm,
                                                        fh->f_filename,
                                                        fh->f_amode,
                                                        fh->f_info,
                                                        fh);
        if (ret != OMPI_SUCCESS) {
            opal_output(0,"sharedfp_lockedfile_seek - error opening the shared file pointer\n");
            return ret;
        }
    }

    sh = fh->f_sharedfp_data;
    rank = ompi_comm_rank ( sh->comm );

    if( 0 == rank ){
        if ( MPI_SEEK_SET == whence ){
            /*don't need to read current value*/
            if(offset < 0){
                opal_output(0,"sharedfp_lockedfile_seek - MPI_SEEK_SET, offset must be > 0, got offset=%lld.\n",offset);
                ret = -1;
            }
            opal_output(ompi_sharedfp_base_framework.framework_output,"MPI_SEEK_SET: new_offset=%lld\n",offset);
            fflush(stdout);
        }
	else if ( MPI_SEEK_CUR == whence){
            OMPI_MPI_OFFSET_TYPE current_position;
            int status = mca_sharedfp_lockedfile_get_position(fh,&current_position);
            opal_output(ompi_sharedfp_base_framework.framework_output,
			"MPI_SEEK_CUR: curr=%lld, offset=%lld, call status=%d\n",current_position,offset,status);

            offset = current_position + offset;
            opal_output(ompi_sharedfp_base_framework.framework_output,
			"MPI_SEEK_CUR: new_offset=%lld\n",offset);
            fflush(stdout);
            if(offset < 0){
                opal_output(0,"sharedfp_lockedfile_seek - MPI_SEEK_CURE, offset must be > 0, got offset=%lld.\n",offset);
                ret = -1;
            }
        }
	else if( MPI_SEEK_END == whence ){
            OMPI_MPI_OFFSET_TYPE end_position=0;
            mca_common_ompio_file_get_size(sh->sharedfh,&end_position);
            offset = end_position + offset;
	    opal_output(ompi_sharedfp_base_framework.framework_output,
			"MPI_SEEK_END: file_get_size=%lld\n",end_position);

            if ( offset < 0){
                opal_output(0,"sharedfp_lockedfile_seek - MPI_SEEK_CUR, offset must be > 0, got offset=%lld.\n",offset);
                ret = -1;
            }
        }else{
            opal_output(0,"sharedfp_lockedfile_seek - whence=%i is not supported\n",whence);
            ret = -1;
        }


        /* Set Shared file pointer  */
        lockedfile_data = sh->selected_module_data;
        fd_lockedfilehandle = lockedfile_data->handle;

	opal_output(ompi_sharedfp_base_framework.framework_output,
		    "sharedfp_lockedfile_seek: Aquiring lock...");

        /* set up the flock structure */
        fl.l_type   = F_WRLCK;
        fl.l_whence = SEEK_SET;
        fl.l_start  = 0;
        fl.l_len    = 0;
        fl.l_pid    = getpid();

        /* Aquire an exclusive lock */
        if ( fcntl(fd_lockedfilehandle, F_SETLKW, &fl) == -1) {
            opal_output(0, "Erorr acquiring lock: fcntl(%d,F_SETLKW,&fl)\n",fd_lockedfilehandle);
            opal_output(0,"error(%i): %s", errno, strerror(errno));
            return OMPI_ERROR;
        }
	else{
	    opal_output(ompi_sharedfp_base_framework.framework_output,
			"sharedfp_lockedfile_seek: Success! acquired lock.for fd: %d\n",fd_lockedfilehandle);
        }

        /*-- -----------------
	 *write to the file
	 *--------------------
	 */
	lseek ( fd_lockedfilehandle, 0, SEEK_SET);
        write ( fd_lockedfilehandle, &offset, sizeof(OMPI_MPI_OFFSET_TYPE));

        /*-------------------
	 * unlock the file
	 *--------------------
	 */
	if ( mca_sharedfp_lockedfile_verbose ) {
            opal_output(ompi_sharedfp_base_framework.framework_output,
                        "sharedfp_lockedfile_seek: Releasing lock...");
	}
	fl.l_type   = F_UNLCK;  /* set to unlock same region */
        fl.l_whence = SEEK_SET;
        fl.l_start  = 0;
        fl.l_len    = 0;
        fl.l_pid    = getpid();

        if (fcntl(fd_lockedfilehandle, F_SETLK, &fl) == -1) {
            opal_output(0,"Failed to release lock for fd: %d\n",fd_lockedfilehandle);
            opal_output(0,"error(%i): %s", errno, strerror(errno));
            return OMPI_ERROR;
        }
	else{
	    opal_output(ompi_sharedfp_base_framework.framework_output,
			"sharedfp_lockedfile_seek: released lock.for fd: %d\n",fd_lockedfilehandle);
        }
    }

    sh->comm->c_coll.coll_barrier ( sh->comm , sh->comm->c_coll.coll_barrier_module );
    return ret;
}
