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
 * Copyright (c) 2013      University of Houston. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */


#include "ompi_config.h"
#include "sharedfp_sm.h"

#include "mpi.h"
#include "ompi/constants.h"
#include "ompi/mca/sharedfp/sharedfp.h"

/*use a semaphore to lock the shared memory location*/
#include <semaphore.h>

int
mca_sharedfp_sm_seek (mca_io_ompio_file_t *fh,
                      OMPI_MPI_OFFSET_TYPE offset, int whence)
{
    int rank, status=0;
    OMPI_MPI_OFFSET_TYPE end_position=0;
    int ret = OMPI_SUCCESS;
    struct mca_sharedfp_base_data_t *sh = NULL;
    mca_sharedfp_base_module_t * shared_fp_base_module = NULL;
    struct mca_sharedfp_sm_data * sm_data = NULL;
    struct sm_offset * sm_offset_ptr = NULL;

    if( NULL == fh->f_sharedfp_data ) {
	if ( mca_sharedfp_sm_verbose ) {
	    printf("sharedfp_sm_seek: opening the shared file pointer\n");
	}
        shared_fp_base_module = fh->f_sharedfp;

        ret = shared_fp_base_module->sharedfp_file_open(fh->f_comm,
                                                        fh->f_filename,
                                                        fh->f_amode,
                                                        fh->f_info,
                                                        fh);
        if ( OMPI_SUCCESS != ret ) {
            opal_output(0,"sharedfp_sm_seek - error opening the shared file pointer\n");
            return ret;
        }
    }

    sh = fh->f_sharedfp_data;
    rank = ompi_comm_rank ( sh->comm );

    if( 0 == rank ){
        if ( MPI_SEEK_SET == whence){
            /*no nothing*/
            if ( offset < 0){
                opal_output(0,"sharedfp_sm_seek - MPI_SEEK_SET, offset must be > 0, got offset=%lld.\n",offset);
                ret = -1;
            }
	    if ( mca_sharedfp_sm_verbose ) {
		printf("sharedfp_sm_seek: MPI_SEEK_SET new_offset=%lld\n",offset);
	    }
        }
	else if( MPI_SEEK_CUR == whence){
            OMPI_MPI_OFFSET_TYPE current_position;
            ret = mca_sharedfp_sm_get_position ( fh, &current_position);
	    if ( mca_sharedfp_sm_verbose ) {
		printf("sharedfp_sm_seek: MPI_SEEK_CUR: curr=%lld, offset=%lld, call status=%d\n",
		       current_position,offset,status);
	    }
            offset = current_position + offset;
	    if ( mca_sharedfp_sm_verbose ) {
		printf("sharedfp_sm_seek: MPI_SEEK_CUR: new_offset=%lld\n",offset);
	    }
            if(offset < 0){
                opal_output(0,"sharedfp_sm_seek - MPI_SEEK_CURE, offset must be > 0, got offset=%lld.\n",offset);
                ret = -1;
            }
        }
	else if( MPI_SEEK_END == whence){
            end_position=0;
            ompio_io_ompio_file_get_size(sh->sharedfh,&end_position);

            offset = end_position + offset;
	    if ( mca_sharedfp_sm_verbose ) {
		printf("sharedfp_sm_seek: MPI_SEEK_END: file_get_size=%lld\n",end_position);
	    }
            if(offset < 0){
                opal_output(0,"sharedfp_sm_seek - MPI_SEEK_CUR, offset must be > 0, got offset=%lld.\n",offset);
                ret = -1;
            }
        }
	else {
            opal_output(0,"sharedfp_sm_seek - whence=%i is not supported\n",whence);
            ret = -1;
        }

        /*-----------------------------------------------------*/
        /* Set Shared file pointer                             */
        /*-----------------------------------------------------*/
        sm_data = sh->selected_module_data;
        sm_offset_ptr = sm_data->sm_offset_ptr;

        /*-------------------*/
        /*lock the file  */
        /*--------------------*/
	if ( mca_sharedfp_sm_verbose ) {
	    printf("sharedfp_sm_seek: Aquiring lock, rank=%d...",rank); fflush(stdout);
	}

        /* Aquire an exclusive lock */
        sm_offset_ptr = sm_data->sm_offset_ptr;
        sem_wait(&sm_offset_ptr->mutex);

	if ( mca_sharedfp_sm_verbose ) {
	    printf("sharedfp_sm_seek: Success! Acquired sm lock.for rank=%d\n",rank);
	}
        sm_offset_ptr->offset=offset;
	if ( mca_sharedfp_sm_verbose ) {
	    printf("sharedfp_sm_seek: Releasing sm lock...rank=%d",rank); fflush(stdout);
	}
        sem_post(&sm_offset_ptr->mutex);
    }

    /* since we are only letting process 0, update the current pointer
     * all of the other processes need to wait before proceeding.
     */
    sh->comm->c_coll.coll_barrier ( sh->comm, sh->comm->c_coll.coll_barrier_module );

    return ret;
}
