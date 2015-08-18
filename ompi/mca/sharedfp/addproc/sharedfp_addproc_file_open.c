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
#include "sharedfp_addproc.h"

#include "mpi.h"
#include "ompi/constants.h"
#include "ompi/mca/sharedfp/sharedfp.h"
#include "ompi/mca/pml/pml.h"

#include <semaphore.h>
#include <sys/mman.h>
#include "ompi/mca/sharedfp/base/base.h"

int mca_sharedfp_addproc_file_open (struct ompi_communicator_t *comm,
                                    char* filename,
                                    int amode,
                                    struct ompi_info_t *info,
                                    mca_io_ompio_file_t *fh)
{
    int ret = OMPI_SUCCESS, err;
    int rank;
    struct mca_sharedfp_base_data_t* sh;
    mca_io_ompio_file_t * shfileHandle;
    MPI_Comm newInterComm;
    struct mca_sharedfp_addproc_data * addproc_data = NULL;

    /*-------------------------------------------------*/
    /*Open the same file again without shared file pointer*/
    /*-------------------------------------------------*/
    shfileHandle =  (mca_io_ompio_file_t *)malloc(sizeof(mca_io_ompio_file_t));
    ret = ompio_io_ompio_file_open(comm,filename,amode,info,shfileHandle,false);
    if ( OMPI_SUCCESS != ret) {
        printf( "mca_sharedfp_addproc_file_open: Error during file open\n");
        return ret;
    }

    /*Memory is allocated here for the sh structure*/
    if ( mca_sharedfp_addproc_verbose ) {
	printf( "mca_sharedfp_addproc_file_open: malloc f_sharedfp_ptr struct\n");
    }
    sh = (struct mca_sharedfp_base_data_t*)malloc(sizeof(struct mca_sharedfp_base_data_t));
    if ( NULL == sh ){
        printf( "mca_sharedfp_addproc_file_open: Error, unable to malloc f_sharedfp_ptr struct\n");
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /*Populate the sh file structure based on the implementation*/
    sh->sharedfh      = shfileHandle;			/* Shared file pointer*/
    sh->global_offset = 0;				/* Global Offset*/
    sh->comm          = comm; 				/* Communicator*/
    sh->selected_module_data = NULL;

    rank = ompi_comm_rank ( sh->comm );

    if ( mca_sharedfp_addproc_verbose ) {
	printf( "mca_sharedfp_addproc_file_open: START spawn by rank=%d\n",rank);
    }

    /*Spawn a new process which will maintain the offsets for this file open*/
    ret = MPI_Comm_spawn("mca_sharedfp_addproc_control", MPI_ARGV_NULL, 1, MPI_INFO_NULL,
		   0, sh->comm, &newInterComm, &err);
    if ( OMPI_SUCCESS != ret  ) {
	printf( "mca_sharedfp_addproc_file_open: error spawning control process ret=%d\n",
		ret);
    }

    /*If spawning successful*/
    if (newInterComm)    {
        addproc_data = (struct mca_sharedfp_addproc_data*)malloc(sizeof(struct mca_sharedfp_addproc_data));
        if ( NULL == addproc_data ){
            printf( "mca_sharedfp_addproc_file_open: Error, unable to malloc addproc_data struct\n");
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        /*Store the new Intercommunicator*/
        addproc_data->intercom = newInterComm;

        /*save the addproc data*/
        sh->selected_module_data = addproc_data;
        /*remember the shared file handle*/
        fh->f_sharedfp_data = sh;
    }
    else{
        printf( "mca_sharedfp_addproc_file_open: DONE spawn by rank=%d, errcode[success=%d, err=%d]=%d\n",
		    rank, MPI_SUCCESS, MPI_ERR_SPAWN, ret);
        ret = OMPI_ERROR;
    }

    return ret;
}

int mca_sharedfp_addproc_file_close (mca_io_ompio_file_t *fh)
{
    struct mca_sharedfp_base_data_t *sh=NULL;
    int err = OMPI_SUCCESS;
    long sendBuff = 0;
    int count = 1;
    int rank;
    struct mca_sharedfp_addproc_data * addproc_data = NULL;

    if ( NULL == fh->f_sharedfp_data){
	/* Can happen with lazy initialization of the sharedfp structures */
	if ( mca_sharedfp_addproc_verbose ) {
	    printf( "sharedfp_addproc_file_close - shared file pointer structure not initialized\n");
	}
        return OMPI_SUCCESS;
    }
    sh = fh->f_sharedfp_data;

    rank = ompi_comm_rank ( sh->comm );

    /* Make sure that all processes are ready to release the
    ** shared file pointer resources
    */
    sh->comm->c_coll.coll_barrier(sh->comm, sh->comm->c_coll.coll_barrier_module );

    addproc_data = (struct mca_sharedfp_addproc_data*)(sh->selected_module_data);

    if (addproc_data) {
        /*tell additional proc to stop listening*/
        if(0 == rank){
            MCA_PML_CALL(send( &sendBuff, count, OMPI_OFFSET_DATATYPE, 0, END_TAG,
			       MCA_PML_BASE_SEND_STANDARD, addproc_data->intercom));
        }

        /* Free  intercommunicator */
        if(addproc_data->intercom){
            ompi_comm_free(&(addproc_data->intercom));
        }
        free(addproc_data);
    }

    /* Close the main file opened by this component*/
    err = ompio_io_ompio_file_close(sh->sharedfh);

    /*free shared file pointer data struct*/
    free(sh);
    return err;
}
