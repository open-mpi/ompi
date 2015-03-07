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
 * Copyright (c) 2013-2015 University of Houston. All rights reserved.
 * Copyright (c) 2013      Intel, Inc. All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */


#include "ompi_config.h"

#if HAVE_LIBGEN_H
#include <libgen.h>
#endif
#if HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

#include "sharedfp_sm.h"

#include "mpi.h"
#include "ompi/constants.h"
#include "ompi/mca/sharedfp/sharedfp.h"
#include "ompi/mca/sharedfp/base/base.h"

#include <semaphore.h>
#include <sys/mman.h>
#include <libgen.h>


int mca_sharedfp_sm_file_open (struct ompi_communicator_t *comm,
                               char* filename,
                               int amode,
                               struct ompi_info_t *info,
                               mca_io_ompio_file_t *fh)
{
    int err = OMPI_SUCCESS;
    struct mca_sharedfp_base_data_t* sh;
    struct mca_sharedfp_sm_data * sm_data = NULL;
    mca_io_ompio_file_t * shfileHandle;
    char * filename_basename;
    char * sm_filename;
    struct sm_offset * sm_offset_ptr;
    struct sm_offset sm_offset;
    int sm_fd;
    int rank;

    /*----------------------------------------------------*/
    /*Open the same file again without shared file pointer*/
    /*----------------------------------------------------*/
    shfileHandle = (mca_io_ompio_file_t *)malloc(sizeof(mca_io_ompio_file_t));
    err = ompio_io_ompio_file_open(comm,filename,amode,info,shfileHandle,false);
    if ( OMPI_SUCCESS != err) {
        opal_output(0, "mca_sharedfp_sm_file_open: Error during file open\n");
        return err;
    }

    /*Memory is allocated here for the sh structure*/
    if ( mca_sharedfp_sm_verbose ) {
	printf( "mca_sharedfp_sm_file_open: malloc f_sharedfp_ptr struct\n");
    }

    sh = (struct mca_sharedfp_base_data_t*)malloc(sizeof(struct mca_sharedfp_base_data_t));
    if ( NULL == sh ) {
	opal_output(0, "mca_sharedfp_sm_file_open: Error, unable to malloc f_sharedfp_ptr struct\n");
	free(shfileHandle);
	return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /*Populate the sh file structure based on the implementation*/
    sh->sharedfh      = shfileHandle;			/* Shared file pointer*/
    sh->global_offset = 0;				/* Global Offset*/
    sh->comm          = comm; 				/* Communicator*/
    sh->selected_module_data = NULL;

    rank = ompi_comm_rank ( sh->comm );

    /*Open a shared memory segment which will hold the shared file pointer*/
    if ( mca_sharedfp_sm_verbose ) {
	printf( "mca_sharedfp_sm_file_open: allocatge shared memory segment.\n");
    }

    
    sm_data = (struct mca_sharedfp_sm_data*) malloc ( sizeof(struct mca_sharedfp_sm_data));
    if ( NULL == sm_data ){
        opal_output(0, "mca_sharedfp_sm_file_open: Error, unable to malloc sm_data struct\n");
        free(sh);
        free(shfileHandle);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    sm_data->sm_filename=NULL;


    /* the shared memory segment is identified opening a file
    ** and then mapping it to memory
    ** For sharedfp we also want to put the file backed shared memory into the tmp directory
    ** TODO: properly name the file so that different jobs can run on the same system w/o
    **      overwriting each other, e.g.  orte_process_info.proc_session_dir
    */
    /*sprintf(sm_filename,"%s%s",filename,".sm");*/
    filename_basename = basename(filename);
    sm_filename = (char*) malloc( sizeof(char) * (strlen(filename_basename)+64) );
    if (NULL == sm_filename) {
        free(sm_data);
        free(sh);
        free(shfileHandle);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    sprintf(sm_filename,"/tmp/OMPIO_sharedfp_sm_%s%s",filename_basename,".sm");

    /* open shared memory file, initialize to 0, map into memory */
    sm_fd = open(sm_filename, O_RDWR | O_CREAT,
                 S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH);
    if ( sm_fd == -1){
        /*error opening file*/
        printf("mca_sharedfp_sm_file_open: Error, unable to open file for mmap: %s\n",sm_filename);
        free(sm_filename);
        free(sm_data);
        free(sh);
        free(shfileHandle);
        return OMPI_ERROR;	
    }

        free(sm_filename);
    sm_data->sm_filename = sm_filename;
    
    /*TODO: is it necessary to write to the file first?*/
    if( 0 == rank ){
	memset ( &sm_offset, 0, sizeof (struct sm_offset ));
	write ( sm_fd, &sm_offset, sizeof(struct sm_offset));
    }
    comm->c_coll.coll_barrier (comm, comm->c_coll.coll_barrier_module );
    
    /*the file has been written to, now we can map*/
    sm_offset_ptr = mmap(NULL, sizeof(struct sm_offset), PROT_READ | PROT_WRITE,
			 MAP_SHARED, sm_fd, 0);
    
    close(sm_fd);
    
    if ( sm_offset_ptr==MAP_FAILED){
	err = OMPI_ERROR;
	printf("mca_sharedfp_sm_file_open: Error, unable to mmap file: %s\n",sm_filename);
	printf("%s\n", strerror(errno));
        free(sm_filename);
        free(sm_data);
        free(sh);
        free(shfileHandle);
        return OMPI_ERROR;	
    }

    /* Initialize semaphore so that is shared between processes.           */
    /* the semaphore is shared by keeping it in the shared memory segment  */

#ifdef OMPIO_SHAREDFP_USE_UNNAMED_SEMAPHORES
    if(sem_init(&sm_offset_ptr->mutex, 1, 1) != -1){
#else
    sm_data->sem_name = (char*) malloc( sizeof(char) * (strlen(filename_basename)+32) );
    sprintf(sm_data->sem_name,"OMPIO_sharedfp_sem_%s",filename_basename);

    if( (sm_data->mutex = sem_open(sm_data->sem_name, O_CREAT, 0644, 1)) != SEM_FAILED ) {
#endif
	/*If opening was successful*/
	/*Store the new file handle*/
	sm_data->sm_offset_ptr = sm_offset_ptr;
	/* Assign the sm_data to sh->selected_module_data*/
	sh->selected_module_data   = sm_data;
	/*remember the shared file handle*/
	fh->f_sharedfp_data = sh;
	
	/*write initial zero*/
	if(rank==0){
	    MPI_Offset position=0;

#ifdef OMPIO_SHAREDFP_USE_UNNAMED_SEMAPHORES
	    sem_wait(sm_offset_ptr->mutex);
	    sm_offset_ptr->offset=position;
	    sem_post(sm_offset_ptr->mutex);
#else
	    sem_wait(sm_data->mutex);
	    sm_offset_ptr->offset=position;
	    sem_post(sm_data->mutex);
#endif
	}
    }else{
        free(sm_filename);
	free(sm_data);
	free(sh);
	free(shfileHandle);
        munmap(sm_offset_ptr, sizeof(struct sm_offset));
	err = OMPI_ERROR;
    }

    comm->c_coll.coll_barrier (comm, comm->c_coll.coll_barrier_module );

    return err;
}

int mca_sharedfp_sm_file_close (mca_io_ompio_file_t *fh)
{
    int err = OMPI_SUCCESS;
    /*sharedfp data structure*/
    struct mca_sharedfp_base_data_t *sh=NULL;
    /*sharedfp sm module data structure*/
    struct mca_sharedfp_sm_data * file_data=NULL;

    if( NULL == fh->f_sharedfp_data ){
	if ( mca_sharedfp_sm_verbose ) {
	    printf("sharedfp_sm_file_close: shared file pointer structure not initialized\n");
	}
        return OMPI_SUCCESS;
    }
    sh = fh->f_sharedfp_data;

    /* Use an MPI Barrier in order to make sure that
     * all processes are ready to release the
     * shared file pointer resources
     */
    sh->comm->c_coll.coll_barrier (sh->comm, sh->comm->c_coll.coll_barrier_module );

    file_data = (sm_data*)(sh->selected_module_data);
    if (file_data)  {
        /*Close sm handle*/
        if (file_data->sm_offset_ptr) {
            /* destroy semaphore */
#ifdef OMPIO_SHAREDFP_USE_UNNAMED_SEMAPHORES
	    sem_destroy(file_data->sm_offset_ptr->mutex);
#else
 	    sem_unlink (file_data->sem_name);
 	    free (file_data->sem_name);
#endif
            /*Release the shared memory segment.*/
            munmap(file_data->sm_offset_ptr,sizeof(struct sm_offset));
            /*Q: Do we need to delete the file? */
            remove(file_data->sm_filename);
        }
        /*free our sm data structure*/
        if(file_data->sm_filename){
            free(file_data->sm_filename);
        }
        free(file_data);
    }

    /* Close the main file opened by this component*/
    err = ompio_io_ompio_file_close(sh->sharedfh);

    /*free shared file pointer data struct*/
    free(sh);

    return err;

}
