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
 * Copyright (c) 2013-2017 University of Houston. All rights reserved.
 * Copyright (c) 2013      Intel, Inc. All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2015      Cisco Systems, Inc.  All rights reserved.
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
#include "ompi/group/group.h"
#include "ompi/proc/proc.h"
#include "ompi/mca/sharedfp/sharedfp.h"
#include "ompi/mca/sharedfp/base/base.h"

#include <semaphore.h>
#include <sys/mman.h>
#include <libgen.h>


int mca_sharedfp_sm_file_open (struct ompi_communicator_t *comm,
                               const char* filename,
                               int amode,
                               struct ompi_info_t *info,
                               mca_io_ompio_file_t *fh)
{
    int err = OMPI_SUCCESS;
    struct mca_sharedfp_base_data_t* sh;
    struct mca_sharedfp_sm_data * sm_data = NULL;
    mca_io_ompio_file_t * shfileHandle, *ompio_fh;
    char * filename_basename;
    char * sm_filename;
    struct mca_sharedfp_sm_offset * sm_offset_ptr;
    struct mca_sharedfp_sm_offset sm_offset;
    mca_io_ompio_data_t *data;
    int sm_fd;
    int rank;

    /*----------------------------------------------------*/
    /*Open the same file again without shared file pointer*/
    /*----------------------------------------------------*/
    shfileHandle = (mca_io_ompio_file_t *)malloc(sizeof(mca_io_ompio_file_t));
    if ( NULL == shfileHandle ) {
        opal_output(0, "mca_sharedfp_sm_file_open: Error during memory allocation\n");
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    err = ompio_io_ompio_file_open(comm,filename,amode,info,shfileHandle,false);
    if ( OMPI_SUCCESS != err) {
        opal_output(0, "mca_sharedfp_sm_file_open: Error during file open\n");
        free (shfileHandle);
        return err;
    }
    shfileHandle->f_fh = fh->f_fh;
    data = (mca_io_ompio_data_t *) fh->f_fh->f_io_selected_data;
    ompio_fh = &data->ompio_fh;

    err = mca_io_ompio_set_view_internal (shfileHandle,
                                          ompio_fh->f_disp,
                                          ompio_fh->f_etype,
                                          ompio_fh->f_orig_filetype,
                                          ompio_fh->f_datarep,
                                          MPI_INFO_NULL);

    /*Memory is allocated here for the sh structure*/
    if ( mca_sharedfp_sm_verbose ) {
        opal_output(ompi_sharedfp_base_framework.framework_output,
                    "mca_sharedfp_sm_file_open: malloc f_sharedfp_ptr struct\n");
    }

    sh = (struct mca_sharedfp_base_data_t*)malloc(sizeof(struct mca_sharedfp_base_data_t));
    if ( NULL == sh ) {
        opal_output(0, "mca_sharedfp_sm_file_open: Error, unable to malloc f_sharedfp_ptr struct\n");
        free(shfileHandle);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /*Populate the sh file structure based on the implementation*/
    sh->sharedfh      = shfileHandle;                        /* Shared file pointer*/
    sh->global_offset = 0;                                /* Global Offset*/
    sh->comm          = comm;                                 /* Communicator*/
    sh->selected_module_data = NULL;

    rank = ompi_comm_rank ( sh->comm );

    /*Open a shared memory segment which will hold the shared file pointer*/
    if ( mca_sharedfp_sm_verbose ) {
        opal_output(ompi_sharedfp_base_framework.framework_output,
                    "mca_sharedfp_sm_file_open: allocatge shared memory segment.\n");
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
    filename_basename = basename((void *)filename);
    sm_filename = (char*) malloc( sizeof(char) * (strlen(filename_basename)+64) );
    if (NULL == sm_filename) {
        free(sm_data);
        free(sh);
        free(shfileHandle);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    opal_jobid_t masterjobid;
    if ( 0 == comm->c_my_rank ) {
        ompi_proc_t *masterproc = ompi_group_peer_lookup(comm->c_local_group, 0 );
        masterjobid = OMPI_CAST_RTE_NAME(&masterproc->super.proc_name)->jobid;
    }
    comm->c_coll.coll_bcast ( &masterjobid, 1, MPI_UNSIGNED, 0, comm, 
                              comm->c_coll.coll_bcast_module );

    sprintf(sm_filename,"/tmp/OMPIO_%s_%d_%s",filename_basename, masterjobid, ".sm");
    /* open shared memory file, initialize to 0, map into memory */
    sm_fd = open(sm_filename, O_RDWR | O_CREAT,
                 S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH);
    if ( sm_fd == -1){
        /*error opening file*/
        opal_output(0,"mca_sharedfp_sm_file_open: Error, unable to open file for mmap: %s\n",sm_filename);
        free(sm_filename);
        free(sm_data);
        free(sh);
        free(shfileHandle);
        return OMPI_ERROR;
    }

    sm_data->sm_filename = sm_filename;

    /*TODO: is it necessary to write to the file first?*/
    if( 0 == rank ){
        memset ( &sm_offset, 0, sizeof (struct mca_sharedfp_sm_offset ));
        write ( sm_fd, &sm_offset, sizeof(struct mca_sharedfp_sm_offset));
    }
    comm->c_coll.coll_barrier (comm, comm->c_coll.coll_barrier_module );

    /*the file has been written to, now we can map*/
    sm_offset_ptr = mmap(NULL, sizeof(struct mca_sharedfp_sm_offset), PROT_READ | PROT_WRITE,
                         MAP_SHARED, sm_fd, 0);

    close(sm_fd);

    if ( sm_offset_ptr==MAP_FAILED){
        err = OMPI_ERROR;
        opal_output(0, "mca_sharedfp_sm_file_open: Error, unable to mmap file: %s\n",sm_filename);
        opal_output(0, "%s\n", strerror(errno));
        free(sm_filename);
        free(sm_data);
        free(sh);
        free(shfileHandle);
        return OMPI_ERROR;
    }

    /* Initialize semaphore so that is shared between processes.           */
    /* the semaphore is shared by keeping it in the shared memory segment  */

#if defined(HAVE_SEM_OPEN)

#if defined (__APPLE__)    
    sm_data->sem_name = (char*) malloc( sizeof(char) * 32);
    snprintf(sm_data->sem_name,31,"OMPIO_%s",filename_basename);
#else
    sm_data->sem_name = (char*) malloc( sizeof(char) * 253);
    snprintf(sm_data->sem_name,252,"OMPIO_%s",filename_basename);
#endif

    if( (sm_data->mutex = sem_open(sm_data->sem_name, O_CREAT, 0644, 1)) != SEM_FAILED ) {
#elif defined(HAVE_SEM_INIT)
    sm_data->mutex = &sm_offset_ptr->mutex;
    if(sem_init(&sm_offset_ptr->mutex, 1, 1) != -1){
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

            sem_wait(sm_data->mutex);
            sm_offset_ptr->offset=position;
            sem_post(sm_data->mutex);
        }
    }else{
        free(sm_filename);
        free(sm_data);
        free(sh);
        free(shfileHandle);
        munmap(sm_offset_ptr, sizeof(struct mca_sharedfp_sm_offset));
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
            opal_output(ompi_sharedfp_base_framework.framework_output,
                        "sharedfp_sm_file_close: shared file pointer structure not initialized\n");
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
#if defined(HAVE_SEM_OPEN)
             sem_unlink (file_data->sem_name);
             free (file_data->sem_name);
#elif defined(HAVE_SEM_INIT)
            sem_destroy(&file_data->sm_offset_ptr->mutex);
#endif
            /*Release the shared memory segment.*/
            munmap(file_data->sm_offset_ptr,sizeof(struct mca_sharedfp_sm_offset));
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
