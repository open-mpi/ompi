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
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
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
#include "ompi/group/group.h"
#include "ompi/proc/proc.h"
#include "ompi/mca/sharedfp/sharedfp.h"
#include "ompi/mca/sharedfp/base/base.h"

#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#include <fcntl.h>

int mca_sharedfp_lockedfile_file_open (struct ompi_communicator_t *comm,
				       const char* filename,
				       int amode,
				       struct ompi_info_t *info,
				       mca_io_ompio_file_t *fh)
{
    int err = MPI_SUCCESS;
    char * lockedfilename;
    int handle, rank;
    struct mca_sharedfp_lockedfile_data * module_data = NULL;
    struct mca_sharedfp_base_data_t* sh;
    mca_io_ompio_file_t * shfileHandle, *ompio_fh;
    mca_io_ompio_data_t *data;

    /*------------------------------------------------------------*/
    /*Open the same file again without shared file pointer support*/
    /*------------------------------------------------------------*/
    shfileHandle =  (mca_io_ompio_file_t *)malloc(sizeof(mca_io_ompio_file_t));
    err = ompio_io_ompio_file_open(comm,filename,amode,info,shfileHandle,false);
    if ( OMPI_SUCCESS != err)  {
        opal_output(0, "mca_sharedfp_lockedfile_file_open: Error during file open\n");
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
    sh = (struct mca_sharedfp_base_data_t*)malloc(sizeof(struct mca_sharedfp_base_data_t));
    if ( NULL == sh){
        opal_output(0, "mca_sharedfp_lockedfile_file_open: Error, unable to malloc f_sharedfp_ptr struct\n");
	free ( shfileHandle);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    /*Populate the sh file structure based on the implementation*/
    sh->sharedfh      = shfileHandle;			/* Shared file pointer*/
    sh->global_offset = 0;				/* Global Offset*/
    sh->comm          = comm; 				/* Communicator*/
    sh->selected_module_data = NULL;

    rank = ompi_comm_rank ( sh->comm);

    /*Open a new file which will maintain the pointer for this file open*/
    if ( mca_sharedfp_lockedfile_verbose ) {
        opal_output(ompi_sharedfp_base_framework.framework_output,
                    "mca_sharedfp_lockedfile_file_open: open locked file.\n");
    }


    module_data = (struct mca_sharedfp_lockedfile_data*)malloc(sizeof(struct mca_sharedfp_lockedfile_data));
    if ( NULL == module_data ) {
        opal_output(ompi_sharedfp_base_framework.framework_output,
                    "mca_sharedfp_lockedfile_file_open: Error, unable to malloc lockedfile_data struct\n");
	free (shfileHandle);
	free (sh);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    opal_jobid_t masterjobid;
    if ( 0 == comm->c_my_rank  ) {
        ompi_proc_t *masterproc = ompi_group_peer_lookup(comm->c_local_group, 0 );
        masterjobid = OMPI_CAST_RTE_NAME(&masterproc->super.proc_name)->jobid;
    }
    comm->c_coll.coll_bcast ( &masterjobid, 1, MPI_UNSIGNED, 0, comm, 
                              comm->c_coll.coll_bcast_module );
 
    size_t filenamelen = strlen(filename) + 16;
    lockedfilename = (char*)malloc(sizeof(char) * filenamelen);
    if ( NULL == lockedfilename ) {
	free (shfileHandle);
	free (sh);
        free (module_data);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    snprintf(lockedfilename, filenamelen, "%s-%u%s",filename,masterjobid,".lock");
    module_data->filename = lockedfilename;

    /*-------------------------------------------------*/
    /*Open the lockedfile without shared file pointer  */
    /*-------------------------------------------------*/
    if ( 0 == rank ) {
	OMPI_MPI_OFFSET_TYPE position=0;
	/*only let main process initialize file pointer,
	 *therefore there is no need to lock the file
	 */
	handle = open ( lockedfilename, O_RDWR | O_CREAT, 0644 );
	write ( handle, &position, sizeof(OMPI_MPI_OFFSET_TYPE) );
	close ( handle );
    }
    comm->c_coll.coll_barrier ( comm, comm->c_coll.coll_barrier_module );

    handle = open ( lockedfilename, O_RDWR, 0644  );
    if ( -1 == handle ) {
        opal_output(0, "[%d]mca_sharedfp_lockedfile_file_open: Error during file open\n", rank);
	free (shfileHandle);
	free (sh);
	free(module_data);
        return OMPI_ERROR;
    }

    /*Store the new file handle*/
    module_data->handle = handle;
    /* Assign the lockedfile_data to sh->handle*/
    sh->selected_module_data   = module_data;
    /*remember the shared file handle*/
    fh->f_sharedfp_data = sh;

    comm->c_coll.coll_barrier ( comm, comm->c_coll.coll_barrier_module );

    return err;
}

int mca_sharedfp_lockedfile_file_close (mca_io_ompio_file_t *fh)
{
    int err = OMPI_SUCCESS;
    struct mca_sharedfp_lockedfile_data * module_data = NULL;
    struct mca_sharedfp_base_data_t *sh;
    int rank = ompi_comm_rank ( fh->f_comm );

    if ( fh->f_sharedfp_data==NULL){
	/* Can happen with lazy_open being set */
	if ( mca_sharedfp_lockedfile_verbose ) {
	    opal_output(0, "sharedfp_lockedfile_file_close - shared file pointer structure not initialized\n");
	}
        return OMPI_SUCCESS;
    }
    sh = fh->f_sharedfp_data;

    module_data = (lockedfile_data*)(sh->selected_module_data);
    if ( module_data)   {
        /*Close lockedfile handle*/
        if ( module_data->handle)  {
            close (module_data->handle );
	    if ( 0 == rank ) {
		unlink ( module_data->filename);
	    }
        }
        if ( NULL != module_data->filename ){
            free ( module_data->filename);
        }
        free ( module_data );
    }

    /* Close the main file opened by this component*/
    err = ompio_io_ompio_file_close(sh->sharedfh);

    /*free shared file pointer data struct*/
    free(sh);

    return err;

}

