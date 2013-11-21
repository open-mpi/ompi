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
#include "sharedfp_individual.h"

#include "mpi.h"
#include "ompi/constants.h"
#include "ompi/mca/sharedfp/sharedfp.h"
#include "ompi/mca/sharedfp/base/base.h"
#include "ompi/file/file.h"


int mca_sharedfp_individual_file_open (struct ompi_communicator_t *comm,
				       char* filename,
				       int amode,
				       struct ompi_info_t *info,
				       mca_io_ompio_file_t *fh)
{
    int err = 0;
    int rank;
    char * datafilename;	/*The array size would change as it is based on the current path*/
    char * metadatafilename;	/*The array size would change as it is based on the current path*/
    mca_io_ompio_file_t * datafilehandle;
    mca_io_ompio_file_t * metadatafilehandle;
    mca_sharedfp_individual_header_record* headnode = NULL;
    struct mca_sharedfp_base_data_t* sh;
    mca_io_ompio_file_t * shfileHandle;
    size_t len=0;

    /*-------------------------------------------------*/
    /*Open the same file again without shared file pointer*/
    /*-------------------------------------------------*/
    shfileHandle =  (mca_io_ompio_file_t *) malloc ( sizeof(mca_io_ompio_file_t));
    err = ompio_io_ompio_file_open ( comm, filename, amode, info, shfileHandle, false);
    if ( OMPI_SUCCESS != err )  {
        opal_output(0, "mca_sharedfp_individual_file_open: Error during file open\n");
        return err;
    }

    sh = (struct mca_sharedfp_base_data_t*) malloc ( sizeof(struct mca_sharedfp_base_data_t));
    if ( NULL == sh ){
        opal_output(0, "mca_sharedfp_individual_file_open: Error, unable to malloc "
		    "f_sharedfp_ptr struct\n");
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    
    rank = ompi_comm_rank ( comm );
    
    /*Populate the sh file structure based on the implementation*/
    sh->sharedfh      = shfileHandle;		/* Shared file pointer*/
    sh->global_offset = 0;			/* Global Offset*/
    sh->comm          = comm; 			/* Communicator*/
    sh->selected_module_data = NULL;

    /* Assign the metadatalinked list to sh->handle */
    sh->selected_module_data = (mca_sharedfp_individual_header_record *)mca_sharedfp_individual_insert_headnode();

    /*--------------------------------------------------------*/
    /* Open an individual data file for each process          */
    /* NOTE: Open the data file without shared file pointer   */
    /*--------------------------------------------------------*/
    if ( mca_sharedfp_individual_verbose ) {
	printf("mca_sharedfp_individual_file_open: open data file.\n");
    }

    /* data filename created by appending .data.$rank to the original filename*/
    len = strlen (filename ) + 64;
    datafilename = (char*)malloc( len );
    snprintf(datafilename, len, "%s%s%d",filename,".data.",rank);


    datafilehandle = (mca_io_ompio_file_t *)malloc(sizeof(mca_io_ompio_file_t));
    err = ompio_io_ompio_file_open(MPI_COMM_SELF, datafilename,
                                   MPI_MODE_RDWR | MPI_MODE_CREATE | MPI_MODE_DELETE_ON_CLOSE,
                                   MPI_INFO_NULL, datafilehandle, false);
    if ( OMPI_SUCCESS != err) {
        opal_output(0, "mca_sharedfp_individual_file_open: Error during datafile file open\n");
        return err;
    }

    /*----------------------------------------------------------*/
    /* Open an individual metadata file for each of the process */
    /* NOTE: Open the meta file without shared file pointer     */
    /*----------------------------------------------------------*/
    if ( mca_sharedfp_individual_verbose ) {
	printf("mca_sharedfp_individual_file_open: metadata file.\n");
    }

    /* metadata filename created by appending .metadata.$rank to the original filename*/
    metadatafilename = (char*) malloc ( len );   
    snprintf ( metadatafilename, len, "%s%s%d", filename, ".metadata.",rank);

    metadatafilehandle = (mca_io_ompio_file_t *)malloc(sizeof(mca_io_ompio_file_t));
    err = ompio_io_ompio_file_open ( MPI_COMM_SELF,metadatafilename,
                                     MPI_MODE_RDWR | MPI_MODE_CREATE | MPI_MODE_DELETE_ON_CLOSE,
                                     MPI_INFO_NULL, metadatafilehandle, false);
    if ( OMPI_SUCCESS != err) {
        opal_output(0, "mca_sharedfp_individual_file_open: Error during metadatafile file open\n");
        return err;
    }

    /*save the datafilehandle and metadatahandle in the sharedfp individual module data structure*/
    headnode = (mca_sharedfp_individual_header_record*)(sh->selected_module_data);
    if ( headnode) {
        headnode->datafilehandle     = datafilehandle;
        headnode->metadatafilehandle = metadatafilehandle;
        headnode->datafilename       = datafilename;
        headnode->metadatafilename   = metadatafilename;
    }

    /*save the sharedfp individual module data structure in the ompio filehandle structure*/
    fh->f_sharedfp_data = sh;

    return err;
}

int mca_sharedfp_individual_file_close (mca_io_ompio_file_t *fh)
{
    mca_sharedfp_individual_header_record* headnode = NULL;
    struct mca_sharedfp_base_data_t *sh;
    int err = OMPI_SUCCESS;

    if ( NULL == fh->f_sharedfp_data ){
	if ( mca_sharedfp_individual_verbose ) {
	    printf("sharedfp_inidividual_file_close - shared file pointer structure not initialized\n");
	}
        return OMPI_SUCCESS;
    }
    sh = fh->f_sharedfp_data;

    /* Merge data from individal files to final output file */
    err = mca_sharedfp_individual_collaborate_data (sh);

    headnode = (mca_sharedfp_individual_header_record*)(sh->selected_module_data);
    if (headnode)  {
        /*Close datafile*/
        if (headnode->datafilehandle)  {
            /*TODO: properly deal with returned error code*/
            err = ompio_io_ompio_file_close(headnode->datafilehandle);
            /* NOTE: No neeed to manually delete the file,
	    ** the amode should have been set to delete on close
	    */
        }
        if(headnode->datafilename){
            free(headnode->datafilename);
        }

        /*Close metadatafile*/
        if (headnode->metadatafilehandle)  {
            /*TODO: properly deal with returned error code*/
            err = ompio_io_ompio_file_close(headnode->metadatafilehandle);
            /* NOTE: No neeed to manually delete the file,
	    ** the amode should have been set to delete on close
	    */
        }
        if(headnode->metadatafilename){
            free(headnode->metadatafilename);
        }
    }

    /* Close the main file opened by this component*/
    err = ompio_io_ompio_file_close(sh->sharedfh);

    /*free shared file pointer data struct*/
    free(sh);

    return err;
}


mca_sharedfp_individual_header_record* mca_sharedfp_individual_insert_headnode ( void )
{
    mca_sharedfp_individual_header_record *headnode = NULL;
    if ( !headnode )  {
	/*Allocate a headnode*/
	headnode = (mca_sharedfp_individual_header_record*)malloc(sizeof(mca_sharedfp_individual_header_record));
	if (!headnode)
	    return NULL;
    }
    
    headnode->numofrecords = 0;			/* No records in the linked list */
    headnode->numofrecordsonfile = 0;		/* No records in the metadatafile for this file */
    
    headnode->datafile_offset = 0;
    headnode->metadatafile_offset = 0;
    
    headnode->metafile_start_offset = 0;
    headnode->datafile_start_offset = 0;
    
    headnode->metadatafilehandle = 0;
    headnode->datafilehandle     = 0;
    headnode->next   = NULL;

    return headnode;
}
