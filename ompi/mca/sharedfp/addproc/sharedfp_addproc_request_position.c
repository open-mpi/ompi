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
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/sharedfp/sharedfp.h"

int mca_sharedfp_addproc_request_position(struct mca_sharedfp_base_data_t * sh,
                                          int bytes_requested,
                                          OMPI_MPI_OFFSET_TYPE *offset)
{
    int ret = OMPI_SUCCESS;
    OMPI_MPI_OFFSET_TYPE position = 0;
    long sendBuff = bytes_requested ;
    int count = 1;


    struct mca_sharedfp_addproc_data * addproc_data = sh->selected_module_data;

    *offset = 0;

    ret = MCA_PML_CALL(send( &sendBuff, count, OMPI_OFFSET_DATATYPE, 0, REQUEST_TAG,
			     MCA_PML_BASE_SEND_STANDARD, addproc_data->intercom));
    if ( OMPI_SUCCESS != ret ) {
	return ret;
    }
    ret = MCA_PML_CALL(recv( &position, count, OMPI_OFFSET_DATATYPE, 0, OFFSET_TAG,
			     addproc_data->intercom, MPI_STATUS_IGNORE));

    *offset = position;
    return ret;
}

int mca_sharedfp_addproc_get_position(mca_io_ompio_file_t *fh,
				      OMPI_MPI_OFFSET_TYPE * offset)
{
    int ret = OMPI_SUCCESS;
    struct mca_sharedfp_base_data_t *sh = NULL;

    if(NULL == fh->f_sharedfp_data){
        opal_output(0, "sharedfp_addproc_get_position - shared file pointer structure not initialized correctly\n");
        return OMPI_ERROR;
    }

    /* Retrieve the shared file data struct*/
    sh = fh->f_sharedfp_data;

    /* Requesting the offset to write 0 bytes,
    ** returns the current offset w/o updating it
    */
    ret = mca_sharedfp_addproc_request_position(sh, 0, offset);

    return ret;
}
