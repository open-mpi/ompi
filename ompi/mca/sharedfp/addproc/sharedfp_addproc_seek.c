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

int
mca_sharedfp_addproc_seek (mca_io_ompio_file_t *fh,
                           OMPI_MPI_OFFSET_TYPE offset, int whence)
{
    int rank;
    int ret = OMPI_SUCCESS;
    OMPI_MPI_OFFSET_TYPE position = 0;
    struct mca_sharedfp_base_data_t *sh = NULL;
    struct mca_sharedfp_addproc_data * addproc_data = sh->selected_module_data;
    long buff = 0;

    if(NULL == fh->f_sharedfp_data){
        opal_output(0, "sharedfp_addproc_write_ordered - shared file pointer structure not initialized correctly\n");
        return OMPI_ERROR;
    }

    sh = fh->f_sharedfp_data;
    rank = ompi_comm_rank ( sh->comm );
    buff = offset;


    /* This is a collective call,
     * only one process needs to communicate with the */
    if(0 == rank){
        ret = MCA_PML_CALL(send ( &buff, 1, OMPI_OFFSET_DATATYPE, 0, whence,
				  MCA_PML_BASE_SEND_STANDARD,
				  addproc_data->intercom));
	if ( OMPI_SUCCESS != ret ) {
	    return OMPI_ERROR;
	}
        ret = MCA_PML_CALL(recv(&position, 1, OMPI_OFFSET_DATATYPE, 0, whence,
				addproc_data->intercom, MPI_STATUS_IGNORE));
	if ( OMPI_SUCCESS != ret ) {
	    return OMPI_ERROR;
	}

    }
    ret = sh->comm->c_coll.coll_barrier(sh->comm, sh->comm->c_coll.coll_barrier_module);

    return ret;
}
