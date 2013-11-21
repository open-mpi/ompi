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

int mca_sharedfp_sm_iwrite(mca_io_ompio_file_t *fh,
                           void *buf,
                           int count,
                           ompi_datatype_t *datatype,
                           MPI_Request * request)
{
     int ret = OMPI_SUCCESS;
     OMPI_MPI_OFFSET_TYPE offset = 0;
     long bytesRequested = 0;
     size_t numofBytes;
     struct mca_sharedfp_base_data_t *sh = NULL;
     mca_sharedfp_base_module_t * shared_fp_base_module = NULL;

     if( NULL == fh->f_sharedfp_data){
	 if ( mca_sharedfp_sm_verbose ) {
	     printf("sharedfp_sm_iwrite - opening the shared file pointer\n");
	 }
	 shared_fp_base_module = fh->f_sharedfp;

	 ret = shared_fp_base_module->sharedfp_file_open(fh->f_comm,
							 fh->f_filename,
							 fh->f_amode,
							 fh->f_info,
							 fh);
	 if ( OMPI_SUCCESS != ret ) {
	     opal_output(0,"sharedfp_sm_iwrite - error opening the shared file pointer\n");
	     return ret;
	 }
    }

    /* Calculate the number of bytes to write */
     opal_datatype_type_size ( &datatype->super, &numofBytes);
     bytesRequested = count * numofBytes;

     /* Retrieve the shared file data struct */
     sh = fh->f_sharedfp_data;

     if ( mca_sharedfp_sm_verbose ) {
	 printf("sharedfp_sm_iwrite: Bytes Requested is %ld\n",bytesRequested);
     }
    /* Request the offset to write bytesRequested bytes */
     ret = mca_sharedfp_sm_request_position(sh,bytesRequested,&offset);

    if ( -1 != ret ) {
	if ( mca_sharedfp_sm_verbose ) {
	    printf("sharedfp_sm_iwrite: Offset received is %lld\n",offset);
	}
        /* Write to the file */
        ret = ompio_io_ompio_file_iwrite_at(sh->sharedfh,offset,buf,count,datatype,request);
    }

    return ret;

}

int mca_sharedfp_sm_write_ordered_begin(mca_io_ompio_file_t *fh,
                                        void *buf,
                                        int count,
                                        struct ompi_datatype_t *datatype)
{
    opal_output(0,"mca_sharedfp_sm_write_ordered_begin: NOT IMPLEMENTED\n");
    return OMPI_ERROR;
}


int mca_sharedfp_sm_write_ordered_end(mca_io_ompio_file_t *fh,
                                      void *buf,
                                      ompi_status_public_t *status)
{
    opal_output(0,"mca_sharedfp_sm_write_ordered_end: NOT IMPLEMENTED\n");
    return OMPI_ERROR;
}
