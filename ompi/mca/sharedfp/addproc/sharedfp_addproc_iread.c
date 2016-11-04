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
#include "sharedfp_addproc.h"

#include "mpi.h"
#include "ompi/constants.h"
#include "ompi/mca/sharedfp/sharedfp.h"
#include "ompi/mca/sharedfp/base/base.h"



int mca_sharedfp_addproc_iread(mca_io_ompio_file_t *fh,
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

    if(NULL == fh->f_sharedfp_data){
        opal_output(0, "sharedfp_addproc_iread - shared file pointer structure not initialized correctly\n");
        return OMPI_ERROR;
    }

    /* Calculate the number of bytes to write */
    opal_datatype_type_size ( &datatype->super ,&numofBytes);
    bytesRequested = count * numofBytes;

    if ( mca_sharedfp_addproc_verbose ){
	opal_output(ompi_sharedfp_base_framework.framework_output,
                    "mca_sharedfp_addproc_iread: Bytes Requested is %ld\n",bytesRequested);
    }
    /* Retrieve the shared file data struct */
    sh = fh->f_sharedfp_data;

    /*Request to the additional process for the offset*/
    ret = mca_sharedfp_addproc_request_position(sh,bytesRequested,&offset);
    offset /= sh->sharedfh->f_etype_size;

    if( OMPI_SUCCESS == ret ){
	if ( mca_sharedfp_addproc_verbose ){
	    opal_output(ompi_sharedfp_base_framework.framework_output,
                        "mca_sharedfp_addproc_iread: Offset received is %lld\n",offset);
	}
        /* Read from the file */
        ret = mca_common_ompio_file_iread_at ( sh->sharedfh, offset, buf, count, datatype, request);
    }

    return ret;
}
int mca_sharedfp_addproc_read_ordered_begin(mca_io_ompio_file_t *fh,
                                       void *buf,
                                       int count,
                                       struct ompi_datatype_t *datatype)
{
    int ret = OMPI_SUCCESS;
    OMPI_MPI_OFFSET_TYPE offset = 0, offsetReceived = 0;
    long sendBuff = 0;
    long *buff=NULL;
    long offsetBuff, bytesRequested = 0;
    size_t numofBytes;
    int rank, size, i;
    struct mca_sharedfp_base_data_t *sh = NULL;

    if(NULL == fh->f_sharedfp_data){
        opal_output(0, "sharedfp_addproc_read_ordered_begin: shared file pointer "
                    "structure not initialized correctly\n");
        return OMPI_ERROR;
    }

    if ( true == fh->f_split_coll_in_use ) {
        opal_output(0, "Only one split collective I/O operation allowed per "
                    "file handle at any given point in time!\n");
        return MPI_ERR_REQUEST;
    }

    /*Retrieve the new communicator*/
    sh = fh->f_sharedfp_data;

    /* Calculate the number of bytes to read*/
    opal_datatype_type_size ( &datatype->super, &numofBytes);
    sendBuff = count * numofBytes;

    /* Get the ranks in the communicator */
    rank = ompi_comm_rank ( sh->comm);
    size = ompi_comm_size ( sh->comm);

    if ( 0  == rank )   {
        buff = (long*)malloc(sizeof(OMPI_MPI_OFFSET_TYPE) * size);
        if ( NULL ==  buff )
            return OMPI_ERR_OUT_OF_RESOURCE;
    }

    ret = sh->comm->c_coll.coll_gather( &sendBuff, 1, OMPI_OFFSET_DATATYPE,
					buff, 1, OMPI_OFFSET_DATATYPE, 0, sh->comm,
					sh->comm->c_coll.coll_gather_module);
    if ( OMPI_SUCCESS != ret ) {
	goto exit;
    }

    /* All the counts are present now in the recvBuff.
       The size of recvBuff is sizeof_newComm
     */
    if ( 0 == rank ) {
        for (i = 0; i < size ; i ++) {
	    if ( mca_sharedfp_addproc_verbose ){
		opal_output(ompi_sharedfp_base_framework.framework_output,
                            "sharedfp_addproc_read_ordered_begin: Buff is %ld\n",buff[i]);
	    }
            bytesRequested += buff[i];

	    if ( mca_sharedfp_addproc_verbose ){
		opal_output(ompi_sharedfp_base_framework.framework_output,
                            "sharedfp_addproc_read_ordered_begin: Bytes requested are %ld\n",
                            bytesRequested);
	    }
	}

        /* Request the offset to read bytesRequested bytes
	** only the root process needs to do the request,
	** since the root process will then tell the other
	** processes at what offset they should read their
	** share of the data.
         */
        ret = mca_sharedfp_addproc_request_position(sh,bytesRequested,&offsetReceived);
        if( OMPI_SUCCESS != ret ){
	    goto exit;
        }
	if ( mca_sharedfp_addproc_verbose ){
	    opal_output(ompi_sharedfp_base_framework.framework_output,
                        "sharedfp_addproc_read_ordered_begin: Offset received is %lld\n",
                        offsetReceived);
	}
        buff[0] += offsetReceived;


        for (i = 1 ; i < size; i++) {
            buff[i] += buff[i-1];
        }
    }

    /* Scatter the results to the other processes*/
    ret = sh->comm->c_coll.coll_scatter ( buff, 1, OMPI_OFFSET_DATATYPE, &offsetBuff,
					  1, OMPI_OFFSET_DATATYPE, 0, sh->comm,
					  sh->comm->c_coll.coll_scatter_module );
    if ( OMPI_SUCCESS != ret ) {
	goto exit;
    }

    /*Each process now has its own individual offset in recvBUFF*/
    offset = offsetBuff - sendBuff;
    offset /= sh->sharedfh->f_etype_size;

    if ( mca_sharedfp_addproc_verbose ){
	opal_output(ompi_sharedfp_base_framework.framework_output,
                    "sharedfp_addproc_read_ordered_begin: Offset returned is %lld\n",offset);
    }

    /* read from the file */
    ret = mca_common_ompio_file_iread_at_all(sh->sharedfh,offset,buf,count,datatype,&fh->f_split_coll_req);
    fh->f_split_coll_in_use = true;

exit:
    if ( NULL != buff ) {
	free ( buff );
    }

    return ret;

}


int mca_sharedfp_addproc_read_ordered_end(mca_io_ompio_file_t *fh,
                                              void *buf,
                                              ompi_status_public_t *status)
{
    int ret = OMPI_SUCCESS;
    ret = ompi_request_wait ( &fh->f_split_coll_req, status );

    /* remove the flag again */
    fh->f_split_coll_in_use = false;
    return ret;
}
