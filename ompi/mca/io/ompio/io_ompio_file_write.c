/*
 *  Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                          University Research and Technology
 *                          Corporation.  All rights reserved.
 *  Copyright (c) 2004-2005 The University of Tennessee and The University
 *                          of Tennessee Research Foundation.  All rights
 *                          reserved.
 *  Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                          University of Stuttgart.  All rights reserved.
 *  Copyright (c) 2004-2005 The Regents of the University of California.
 *                          All rights reserved.
 *  Copyright (c) 2008-2014 University of Houston. All rights reserved.
 *  $COPYRIGHT$
 *  
 *  Additional copyrights may follow
 *  
 *  $HEADER$
 */

#include "ompi_config.h"

#include "ompi/communicator/communicator.h"
#include "ompi/info/info.h"
#include "ompi/file/file.h"
#include "ompi/mca/fs/fs.h"
#include "ompi/mca/fs/base/base.h"
#include "ompi/mca/fcoll/fcoll.h"
#include "ompi/mca/fcoll/base/base.h"
#include "ompi/mca/fbtl/fbtl.h"
#include "ompi/mca/fbtl/base/base.h"
#include "ompi/mca/sharedfp/sharedfp.h"
#include "ompi/mca/sharedfp/base/base.h"

#include "io_ompio.h"
#include "io_ompio_request.h"
#include "math.h"
#include <unistd.h>

/* Read and write routines are split into two interfaces. 
**   The 
**   mca_io_ompio_file_read/write[_at] 
**  
** routines are the ones registered with the ompio modules.
** The
** 
** ompio_io_ompio_file_read/write[_at]
**
** routesin are used e.g. from the shared file pointer modules.
** The main difference is, that the first one takes an ompi_file_t
** as a file pointer argument, while the second uses the ompio internal
** mca_io_ompio_file_t structure.
*/


int mca_io_ompio_file_write (ompi_file_t *fp,
			     void *buf,
			     int count,
			     struct ompi_datatype_t *datatype,
			     ompi_status_public_t *status)
{
    int ret = OMPI_SUCCESS;
    mca_io_ompio_data_t *data;
    mca_io_ompio_file_t *fh;

    data = (mca_io_ompio_data_t *) fp->f_io_selected_data;
    fh = &data->ompio_fh;

    ret = ompio_io_ompio_file_write(fh,buf,count,datatype,status);
    return ret;
}

int ompio_io_ompio_file_write (mca_io_ompio_file_t *fh,
			       void *buf,
			       int count,
			       struct ompi_datatype_t *datatype,
			       ompi_status_public_t *status)
{
    int ret = OMPI_SUCCESS;
    int index = 0;
    int cycles = 0;

    uint32_t iov_count = 0;
    struct iovec *decoded_iov = NULL;
    size_t bytes_per_cycle=0;
    size_t total_bytes_written = 0;
    size_t max_data=0, real_bytes_written=0; 
    ssize_t ret_code=0;
    int i = 0; /* index into the decoded iovec of the buffer */
    int j = 0; /* index into the file view iovec */

    if ( 0 == count ) {
	if ( MPI_STATUS_IGNORE != status ) {
	    status->_ucount = 0;
	}
	return ret;
    }

    ompi_io_ompio_decode_datatype (fh, 
                                   datatype, 
                                   count, 
                                   buf, 
                                   &max_data, 
                                   &decoded_iov, 
                                   &iov_count);

    bytes_per_cycle = mca_io_ompio_cycle_buffer_size;
    cycles = ceil((float)max_data/bytes_per_cycle);

#if 0
    printf ("Bytes per Cycle: %d   Cycles: %d\n", bytes_per_cycle, cycles);
#endif

    j = fh->f_index_in_file_view;
    for (index = 0; index < cycles; index++) {
	mca_io_ompio_build_io_array ( fh, 
					 index, 
					 cycles, 
					 bytes_per_cycle, 
					 max_data, 
					 iov_count, 
					 decoded_iov, 
					 &i, 
					 &j, 
					 &total_bytes_written);

        if (fh->f_num_of_io_entries) {
            ret_code =fh->f_fbtl->fbtl_pwritev (fh);
	    if ( 0<= ret_code ) {
		real_bytes_written+= (size_t)ret_code;
	    }
        }

        fh->f_num_of_io_entries = 0;
        if (NULL != fh->f_io_array) {
            free (fh->f_io_array);
            fh->f_io_array = NULL;
        }
    }

    if (NULL != decoded_iov) {
        free (decoded_iov);
        decoded_iov = NULL;
    }

    if ( MPI_STATUS_IGNORE != status ) {
	status->_ucount = real_bytes_written;
    }

    return ret;
}

int mca_io_ompio_file_write_at (ompi_file_t *fh,
				OMPI_MPI_OFFSET_TYPE offset,
				void *buf,
				int count,
				struct ompi_datatype_t *datatype,
				ompi_status_public_t *status)
{
    int ret = OMPI_SUCCESS;
    mca_io_ompio_data_t *data;

    data = (mca_io_ompio_data_t *) fh->f_io_selected_data;
    ret = ompio_io_ompio_file_write_at (&data->ompio_fh, offset,buf,count,datatype,status);

    return ret;
}

int ompio_io_ompio_file_write_at (mca_io_ompio_file_t *fh,
				  OMPI_MPI_OFFSET_TYPE offset,
				  void *buf,
				  int count,
				  struct ompi_datatype_t *datatype,
				  ompi_status_public_t *status)
{
    int ret = OMPI_SUCCESS;
    OMPI_MPI_OFFSET_TYPE prev_offset;
    ompio_io_ompio_file_get_position (fh, &prev_offset );

    ompi_io_ompio_set_explicit_offset (fh, offset);
    ret = ompio_io_ompio_file_write (fh,
                                     buf,
                                     count,
                                     datatype,
                                     status);
    // An explicit offset file operation is not suppsed to modify
    // the internal file pointer. So reset the pointer
    // to the previous value
    ompi_io_ompio_set_explicit_offset (fh, prev_offset );
    return ret;
}

int mca_io_ompio_file_iwrite (ompi_file_t *fp,
			      void *buf,
			      int count,
			      struct ompi_datatype_t *datatype,
			      ompi_request_t **request)
{
    int ret = OMPI_SUCCESS;
    mca_io_ompio_data_t *data;

    data = (mca_io_ompio_data_t *) fp->f_io_selected_data;
    ret = ompio_io_ompio_file_iwrite(&data->ompio_fh,buf,count,datatype,request);

    return ret;
}

int ompio_io_ompio_file_iwrite (mca_io_ompio_file_t *fh,
				void *buf,
				int count,
				struct ompi_datatype_t *datatype,
				ompi_request_t **request)
{
    int ret = OMPI_SUCCESS;
    mca_ompio_request_t *ompio_req=NULL;

    ompio_req = OBJ_NEW(mca_ompio_request_t);
    ompio_req->req_type = MCA_OMPIO_REQUEST_WRITE;

    if ( 0 == count ) {
	ompi_request_complete (&ompio_req->req_ompi, 0);
	ompio_req->req_ompi.req_status.MPI_ERROR = OMPI_SUCCESS;
	ompio_req->req_ompi.req_status._ucount = 0;
	return OMPI_SUCCESS;
    }

    if ( NULL != fh->f_fbtl->fbtl_ipwritev ) {
	/* This fbtl has support for non-blocking operations */

	uint32_t iov_count = 0;
	struct iovec *decoded_iov = NULL;
	size_t max_data = 0; 
	size_t total_bytes_written =0;
	int i = 0; /* index into the decoded iovec of the buffer */
	int j = 0; /* index into the file vie iovec */

	ompi_io_ompio_decode_datatype (fh, 
				       datatype, 
				       count, 
				       buf, 
				       &max_data, 
				       &decoded_iov, 
				       &iov_count);
	j = fh->f_index_in_file_view;

	/* Non blocking operations have to occur in a single cycle */
	mca_io_ompio_build_io_array ( fh, 
				      0,         // index of current cycle iteration
				      1,         // number of cycles
				      max_data,  // setting bytes_per_cycle to max_data 
				      max_data, 
				      iov_count, 
				      decoded_iov, 
				      &i, 
				      &j, 
				      &total_bytes_written);
	
        if (fh->f_num_of_io_entries) {
            fh->f_fbtl->fbtl_ipwritev (fh, request);
        }
	
        fh->f_num_of_io_entries = 0;
        if (NULL != fh->f_io_array) {
            free (fh->f_io_array);
            fh->f_io_array = NULL;
        }
	if (NULL != decoded_iov) {
	    free (decoded_iov);
	    decoded_iov = NULL;
	}
    }
    else {
	// This fbtl does not support non-blocking write operations
	ompi_status_public_t status;
	ret = ompio_io_ompio_file_write(fh,buf,count,datatype, &status);
	
	ompi_request_complete (&ompio_req->req_ompi, 0);
	ompio_req->req_ompi.req_status.MPI_ERROR = ret;
	ompio_req->req_ompi.req_status._ucount = status._ucount;
    }

    *request = (ompi_request_t *) ompio_req;
    return ret;
}

int mca_io_ompio_file_iwrite_at (ompi_file_t *fh,
				 OMPI_MPI_OFFSET_TYPE offset,
				 void *buf,
				 int count,
				 struct ompi_datatype_t *datatype,
				 ompi_request_t **request)
{
    int ret = OMPI_SUCCESS;
    mca_io_ompio_data_t *data;
        
    data = (mca_io_ompio_data_t *) fh->f_io_selected_data;
    ret = ompio_io_ompio_file_iwrite_at(&data->ompio_fh,offset,buf,count,datatype,request);

    return ret;
}

int ompio_io_ompio_file_iwrite_at (mca_io_ompio_file_t *fh,
				   OMPI_MPI_OFFSET_TYPE offset,
				   void *buf,
				   int count,
				   struct ompi_datatype_t *datatype,
				   ompi_request_t **request)
{
    int ret = OMPI_SUCCESS;
    OMPI_MPI_OFFSET_TYPE prev_offset;
    ompio_io_ompio_file_get_position (fh, &prev_offset );

    ompi_io_ompio_set_explicit_offset (fh, offset);
    ret = ompio_io_ompio_file_iwrite (fh,
                                    buf,
                                    count,
                                    datatype,
                                    request);

    /* An explicit offset file operation is not suppsed to modify
    ** the internal file pointer. So reset the pointer
    ** to the previous value
    ** It is OK to reset the position already here, althgouth 
    ** the operation might still be pending/ongoing, since
    ** the entire array of <offset, length, memaddress> have 
    ** already been constructed in the file_iwrite operation
    */
    ompi_io_ompio_set_explicit_offset (fh, prev_offset);

    return ret;
}

/* Helper function used by both read and write operations     */
/**************************************************************/

int mca_io_ompio_build_io_array ( mca_io_ompio_file_t *fh, int index, int cycles, 
				  size_t bytes_per_cycle, int max_data, uint32_t iov_count, 
				  struct iovec *decoded_iov, int *ii, int *jj, size_t *tbw ) 
{
    OPAL_PTRDIFF_TYPE disp;
    int block = 1;
    size_t total_bytes_written = *tbw;  /* total bytes that have been written*/
    size_t bytes_to_write_in_cycle = 0; /* left to be written in a cycle*/
    size_t sum_previous_counts = 0;
    size_t sum_previous_length = 0;
    int k = 0; /* index into the io_array */
    int i = *ii;
    int j = *jj;
    
    sum_previous_length = fh->f_position_in_file_view;

    if ((index == cycles-1) && (max_data % bytes_per_cycle)) {
	bytes_to_write_in_cycle = max_data % bytes_per_cycle;
    }
    else {
	bytes_to_write_in_cycle = bytes_per_cycle;
    }
    
    fh->f_io_array = (mca_io_ompio_io_array_t *)malloc 
	(OMPIO_IOVEC_INITIAL_SIZE * sizeof (mca_io_ompio_io_array_t));
    if (NULL == fh->f_io_array) {
	opal_output(1, "OUT OF MEMORY\n");
	return OMPI_ERR_OUT_OF_RESOURCE;
    }
    
    while (bytes_to_write_in_cycle) {
	/* reallocate if needed  */
	if (OMPIO_IOVEC_INITIAL_SIZE*block <= k) {
	    block ++;
	    fh->f_io_array = (mca_io_ompio_io_array_t *)realloc 
		(fh->f_io_array, OMPIO_IOVEC_INITIAL_SIZE * 
		 block * sizeof (mca_io_ompio_io_array_t));
	    if (NULL == fh->f_io_array) {
		opal_output(1, "OUT OF MEMORY\n");
		return OMPI_ERR_OUT_OF_RESOURCE;
	    }
	}
	
	if (decoded_iov[i].iov_len - 
	    (total_bytes_written - sum_previous_counts) <= 0) {
	    sum_previous_counts += decoded_iov[i].iov_len;
	    i = i + 1;
	}
	
	disp = (OPAL_PTRDIFF_TYPE)decoded_iov[i].iov_base + 
	    (total_bytes_written - sum_previous_counts);
	fh->f_io_array[k].memory_address = (IOVBASE_TYPE *)disp;
        
	if (decoded_iov[i].iov_len - 
	    (total_bytes_written - sum_previous_counts) >= 
	    bytes_to_write_in_cycle) {
	    fh->f_io_array[k].length = bytes_to_write_in_cycle;
	}
	else {
	    fh->f_io_array[k].length =  decoded_iov[i].iov_len - 
		(total_bytes_written - sum_previous_counts);
	}
	
	if (! (fh->f_flags & OMPIO_CONTIGUOUS_FVIEW)) { 
	    if (fh->f_decoded_iov[j].iov_len - 
		(fh->f_total_bytes - sum_previous_length) <= 0) {
		sum_previous_length += fh->f_decoded_iov[j].iov_len;
		j = j + 1;
		if (j == (int)fh->f_iov_count) {
		    j = 0;
		    sum_previous_length = 0;
		    fh->f_offset += fh->f_view_extent;
		    fh->f_position_in_file_view = sum_previous_length;
		    fh->f_index_in_file_view = j;
		    fh->f_total_bytes = 0;
		}
	    }
	} 
	
	disp = (OPAL_PTRDIFF_TYPE)fh->f_decoded_iov[j].iov_base + 
	    (fh->f_total_bytes - sum_previous_length);
	fh->f_io_array[k].offset = (IOVBASE_TYPE *)(intptr_t)(disp + fh->f_offset);
	
	if (! (fh->f_flags & OMPIO_CONTIGUOUS_FVIEW)) { 
	    if (fh->f_decoded_iov[j].iov_len - 
		(fh->f_total_bytes - sum_previous_length) 
		< fh->f_io_array[k].length) {
		fh->f_io_array[k].length = fh->f_decoded_iov[j].iov_len - 
		    (fh->f_total_bytes - sum_previous_length);
	    }
	}
	
	total_bytes_written += fh->f_io_array[k].length;
	fh->f_total_bytes += fh->f_io_array[k].length;
	bytes_to_write_in_cycle -= fh->f_io_array[k].length;
	k = k + 1;
    }
    fh->f_position_in_file_view = sum_previous_length;
    fh->f_index_in_file_view = j;
    fh->f_num_of_io_entries = k;
    
#if 0
    if (fh->f_rank == 0) {
	int d;
	printf("*************************** %d\n", fh->f_num_of_io_entries);
	
	for (d=0 ; d<fh->f_num_of_io_entries ; d++) {
	    printf(" ADDRESS: %p  OFFSET: %p   LENGTH: %d\n",
		   fh->f_io_array[d].memory_address,
		   fh->f_io_array[d].offset,
		   fh->f_io_array[d].length);
	}
    }
#endif
    *ii = i;
    *jj = j;
    *tbw = total_bytes_written;

    return OMPI_SUCCESS;
}

/* Collective operations                                          */
/******************************************************************/

int mca_io_ompio_file_write_all (ompi_file_t *fh,
				 void *buf,
				 int count,
				 struct ompi_datatype_t *datatype,
				 ompi_status_public_t *status)
{
    int ret = OMPI_SUCCESS;
    mca_io_ompio_data_t *data;

    data = (mca_io_ompio_data_t *) fh->f_io_selected_data;

    ret = data->ompio_fh.
        f_fcoll->fcoll_file_write_all (&data->ompio_fh, 
                                       buf, 
                                       count, 
                                       datatype,
                                       status);
    
    if ( MPI_STATUS_IGNORE != status ) {
	size_t size;

	opal_datatype_type_size (&datatype->super, &size);
	status->_ucount = count * size;
    }

    return ret;
}

int mca_io_ompio_file_write_at_all (ompi_file_t *fh,
				    OMPI_MPI_OFFSET_TYPE offset,
				    void *buf,
				    int count,
				    struct ompi_datatype_t *datatype,
				    ompi_status_public_t *status)
{
    int ret = OMPI_SUCCESS;
    mca_io_ompio_data_t *data;

    data = (mca_io_ompio_data_t *) fh->f_io_selected_data;
    ret = ompio_io_ompio_file_write_at_all(&data->ompio_fh,offset,buf,count,datatype,status);

    return ret;
}

int ompio_io_ompio_file_write_at_all (mca_io_ompio_file_t *fh,
				      OMPI_MPI_OFFSET_TYPE offset,
				      void *buf,
				      int count,
				      struct ompi_datatype_t *datatype,
				      ompi_status_public_t *status)
{
    int ret = OMPI_SUCCESS;
    OMPI_MPI_OFFSET_TYPE prev_offset;
    ompio_io_ompio_file_get_position (fh, &prev_offset );

    ompi_io_ompio_set_explicit_offset (fh, offset);
    ret = fh->f_fcoll->fcoll_file_write_all (fh,
                                             buf,
                                             count,
                                             datatype,
                                             status);

    ompi_io_ompio_set_explicit_offset (fh, prev_offset);
    return ret;
}


/* Infrastructure for shared file pointer operations */
/* (Individual and collective */
/******************************************************/

int mca_io_ompio_file_write_shared (ompi_file_t *fp,
				    void *buf,
				    int count,
				    struct ompi_datatype_t *datatype,
				    ompi_status_public_t * status)
{
    int ret = OMPI_SUCCESS;
    mca_io_ompio_data_t *data;
    mca_io_ompio_file_t *fh;
    mca_sharedfp_base_module_t * shared_fp_base_module;

    data = (mca_io_ompio_data_t *) fp->f_io_selected_data;
    fh = &data->ompio_fh;

    /*get the shared fp module associated with this file*/
    shared_fp_base_module = fh->f_sharedfp;
    if ( NULL == shared_fp_base_module ){
        opal_output(0, "No shared file pointer component found for this communicator. Can not execute\n");
        return OMPI_ERROR;
    }
    ret = shared_fp_base_module->sharedfp_write(fh,buf,count,datatype,status);

    return ret;
}

int mca_io_ompio_file_iwrite_shared (ompi_file_t *fp,
				     void *buf,
				     int count,
				     struct ompi_datatype_t *datatype,
				     ompi_request_t **request)
{
    int ret = OMPI_SUCCESS;
    mca_io_ompio_data_t *data;
    mca_io_ompio_file_t *fh;
    mca_sharedfp_base_module_t * shared_fp_base_module;

    data = (mca_io_ompio_data_t *) fp->f_io_selected_data;
    fh = &data->ompio_fh;

    /*get the shared fp module associated with this file*/
    shared_fp_base_module = fh->f_sharedfp;
    if ( NULL == shared_fp_base_module ){
        opal_output(0, "No shared file pointer component found for this communicator. Can not execute\n");
        return OMPI_ERROR;
    }
    ret = shared_fp_base_module->sharedfp_iwrite(fh,buf,count,datatype,request);

    return ret;
}

int mca_io_ompio_file_write_ordered (ompi_file_t *fp,
				     void *buf,
				     int count,
				     struct ompi_datatype_t *datatype,
				     ompi_status_public_t * status)
{
    int ret = OMPI_SUCCESS;
    mca_io_ompio_data_t *data;
    mca_io_ompio_file_t *fh;
    mca_sharedfp_base_module_t * shared_fp_base_module;

    data = (mca_io_ompio_data_t *) fp->f_io_selected_data;
    fh = &data->ompio_fh;

    /*get the shared fp module associated with this file*/
    shared_fp_base_module = fh->f_sharedfp;
    if ( NULL == shared_fp_base_module ){
        opal_output(0,"No shared file pointer component found for this communicator. Can not execute\n");
        return OMPI_ERROR;
    }
    ret = shared_fp_base_module->sharedfp_write_ordered(fh,buf,count,datatype,status);

    return ret;
}

int mca_io_ompio_file_write_ordered_begin (ompi_file_t *fp,
					   void *buf,
					   int count,
					   struct ompi_datatype_t *datatype)
{
    int ret = OMPI_SUCCESS;
    mca_io_ompio_data_t *data;
    mca_io_ompio_file_t *fh;
    mca_sharedfp_base_module_t * shared_fp_base_module;

    data = (mca_io_ompio_data_t *) fp->f_io_selected_data;
    fh = &data->ompio_fh;

    /*get the shared fp module associated with this file*/
    shared_fp_base_module = fh->f_sharedfp;
    if ( NULL == shared_fp_base_module ){
        opal_output(0, "No shared file pointer component found for this communicator. Can not execute\n");
	return OMPI_ERROR;
    }
    ret = shared_fp_base_module->sharedfp_write_ordered_begin(fh,buf,count,datatype);

    return ret;
}

int mca_io_ompio_file_write_ordered_end (ompi_file_t *fp,
					 void *buf,
					 ompi_status_public_t *status)
{
    int ret = OMPI_SUCCESS;
    mca_io_ompio_data_t *data;
    mca_io_ompio_file_t *fh;
    mca_sharedfp_base_module_t * shared_fp_base_module;

    data = (mca_io_ompio_data_t *) fp->f_io_selected_data;
    fh = &data->ompio_fh;

    /*get the shared fp module associated with this file*/
    shared_fp_base_module = fh->f_sharedfp;
    if ( NULL == shared_fp_base_module ){
        opal_output(0, "No shared file pointer component found for this communicator. Can not execute\n");
	return OMPI_ERROR;
    }
    ret = shared_fp_base_module->sharedfp_write_ordered_end(fh,buf,status);

    return ret;
}


/* Split collectives . Not really used but infrastructure is in place */
/**********************************************************************/
int mca_io_ompio_file_write_at_all_begin (ompi_file_t *fh,
					  OMPI_MPI_OFFSET_TYPE offset,
					  void *buf,
					  int count,
					  struct ompi_datatype_t *datatype)
{
    int ret = OMPI_SUCCESS;
    mca_io_ompio_data_t *data;

    data = (mca_io_ompio_data_t *) fh->f_io_selected_data;
    ret = ompio_io_ompio_file_write_at_all_begin(&data->ompio_fh,offset,buf,count,datatype);

    return ret;
}

int ompio_io_ompio_file_write_at_all_begin (mca_io_ompio_file_t *fh,
					    OMPI_MPI_OFFSET_TYPE offset,
					    void *buf,
					    int count,
					    struct ompi_datatype_t *datatype)
{
    int ret = OMPI_SUCCESS;
    OMPI_MPI_OFFSET_TYPE prev_offset;
    ompio_io_ompio_file_get_position (fh, &prev_offset );

    ompi_io_ompio_set_explicit_offset (fh, offset);
    ret = fh->f_fcoll->fcoll_file_write_all_begin (fh,
                                                   buf,
                                                   count,
                                                   datatype);

    /* It is OK to reset the position already here, althgouth 
    ** the operation might still be pending/ongoing, since
    ** the entire array of <offset, length, memaddress> have 
    ** already been constructed in the file_write_all_begin operation
    */
    ompi_io_ompio_set_explicit_offset (fh, prev_offset);
    return ret;
}

int mca_io_ompio_file_write_at_all_end (ompi_file_t *fh,
					void *buf,
					ompi_status_public_t * status)
{
    int ret = OMPI_SUCCESS;
    mca_io_ompio_data_t *data;

    data = (mca_io_ompio_data_t *) fh->f_io_selected_data;
    ret = ompio_io_ompio_file_write_at_all_end(&data->ompio_fh,buf,status);

    return ret;
}

int ompio_io_ompio_file_write_at_all_end (mca_io_ompio_file_t *fh,
					  void *buf,
					  ompi_status_public_t * status)
{
    int ret = OMPI_SUCCESS;
    
    ret = fh->f_fcoll->fcoll_file_write_all_end (fh,
                                                 buf,
                                                 status);

    return ret;
}

int mca_io_ompio_file_write_all_begin (ompi_file_t *fh,
				       void *buf,
				       int count,
				       struct ompi_datatype_t *datatype)
{
    int ret = OMPI_SUCCESS;
    mca_io_ompio_data_t *data;

    data = (mca_io_ompio_data_t *) fh->f_io_selected_data;
    ret = data->ompio_fh.
        f_fcoll->fcoll_file_write_all_begin (&data->ompio_fh, 
                                            buf, 
                                            count,
                                            datatype);

    return ret;
}

int mca_io_ompio_file_write_all_end (ompi_file_t *fh,
				     void *buf,
				     ompi_status_public_t *status)
{
    int ret = OMPI_SUCCESS;
    mca_io_ompio_data_t *data;

    data = (mca_io_ompio_data_t *) fh->f_io_selected_data;
    ret = data->ompio_fh.
        f_fcoll->fcoll_file_write_all_end (&data->ompio_fh, 
                                          buf, 
                                          status);
    
    return ret;
}

