/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2016 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008-2016 University of Houston. All rights reserved.
 * Copyright (c) 2015-2017 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/communicator/communicator.h"
#include "ompi/info/info.h"
#include "ompi/file/file.h"
#include "ompi/mca/fcoll/fcoll.h"
#include "ompi/mca/fcoll/base/base.h"
#include "ompi/mca/fbtl/fbtl.h"
#include "ompi/mca/fbtl/base/base.h"

#include "common_ompio.h"
#include "ompi/mca/io/ompio/io_ompio.h"
#include "ompi/mca/io/ompio/io_ompio_request.h"
#include "math.h"
#include <unistd.h>

int mca_common_ompio_file_write (mca_io_ompio_file_t *fh,
			       const void *buf,
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
    size_t spc=0;
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

    if ( -1 == mca_io_ompio_cycle_buffer_size ) {
	bytes_per_cycle = max_data;
    }
    else {
	bytes_per_cycle = mca_io_ompio_cycle_buffer_size;
    }
    cycles = ceil((float)max_data/bytes_per_cycle);

#if 0
    printf ("Bytes per Cycle: %d   Cycles: %d\n", bytes_per_cycle, cycles);
#endif

    j = fh->f_index_in_file_view;
    for (index = 0; index < cycles; index++) {
	mca_common_ompio_build_io_array ( fh,
                                          index,
                                          cycles,
                                          bytes_per_cycle,
                                          max_data,
                                          iov_count,
                                          decoded_iov,
                                          &i,
                                          &j,
                                          &total_bytes_written, 
                                          &spc);

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

int mca_common_ompio_file_write_at (mca_io_ompio_file_t *fh,
				  OMPI_MPI_OFFSET_TYPE offset,
				  const void *buf,
				  int count,
				  struct ompi_datatype_t *datatype,
				  ompi_status_public_t *status)
{
    int ret = OMPI_SUCCESS;
    OMPI_MPI_OFFSET_TYPE prev_offset;
    mca_common_ompio_file_get_position (fh, &prev_offset );

    mca_common_ompio_set_explicit_offset (fh, offset);
    ret = mca_common_ompio_file_write (fh,
                                     buf,
                                     count,
                                     datatype,
                                     status);
    // An explicit offset file operation is not suppsed to modify
    // the internal file pointer. So reset the pointer
    // to the previous value
    mca_common_ompio_set_explicit_offset (fh, prev_offset );
    return ret;
}

int mca_common_ompio_file_iwrite (mca_io_ompio_file_t *fh,
				const void *buf,
				int count,
				struct ompi_datatype_t *datatype,
				ompi_request_t **request)
{
    int ret = OMPI_SUCCESS;
    mca_ompio_request_t *ompio_req=NULL;
    size_t spc=0;

    ompio_req = OBJ_NEW(mca_ompio_request_t);
    ompio_req->req_type = MCA_OMPIO_REQUEST_WRITE;
    ompio_req->req_ompi.req_state = OMPI_REQUEST_ACTIVE;

  if ( 0 == count ) {
	ompio_req->req_ompi.req_status.MPI_ERROR = OMPI_SUCCESS;
	ompio_req->req_ompi.req_status._ucount = 0;
	ompi_request_complete (&ompio_req->req_ompi, false);
        *request = (ompi_request_t *) ompio_req;

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
	mca_common_ompio_build_io_array ( fh,
                                          0,         // index of current cycle iteration
                                          1,         // number of cycles
                                          max_data,  // setting bytes_per_cycle to max_data
                                          max_data,
                                          iov_count,
                                          decoded_iov,
                                          &i,
                                          &j,
                                          &total_bytes_written, 
                                          &spc);
        
        if (fh->f_num_of_io_entries) {
	  fh->f_fbtl->fbtl_ipwritev (fh, (ompi_request_t *) ompio_req);
        }

	if ( false == mca_io_ompio_progress_is_registered ) {
	    // Lazy initialization of progress function to minimize impact
	    // on other ompi functionality in case its not used.
	    opal_progress_register (mca_io_ompio_component_progress);
	    mca_io_ompio_progress_is_registered=true;
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
	ret = mca_common_ompio_file_write(fh,buf,count,datatype, &status);

	ompio_req->req_ompi.req_status.MPI_ERROR = ret;
	ompio_req->req_ompi.req_status._ucount = status._ucount;
	ompi_request_complete (&ompio_req->req_ompi, false);
    }

    *request = (ompi_request_t *) ompio_req;
    return ret;
}

int mca_common_ompio_file_iwrite_at (mca_io_ompio_file_t *fh,
				   OMPI_MPI_OFFSET_TYPE offset,
				   const void *buf,
				   int count,
				   struct ompi_datatype_t *datatype,
				   ompi_request_t **request)
{
    int ret = OMPI_SUCCESS;
    OMPI_MPI_OFFSET_TYPE prev_offset;
    mca_common_ompio_file_get_position (fh, &prev_offset );

    mca_common_ompio_set_explicit_offset (fh, offset);
    ret = mca_common_ompio_file_iwrite (fh,
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
    mca_common_ompio_set_explicit_offset (fh, prev_offset);

    return ret;
}

/* Collective operations                                          */
/******************************************************************/

int mca_common_ompio_file_write_at_all (mca_io_ompio_file_t *fh,
				      OMPI_MPI_OFFSET_TYPE offset,
				      const void *buf,
				      int count,
				      struct ompi_datatype_t *datatype,
				      ompi_status_public_t *status)
{
    int ret = OMPI_SUCCESS;
    OMPI_MPI_OFFSET_TYPE prev_offset;
    mca_common_ompio_file_get_position (fh, &prev_offset );

    mca_common_ompio_set_explicit_offset (fh, offset);
    ret = fh->f_fcoll->fcoll_file_write_all (fh,
                                             buf,
                                             count,
                                             datatype,
                                             status);

    mca_common_ompio_set_explicit_offset (fh, prev_offset);
    return ret;
}

int mca_common_ompio_file_iwrite_at_all (mca_io_ompio_file_t *fp,
				       OMPI_MPI_OFFSET_TYPE offset,
				       const void *buf,
				       int count,
				       struct ompi_datatype_t *datatype,
				       ompi_request_t **request)
{

    int ret = OMPI_SUCCESS;
    OMPI_MPI_OFFSET_TYPE prev_offset;

    mca_common_ompio_file_get_position (fp, &prev_offset );

    mca_common_ompio_set_explicit_offset (fp, offset);

    if ( NULL != fp->f_fcoll->fcoll_file_iwrite_all ) {
	ret = fp->f_fcoll->fcoll_file_iwrite_all (fp,
						  buf,
						  count,
						  datatype,
						  request);
    }
    else {
	/* this fcoll component does not support non-blocking
	   collective I/O operations. WE fake it with
	   individual non-blocking I/O operations. */
	ret = mca_common_ompio_file_iwrite ( fp, buf, count, datatype, request );
    }

    mca_common_ompio_set_explicit_offset (fp, prev_offset);
    return ret;
}


/* Helper function used by both read and write operations     */
/**************************************************************/

int mca_common_ompio_build_io_array ( mca_io_ompio_file_t *fh, int index, int cycles,
                                      size_t bytes_per_cycle, int max_data, uint32_t iov_count,
                                      struct iovec *decoded_iov, int *ii, int *jj, size_t *tbw, 
                                      size_t *spc)
{
    ptrdiff_t disp;
    int block = 1;
    size_t total_bytes_written = *tbw;  /* total bytes that have been written*/
    size_t bytes_to_write_in_cycle = 0; /* left to be written in a cycle*/
    size_t sum_previous_counts = *spc;  /* total bytes used, up to the start
                                           of the memory block decoded_iov[*ii];
                                           is always less or equal to tbw */
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

	disp = (ptrdiff_t)decoded_iov[i].iov_base +
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

	disp = (ptrdiff_t)fh->f_decoded_iov[j].iov_base +
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
	    printf(" ADDRESS: %p  OFFSET: %p   LENGTH: %d prev_count=%ld prev_length=%ld\n",
		   fh->f_io_array[d].memory_address,
		   fh->f_io_array[d].offset,
		   fh->f_io_array[d].length, 
                   sum_previous_counts, sum_previous_length);
	}
    }
#endif
    *ii = i;
    *jj = j;
    *tbw = total_bytes_written;
    *spc = sum_previous_counts;

    return OMPI_SUCCESS;
}

