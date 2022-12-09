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
 * Copyright (c) 2008-2019 University of Houston. All rights reserved.
 * Copyright (c) 2015-2018 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2022-2023 Advanced Micro Devices, Inc. All rights reserved.
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
#include "common_ompio_request.h"
#include "common_ompio_buffer.h"
#include <unistd.h>
#include <math.h>

static int mca_common_ompio_file_write_pipelined (ompio_file_t *fh, const void *buf,
                                                  int count, struct ompi_datatype_t *datatype,
                                                  ompi_status_public_t *status);

static int mca_common_ompio_file_write_default (ompio_file_t *fh, const void *buf,
                                                int count, struct ompi_datatype_t *datatype,
                                                ompi_status_public_t *status);

int mca_common_ompio_file_write (ompio_file_t *fh,
                               const void *buf,
                               int count,
                               struct ompi_datatype_t *datatype,
                               ompi_status_public_t *status)
{
    int ret = OMPI_SUCCESS;
    if (fh->f_amode & MPI_MODE_RDONLY){
        ret = MPI_ERR_READ_ONLY;
        return ret;
    }

    if (0 == count || 0 == fh->f_fview.f_iov_count) {
        if (MPI_STATUS_IGNORE != status) {
            status->_ucount = 0;
        }
        return ret;
    }

    bool need_to_copy = false;
    int is_gpu, is_managed;
    mca_common_ompio_check_gpu_buf (fh, buf, &is_gpu, &is_managed);
    if (is_gpu && !is_managed) {
        need_to_copy = true;
    }

    if ( !(fh->f_flags & OMPIO_DATAREP_NATIVE ) &&
         !(datatype == &ompi_mpi_byte.dt  ||
           datatype == &ompi_mpi_char.dt   )) {
        /* only need to copy if any of these conditions are given:
           1. buffer is an unmanaged device buffer (checked above).
           2. Datarepresentation is anything other than 'native' and
           3. datatype is not byte or char (i.e it does require some actual
              work to be done e.g. for external32.
        */
        need_to_copy = true;
    }         

    if (need_to_copy) {
        return mca_common_ompio_file_write_pipelined (fh, buf, count, datatype, status);
    } else {
        return mca_common_ompio_file_write_default (fh, buf, count, datatype, status);
    }

    return OMPI_SUCCESS; //silence compiler
}

int mca_common_ompio_file_write_default (ompio_file_t *fh,
                                         const void *buf,
                                         int count,
                                         struct ompi_datatype_t *datatype,
                                         ompi_status_public_t *status)
{
    int index = 0;
    int cycles = 0;
    uint32_t iov_count = 0;
    struct iovec *decoded_iov = NULL;
    size_t bytes_per_cycle = 0;
    size_t total_bytes_written = 0;
    size_t max_data = 0, real_bytes_written = 0;
    ssize_t ret_code = 0;
    size_t spc = 0;
    int i = 0; /* index into the decoded iovec of the buffer */

    mca_common_ompio_decode_datatype (fh, datatype, count,
                                      buf, &max_data,
                                      fh->f_mem_convertor,
                                      &decoded_iov, &iov_count);

    bytes_per_cycle = OMPIO_MCA_GET(fh, cycle_buffer_size);
    cycles = ceil((double)max_data/bytes_per_cycle);

    for (index = 0; index < cycles; index++) {
        mca_common_ompio_build_io_array ( &(fh->f_fview), index, cycles,
                                          bytes_per_cycle, max_data,
                                          iov_count, decoded_iov,
                                          &i, &total_bytes_written, &spc,
                                          &fh->f_io_array, &fh->f_num_of_io_entries);
        if (fh->f_num_of_io_entries == 0) {
	    ret_code = 0;
	    goto exit;
	}

	ret_code = fh->f_fbtl->fbtl_pwritev (fh);
	if (0 <= ret_code) {
	    real_bytes_written+= (size_t)ret_code;
	    // Reset ret_code since it is also used to return an error
	    ret_code = 0;
	} else {
	    goto exit;
	}

        fh->f_num_of_io_entries = 0;
        free (fh->f_io_array);
        fh->f_io_array = NULL;
    }

 exit:
    free (decoded_iov);
    if ( MPI_STATUS_IGNORE != status ) {
        status->_ucount = real_bytes_written;
    }

    return ret_code;
}

int mca_common_ompio_file_write_pipelined (ompio_file_t *fh,
                                           const void *buf,
                                           int count,
                                           struct ompi_datatype_t *datatype,
                                           ompi_status_public_t *status)
{
    int index = 0;
    int cycles = 0;

    uint32_t iov_count = 0;
    struct iovec decoded_iov;
    size_t bytes_per_cycle=0, tbw = 0;
    size_t max_data=0, real_bytes_written=0;
    ssize_t ret_code=0;
    size_t spc=0;
    int i = 0; /* index into the decoded iovec of the buffer */

    size_t pos=0;
    char *tbuf1=NULL, *tbuf2=NULL;
    char *packbuf=NULL, *writebuf=NULL;
    mca_ompio_request_t *ompio_req=NULL, *prev_ompio_req=NULL;
    opal_convertor_t convertor;
    bool can_overlap = (NULL != fh->f_fbtl->fbtl_ipwritev);

    bytes_per_cycle = OMPIO_MCA_GET(fh, pipeline_buffer_size);
    OMPIO_PREPARE_BUF (fh, buf, count, datatype, tbuf1, &convertor,
		       max_data, bytes_per_cycle, &decoded_iov, iov_count);
    cycles = ceil((double)max_data/bytes_per_cycle);

    packbuf = tbuf1;
    if (can_overlap) {
        //Allocate second buffer to alternate packing and writing
        tbuf2 = mca_common_ompio_alloc_buf (fh, bytes_per_cycle);
        if (NULL == tbuf2) {
            opal_output(1, "common_ompio: error allocating memory\n");
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        writebuf = tbuf2;
    }

    /*
    ** The code combines two scenarios:
    ** 1. having async write (i.e. ipwritev) which allows to overlap two 
    **    iterations.
    ** 2. not having async write, which doesn't allow for overlap.
    ** 
    ** In the first case we use a double buffering technique, the sequence is
    **    - construct io-array for iter i 
    **    - pack buffer for iter i
    **    - post ipwritev for iter i
    **    - wait for iter i-1
    **    - swap buffers
    **
    ** In the second case, the sequence is
    **    - construct io-array for iter i
    **    - pack buffer i
    **    - post pwrite for iter i
    */
    
    if (can_overlap) {
	mca_common_ompio_register_progress ();	    
    }

    for (index = 0; index <= cycles; index++) {
        if (index < cycles) {
            decoded_iov.iov_base = packbuf;
            decoded_iov.iov_len  = bytes_per_cycle;
            iov_count             = 1;

            opal_convertor_pack (&convertor, &decoded_iov, &iov_count, &pos);
            spc = 0;
            tbw = 0;
            i   = 0;
            mca_common_ompio_build_io_array (&(fh->f_fview), index, cycles,
                                             bytes_per_cycle, pos,
                                             iov_count, &decoded_iov,
                                             &i, &tbw, &spc,
                                             &fh->f_io_array, &fh->f_num_of_io_entries);
	    if (fh->f_num_of_io_entries== 0) {
		ret_code = 0;
		goto exit;
	    }

	    if (can_overlap) {
		mca_common_ompio_request_alloc ( &ompio_req, MCA_OMPIO_REQUEST_WRITE);
		fh->f_fbtl->fbtl_ipwritev (fh, (ompi_request_t *)ompio_req);
	    } else {
		ret_code = fh->f_fbtl->fbtl_pwritev (fh);
		if (0 <= ret_code) {
		    real_bytes_written += (size_t)ret_code;
		    // Reset ret_code since it is also used to return an error
		    ret_code = 0;
		} else {
		    goto exit;
		}
            }
	}
	
        if (can_overlap) {
            if (index != 0) {
                ompi_status_public_t stat;
                ret_code = ompi_request_wait ((ompi_request_t **)&prev_ompio_req, &stat);
                if (OMPI_SUCCESS != ret_code) {
                    goto exit;
                }
                real_bytes_written += stat._ucount;
            }
	    prev_ompio_req = ompio_req;
        }
	    
	fh->f_num_of_io_entries = 0;
	free (fh->f_io_array);
	fh->f_io_array = NULL;
	
	if (can_overlap) {
	    char *tmp = packbuf;
	    packbuf   = writebuf;
	    writebuf  = tmp;
	}
    }

 exit:
    mca_common_ompio_release_buf (fh, tbuf1);
    if (can_overlap) {
        mca_common_ompio_release_buf (fh, tbuf2);
    }

    opal_convertor_cleanup (&convertor);

    if ( MPI_STATUS_IGNORE != status ) {
        status->_ucount = real_bytes_written;
    }

    return ret_code;
}

int mca_common_ompio_file_write_at (ompio_file_t *fh,
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

static void mca_common_ompio_post_next_write_subreq(struct mca_ompio_request_t *req, int index)
{
    uint32_t iov_count = 1;
    size_t bytes_per_cycle = OMPIO_MCA_GET(req->req_fh, pipeline_buffer_size);
    size_t pos=0, spc = 0, tbw = 0;
    int i = 0;
    mca_ompio_request_t *ompio_subreq=NULL;
    struct iovec decoded_iov;

    if (req->req_num_subreqs == index) {
        /* all done */
        return;
    }

    decoded_iov.iov_base = req->req_tbuf;
    decoded_iov.iov_len  = req->req_size;
    opal_convertor_pack (&req->req_convertor, &decoded_iov, &iov_count, &pos);
    mca_common_ompio_build_io_array (req->req_fview, index, req->req_num_subreqs,
                                     bytes_per_cycle, pos,
                                     iov_count, &decoded_iov,
                                     &i, &tbw, &spc,
                                     &req->req_fh->f_io_array,
                                     &req->req_fh->f_num_of_io_entries);

    mca_common_ompio_request_alloc (&ompio_subreq, MCA_OMPIO_REQUEST_WRITE);
    ompio_subreq->req_parent = req;
    req->req_fh->f_fbtl->fbtl_ipwritev (req->req_fh, (ompi_request_t *)ompio_subreq);

    free(req->req_fh->f_io_array);
    req->req_fh->f_io_array = NULL;
    req->req_fh->f_num_of_io_entries = 0;
}

int mca_common_ompio_file_iwrite (ompio_file_t *fh,
                                const void *buf,
                                int count,
                                struct ompi_datatype_t *datatype,
                                ompi_request_t **request)
{
    int ret = OMPI_SUCCESS;
    mca_ompio_request_t *ompio_req=NULL;
    struct iovec *decoded_iov = NULL;
    size_t spc=0;

    if (fh->f_amode & MPI_MODE_RDONLY){
        ret = MPI_ERR_READ_ONLY;
        return ret;
    }

    mca_common_ompio_request_alloc (&ompio_req, MCA_OMPIO_REQUEST_WRITE);

    if (0 == count || 0 == fh->f_fview.f_iov_count) {
        ompio_req->req_ompi.req_status.MPI_ERROR = OMPI_SUCCESS;
        ompio_req->req_ompi.req_status._ucount = 0;
        ompi_request_complete (&ompio_req->req_ompi, false);
        *request = (ompi_request_t *) ompio_req;

        return OMPI_SUCCESS;
    }

    if (NULL != fh->f_fbtl->fbtl_ipwritev) {
        /* This fbtl has support for non-blocking operations */
        uint32_t iov_count = 0;
        size_t max_data = 0;
        size_t total_bytes_written =0;
        int i = 0; /* index into the decoded iovec of the buffer */
        bool need_to_copy = false;

        int is_gpu, is_managed;
        mca_common_ompio_check_gpu_buf ( fh, buf, &is_gpu, &is_managed);
        if ( is_gpu && !is_managed ) {
            need_to_copy = true;
        }
        mca_common_ompio_register_progress ();

        if ( !(fh->f_flags & OMPIO_DATAREP_NATIVE) &&
             !(datatype == &ompi_mpi_byte.dt  ||
               datatype == &ompi_mpi_char.dt) ) {
            /* only need to copy if any of these conditions are given:
               1. buffer is an unmanaged device buffer (checked above).
               2. Datarepresentation is anything other than 'native' and
               3. datatype is not byte or char (i.e it does require some actual
               work to be done e.g. for external32.
            */
            need_to_copy = true;
        }         
        
        if (need_to_copy) {
            OMPI_MPI_OFFSET_TYPE prev_offset;
            size_t pipeline_buf_size = OMPIO_MCA_GET(fh, pipeline_buffer_size);

            OMPIO_PREPARE_BUF (fh, buf, count, datatype, ompio_req->req_tbuf,
                               &ompio_req->req_convertor, max_data,
                               pipeline_buf_size, NULL, iov_count);

            ompio_req->req_num_subreqs = ceil((double)max_data/pipeline_buf_size);
            ompio_req->req_size        = pipeline_buf_size;
            ompio_req->req_max_data    = max_data;
            ompio_req->req_post_next_subreq = mca_common_ompio_post_next_write_subreq;
            ompio_req->req_fh          = fh;
            ompio_req->req_ompi.req_status.MPI_ERROR = MPI_SUCCESS;

	    ompio_req->req_fview = (struct ompio_fview_t *) malloc(sizeof(struct ompio_fview_t));
	    if (NULL == ompio_req->req_fview) {
		opal_output(1, "common_ompio: error allocating memory\n");
		return OMPI_ERR_OUT_OF_RESOURCE;
	    }
            ret = mca_common_ompio_fview_duplicate(ompio_req->req_fview, &fh->f_fview);
            if (OMPI_SUCCESS != ret) {
                return ret;
            }
            mca_common_ompio_file_get_position (fh, &prev_offset );
            mca_common_ompio_post_next_write_subreq (ompio_req, 0);

            /* Move file pointer to the end of the operation.
             * Otherwise posting another I/O operation will start of
             * from the wrong file position. The request will update
             * the position where to write the next chunk of data
             * using its internal copy of the file view and file pointer
             * position.
             */
            mca_common_ompio_set_explicit_offset (fh, prev_offset+max_data);
        }
        else {
            mca_common_ompio_decode_datatype (fh, datatype, count,
                                              buf, &max_data,
                                              fh->f_mem_convertor,
                                              &decoded_iov, &iov_count);

            /**
             * Non blocking operations have to occur in a single cycle
             * If the f_io_array is too long, the fbtl will chunk it up
             * internally.
             */
            mca_common_ompio_build_io_array (&(fh->f_fview), 0, 1, max_data, max_data,
					     iov_count, decoded_iov,
					     &i, &total_bytes_written, &spc,
					     &fh->f_io_array, &fh->f_num_of_io_entries);

            fh->f_fbtl->fbtl_ipwritev (fh, (ompi_request_t *) ompio_req);
        }
    } else {
        // This fbtl does not support non-blocking write operations
        ompi_status_public_t status;
        ret = mca_common_ompio_file_write(fh,buf,count,datatype, &status);
        
        ompio_req->req_ompi.req_status.MPI_ERROR = ret;
        ompio_req->req_ompi.req_status._ucount = status._ucount;
        ompi_request_complete (&ompio_req->req_ompi, false);
    }

    fh->f_num_of_io_entries = 0;
    free (fh->f_io_array);
    fh->f_io_array = NULL;
    free (decoded_iov);

    *request = (ompi_request_t *) ompio_req;
    return ret;
}

int mca_common_ompio_file_iwrite_at (ompio_file_t *fh,
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
int mca_common_ompio_file_write_all (ompio_file_t *fh,
                                     const void *buf,
                                     int count,
                                     struct ompi_datatype_t *datatype,
                                     ompi_status_public_t *status)
{
    int ret = OMPI_SUCCESS;
    
    if ( !( fh->f_flags & OMPIO_DATAREP_NATIVE ) &&
         !(datatype == &ompi_mpi_byte.dt  ||
           datatype == &ompi_mpi_char.dt   )) {
        /* No need to check for GPU buffer for collective I/O.
           Most algorithms first copy data to aggregators, and send/recv
           to/from GPU buffers works if ompi was compiled was GPU support.
           
           If the individual fcoll component is used: there are no aggregators 
           in that concept. However, since they call common_ompio_file_write, 
           device buffers are handled by that routine.

           Thus, we only check for
           1. Datarepresentation is anything other than 'native' and
           2. datatype is not byte or char (i.e it does require some actual
              work to be done e.g. for external32.
        */
        size_t pos=0, max_data=0;
        char *tbuf=NULL;
        opal_convertor_t convertor;
        struct iovec decoded_iov;
        uint32_t iov_count = 0;
        
        OMPIO_PREPARE_BUF (fh, buf, count, datatype, tbuf, &convertor,
			   max_data, 0, &decoded_iov, iov_count);
        opal_convertor_pack (&convertor, &decoded_iov, &iov_count, &pos );
        opal_convertor_cleanup (&convertor);

        ret = fh->f_fcoll->fcoll_file_write_all (fh,
                                                 decoded_iov.iov_base,
                                                 decoded_iov.iov_len,
                                                 MPI_BYTE,
                                                 status);


        mca_common_ompio_release_buf (fh, decoded_iov.iov_base);
    }
    else {
        ret = fh->f_fcoll->fcoll_file_write_all (fh,
                                                 buf,
                                                 count,
                                                 datatype,
                                                 status);
    }
    return ret;
}

int mca_common_ompio_file_write_at_all (ompio_file_t *fh,
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
    ret = mca_common_ompio_file_write_all (fh,
                                           buf,
                                           count,
                                           datatype,
                                           status);
    
    mca_common_ompio_set_explicit_offset (fh, prev_offset);
    return ret;
}

int mca_common_ompio_file_iwrite_all (ompio_file_t *fp,
                                      const void *buf,
                                      int count,
                                      struct ompi_datatype_t *datatype,
                                      ompi_request_t **request)
{
    int ret = OMPI_SUCCESS;

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

    return ret;
}


int mca_common_ompio_file_iwrite_at_all (ompio_file_t *fp,
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
    
    ret = mca_common_ompio_file_iwrite_all ( fp, buf, count, datatype, request );

    mca_common_ompio_set_explicit_offset (fp, prev_offset);
    return ret;
}



/* Helper function used by both read and write operations     */
/**************************************************************/

int mca_common_ompio_build_io_array ( ompio_fview_t *fview, int index, int cycles,
                                      size_t bytes_per_cycle, size_t  max_data, uint32_t iov_count,
                                      struct iovec *decoded_iov, int *ii, size_t *tbw,
                                      size_t *spc, mca_common_ompio_io_array_t **io_array,
                                      int *num_io_entries)
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
    int j = fview->f_index_in_file_view;
    mca_common_ompio_io_array_t *f_io_array=NULL;
    int f_num_io_entries=0;
    
    sum_previous_length = fview->f_position_in_file_view;

    if ((index == cycles-1) && (max_data % bytes_per_cycle)) {
	bytes_to_write_in_cycle = max_data % bytes_per_cycle;
    }
    else {
	bytes_to_write_in_cycle = bytes_per_cycle;
    }

    f_io_array = (mca_common_ompio_io_array_t *)malloc
	(OMPIO_IOVEC_INITIAL_SIZE * sizeof (mca_common_ompio_io_array_t));
    if (NULL == f_io_array) {
	opal_output(1, "OUT OF MEMORY\n");
	return OMPI_ERR_OUT_OF_RESOURCE;
    }

    while (bytes_to_write_in_cycle) {
	/* reallocate if needed  */
	if (OMPIO_IOVEC_INITIAL_SIZE*block <= k) {
	    block ++;
	    f_io_array = (mca_common_ompio_io_array_t *)realloc
		(f_io_array, OMPIO_IOVEC_INITIAL_SIZE *
		 block * sizeof (mca_common_ompio_io_array_t));
	    if (NULL == f_io_array) {
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
	f_io_array[k].memory_address = (IOVBASE_TYPE *)disp;

	if (decoded_iov[i].iov_len -
	    (total_bytes_written - sum_previous_counts) >=
	    bytes_to_write_in_cycle) {
	    f_io_array[k].length = bytes_to_write_in_cycle;
	}
	else {
	    f_io_array[k].length =  decoded_iov[i].iov_len -
		(total_bytes_written - sum_previous_counts);
	}

	if (! (fview->f_flags & OMPIO_CONTIGUOUS_FVIEW)) {
	    if (fview->f_decoded_iov[j].iov_len -
		(fview->f_total_bytes - sum_previous_length) <= 0) {
		sum_previous_length += fview->f_decoded_iov[j].iov_len;
		j = j + 1;
		if (j == (int)fview->f_iov_count) {
		    j = 0;
		    sum_previous_length = 0;
		    fview->f_offset += fview->f_view_extent;
		    fview->f_position_in_file_view = sum_previous_length;
		    fview->f_index_in_file_view = j;
		    fview->f_total_bytes = 0;
		}
	    }
	}

	disp = (ptrdiff_t)fview->f_decoded_iov[j].iov_base +
	    (fview->f_total_bytes - sum_previous_length);
	f_io_array[k].offset = (IOVBASE_TYPE *)(intptr_t)(disp + fview->f_offset);

	if (! (fview->f_flags & OMPIO_CONTIGUOUS_FVIEW)) {
	    if (fview->f_decoded_iov[j].iov_len -
		(fview->f_total_bytes - sum_previous_length)
		< f_io_array[k].length) {
		f_io_array[k].length = fview->f_decoded_iov[j].iov_len -
		    (fview->f_total_bytes - sum_previous_length);
	    }
	}

	total_bytes_written += f_io_array[k].length;
	fview->f_total_bytes += f_io_array[k].length;
	bytes_to_write_in_cycle -= f_io_array[k].length;
	k = k + 1;
    }
    fview->f_position_in_file_view = sum_previous_length;
    fview->f_index_in_file_view = j;
    f_num_io_entries = k;

    *ii = i;
    *tbw = total_bytes_written;
    *spc = sum_previous_counts;
    *io_array = f_io_array;
    *num_io_entries = f_num_io_entries;
    
    return OMPI_SUCCESS;
}

