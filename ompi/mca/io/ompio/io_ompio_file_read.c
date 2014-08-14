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

#include "io_ompio.h"
#include "math.h"

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

int mca_io_ompio_file_read (ompi_file_t *fp,
			    void *buf,
			    int count,
			    struct ompi_datatype_t *datatype,
			    ompi_status_public_t *status)
{
    int ret = OMPI_SUCCESS;
    mca_io_ompio_data_t *data;

    data = (mca_io_ompio_data_t *) fp->f_io_selected_data;
    ret = ompio_io_ompio_file_read(&data->ompio_fh,buf,count,datatype,status);

    return ret;
}

int ompio_io_ompio_file_read (mca_io_ompio_file_t *fh,
			      void *buf,
			      int count,
			      struct ompi_datatype_t *datatype,
			      ompi_status_public_t *status)
{
    int ret = OMPI_SUCCESS;

    size_t total_bytes_read = 0;       /* total bytes that have been read*/
    size_t bytes_per_cycle = 0;        /* total read in each cycle by each process*/
    int index = 0;
    int cycles = 0;

    uint32_t iov_count = 0;
    struct iovec *decoded_iov = NULL;

    size_t max_data = 0; 
    int i = 0; /* index into the decoded iovec of the buffer */
    int j = 0; /* index into the file vie iovec */

    if ( 0 == count ) {
	if ( MPI_STATUS_IGNORE != status ) {
	    status->_ucount = 0;
	}
	return ret;
    }

    if (fh->f_amode & MPI_MODE_WRONLY){
      printf("Improper use of FILE Mode, Using WRONLY for Read!\n");
      ret = OMPI_ERROR;
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
    printf ("Bytes per Cycle: %d   Cycles: %d\n",bytes_per_cycle, cycles);
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
				      &total_bytes_read);
	
        if (fh->f_num_of_io_entries) {
            fh->f_fbtl->fbtl_preadv (fh, NULL);
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
	status->_ucount = max_data;
    }

    return ret;
}

int mca_io_ompio_file_read_at (ompi_file_t *fh,
			       OMPI_MPI_OFFSET_TYPE offset,
			       void *buf,
			       int count,
			       struct ompi_datatype_t *datatype,
			       ompi_status_public_t * status)
{
    int ret = OMPI_SUCCESS;
    mca_io_ompio_data_t *data;

    data = (mca_io_ompio_data_t *) fh->f_io_selected_data;
    ret = ompio_io_ompio_file_read_at(&data->ompio_fh, offset,buf,count,datatype,status);

    return ret;
}

int ompio_io_ompio_file_read_at (mca_io_ompio_file_t *fh,
				 OMPI_MPI_OFFSET_TYPE offset,
				 void *buf,
				 int count,
				 struct ompi_datatype_t *datatype,
				 ompi_status_public_t * status)
{
    int ret = OMPI_SUCCESS;

    ompi_io_ompio_set_explicit_offset (fh, offset);
    ret = ompio_io_ompio_file_read (fh,
				    buf,
				    count,
				    datatype,
				    status);
    return ret;
}


int mca_io_ompio_file_iread (ompi_file_t *fh,
			     void *buf,
			     int count,
			     struct ompi_datatype_t *datatype,
			     ompi_request_t **request)
{
    int ret = OMPI_SUCCESS;
    mca_io_ompio_data_t *data;

    data = (mca_io_ompio_data_t *) fh->f_io_selected_data;
    ret = ompio_io_ompio_file_iread(&data->ompio_fh,buf,count,datatype,request);

    return ret;
}


int ompio_io_ompio_file_iread (mca_io_ompio_file_t *fh,
			       void *buf,
			       int count,
			       struct ompi_datatype_t *datatype,
			       ompi_request_t **request)
{
    int ret = OMPI_SUCCESS;

    if ( NULL != fh->f_fbtl->fbtl_ipreadv ) {
	// This fbtl has support for non-blocking operations

	size_t total_bytes_read = 0;       /* total bytes that have been read*/
	uint32_t iov_count = 0;
	struct iovec *decoded_iov = NULL;
	
	size_t max_data = 0; 
	int i = 0; /* index into the decoded iovec of the buffer */
	int j = 0; /* index into the file vie iovec */
	
	if ( 0 == count ) {
//	    if ( MPI_STATUS_IGNORE != status ) {
//		status->_ucount = 0;
//	    }
//	    return ret;
	}
	
	ompi_io_ompio_decode_datatype (fh, 
				       datatype, 
				       count, 
				       buf, 
				       &max_data, 
				       &decoded_iov, 
				       &iov_count);
	
	// Non-blocking operations have to occur in a single cycle
	j = fh->f_index_in_file_view;
	
	mca_io_ompio_build_io_array ( fh, 
				      0,         // index 
				      1,         // no. of cyces 
				      max_data,  // setting bytes per cycle to match data 
				      max_data, 
				      iov_count, 
				      decoded_iov, 
				      &i, 
				      &j, 
				      &total_bytes_read);
	    
	if (fh->f_num_of_io_entries) {
	    fh->f_fbtl->fbtl_ipreadv (fh, NULL, request);
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
	
//	if ( MPI_STATUS_IGNORE != status ) {
//	    status->_ucount = max_data;
//	}
    }
    else {
	// This fbtl does not  support non-blocking operations
	ompio_io_ompio_file_read (fh, buf, count, datatype, MPI_STATUS_IGNORE);
    }

    return ret;
}

int mca_io_ompio_file_iread_at (ompi_file_t *fh,
				OMPI_MPI_OFFSET_TYPE offset,
				void *buf,
				int count,
				struct ompi_datatype_t *datatype,
				ompi_request_t **request)
{
    int ret = OMPI_SUCCESS;
    mca_io_ompio_data_t *data;

    data = (mca_io_ompio_data_t *) fh->f_io_selected_data;
    ret = ompio_io_ompio_file_iread_at(&data->ompio_fh,offset,buf,count,datatype,request);

    return ret;
}

int ompio_io_ompio_file_iread_at (mca_io_ompio_file_t *fh,
				  OMPI_MPI_OFFSET_TYPE offset,
				  void *buf,
				  int count,
				  struct ompi_datatype_t *datatype,
				  ompi_request_t **request)
{
    int ret = OMPI_SUCCESS;

    ompi_io_ompio_set_explicit_offset (fh, offset);
    ret = ompio_io_ompio_file_iread (fh,
				    buf,
				    count,
				    datatype,
				    request);
    return ret;
}


/* Infrastructure for collective operations  */
/******************************************************/
int mca_io_ompio_file_read_all (ompi_file_t *fh,
				void *buf,
				int count,
				struct ompi_datatype_t *datatype,
				ompi_status_public_t * status)
{
    int ret = OMPI_SUCCESS;
    mca_io_ompio_data_t *data;

    data = (mca_io_ompio_data_t *) fh->f_io_selected_data;

    ret = data->ompio_fh.
        f_fcoll->fcoll_file_read_all (&data->ompio_fh, 
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


int mca_io_ompio_file_read_at_all (ompi_file_t *fh,
				   OMPI_MPI_OFFSET_TYPE offset,
				   void *buf,
				   int count,
				   struct ompi_datatype_t *datatype,
				   ompi_status_public_t * status)
{
    int ret = OMPI_SUCCESS;
    mca_io_ompio_data_t *data;

    data = (mca_io_ompio_data_t *) fh->f_io_selected_data;
    ret = ompio_io_ompio_file_read_at_all(&data->ompio_fh,offset,buf,count,datatype,status);

    return ret;
}

int ompio_io_ompio_file_read_at_all (mca_io_ompio_file_t *fh,
				     OMPI_MPI_OFFSET_TYPE offset,
				     void *buf,
				     int count,
				     struct ompi_datatype_t *datatype,
				     ompi_status_public_t * status)
{
    int ret = OMPI_SUCCESS;

    ompi_io_ompio_set_explicit_offset (fh, offset);
    ret = fh->f_fcoll->fcoll_file_read_all (fh,
                                            buf,
                                            count,
                                            datatype,
                                            status);
    return ret;
}


/* Infrastructure for shared file pointer operations 
** (individual and ordered)*/
/******************************************************/
int mca_io_ompio_file_read_shared (ompi_file_t *fp,
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
    shared_fp_base_module = (mca_sharedfp_base_module_t *)(fh->f_sharedfp);
    ret = shared_fp_base_module->sharedfp_read(fh,buf,count,datatype,status);

    return ret;
}

int mca_io_ompio_file_iread_shared (ompi_file_t *fh,
				    void *buf,
				    int count,
				    struct ompi_datatype_t *datatype,
				    ompi_request_t **request)
{
    int ret = OMPI_SUCCESS;
    mca_io_ompio_data_t *data;
    mca_io_ompio_file_t *ompio_fh;
    mca_sharedfp_base_module_t * shared_fp_base_module;

    data = (mca_io_ompio_data_t *) fh->f_io_selected_data;
    ompio_fh = &data->ompio_fh;

    /*get the shared fp module associated with this file*/
    shared_fp_base_module = (mca_sharedfp_base_module_t *)(ompio_fh->f_sharedfp);
    ret = shared_fp_base_module->sharedfp_iread(ompio_fh,buf,count,datatype,request);

    return ret;
}

int mca_io_ompio_file_read_ordered (ompi_file_t *fh,
				    void *buf,
				    int count,
				    struct ompi_datatype_t *datatype,
				    ompi_status_public_t * status)
{
    int ret = OMPI_SUCCESS;
    mca_io_ompio_data_t *data;
    mca_io_ompio_file_t *ompio_fh;
    mca_sharedfp_base_module_t * shared_fp_base_module;

    data = (mca_io_ompio_data_t *) fh->f_io_selected_data;
    ompio_fh = &data->ompio_fh;

    /*get the shared fp module associated with this file*/
    shared_fp_base_module = (mca_sharedfp_base_module_t *)(ompio_fh->f_sharedfp);
    ret = shared_fp_base_module->sharedfp_read_ordered(ompio_fh,buf,count,datatype,status);

    return ret;
}

int mca_io_ompio_file_read_ordered_begin (ompi_file_t *fh,
					  void *buf,
					  int count,
					  struct ompi_datatype_t *datatype)
{
    int ret = OMPI_SUCCESS;
    mca_io_ompio_data_t *data;
    mca_io_ompio_file_t *ompio_fh;
    mca_sharedfp_base_module_t * shared_fp_base_module;

    data = (mca_io_ompio_data_t *) fh->f_io_selected_data;
    ompio_fh = &data->ompio_fh;

    /*get the shared fp module associated with this file*/
    shared_fp_base_module = ompio_fh->f_sharedfp;
    ret = shared_fp_base_module->sharedfp_read_ordered_begin(ompio_fh,buf,count,datatype);

    return ret;
}

int mca_io_ompio_file_read_ordered_end (ompi_file_t *fh,
					void *buf,
					ompi_status_public_t * status)
{
    int ret = OMPI_SUCCESS;
    mca_io_ompio_data_t *data;
    mca_io_ompio_file_t *ompio_fh;
    mca_sharedfp_base_module_t * shared_fp_base_module;

    data = (mca_io_ompio_data_t *) fh->f_io_selected_data;
    ompio_fh = &data->ompio_fh;

    /*get the shared fp module associated with this file*/
    shared_fp_base_module = ompio_fh->f_sharedfp;
    ret = shared_fp_base_module->sharedfp_read_ordered_end(ompio_fh,buf,status);

    return ret;
}


/* Split collectives . Not really used but infrastructure is in place */
/**********************************************************************/
int mca_io_ompio_file_read_all_begin (ompi_file_t *fh,
				      void *buf,
				      int count,
				      struct ompi_datatype_t *datatype)
{
    int ret = OMPI_SUCCESS;
    mca_io_ompio_data_t *data;

    data = (mca_io_ompio_data_t *) fh->f_io_selected_data;

    ret = data->ompio_fh.
        f_fcoll->fcoll_file_read_all_begin (&data->ompio_fh, 
                                           buf, 
                                           count,
                                           datatype);

    return ret;
}

int mca_io_ompio_file_read_all_end (ompi_file_t *fh,
				    void *buf,
				    ompi_status_public_t * status)
{
    int ret = OMPI_SUCCESS;
    mca_io_ompio_data_t *data;

    data = (mca_io_ompio_data_t *) fh->f_io_selected_data;

    ret = data->ompio_fh.
        f_fcoll->fcoll_file_read_all_end (&data->ompio_fh, 
                                         buf, 
                                         status);

    return ret;
}

int mca_io_ompio_file_read_at_all_begin (ompi_file_t *fh,
					 OMPI_MPI_OFFSET_TYPE offset,
					 void *buf,
					 int count,
					 struct ompi_datatype_t *datatype)
{
    int ret = OMPI_SUCCESS;
    mca_io_ompio_data_t *data;

    data = (mca_io_ompio_data_t *) fh->f_io_selected_data;
    ret = ompio_io_ompio_file_read_at_all_begin(&data->ompio_fh,offset,buf,count,datatype);

    return ret;
}

int ompio_io_ompio_file_read_at_all_begin (mca_io_ompio_file_t *ompio_fh,
					   OMPI_MPI_OFFSET_TYPE offset,
					   void *buf,
					   int count,
					   struct ompi_datatype_t *datatype)
{
    int ret = OMPI_SUCCESS;

    ompi_io_ompio_set_explicit_offset (ompio_fh, offset);
    ret = ompio_fh->f_fcoll->fcoll_file_read_all_begin (ompio_fh,
                                                        buf,
                                                        count,
                                                        datatype);

    return ret;
}

int mca_io_ompio_file_read_at_all_end (ompi_file_t *fh,
				       void *buf,
				       ompi_status_public_t * status)
{
    int ret = OMPI_SUCCESS;
    mca_io_ompio_data_t *data;

    data = (mca_io_ompio_data_t *) fh->f_io_selected_data;
    ret = ompio_io_ompio_file_read_at_all_end(&data->ompio_fh,buf,status);

    return ret;
}

int ompio_io_ompio_file_read_at_all_end (mca_io_ompio_file_t *ompio_fh,
					 void *buf,
					 ompi_status_public_t * status)
{
    int ret = OMPI_SUCCESS;

    ret = ompio_fh->f_fcoll->fcoll_file_read_all_end (ompio_fh,
                                                      buf,
                                                      status);

    return ret;
}
