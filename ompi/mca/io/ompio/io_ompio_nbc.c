/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008-2011 University of Houston. All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/runtime/params.h"
#include "ompi/communicator/communicator.h"
#include "opal/datatype/opal_convertor.h"
#include "opal/datatype/opal_datatype.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/info/info.h"
#include "ompi/request/request.h"

#include <math.h>
#include <unistd.h>
#include "io_ompio_nbc.h"



int mca_io_ompio_get_f_aggregator_index (ompi_file_t *fh)
{
     mca_io_ompio_data_t *data;
     mca_io_ompio_file_t *file;

     data = (mca_io_ompio_data_t *) fh->f_io_selected_data;
     file = &data->ompio_fh;

     return file->f_aggregator_index;
}

int mca_io_ompio_get_f_num_of_io_entries(ompi_file_t *fh)
{
    
    mca_io_ompio_data_t *data;
    mca_io_ompio_file_t *file;
    
    data = (mca_io_ompio_data_t *) fh->f_io_selected_data;
    file = &data->ompio_fh;
    
    return file->f_num_of_io_entries;
}

int mca_io_ompio_get_fcoll_dynamic_num_io_procs (int *num_procs)
{
    int param;

    param = mca_base_var_find("ompi", "fcoll", "dynamic", "num_io_procs");
    if (param >= 0){
        const int *value = NULL;
        mca_base_var_get_value(param, &value, NULL, NULL);
        *num_procs = value[0];
/*	printf("num procs : %d\n", num_procs);*/
	return OMPI_SUCCESS;
    }
    else
	return -1;

}

int mca_io_ompio_get_fcoll_dynamic_constant_cbs (int *constant_cbs)
{
    int param;
    
    param = mca_base_var_find("ompi", "fcoll", "dynamic", "constant_cbs");
    if (param >= 0){
        const int *value = NULL;
        mca_base_var_get_value(param, &value, NULL, NULL);
        *constant_cbs = value[0];
/*	printf ("constant_cbs: %d\n", constant_cbs);*/
	return OMPI_SUCCESS;
    }
    else{
	constant_cbs[0] = -1;
	return OMPI_SUCCESS;
    }

}

int mca_io_ompio_get_fcoll_dynamic_cycle_buffer_size (int *cycle_buffer_size)
{
   
    int param;

    param = mca_base_var_find("ompi", "fcoll", "dynamic", "cycle_buffer_size");
    if (param >= 0){
        const int *value = NULL;
        mca_base_var_get_value(param, &value, NULL, NULL);
        *cycle_buffer_size = value[0];
/*	printf ("cycle_buffer_size : %d\n", *cycle_buffer_size);*/
	return OMPI_SUCCESS;
    }
    else
	return -1;

}

int mca_io_ompio_get_f_io_array(ompi_file_t *fh,
				mca_io_ompio_io_array_t **f_io_array)
{
    
     mca_io_ompio_data_t *data;
     mca_io_ompio_file_t *file;

     data = (mca_io_ompio_data_t *) fh->f_io_selected_data;
     file = &data->ompio_fh;
     *f_io_array = file->f_io_array;
     return OMPI_SUCCESS;
}

int mca_io_ompio_get_f_comm(ompi_file_t *fh, ompi_communicator_t **value)
{
    mca_io_ompio_data_t *data;
    mca_io_ompio_file_t *file;

    data = (mca_io_ompio_data_t *)fh->f_io_selected_data;
    file = &(data->ompio_fh);
    *value = file->f_comm;

    return OMPI_SUCCESS;
}

int mca_io_ompio_get_iov_type(ompi_file_t *fh, ompi_datatype_t **value)
{
    mca_io_ompio_data_t *data;
    mca_io_ompio_file_t *file;

    data = (mca_io_ompio_data_t *)fh->f_io_selected_data;
    file = &data->ompio_fh;

    *value = file->f_iov_type;
    return OMPI_SUCCESS;
}

int mca_io_ompio_get_f_procs_in_group(ompi_file_t *fh, int **value)
{
    mca_io_ompio_data_t *data;
    mca_io_ompio_file_t *file;

    data = (mca_io_ompio_data_t *)fh->f_io_selected_data;
    file = &data->ompio_fh;

    *value =  file->f_procs_in_group;
    return OMPI_SUCCESS;
}

int mca_io_ompio_get_f_procs_per_group(ompi_file_t *fh)
{
    mca_io_ompio_data_t *data;
    mca_io_ompio_file_t *file;

    data = (mca_io_ompio_data_t *)fh->f_io_selected_data;
    file = &data->ompio_fh;

    return file->f_procs_per_group;
}

signed int mca_io_ompio_get_f_flags(ompi_file_t *fh)
{
    mca_io_ompio_data_t *data;
    mca_io_ompio_file_t *file;

    data = (mca_io_ompio_data_t *) fh->f_io_selected_data;
    file = &data->ompio_fh;

    return file->f_flags;
}

int mca_io_ompio_get_fd(ompi_file_t *fh)
{
    mca_io_ompio_data_t *data;
    mca_io_ompio_file_t *file;

    data = (mca_io_ompio_data_t *) fh->f_io_selected_data;
    file = &data->ompio_fh;

    return file->fd;
}

int mca_io_ompio_generate_io_array(ompi_file_t *file,
				   struct iovec *global_fview,
				   int *tglobal_count,
				   int *fview_count,
				   int *bytes_per_process,
				   char *global_buf,
				   int *tblocks,
				   int *sorted, 
				   int *nvalue,
				   int *bytes_left_ptr,
				   int *sorted_index)
{
    mca_io_ompio_data_t *data;
    mca_io_ompio_file_t *fh;
    int k, j, x=sorted_index[0];
    int blocks = *tblocks;
    int bytes_left = bytes_left_ptr[0];
   
    
    data = (mca_io_ompio_data_t *) file->f_io_selected_data;
    fh = &data->ompio_fh;
    

    if (fh->f_procs_in_group[fh->f_aggregator_index] == fh->f_rank) {
	int global_count = *tglobal_count;
	int bytes_to_write = global_count;
	int *temp = NULL;
	int block = 1;
	k = 0;
	temp = (int *)malloc (sizeof(int) * fh->f_procs_per_group);
	if (NULL == temp) {
	    opal_output(1, "OUT OF MEMORY\n");
	    return OMPI_ERR_OUT_OF_RESOURCE;
	}
	memset(temp, 0x0, fh->f_procs_per_group*sizeof(int));
	if (NULL != fh->f_io_array){
	    fh->f_num_of_io_entries = 0;
	    free (fh->f_io_array);
	    fh->f_io_array = NULL;
	}
	    
	fh->f_io_array = (mca_io_ompio_io_array_t *) malloc 
	    (OMPIO_IOVEC_INITIAL_SIZE * sizeof (mca_io_ompio_io_array_t));
	if (NULL == fh->f_io_array) {
	    opal_output(1, "OUT OF MEMORY\n");
            free(temp);
            return OMPI_ERR_OUT_OF_RESOURCE;
	}
	while (bytes_to_write) {
	    int start = 0;
	    if (OMPIO_IOVEC_INITIAL_SIZE*block <= k) {
		block ++;
		fh->f_io_array = (mca_io_ompio_io_array_t *)realloc
		    (fh->f_io_array, OMPIO_IOVEC_INITIAL_SIZE * block *
		     sizeof(mca_io_ompio_io_array_t));
		if (NULL == fh->f_io_array) {
		    opal_output(1, "OUT OF MEMORY\n");
                    free(temp);
		    return OMPI_ERR_OUT_OF_RESOURCE;
		}
	    }
	    blocks= fview_count[0];
	    for (j=0 ; j<fh->f_procs_per_group ; j++) {
		if (sorted[x] < blocks) {
		    nvalue[0] = j;
		    break;
		}
		else {
		    blocks += fview_count[j+1];
		}
	    }
	    for (j=0 ; j<nvalue[0] ; j++) {
		start += bytes_per_process[j];
	    }
	    if (bytes_left) {
		if (bytes_left <= bytes_to_write) {
		    fh->f_io_array[k].offset = (IOVBASE_TYPE *)
			((OPAL_PTRDIFF_TYPE)global_fview[sorted[x]].iov_base +
			 (global_fview[sorted[x]].iov_len - bytes_left));

		    fh->f_io_array[k].length = bytes_left;
		    fh->f_io_array[k].memory_address = &global_buf[start+temp[nvalue[0]]];
/*		    printf("global_buf[%d] : %d\n",
		    (start+temp[nvalue[0]]),(int)global_buf[start+temp[nvalue[0]]]);*/

		    temp[nvalue[0]] += (int)fh->f_io_array[k].length;
		    bytes_to_write -= bytes_left;
		    bytes_left = 0;
		    k ++;
		    x ++;
		    continue;
		}
		else {
		    fh->f_io_array[k].offset = (IOVBASE_TYPE *) 
			((OPAL_PTRDIFF_TYPE)global_fview[sorted[x]].iov_base +  (global_fview[sorted[x]].iov_len - bytes_left));
		    
		    fh->f_io_array[k].length = bytes_to_write;
		    fh->f_io_array[k].memory_address = 
                            &global_buf[start+temp[nvalue[0]]];
/*		    printf("global_buf[%d] : %d\n",
		    (start+temp[nvalue[0]]),(int)global_buf[start+temp[nvalue[0]]]);*/

		    temp[nvalue[0]] += (int)fh->f_io_array[k].length;
		    bytes_left -= bytes_to_write;
		    bytes_to_write = 0;;
		    k ++;
		    break;
		}
	    }
	    else {
		if (bytes_to_write < (int)global_fview[sorted[x]].iov_len) {
		    fh->f_io_array[k].offset = global_fview[sorted[x]].iov_base;

		    fh->f_io_array[k].length = bytes_to_write;
		    fh->f_io_array[k].memory_address = &global_buf[start+temp[nvalue[0]]];
/*		    printf("global_buf[%d] : %d\n",
		    (start+temp[nvalue[0]]),(int)global_buf[start+temp[nvalue[0]]]);*/

		    bytes_left = 
                            global_fview[sorted[x]].iov_len - bytes_to_write;
		    bytes_to_write = 0;
		    k ++;
		    break;
		}
		else {
		    fh->f_io_array[k].offset = global_fview[sorted[x]].iov_base;

		    fh->f_io_array[k].length = global_fview[sorted[x]].iov_len;
		    fh->f_io_array[k].memory_address = &global_buf[start+temp[nvalue[0]]];
		    temp[nvalue[0]] += (int)fh->f_io_array[k].length;
/*		    printf("global_buf[%d] : %d\n",
		    (start+temp[nvalue[0]]),(int)global_buf[start+temp[nvalue[0]]]);*/
		    
		    bytes_to_write -= global_fview[sorted[x]].iov_len;
		    k ++;
		    x ++;
		    continue;
		}
	    }
	}
	fh->f_num_of_io_entries = k;
/*	for (i=0 ; i<fh->f_num_of_io_entries ; i++) {
                   printf("OFFSET: %lu LENGTH: %d\n",
			  fh->f_io_array[i].offset,
			  fh->f_io_array[i].length);
			  }*/
	

	bytes_left_ptr[0] = bytes_left;
	sorted_index[0] = x;

	if (NULL != temp) {
	    free (temp);
	    temp = NULL;
	}

    }
    return OMPI_SUCCESS;
}

int mca_io_ompio_non_contiguous_create_receive_buf(int *bytes_received,
						   struct iovec *decoded_iov,
						   char *receive_buf)
{

    OPAL_PTRDIFF_TYPE mem_address;
    size_t remaining = 0;
    size_t temp_position = 0;
    int current_position = 0, iov_index = 0;

    remaining = *bytes_received;
    
    while (remaining) {
	mem_address = (OPAL_PTRDIFF_TYPE)
	    (decoded_iov[iov_index].iov_base) + current_position;
	
	if (remaining >= 
	    (decoded_iov[iov_index].iov_len - current_position)) {
	    memcpy ((IOVBASE_TYPE *) mem_address,
		    receive_buf+temp_position,
		    decoded_iov[iov_index].iov_len - current_position);
	    remaining = remaining - 
		(decoded_iov[iov_index].iov_len - current_position);
	    temp_position = temp_position +
		(decoded_iov[iov_index].iov_len - current_position);
	    iov_index = iov_index + 1;
	    current_position = 0;
	}
	else {
	    memcpy ((IOVBASE_TYPE *) mem_address,
		    receive_buf+temp_position,
		    remaining);
	    current_position = current_position + remaining;
                    remaining = 0;
	}
    }
    if (NULL != receive_buf) {
	free (receive_buf);
	receive_buf = NULL;
    }
    return OMPI_SUCCESS;
}


int mca_io_ompio_non_contiguous_create_send_buf(int *bytes_sent,
						struct iovec *decoded_iov,
						char *send_buf)
{

    OPAL_PTRDIFF_TYPE mem_address;
    size_t remaining = 0;
    size_t temp_position = 0;
    int current_position = 0, iov_index = 0;
    
    remaining = *bytes_sent;
    while (remaining) {
	mem_address = (OPAL_PTRDIFF_TYPE)
	    (decoded_iov[iov_index].iov_base) + current_position;
	if (remaining >= 
                    (decoded_iov[iov_index].iov_len - current_position)) {
	    memcpy (send_buf+temp_position,
		    (IOVBASE_TYPE *)mem_address,
		    decoded_iov[iov_index].iov_len - current_position);
	    remaining = remaining - 
		(decoded_iov[iov_index].iov_len - current_position);
	    temp_position = temp_position +
		(decoded_iov[iov_index].iov_len - current_position);
	    iov_index = iov_index + 1;
	    current_position = 0;
	}
	else {
	    memcpy (send_buf+temp_position, (IOVBASE_TYPE *) mem_address,
		    remaining);
	    current_position = current_position + remaining;
	    remaining = 0;
	}
    }
    return OMPI_SUCCESS;
}


 
int mca_io_ompio_get_datatype_size (ompi_datatype_t * datatype)
{
    return datatype->super.size;
}

int mca_io_ompio_decode_datatype_external (ompi_file_t *fp, 
					   ompi_datatype_t *datatype,
					   int count,
					   void *buf,
					   size_t *max_data,
					   struct iovec **iov,
					   uint32_t *iovec_count)
{                      

     int res;
     mca_io_ompio_data_t *data;
     mca_io_ompio_file_t *fh;

     data = (mca_io_ompio_data_t *) fp->f_io_selected_data;
     fh = &data->ompio_fh;
     res = ompi_io_ompio_decode_datatype (fh,
					  datatype,
					  count,
					  buf,
					  max_data,
					  iov,
					  iovec_count);
     if(res != OMPI_SUCCESS){ 
	 printf("Error in ompio decode datatype\n");
	 return res;
     }
     return OMPI_SUCCESS;

}

int mca_io_ompio_datatype_is_contiguous(ompi_datatype_t *datatype,
					ompi_file_t *fp)
{
    mca_io_ompio_data_t *data;
    mca_io_ompio_file_t *fh;
    
    data = (mca_io_ompio_data_t *) fp->f_io_selected_data;
    fh = &data->ompio_fh;

    if (opal_datatype_is_contiguous_memory_layout(&datatype->super,1)){
	fh->f_flags |= OMPIO_CONTIGUOUS_MEMORY;
	return 1;
    }
    else
	return 0;
}


int mca_io_ompio_set_aggregator_props (ompi_file_t *fp,
				      int num_aggregators,
				      size_t bytes_per_proc)
{
    int res;
    mca_io_ompio_data_t *data;
    mca_io_ompio_file_t *fh;

    data = (mca_io_ompio_data_t *) fp->f_io_selected_data;
    fh = &data->ompio_fh;
    res = ompi_io_ompio_set_aggregator_props (fh,
					      num_aggregators,
					      bytes_per_proc);
    if(res != OMPI_SUCCESS){
	printf("Error in aggregator props external\n"); 
	return res;
    }

    return OMPI_SUCCESS;
}

int mca_io_ompio_generate_current_file_view (ompi_file_t *fp,
					     size_t max_data,
					     struct iovec **f_iov,
					     int *iov_count)
{
    int res;
    mca_io_ompio_data_t *data;
    mca_io_ompio_file_t *fh;

    data = (mca_io_ompio_data_t *) fp->f_io_selected_data;
    fh = &data->ompio_fh;
    res = ompi_io_ompio_generate_current_file_view (fh,
						    max_data,
						    f_iov,
						    iov_count);
    if(res != OMPI_SUCCESS){
	printf("Error in ompi_io_generate_current_file_view\n");
	return res;
    }

    return OMPI_SUCCESS;
}

int mca_io_ompio_free_f_io_array (ompi_file_t *fp){

    mca_io_ompio_data_t *data;
    mca_io_ompio_file_t *fh;
    
    data = (mca_io_ompio_data_t *) fp->f_io_selected_data;
    fh = &data->ompio_fh;
    
    if (NULL != fh->f_io_array) {
	free (fh->f_io_array);
	fh->f_io_array = NULL;
    }
    
    return OMPI_SUCCESS;
}
