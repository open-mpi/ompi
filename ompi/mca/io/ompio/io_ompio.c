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
 * Copyright (c) 2011      Cisco Systems, Inc. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/runtime/params.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/pml/pml.h"
#include "opal/datatype/opal_convertor.h"
#include "opal/datatype/opal_datatype.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/info/info.h"
#include "ompi/request/request.h"

#include <math.h>
#include <unistd.h>

#ifdef HAVE_SYS_STATFS_H
#include <sys/statfs.h> /* or <sys/vfs.h> */ 
#endif
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#ifdef HAVE_SYS_MOUNT_H
#include <sys/mount.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

#ifdef HAVE_PVFS2_H
#include "pvfs2.h"
#endif

#include "io_ompio.h"

static void get_parent_dir (char *filename, char **dirnamep);

int ompi_io_ompio_set_file_defaults (mca_io_ompio_file_t *fh)
{

    if (NULL != fh) {
        ompi_datatype_t *types[2], *default_file_view;
        int blocklen[2] = {1, 1};
        OPAL_PTRDIFF_TYPE d[2], base;
        int i;

        fh->f_info = MPI_INFO_NULL;
        fh->f_comm = MPI_COMM_NULL;
        fh->f_rank = -1;
        fh->f_size = 0;
        fh->f_io_array = NULL;
        fh->f_perm = OMPIO_PERM_NULL;
        fh->f_flags = 0;
        fh->f_bytes_per_agg = mca_io_ompio_bytes_per_agg;
        fh->f_datarep = strdup ("native");

        fh->f_offset = 0;
        fh->f_disp = 0;
        fh->f_position_in_file_view = 0;
        fh->f_index_in_file_view = 0;
        fh->f_total_bytes = 0;

        fh->f_procs_in_group = NULL;
        fh->f_procs_per_group = -1;


	ompi_datatype_create_contiguous(1048576, &ompi_mpi_byte.dt, &default_file_view);
		
	fh->f_etype = default_file_view;
	fh->f_filetype =  default_file_view; 

        /* Default file View */
        fh->f_iov_type = MPI_DATATYPE_NULL;
        fh->f_iov_count = 1;
        fh->f_decoded_iov = (struct iovec*)malloc(fh->f_iov_count * 
                                                  sizeof(struct iovec));
        if (NULL == fh->f_decoded_iov) {
            opal_output (1, "OUT OF MEMORY\n");
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        fh->f_cc_size = 1;
        fh->f_stripe_size = mca_io_ompio_bytes_per_agg;
        fh->f_decoded_iov[0].iov_len = 1;
        fh->f_decoded_iov[0].iov_base = 0;

        /* 
         * Create a derived datatype for the created iovec 
         */
        types[0] = &ompi_mpi_long.dt;
        types[1] = &ompi_mpi_long.dt;
        MPI_Address( fh->f_decoded_iov, d); 
        MPI_Address( &fh->f_decoded_iov[0].iov_len, d+1);
        base = d[0];
        for (i=0 ; i<2 ; i++) {
            d[i] -= base;
        }
        ompi_datatype_create_struct (2,
                                     blocklen,
                                     d,
                                     types,
                                     &fh->f_iov_type);
        ompi_datatype_commit (&fh->f_iov_type);

        fh->f_view_extent = 1;
        fh->f_view_size = 1;
        fh->f_etype_size = 1;

        return OMPI_SUCCESS;
    }
    else {
        return OMPI_ERROR;
    }
}

int ompi_io_ompio_generate_current_file_view (mca_io_ompio_file_t *fh,
                                              size_t max_data,
                                              struct iovec **f_iov,
                                              int *iov_count)
{




    struct iovec *iov = NULL;
    size_t bytes_to_write;
    size_t sum_previous_counts = 0;
    int j, k;
    int block = 1;


    /* allocate an initial iovec, will grow if needed */
    iov = (struct iovec *) malloc 
        (OMPIO_IOVEC_INITIAL_SIZE * sizeof (struct iovec));
    if (NULL == iov) {
        opal_output(1, "OUT OF MEMORY\n");
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    sum_previous_counts = fh->f_position_in_file_view;
    j = fh->f_index_in_file_view;
    bytes_to_write = max_data;
    k = 0;

    while (bytes_to_write) {
        OPAL_PTRDIFF_TYPE disp;
        /* reallocate if needed */
        if (OMPIO_IOVEC_INITIAL_SIZE*block <= k) {
            block ++;
            iov = (struct iovec *)realloc
                (iov, OMPIO_IOVEC_INITIAL_SIZE *block *sizeof(struct iovec));
            if (NULL == iov) {
                opal_output(1, "OUT OF MEMORY\n");
                return OMPI_ERR_OUT_OF_RESOURCE;
            }
        }

        if (fh->f_decoded_iov[j].iov_len - 
            (fh->f_total_bytes - sum_previous_counts) <= 0) {
            sum_previous_counts += fh->f_decoded_iov[j].iov_len;
            j = j + 1;
            if (j == (int)fh->f_iov_count) {
                j = 0;
                sum_previous_counts = 0;
                fh->f_offset += fh->f_view_extent;
                fh->f_position_in_file_view = sum_previous_counts;
                fh->f_index_in_file_view = j;
                fh->f_total_bytes = 0;
            }
        }
	
        disp = (OPAL_PTRDIFF_TYPE)(fh->f_decoded_iov[j].iov_base) + 
            (fh->f_total_bytes - sum_previous_counts);
        iov[k].iov_base = (IOVBASE_TYPE *)(disp + fh->f_offset);

        if ((fh->f_decoded_iov[j].iov_len - 
             (fh->f_total_bytes - sum_previous_counts)) 
            >= bytes_to_write) {
            iov[k].iov_len = bytes_to_write;
        }
        else {
            iov[k].iov_len =  fh->f_decoded_iov[j].iov_len - 
                (fh->f_total_bytes - sum_previous_counts);
        }

        fh->f_total_bytes += iov[k].iov_len;
        bytes_to_write -= iov[k].iov_len;
        k = k + 1;
    }
    fh->f_position_in_file_view = sum_previous_counts;
    fh->f_index_in_file_view = j;

    *iov_count = k;
    *f_iov = iov;

    return OMPI_SUCCESS;
}

int ompi_io_ompio_set_explicit_offset (mca_io_ompio_file_t *fh,
                                       OMPI_MPI_OFFSET_TYPE offset)
{


    size_t i = 0;
    size_t k = 0;
    
    fh->f_offset += fh->f_view_extent *
	(((offset-fh->f_offset)*fh->f_etype_size)/fh->f_view_size);

    fh->f_position_in_file_view = 0;

    fh->f_total_bytes = (offset*fh->f_etype_size) % fh->f_view_size;


    fh->f_index_in_file_view = 0;
    i = fh->f_total_bytes;
    k = 0;
    while (1) {
        k += fh->f_decoded_iov[fh->f_index_in_file_view].iov_len;
	if (i >= k) {
            i = i - fh->f_decoded_iov[fh->f_index_in_file_view].iov_len;
            fh->f_position_in_file_view += 
                fh->f_decoded_iov[fh->f_index_in_file_view].iov_len;
            fh->f_index_in_file_view = fh->f_index_in_file_view+1;
        }
        else {
	    break;
        }
    }

    return OMPI_SUCCESS;
}

int ompi_io_ompio_decode_datatype (mca_io_ompio_file_t *fh, 
                                   ompi_datatype_t *datatype,
                                   int count,
                                   void *buf,
                                   size_t *max_data,
                                   struct iovec **iov,
                                   uint32_t *iovec_count)
{                      


    
    opal_convertor_t convertor;
    size_t remaining_length = 0;
    uint32_t i;
    uint32_t temp_count;
    struct iovec * temp_iov;
    size_t temp_data;

    opal_convertor_clone (fh->f_convertor, &convertor, 0);

    if (OMPI_SUCCESS != opal_convertor_prepare_for_send (&convertor, 
                                                         &(datatype->super),
                                                         count,
                                                         buf)) {
        opal_output (1, "Cannot attach the datatype to a convertor\n");
        return OMPI_ERROR;
    }
    remaining_length = count * datatype->super.size;

    temp_count = OMPIO_IOVEC_INITIAL_SIZE;
    temp_iov = (struct iovec*)malloc(temp_count * sizeof(struct iovec));
    if (NULL == temp_iov) {
        opal_output (1, "OUT OF MEMORY\n");
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    while (0 == opal_convertor_raw(&convertor, 
				   temp_iov,
                                   &temp_count, 
                                   &temp_data)) {
#if 0
        printf ("New raw extraction (iovec_count = %d, max_data = %d)\n",
                temp_count, temp_data);
        for (i = 0; i < temp_count; i++) {
            printf ("\t{%p, %d}\n", temp_iov[i].iov_base, temp_iov[i].iov_len);
        }
#endif

        *iovec_count = *iovec_count + temp_count;
        *max_data = *max_data + temp_data;
        *iov = (struct iovec *) realloc (*iov, *iovec_count * sizeof(struct iovec));
        if (NULL == *iov) {
            opal_output(1, "OUT OF MEMORY\n");
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        for (i=0 ; i<temp_count ; i++) {
            (*iov)[i+(*iovec_count-temp_count)].iov_base = temp_iov[i].iov_base;
            (*iov)[i+(*iovec_count-temp_count)].iov_len = temp_iov[i].iov_len;
        }

        remaining_length -= temp_data;
        temp_count = OMPIO_IOVEC_INITIAL_SIZE;
    }
#if 0
    printf ("LAST raw extraction (iovec_count = %d, max_data = %d)\n",
            temp_count, temp_data);
    for (i = 0; i < temp_count; i++) {
        printf ("\t{%p, %d}\n", temp_iov[i].iov_base, temp_iov[i].iov_len);
    }
#endif
    *iovec_count = *iovec_count + temp_count;
    *max_data = *max_data + temp_data;
    *iov = (struct iovec *) realloc (*iov, *iovec_count * sizeof(struct iovec));
    if (NULL == *iov) {
        opal_output(1, "OUT OF MEMORY\n");
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    for (i=0 ; i<temp_count ; i++) {
        (*iov)[i+(*iovec_count-temp_count)].iov_base = temp_iov[i].iov_base;
        (*iov)[i+(*iovec_count-temp_count)].iov_len = temp_iov[i].iov_len;
    }

    remaining_length -= temp_data;
    /*
    if (0 == fh->f_rank) {
        printf ("%d Entries: \n",*iovec_count);
        for (i=0 ; i<*iovec_count ; i++) {
            printf ("\t{%p, %d}\n", 
                    (*iov)[i].iov_base, 
                    (*iov)[i].iov_len);
        }
    }
    */
    if (remaining_length != 0) {
        printf( "Not all raw description was been extracted (%lu bytes missing)\n",
                (unsigned long) remaining_length );
    }

    if (NULL != temp_iov) {
        free (temp_iov);
        temp_iov = NULL;
    }

    return OMPI_SUCCESS;
}

int ompi_io_ompio_sort (mca_io_ompio_io_array_t *io_array,
                        int num_entries,
                        int *sorted)
{
    int i = 0;
    int j = 0;
    int left = 0;
    int right = 0;
    int largest = 0;
    int heap_size = num_entries - 1;
    int temp = 0;
    unsigned char done = 0;
    int* temp_arr = NULL;

    temp_arr = (int*)malloc(num_entries*sizeof(int));
    if (NULL == temp_arr) {
        opal_output (1, "OUT OF MEMORY\n");
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    temp_arr[0] = 0;
    for (i = 1; i < num_entries; ++i) {
        temp_arr[i] = i;
    }
    /* num_entries can be a large no. so NO RECURSION */
    for (i = num_entries/2-1 ; i>=0 ; i--) {
        done = 0;
        j = i;
        largest = j;

        while (!done) {
            left = j*2+1;
            right = j*2+2;
            if ((left <= heap_size) && 
                (io_array[temp_arr[left]].offset > io_array[temp_arr[j]].offset)) {
                largest = left;
            }
            else {
                largest = j;
            }
            if ((right <= heap_size) && 
                (io_array[temp_arr[right]].offset > 
                 io_array[temp_arr[largest]].offset)) {
                largest = right;
            }
            if (largest != j) {
                temp = temp_arr[largest];
                temp_arr[largest] = temp_arr[j];
                temp_arr[j] = temp;
                j = largest;
            }
            else {
                done = 1;
            }
        }
    }

    for (i = num_entries-1; i >=1; --i) {
        temp = temp_arr[0];
        temp_arr[0] = temp_arr[i];
        temp_arr[i] = temp;            
        heap_size--;            
        done = 0;
        j = 0;
        largest = j;

        while (!done) {
            left =  j*2+1;
            right = j*2+2;
            
            if ((left <= heap_size) && 
                (io_array[temp_arr[left]].offset > 
                 io_array[temp_arr[j]].offset)) {
                largest = left;
            }
            else {
                largest = j;
            }
            if ((right <= heap_size) && 
                (io_array[temp_arr[right]].offset > 
                 io_array[temp_arr[largest]].offset)) {
                largest = right;
            }
            if (largest != j) {
                temp = temp_arr[largest];
                temp_arr[largest] = temp_arr[j];
                temp_arr[j] = temp;
                j = largest;
            }
            else {
                done = 1;
            }
        }
        sorted[i] = temp_arr[i];
    }
    sorted[0] = temp_arr[0];

    if (NULL != temp_arr) {
        free(temp_arr);
        temp_arr = NULL;
    }
    return OMPI_SUCCESS;
}

int ompi_io_ompio_sort_iovec (struct iovec *iov,
                              int num_entries,
                              int *sorted)
{
    int i = 0;
    int j = 0;
    int left = 0;
    int right = 0;
    int largest = 0;
    int heap_size = num_entries - 1;
    int temp = 0;
    unsigned char done = 0;
    int* temp_arr = NULL;

    if (0 == num_entries) {
        return OMPI_SUCCESS;
    }

    temp_arr = (int*)malloc(num_entries*sizeof(int));
    if (NULL == temp_arr) {
        opal_output (1, "OUT OF MEMORY\n");
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    temp_arr[0] = 0;
    for (i = 1; i < num_entries; ++i) {
        temp_arr[i] = i;
    }
    /* num_entries can be a large no. so NO RECURSION */
    for (i = num_entries/2-1 ; i>=0 ; i--) {
        done = 0;
        j = i;
        largest = j;

        while (!done) {
            left = j*2+1;
            right = j*2+2;
            if ((left <= heap_size) && 
                (iov[temp_arr[left]].iov_base > iov[temp_arr[j]].iov_base)) {
                largest = left;
            }
            else {
                largest = j;
            }
            if ((right <= heap_size) && 
                (iov[temp_arr[right]].iov_base > 
                 iov[temp_arr[largest]].iov_base)) {
                largest = right;
            }
            if (largest != j) {
                temp = temp_arr[largest];
                temp_arr[largest] = temp_arr[j];
                temp_arr[j] = temp;
                j = largest;
            }
            else {
                done = 1;
            }
        }
    }

    for (i = num_entries-1; i >=1; --i) {
        temp = temp_arr[0];
        temp_arr[0] = temp_arr[i];
        temp_arr[i] = temp;            
        heap_size--;            
        done = 0;
        j = 0;
        largest = j;

        while (!done) {
            left =  j*2+1;
            right = j*2+2;
            
            if ((left <= heap_size) && 
                (iov[temp_arr[left]].iov_base > 
                 iov[temp_arr[j]].iov_base)) {
                largest = left;
            }
            else {
                largest = j;
            }
            if ((right <= heap_size) && 
                (iov[temp_arr[right]].iov_base > 
                 iov[temp_arr[largest]].iov_base)) {
                largest = right;
            }
            if (largest != j) {
                temp = temp_arr[largest];
                temp_arr[largest] = temp_arr[j];
                temp_arr[j] = temp;
                j = largest;
            }
            else {
                done = 1;
            }
        }
        sorted[i] = temp_arr[i];
    }
    sorted[0] = temp_arr[0];

    if (NULL != temp_arr) {
        free(temp_arr);
        temp_arr = NULL;
    }
    return OMPI_SUCCESS;
}

int ompi_io_ompio_set_aggregator_props (mca_io_ompio_file_t *fh,
                                        int num_aggregators,
                                        size_t bytes_per_proc)
{


    int j;
    int root_offset;
    int ndims, i=1, n=0, total_groups=0;
    int *dims=NULL, *periods=NULL, *coords=NULL, *coords_tmp=NULL;
    int procs_per_node = 1; /* MSC TODO - Figure out a way to get this info */
    size_t max_bytes_per_proc = 0;
 


    /*
    OMPI_MPI_OFFSET_TYPE temp;
    int global_flag, flag;
    */
    fh->f_flags |= OMPIO_AGGREGATOR_IS_SET;

    if (-1 == num_aggregators) {
        /* Determine Topology Information */
        if (fh->f_comm->c_flags & OMPI_COMM_CART) {
            MPI_Cartdim_get(fh->f_comm, &ndims);

            dims = (int*)malloc (ndims * sizeof(int));
            if (NULL == dims) {
                opal_output (1, "OUT OF MEMORY\n");
                return OMPI_ERR_OUT_OF_RESOURCE;
            }

            periods = (int*)malloc (ndims * sizeof(int));
            if (NULL == periods) {
                opal_output (1, "OUT OF MEMORY\n");
                return OMPI_ERR_OUT_OF_RESOURCE;
            }

            coords = (int*)malloc (ndims * sizeof(int));
            if (NULL == coords) {
                opal_output (1, "OUT OF MEMORY\n");
                return OMPI_ERR_OUT_OF_RESOURCE;
            }

            coords_tmp = (int*)malloc (ndims * sizeof(int));
            if (NULL == coords_tmp) {
                opal_output (1, "OUT OF MEMORY\n");
                return OMPI_ERR_OUT_OF_RESOURCE;
            }

            MPI_Cart_get(fh->f_comm, ndims, dims, periods, coords);

            /*
              printf ("NDIMS = %d\n", ndims);
              for (j=0 ; j<ndims; j++) {
              printf ("%d:  dims[%d] = %d     period[%d] = %d    coords[%d] = %d\n",
              fh->f_rank,j,dims[j],j,periods[j],j,coords[j]);
              }
            */

            while (1) {
                if (fh->f_size/dims[0]*i >= procs_per_node) {
                    fh->f_procs_per_group = fh->f_size/dims[0]*i;
                    break;
                }
                i++;
            }

            total_groups = ceil((float)fh->f_size/fh->f_procs_per_group);

            if ((coords[0]/i + 1) == total_groups && 0 != (total_groups%i)) {
                fh->f_procs_per_group = (fh->f_size/dims[0]) * (total_groups%i);
            }
            /*
            printf ("BEFORE ADJUSTMENT: %d ---> procs_per_group = %d   total_groups = %d\n", 
                    fh->f_rank, fh->f_procs_per_group, total_groups);
            */
            /* check if the current grouping needs to be expanded or shrinked */
            if ((size_t)mca_io_ompio_bytes_per_agg < 
                bytes_per_proc * fh->f_procs_per_group) {

                root_offset = ceil ((float)mca_io_ompio_bytes_per_agg/bytes_per_proc);
                if (fh->f_procs_per_group/root_offset != coords[1]/root_offset) {
                    fh->f_procs_per_group = root_offset;
                }
                else {
                    fh->f_procs_per_group = fh->f_procs_per_group%root_offset;
                }
            }
            else if ((size_t)mca_io_ompio_bytes_per_agg > 
                     bytes_per_proc * fh->f_procs_per_group) {
                i = ceil ((float)mca_io_ompio_bytes_per_agg/
                          (bytes_per_proc * fh->f_procs_per_group));
                root_offset = fh->f_procs_per_group * i;

                if (fh->f_size/root_offset != fh->f_rank/root_offset) {
                    fh->f_procs_per_group = root_offset;
                }
                else {
                    fh->f_procs_per_group = fh->f_size%root_offset;
                }
            }
            /*
            printf ("AFTER ADJUSTMENT: %d (%d) ---> procs_per_group = %d\n", 
                    fh->f_rank, coords[1], fh->f_procs_per_group);
            */
            fh->f_procs_in_group = (int*)malloc (fh->f_procs_per_group * sizeof(int));
            if (NULL == fh->f_procs_in_group) {
                opal_output (1, "OUT OF MEMORY\n");
                return OMPI_ERR_OUT_OF_RESOURCE;
            }

            for (j=0 ; j<fh->f_size ; j++) {
                MPI_Cart_coords (fh->f_comm, j, ndims, coords_tmp);
                if (coords_tmp[0]/i == coords[0]/i) {
                    if ((coords_tmp[1]/root_offset)*root_offset == 
                        (coords[1]/root_offset)*root_offset) {
                        fh->f_procs_in_group[n] = j;
                        n++;
                    }
                }
            }

            fh->f_aggregator_index = 0;

            /*
            if (fh->f_procs_in_group[fh->f_aggregator_index] == fh->f_rank)  {
                for (j=0 ; j<fh->f_procs_per_group; j++) {
                    printf ("%d: Proc %d: %d\n", fh->f_rank, j, fh->f_procs_in_group[j]);
                }
            }
            */

            if (NULL != dims) {
                free (dims);
                dims = NULL;
            }
            if (NULL != periods) {
                free (periods);
                periods = NULL;
            }
            if (NULL != coords) {
                free (coords);
                coords = NULL;
            }
            if (NULL != coords_tmp) {
                free (coords_tmp);
                coords_tmp =  NULL;
            }
            return OMPI_SUCCESS;
        }

        /*
        temp = fh->f_iov_count;
        fh->f_comm->c_coll.coll_bcast (&temp,
                                       1,
                                       MPI_LONG,
                                       OMPIO_ROOT,
                                       fh->f_comm,
                                       fh->f_comm->c_coll.coll_bcast_module);

        if (temp != fh->f_iov_count) {
            flag = 0;
        }
        else {
            flag = 1;
        }
        fh->f_comm->c_coll.coll_allreduce (&flag,
                                           &global_flag,
                                           1,
                                           MPI_INT,
                                           MPI_MIN,
                                           fh->f_comm,
                                           fh->f_comm->c_coll.coll_allreduce_module);
        */
        if (fh->f_flags & OMPIO_UNIFORM_FVIEW) {
            OMPI_MPI_OFFSET_TYPE *start_offsets = NULL;
            OMPI_MPI_OFFSET_TYPE stride = 0;

            if (OMPIO_ROOT == fh->f_rank) {
                start_offsets = malloc (fh->f_size * sizeof(OMPI_MPI_OFFSET_TYPE));
            }

            fh->f_comm->c_coll.coll_gather (&fh->f_decoded_iov[0].iov_base,
                                            1,
                                            MPI_LONG,
                                            start_offsets,
                                            1,
                                            MPI_LONG,
                                            OMPIO_ROOT,
                                            fh->f_comm,
                                            fh->f_comm->c_coll.coll_gather_module);
            if (OMPIO_ROOT == fh->f_rank) {
                stride = start_offsets[1] - start_offsets[0];
                for (i=2 ; i<fh->f_size ; i++) {
                    if (stride != start_offsets[i]-start_offsets[i-1]) {
                        break;
                    }
                }
            }

            if (NULL != start_offsets) {
                free (start_offsets);
                start_offsets = NULL;
            }

            fh->f_comm->c_coll.coll_bcast (&i,
                                           1,
                                           MPI_INT,
                                           OMPIO_ROOT,
                                           fh->f_comm,
                                           fh->f_comm->c_coll.coll_bcast_module);

            fh->f_procs_per_group = i;
            max_bytes_per_proc = bytes_per_proc;
        }
        else {
            fh->f_procs_per_group = 1;
            fh->f_comm->c_coll.coll_allreduce (&bytes_per_proc,
                                               &max_bytes_per_proc,
                                               1,
                                               MPI_LONG,
                                               MPI_MAX,
                                               fh->f_comm,
                                               fh->f_comm->c_coll.coll_allreduce_module);
        }
        /*
        printf ("BEFORE ADJUSTMENT: %d ---> procs_per_group = %d\n", 
                fh->f_rank, fh->f_procs_per_group);
        
        printf ("COMPARING %d   to  %d x %d = %d\n", 
                mca_io_ompio_bytes_per_agg,
                bytes_per_proc,
                fh->f_procs_per_group,
                fh->f_procs_per_group*bytes_per_proc);
        */
        /* check if the current grouping needs to be expanded or shrinked */
        if ((size_t)mca_io_ompio_bytes_per_agg < 
            max_bytes_per_proc * fh->f_procs_per_group) {
            root_offset = ceil ((float)mca_io_ompio_bytes_per_agg/max_bytes_per_proc);

            if (fh->f_procs_per_group/root_offset != 
                (fh->f_rank%fh->f_procs_per_group)/root_offset) {
                fh->f_procs_per_group = root_offset;
            }
            else {
                fh->f_procs_per_group = fh->f_procs_per_group%root_offset;
            }
        }
        else if ((size_t)mca_io_ompio_bytes_per_agg > 
                 max_bytes_per_proc * fh->f_procs_per_group) {
            i = ceil ((float)mca_io_ompio_bytes_per_agg/
                      (max_bytes_per_proc * fh->f_procs_per_group));
            root_offset = fh->f_procs_per_group * i;
            i = root_offset;

            if (root_offset > fh->f_size) {
                root_offset = fh->f_size;
            }

            if (fh->f_size/root_offset != fh->f_rank/root_offset) {
                fh->f_procs_per_group = root_offset;
            }
            else {
                fh->f_procs_per_group = fh->f_size%root_offset;
            }
        }
        /*
        printf ("AFTER ADJUSTMENT: %d ---> procs_per_group = %d\n", 
                fh->f_rank, fh->f_procs_per_group);
        */
        fh->f_procs_in_group = (int*)malloc 
            (fh->f_procs_per_group * sizeof(int));
        if (NULL == fh->f_procs_in_group) {
            opal_output (1, "OUT OF MEMORY\n");
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        for (j=0 ; j<fh->f_size ; j++) {
            if (j/i == fh->f_rank/i) {
                if (((j%i)/root_offset)*root_offset == 
                    ((fh->f_rank%i)/root_offset)*root_offset) {
                    fh->f_procs_in_group[n] = j;
                    n++;
                }
            }
        }

        fh->f_aggregator_index = 0;
        /*
          if (fh->f_procs_in_group[fh->f_aggregator_index] == fh->f_rank)  {
          for (j=0 ; j<fh->f_procs_per_group; j++) {
          printf ("%d: Proc %d: %d\n", fh->f_rank, j, fh->f_procs_in_group[j]);
          }
          }
        */
        return OMPI_SUCCESS;
    }

    /* calculate the offset at which each group of processes will start */
    root_offset = ceil ((float)fh->f_size/num_aggregators);

    /* calculate the number of processes in the local group */
    if (fh->f_size/root_offset != fh->f_rank/root_offset) {
        fh->f_procs_per_group = root_offset;
    }
    else {
        fh->f_procs_per_group = fh->f_size%root_offset;
    }

    fh->f_procs_in_group = (int*)malloc (fh->f_procs_per_group * sizeof(int));
    if (NULL == fh->f_procs_in_group) {
        opal_output (1, "OUT OF MEMORY\n");
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    for (j=0 ; j<fh->f_procs_per_group ; j++) {
        fh->f_procs_in_group[j] = (fh->f_rank/root_offset) * root_offset + j;
    }

    fh->f_aggregator_index = 0;

    return OMPI_SUCCESS;
}

void ompi_io_ompio_resolve_fs_type (mca_io_ompio_file_t *fh, 
                                    enum ompio_fs_type *fstype)
{
    /* The code in this function is based on the ADIO FS selection in ROMIO
     *   Copyright (C) 1997 University of Chicago. 
     *   See COPYRIGHT notice in top-level directory.
     */

    int err;
    char *dir;
    struct statfs fsbuf;
    char *tmp;


    tmp = strchr (fh->f_filename, ':');
    if (!tmp) {
        if (OMPIO_ROOT == fh->f_rank) {
            do {
                err = statfs (fh->f_filename, &fsbuf);
            } while (err && (errno == ESTALE));
            
            if (err && (errno == ENOENT)) {
                get_parent_dir (fh->f_filename, &dir);
                err = statfs (dir, &fsbuf);
                free (dir);
            }

#ifdef HAVE_LUSTRE_LIBLUSTREAPI_H
#ifndef LL_SUPER_MAGIC
#define LL_SUPER_MAGIC 0x0BD00BD0
#endif
            if (fsbuf.f_type == LL_SUPER_MAGIC) {
                *fstype = LUSTRE;
            }
#endif

#ifdef HAVE_PVFS2_H
            if (fsbuf.f_type == PVFS2_SUPER_MAGIC) {
                *fstype = PVFS2;
            }
#endif
            if (0 == *fstype) {
                *fstype = UFS;
            }
        }

        fh->f_comm->c_coll.coll_bcast (&(*fstype),
                                       1,
                                       MPI_INT,
                                       OMPIO_ROOT,
                                       fh->f_comm,
                                       fh->f_comm->c_coll.coll_bcast_module);
    }
    else {
        if (!strncmp(fh->f_filename, "pvfs2:", 6) || 
            !strncmp(fh->f_filename, "PVFS2:", 6)) {
            *fstype = PVFS2;
        }
        else if (!strncmp(fh->f_filename, "lustre:", 7) || 
                 !strncmp(fh->f_filename, "LUSTRE:", 7)) {
            *fstype = LUSTRE;
        }
        else if (!strncmp(fh->f_filename, "ufs:", 4) || 
                 !strncmp(fh->f_filename, "UFS:", 4)) {
            *fstype = UFS;
        }
    }
    return;
}

static void get_parent_dir (char *filename, char **dirnamep)
{


    int err;
    char *dir = NULL, *slash;
    struct stat statbuf;
    


    err = lstat(filename, &statbuf);

    if (err || (!S_ISLNK(statbuf.st_mode))) {
	/* no such file, or file is not a link; these are the "normal"
	 * cases where we can just return the parent directory.
	 */
	dir = strdup(filename);
    }
    else {
	/* filename is a symlink.  we've presumably already tried
	 * to stat it and found it to be missing (dangling link),
	 * but this code doesn't care if the target is really there
	 * or not.
	 */
	int namelen;
	char *linkbuf;

	linkbuf = malloc(PATH_MAX+1);
	namelen = readlink(filename, linkbuf, PATH_MAX+1);
	if (namelen == -1) {
	    /* something strange has happened between the time that
	     * we determined that this was a link and the time that
	     * we attempted to read it; punt and use the old name.
	     */
	    dir = strdup(filename);
	}
	else {
	    /* successfully read the link */
	    linkbuf[namelen] = '\0'; /* readlink doesn't null terminate */
	    dir = strdup(linkbuf);
	    free(linkbuf);
	}
    }

    slash = strrchr(dir, '/');
    if (!slash) strncpy(dir, ".", 2);
    else {
	if (slash == dir) *(dir + 1) = '\0';
	else *slash = '\0';
    }

    *dirnamep = dir;
    return;
}



int ompi_io_ompio_break_file_view (mca_io_ompio_file_t *fh,
                                   struct iovec *iov,
                                   int count,
                                   int stripe_count,
                                   size_t stripe_size,
                                   struct iovec **broken_iov,
                                   int *broken_count)
{



    struct iovec *temp_iov = NULL;
    int i = 0;
    int k = 0;
    int block = 1;
    int broken = 0;
    size_t remaining = 0;
    size_t temp = 0;
    OPAL_PTRDIFF_TYPE current_offset = 0;


    /* allocate an initial iovec, will grow if needed */
    temp_iov = (struct iovec *) malloc 
        (count * sizeof (struct iovec));
    if (NULL == temp_iov) {
        opal_output(1, "OUT OF MEMORY\n");
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    while (i < count) {
        if (count*block <= k) {
            block ++;
            temp_iov = (struct iovec *)realloc
                (temp_iov, count * block *sizeof(struct iovec));
            if (NULL == temp_iov) {
                opal_output(1, "OUT OF MEMORY\n");
                return OMPI_ERR_OUT_OF_RESOURCE;
            }
        }
        if (0 == broken) {
            temp = (OPAL_PTRDIFF_TYPE)(iov[i].iov_base)%stripe_size;
            if ((stripe_size-temp) >= iov[i].iov_len) {
                temp_iov[k].iov_base = iov[i].iov_base;
                temp_iov[k].iov_len = iov[i].iov_len;
                i++;
                k++;
            }
            else {
                temp_iov[k].iov_base = iov[i].iov_base;
                temp_iov[k].iov_len = stripe_size-temp;
                current_offset = (OPAL_PTRDIFF_TYPE)(temp_iov[k].iov_base) + 
                    temp_iov[k].iov_len;
                remaining = iov[i].iov_len - temp_iov[k].iov_len;
                k++;
                broken ++;
            }
            continue;
        }
        temp = current_offset%stripe_size;
        if ((stripe_size-temp) >= remaining) {
            temp_iov[k].iov_base = (IOVBASE_TYPE *)current_offset;
            temp_iov[k].iov_len = remaining;
            i++;
            k++;
            broken = 0;
            current_offset = 0;
            remaining = 0;
        }
        else {
            temp_iov[k].iov_base = (IOVBASE_TYPE *)current_offset;
            temp_iov[k].iov_len = stripe_size-temp;
            current_offset += temp_iov[k].iov_len;
            remaining -= temp_iov[k].iov_len;
            k++;
            broken ++;
        }
    }
    *broken_iov = temp_iov;
    *broken_count = k;

    return 1;
}
int ompi_io_ompio_distribute_file_view (mca_io_ompio_file_t *fh,
                                        struct iovec *broken_iov,
                                        int broken_count,
                                        int num_aggregators,
                                        size_t stripe_size,
                                        int **fview_count,
                                        struct iovec **iov,
                                        int *count)
{


    int *num_entries = NULL;
    int *broken_index = NULL;
    int temp = 0;
    int *fview_cnt = NULL;
    int global_fview_count = 0;
    int i = 0;
    int *displs = NULL;
    int rc = OMPI_SUCCESS;
    struct iovec *global_fview = NULL;
    struct iovec **broken = NULL;
    MPI_Request *req=NULL, *sendreq=NULL;


    num_entries = (int *) malloc (sizeof (int) * num_aggregators);
    if (NULL == num_entries) {
        opal_output (1, "OUT OF MEMORY\n");
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    broken_index = (int *) malloc (sizeof (int) * num_aggregators);
    if (NULL == broken_index) {
        opal_output (1, "OUT OF MEMORY\n");
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    memset (num_entries, 0x0, num_aggregators * sizeof (int));
    memset (broken_index, 0x0, num_aggregators * sizeof (int));

    /* calculate how many entries in the broken iovec belong to each aggregator */
    for (i=0 ; i<broken_count ; i++) {
        temp = (int)((OPAL_PTRDIFF_TYPE)broken_iov[i].iov_base/stripe_size) % 
            num_aggregators;
        num_entries [temp] ++;
    }

    if (0 == fh->f_rank%fh->f_aggregator_index) {
        fview_cnt = (int *) malloc (sizeof (int) * fh->f_size);
        if (NULL == fview_cnt) {
            opal_output (1, "OUT OF MEMORY\n");
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        req = (MPI_Request *)malloc (fh->f_size * sizeof(MPI_Request));
        if (NULL == req) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
    }

    sendreq = (MPI_Request *)malloc (num_aggregators * sizeof(MPI_Request));
    if (NULL == sendreq) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* gather at each aggregator how many entires from the broken file view it 
       expects from each process */
    if (0 == fh->f_rank%fh->f_aggregator_index) {
        for (i=0; i<fh->f_size ; i++) {
            rc = MCA_PML_CALL(irecv(&fview_cnt[i],
                                    1,
                                    MPI_INT,
                                    i,
                                    OMPIO_TAG_GATHER,
                                    fh->f_comm,
                                    &req[i]));
            if (OMPI_SUCCESS != rc) {
                goto exit;
            }
        }
    }

    for (i=0 ; i<num_aggregators ; i++) {
        rc = MCA_PML_CALL(isend(&num_entries[i],
                                1,
                                MPI_INT,
                                i*fh->f_aggregator_index,
                                OMPIO_TAG_GATHER,
                                MCA_PML_BASE_SEND_STANDARD, 
                                fh->f_comm,
                                &sendreq[i]));
        if (OMPI_SUCCESS != rc) {
            goto exit;
        }
    }

    if (0 == fh->f_rank%fh->f_aggregator_index) {
        rc = ompi_request_wait_all (fh->f_size, req, MPI_STATUSES_IGNORE);
        if (OMPI_SUCCESS != rc) {
            goto exit;
        }
    }
    rc = ompi_request_wait_all (num_aggregators, sendreq, MPI_STATUSES_IGNORE);
    if (OMPI_SUCCESS != rc) {
        goto exit;
    }

    /*
    for (i=0 ; i<num_aggregators ; i++) {
        fh->f_comm->c_coll.coll_gather (&num_entries[i],
                                        1,
                                        MPI_INT,
                                        fview_cnt,
                                        1,
                                        MPI_INT,
                                        i*fh->f_aggregator_index,
                                        fh->f_comm,
                                        fh->f_comm->c_coll.coll_gather_module);
    }
    */

    if (0 == fh->f_rank%fh->f_aggregator_index) {
        displs = (int*) malloc (fh->f_size * sizeof (int));
        if (NULL == displs) {
            opal_output (1, "OUT OF MEMORY\n");
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        displs[0] = 0;
        global_fview_count = fview_cnt[0];
        for (i=1 ; i<fh->f_size ; i++) {
            global_fview_count += fview_cnt[i];
            displs[i] = displs[i-1] + fview_cnt[i-1];
        }

        if (global_fview_count) {
            global_fview = (struct iovec*)malloc (global_fview_count *
                                                  sizeof(struct iovec));
            if (NULL == global_fview) {
                opal_output (1, "OUT OF MEMORY\n");
                return OMPI_ERR_OUT_OF_RESOURCE;
            }
        }
    }

    broken = (struct iovec**)malloc (num_aggregators * sizeof(struct iovec *));
    if (NULL == broken) {
        opal_output (1, "OUT OF MEMORY\n");
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    for (i=0 ; i<num_aggregators ; i++) {
        broken[i] = NULL;
        if (0 != num_entries[i]) {
            broken[i] = (struct iovec*) malloc (num_entries[i] *
                                                sizeof (struct iovec));
            if (NULL == broken[i]) {
                opal_output (1, "OUT OF MEMORY\n");
                return OMPI_ERR_OUT_OF_RESOURCE;
            }
        }
    }

    for (i=0 ; i<broken_count ; i++) {
        temp = (int)((OPAL_PTRDIFF_TYPE)broken_iov[i].iov_base/stripe_size) % 
            num_aggregators;
        broken[temp][broken_index[temp]].iov_base = broken_iov[i].iov_base;
        broken[temp][broken_index[temp]].iov_len = broken_iov[i].iov_len;
        broken_index[temp] ++;
    }
    /*
    for (i=0 ; i<num_aggregators ; i++) {
        int j;
        for (j=0 ; j<num_entries[i] ; j++) {
            printf("%d->%d: OFFSET: %d   LENGTH: %d\n",
                   fh->f_rank,
                   i,
                   broken[i][j].iov_base,
                   broken[i][j].iov_len);
        }
    }
    sleep(1);
    */

    if (0 == fh->f_rank%fh->f_aggregator_index) {
        ptrdiff_t lb, extent;
        rc = ompi_datatype_get_extent(fh->f_iov_type, &lb, &extent);
        if (OMPI_SUCCESS != rc) {
                goto exit;
        }
        for (i=0; i<fh->f_size ; i++) {
            if (fview_cnt[i]) {
                char *ptmp;
                ptmp = ((char *) global_fview) + (extent * displs[i]);
                rc = MCA_PML_CALL(irecv(ptmp,
                                        fview_cnt[i],
                                        fh->f_iov_type,
                                        i,
                                        OMPIO_TAG_GATHERV,
                                        fh->f_comm,
                                        &req[i]));
                if (OMPI_SUCCESS != rc) {
                    goto exit;
                }
            }
        }
    }

    for (i=0 ; i<num_aggregators ; i++) {
        if (num_entries[i]) {
            rc = MCA_PML_CALL(isend(broken[i],
                                    num_entries[i],
                                    fh->f_iov_type,
                                    i*fh->f_aggregator_index,
                                    OMPIO_TAG_GATHERV,
                                    MCA_PML_BASE_SEND_STANDARD, 
                                    fh->f_comm,
                                    &sendreq[i]));
            if (OMPI_SUCCESS != rc) {
                goto exit;
            }
        }
    }

    if (0 == fh->f_rank%fh->f_aggregator_index) {
        for (i=0; i<fh->f_size ; i++) {
            if (fview_cnt[i]) {
                rc = ompi_request_wait (&req[i], MPI_STATUS_IGNORE);
                if (OMPI_SUCCESS != rc) {
                    goto exit;
                }
            }
        }
    }

    for (i=0; i<num_aggregators ; i++) {
        if (num_entries[i]) {
            rc = ompi_request_wait (&sendreq[i], MPI_STATUS_IGNORE);
            if (OMPI_SUCCESS != rc) {
                goto exit;
            }
        }
    }

    /*
    for (i=0 ; i<num_aggregators ; i++) {
        fh->f_comm->c_coll.coll_gatherv (broken[i],
                                         num_entries[i],
                                         fh->f_iov_type,
                                         global_fview,
                                         fview_cnt,
                                         displs,
                                         fh->f_iov_type,
                                         i*fh->f_aggregator_index,
                                         fh->f_comm,
                                         fh->f_comm->c_coll.coll_gatherv_module);
    }
    */
    /*
    for (i=0 ; i<global_fview_count ; i++) {
        printf("%d: OFFSET: %d   LENGTH: %d\n",
               fh->f_rank,
               global_fview[i].iov_base,
               global_fview[i].iov_len);
    }
    */
 exit:
    for (i=0 ; i<num_aggregators ; i++) {
        if (NULL != broken[i]) {
            free (broken[i]);
            broken[i] = NULL;
        }
    }
    if (NULL != req) {
        free (req);
    }
    if (NULL != sendreq) {
        free (sendreq);
    }
    if (NULL != broken) {
        free (broken);
        broken = NULL;
    }
    if (NULL != num_entries) {
        free (num_entries);
        num_entries = NULL;
    }
    if (NULL != broken_index) {
        free (broken_index);
        broken_index = NULL;
    }
    if (NULL != displs) {
        free (displs);
        displs = NULL;
    }

    *fview_count = fview_cnt;
    *iov = global_fview;    
    *count = global_fview_count;
    
    return rc;
}

int ompi_io_ompio_gather_data (mca_io_ompio_file_t *fh,
                               void *send_buf,
                               size_t total_bytes_sent,
                               int *bytes_sent,
                               struct iovec *broken_iovec,
                               int broken_index,
                               size_t partial,
                               void *global_buf,
                               int *bytes_per_process,
                               int *displs,
                               int num_aggregators,
                               size_t stripe_size)
{
    void **sbuf = NULL;
    size_t bytes_remaining;
    size_t *temp_position = NULL;
    size_t part;
    int current;
    int temp = 0;
    int i = 0;
    int rc = OMPI_SUCCESS;
    MPI_Request *req=NULL, *sendreq=NULL;


    current = broken_index;
    part = partial;

    sbuf = (void**) malloc (num_aggregators * sizeof(void *));
    if (NULL == sbuf) {
        opal_output (1, "OUT OF MEMORY\n");
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    temp_position = (size_t *) malloc (num_aggregators * sizeof(size_t));
    if (NULL == temp_position) {
        opal_output (1, "OUT OF MEMORY\n");
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    memset (temp_position, 0x0, num_aggregators * sizeof (size_t));

    for (i=0 ; i<num_aggregators ; i++) {
        sbuf[i] = NULL;
        if (0 != bytes_sent[i]) {
            sbuf[i] = (void *) malloc (bytes_sent[i]);
            if (NULL == sbuf[i]) {
                opal_output (1, "OUT OF MEMORY\n");
                return OMPI_ERR_OUT_OF_RESOURCE;
            }
        }
    }

    bytes_remaining = total_bytes_sent;

    while (bytes_remaining) {
        temp = (int)((OPAL_PTRDIFF_TYPE)broken_iovec[current].iov_base/stripe_size) 
            % num_aggregators;

        if (part) {
            if (bytes_remaining > part) {
                memcpy ((IOVBASE_TYPE *)((OPAL_PTRDIFF_TYPE)sbuf[temp]+
                                         temp_position[temp]),
                        (IOVBASE_TYPE *)((OPAL_PTRDIFF_TYPE)send_buf +
                                         (total_bytes_sent-bytes_remaining)),
                        part);
                bytes_remaining -= part;
                temp_position[temp] += part;
                part = 0;
                current ++;  
            }
            else  {
                memcpy ((IOVBASE_TYPE *)((OPAL_PTRDIFF_TYPE)sbuf[temp]+
                                         temp_position[temp]),
                        (IOVBASE_TYPE *)((OPAL_PTRDIFF_TYPE)send_buf +
                                         (total_bytes_sent-bytes_remaining)),
                        bytes_remaining);
                break;
            }
        }
        else {
            if (bytes_remaining > broken_iovec[current].iov_len) {
                memcpy ((IOVBASE_TYPE *)((OPAL_PTRDIFF_TYPE)sbuf[temp]+
                                         temp_position[temp]),
                        (IOVBASE_TYPE *)((OPAL_PTRDIFF_TYPE)send_buf +
                                         (total_bytes_sent-bytes_remaining)),
                        broken_iovec[current].iov_len);
                bytes_remaining -= broken_iovec[current].iov_len;
                temp_position[temp] += broken_iovec[current].iov_len;
                current ++;
            }
            else {
                memcpy ((IOVBASE_TYPE *)((OPAL_PTRDIFF_TYPE)sbuf[temp]+
                                         temp_position[temp]),
                        (IOVBASE_TYPE *)((OPAL_PTRDIFF_TYPE)send_buf +
                                         (total_bytes_sent-bytes_remaining)),
                        bytes_remaining);
                break;
            }
        }
    }

    sendreq = (MPI_Request *)malloc (num_aggregators * sizeof(MPI_Request));
    if (NULL == sendreq) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    if (0 == fh->f_rank%fh->f_aggregator_index) {
        req = (MPI_Request *)malloc (fh->f_size * sizeof(MPI_Request));
        if (NULL == req) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        for (i=0; i<fh->f_size ; i++) {
            if (bytes_per_process[i]) {
                rc = MCA_PML_CALL(irecv((char *)global_buf + displs[i],
                                        bytes_per_process[i],
                                        MPI_BYTE,
                                        i,
                                        OMPIO_TAG_GATHERV,
                                        fh->f_comm,
                                        &req[i]));
                if (OMPI_SUCCESS != rc) {
                    goto exit;
                }
            }
        }
    }

    for (i=0 ; i<num_aggregators ; i++) {
        if (bytes_sent[i]) {
            rc = MCA_PML_CALL(isend(sbuf[i],
                                    bytes_sent[i],
                                    MPI_BYTE,
                                    i*fh->f_aggregator_index,
                                    OMPIO_TAG_GATHERV,
                                    MCA_PML_BASE_SEND_STANDARD, 
                                    fh->f_comm,
                                    &sendreq[i]));
            if (OMPI_SUCCESS != rc) {
                goto exit;
            }
        }
    }

    if (0 == fh->f_rank%fh->f_aggregator_index) {
        for (i=0; i<fh->f_size ; i++) {
            if (bytes_per_process[i]) {
                rc = ompi_request_wait (&req[i], MPI_STATUS_IGNORE);
                if (OMPI_SUCCESS != rc) {
                    goto exit;
                }
            }
        }
    }
    for (i=0; i<num_aggregators ; i++) {
        if (bytes_sent[i]) {
            rc = ompi_request_wait (&sendreq[i], MPI_STATUS_IGNORE);
            if (OMPI_SUCCESS != rc) {
                goto exit;
            }
        }
    }
    /*
    for (i=0 ; i<num_aggregators ; i++) {
        fh->f_comm->c_coll.coll_gatherv (sbuf[i],
                                         bytes_sent[i],
                                         MPI_BYTE,
                                         global_buf,
                                         bytes_per_process,
                                         displs,
                                         MPI_BYTE,
                                         i*fh->f_aggregator_index,
                                         fh->f_comm,
                                         fh->f_comm->c_coll.coll_gatherv_module);
    }
    */

 exit:
    for (i=0 ; i<num_aggregators ; i++) {
        if (NULL != sbuf[i]) {
            free (sbuf[i]);
            sbuf[i] = NULL;
        }
    }
    if (NULL != req) {
        free (req);
    }
    if (NULL != sendreq) {
        free (sendreq);
    }
    if (NULL != sbuf) {
        free (sbuf);
        sbuf = NULL;
    }
    if (NULL != temp_position) {
        free (temp_position);
        temp_position = NULL;
    }

    return rc;
}

int ompi_io_ompio_scatter_data (mca_io_ompio_file_t *fh,
                                void *receive_buf,
                                size_t total_bytes_recv,
                                int *bytes_received,
                                struct iovec *broken_iovec,
                                int broken_index,
                                size_t partial,
                                void *global_buf,
                                int *bytes_per_process,
                                int *displs,
                                int num_aggregators,
                                size_t stripe_size)
{
    void **rbuf = NULL;
    size_t bytes_remaining;
    size_t *temp_position = NULL;
    size_t part;
    int current;
    int temp = 0;
    int i = 0;
    int rc = OMPI_SUCCESS;
    MPI_Request *req=NULL, *recvreq=NULL;


    current = broken_index;
    part = partial;

    rbuf = (void**) malloc (num_aggregators * sizeof(void *));
    if (NULL == rbuf) {
        opal_output (1, "OUT OF MEMORY\n");
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    temp_position = (size_t *) malloc (num_aggregators * sizeof(size_t));
    if (NULL == temp_position) {
        opal_output (1, "OUT OF MEMORY\n");
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    memset (temp_position, 0x0, num_aggregators * sizeof (size_t));

    for (i=0 ; i<num_aggregators ; i++) {
        rbuf[i] = NULL;
        if (0 != bytes_received[i]) {
            rbuf[i] = (void *) malloc (bytes_received[i]);
            if (NULL == rbuf[i]) {
                opal_output (1, "OUT OF MEMORY\n");
                return OMPI_ERR_OUT_OF_RESOURCE;
            }
        }
    }

    recvreq = (MPI_Request *)malloc (num_aggregators * sizeof(MPI_Request));
    if (NULL == recvreq) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    for (i=0 ; i<num_aggregators ; i++) {
        if (bytes_received[i]) {
            rc = MCA_PML_CALL(irecv(rbuf[i],
                                    bytes_received[i],
                                    MPI_BYTE,
                                    i*fh->f_aggregator_index,
                                    OMPIO_TAG_SCATTERV,
                                    fh->f_comm,
                                    &recvreq[i]));
            if (OMPI_SUCCESS != rc) {
                goto exit;
            }
        }
    }

    if (0 == fh->f_rank%fh->f_aggregator_index) {
        req = (MPI_Request *)malloc (fh->f_size * sizeof(MPI_Request));
        if (NULL == req) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        for (i=0; i<fh->f_size ; i++) {
            if (bytes_per_process[i]) {
                rc = MCA_PML_CALL(isend((char *)global_buf + displs[i],
                                        bytes_per_process[i],
                                        MPI_BYTE,
                                        i,
                                        OMPIO_TAG_SCATTERV,
                                        MCA_PML_BASE_SEND_STANDARD,
                                        fh->f_comm,
                                        &req[i]));
                if (OMPI_SUCCESS != rc) {
                    goto exit;
                }
            }
        }
    }

    for (i=0; i<num_aggregators ; i++) {
        if (bytes_received[i]) {
            rc = ompi_request_wait (&recvreq[i], MPI_STATUS_IGNORE);
            if (OMPI_SUCCESS != rc) {
                goto exit;
            }
        }
    }
    if (0 == fh->f_rank%fh->f_aggregator_index) {
        for (i=0; i<fh->f_size ; i++) {
            if (bytes_per_process[i]) {
                rc = ompi_request_wait (&req[i], MPI_STATUS_IGNORE);
                if (OMPI_SUCCESS != rc) {
                    goto exit;
                }
            }
        }
    }
    /*
    for (i=0 ; i<num_aggregators ; i++) {
        fh->f_comm->c_coll.coll_scatterv (global_buf,
                                          bytes_per_process,
                                          displs,
                                          MPI_BYTE,
                                          rbuf[i],
                                          bytes_received[i],
                                          MPI_BYTE,
                                          i*fh->f_aggregator_index,
                                          fh->f_comm,
                                          fh->f_comm->c_coll.coll_scatterv_module);
    }
    */
    bytes_remaining = total_bytes_recv;

    while (bytes_remaining) {
        temp = (int)((OPAL_PTRDIFF_TYPE)broken_iovec[current].iov_base/stripe_size) 
            % num_aggregators;

        if (part) {
            if (bytes_remaining > part) {
                memcpy ((IOVBASE_TYPE *)((OPAL_PTRDIFF_TYPE)receive_buf +
                                         (total_bytes_recv-bytes_remaining)),
                        (IOVBASE_TYPE *)((OPAL_PTRDIFF_TYPE)rbuf[temp]+
                                         temp_position[temp]),
                        part);
                bytes_remaining -= part;
                temp_position[temp] += part;
                part = 0;
                current ++;  
            }
            else  {
                memcpy ((IOVBASE_TYPE *)((OPAL_PTRDIFF_TYPE)receive_buf +
                                         (total_bytes_recv-bytes_remaining)),
                        (IOVBASE_TYPE *)((OPAL_PTRDIFF_TYPE)rbuf[temp]+
                                         temp_position[temp]),
                        bytes_remaining);
                break;
            }
        }
        else {
            if (bytes_remaining > broken_iovec[current].iov_len) {
                memcpy ((IOVBASE_TYPE *)((OPAL_PTRDIFF_TYPE)receive_buf +
                                         (total_bytes_recv-bytes_remaining)),
                        (IOVBASE_TYPE *)((OPAL_PTRDIFF_TYPE)rbuf[temp]+
                                         temp_position[temp]),
                        broken_iovec[current].iov_len);
                bytes_remaining -= broken_iovec[current].iov_len;
                temp_position[temp] += broken_iovec[current].iov_len;
                current ++;
            }
            else {
                memcpy ((IOVBASE_TYPE *)((OPAL_PTRDIFF_TYPE)receive_buf +
                                         (total_bytes_recv-bytes_remaining)),
                        (IOVBASE_TYPE *)((OPAL_PTRDIFF_TYPE)rbuf[temp]+
                                         temp_position[temp]),
                        bytes_remaining);
                break;
            }
        }
    }

 exit:
    for (i=0 ; i<num_aggregators ; i++) {
        if (NULL != rbuf[i]) {
            free (rbuf[i]);
            rbuf[i] = NULL;
        }
    }
    if (NULL != req) {
        free (req);
    }
    if (NULL != recvreq) {
        free (recvreq);
    }
    if (NULL != rbuf) {
        free (rbuf);
        rbuf = NULL;
    }
    if (NULL != temp_position) {
        free (temp_position);
        temp_position = NULL;
    }

    return rc;
}

int ompi_io_ompio_send_data (mca_io_ompio_file_t *fh,
                             void *send_buf,
                             size_t total_bytes_sent,
                             struct iovec *decoded_iov,
                             int decoded_count,
                             int *bytes_sent,
                             struct iovec *broken_iovec,
                             int *current,
                             size_t *part,
                             void *global_buf,
                             int *bytes_per_process,
                             int *displs,
                             int num_aggregators,
                             size_t stripe_size)
{
    char *sbuf = NULL;
    size_t temp_position = 0;
    int i,k;
    int rc = OMPI_SUCCESS;
    MPI_Request *req=NULL, *sendreq=NULL;

    if (total_bytes_sent) {
        sbuf = malloc (total_bytes_sent);
        if (NULL == sbuf) {
            opal_output (1, "OUT OF MEMORY\n");
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
    }

    /* copy the data to be sent into sbuf */
    if ((fh->f_flags & OMPIO_CONTIGUOUS_MEMORY) && total_bytes_sent) {
        for (i=0 ; i<num_aggregators ; i++) {
            size_t temp = bytes_sent[i];
            size_t position = 0;
            
            if (temp) {
                for (k=0 ; k<current[i] ; k++) {
                    position += broken_iovec[k].iov_len;
                }
            }
            else {
                continue;
            }

            while (temp) {
                if (part[i]) {
                    if (temp > part[i]) {
                        memcpy ((IOVBASE_TYPE *)((OPAL_PTRDIFF_TYPE)sbuf+
                                                 temp_position),
                                (IOVBASE_TYPE *)((OPAL_PTRDIFF_TYPE)send_buf +
                                                 position + 
                                                 (broken_iovec[current[i]].iov_len 
                                                  - part[i])),
                                part[i]);
                        temp -= part[i];
                        temp_position += part[i];
                        part[i] = 0;
                        position += broken_iovec[current[i]].iov_len;
                        current[i] ++;
                    }
                    else {
                        memcpy ((IOVBASE_TYPE *)((OPAL_PTRDIFF_TYPE)sbuf+
                                                 temp_position),
                                (IOVBASE_TYPE *)((OPAL_PTRDIFF_TYPE)send_buf +
                                                 position +
                                                 (broken_iovec[current[i]].iov_len 
                                                  - part[i])),
                                temp);
                        temp_position += temp;
                        break;
                    }
                }
                else {
                    if (temp > broken_iovec[current[i]].iov_len) {
                        memcpy ((IOVBASE_TYPE *)((OPAL_PTRDIFF_TYPE)sbuf+
                                                 temp_position),
                                (IOVBASE_TYPE *)((OPAL_PTRDIFF_TYPE)send_buf +
                                                 position),
                                broken_iovec[current[i]].iov_len);
                        temp -= broken_iovec[current[i]].iov_len;
                        temp_position += broken_iovec[current[i]].iov_len;
                        position += broken_iovec[current[i]].iov_len;
                        current[i] ++;
                    }
                    else {
                        memcpy ((IOVBASE_TYPE *)((OPAL_PTRDIFF_TYPE)sbuf +
                                                 temp_position),
                                (IOVBASE_TYPE *)((OPAL_PTRDIFF_TYPE)send_buf +
                                                 position),
                                temp);
                        temp_position += temp;
                        break;
                    }
                }
            }
        }
    }
    else if (total_bytes_sent) {
        for (i=0 ; i<num_aggregators ; i++) {
            size_t temp = bytes_sent[i];
            size_t position = 0;
            size_t current_position = 0;
            size_t temp2 = 0;
            OPAL_PTRDIFF_TYPE mem_address;

            if (temp) {
                for (k=0 ; k<current[i] ; k++) {
                    position += broken_iovec[k].iov_len;
                }
                if (part[i]) {
                    position += broken_iovec[current[i]].iov_len - part[i];
                }
                for (k=0 ; k<decoded_count ; k++) {
                    if (temp2+decoded_iov[k].iov_len > position) {
                        break;
                    }
                    temp2 += decoded_iov[k].iov_len;
                }
                current_position = position - temp2;
            }
            else {
                continue;
            }

            while (temp) {
                mem_address = (OPAL_PTRDIFF_TYPE)
                    (decoded_iov[k].iov_base) + current_position;
                if (temp >= 
                    (decoded_iov[k].iov_len - current_position)) {
                    memcpy (sbuf+temp_position,
                            (IOVBASE_TYPE *)mem_address,
                            decoded_iov[k].iov_len - current_position);
                    temp -= (decoded_iov[k].iov_len - current_position);
                    temp_position += 
                        (decoded_iov[k].iov_len - current_position);
                    k++;
                    current_position = 0;
                }
                else {
                    memcpy (sbuf+temp_position,
                            (IOVBASE_TYPE *)mem_address,
                            temp);
                    temp_position += temp;
                    break;
                }
            }
        }
    }

    /* send the data */
    sendreq = (MPI_Request *)malloc (num_aggregators * sizeof(MPI_Request));
    if (NULL == sendreq) {
        opal_output (1, "OUT OF MEMORY\n");
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    if (0 == fh->f_rank%fh->f_aggregator_index) {
        req = (MPI_Request *)malloc (fh->f_size * sizeof(MPI_Request));
        if (NULL == req) {
            opal_output (1, "OUT OF MEMORY\n");
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        for (i=0; i<fh->f_size ; i++) {
            if (bytes_per_process[i]) {
                rc = MCA_PML_CALL(irecv((char *)global_buf + displs[i],
                                        bytes_per_process[i],
                                        MPI_BYTE,
                                        i,
                                        OMPIO_TAG_GATHERV,
                                        fh->f_comm,
                                        &req[i]));
                if (OMPI_SUCCESS != rc) {
                    opal_output (1, "Aggregator %d failed to recieve data from process %d\n",
                                 fh->f_rank, i);
                    goto exit;
                }
            }
        }
    }

    temp_position = 0;
    for (i=0 ; i<num_aggregators ; i++) {
        if (bytes_sent[i]) {
            rc = MCA_PML_CALL(isend((char *)sbuf + temp_position,
                                    bytes_sent[i],
                                    MPI_BYTE,
                                    i*fh->f_aggregator_index,
                                    OMPIO_TAG_GATHERV,
                                    MCA_PML_BASE_SEND_STANDARD, 
                                    fh->f_comm,
                                    &sendreq[i]));
            if (OMPI_SUCCESS != rc) {
                opal_output (1, "Process %d failed to send data to Aggregator %d\n",
                             fh->f_rank, i*fh->f_aggregator_index);
                goto exit;
            }
            temp_position += bytes_sent[i];
        }
    }

    if (0 == fh->f_rank%fh->f_aggregator_index) {
        for (i=0; i<fh->f_size ; i++) {
            if (bytes_per_process[i]) {
                rc = ompi_request_wait (&req[i], MPI_STATUS_IGNORE);
                if (OMPI_SUCCESS != rc) {
                    opal_output (1, "%d request_wait failed for %d\n",
                                 fh->f_rank, i);
                    goto exit;
                }
            }
        }
    }
    for (i=0; i<num_aggregators ; i++) {
        if (bytes_sent[i]) {
            rc = ompi_request_wait (&sendreq[i], MPI_STATUS_IGNORE);
            if (OMPI_SUCCESS != rc) {
                opal_output (1, "%d send request_wait failed for %d\n",
                             fh->f_rank, i*fh->f_aggregator_index);
                goto exit;
            }
        }
    }

 exit:

    if (NULL != req) {
        free (req);
    }
    if (NULL != sendreq) {
        free (sendreq);
    }
    if (NULL != sbuf) {
        free (sbuf);
        sbuf = NULL;
    }
    return rc;
}

int ompi_io_ompio_receive_data (mca_io_ompio_file_t *fh,
                                void *recv_buf,
                                size_t total_bytes_recv,
                                struct iovec *decoded_iov,
                                int decoded_count,
                                int *bytes_recv,
                                struct iovec *broken_iovec,
                                int *current,
                                size_t *part,
                                void *global_buf,
                                int *bytes_per_process,
                                int *displs,
                                int num_aggregators,
                                size_t stripe_size)
{
    void *rbuf = NULL;
    size_t temp_position = 0;
    int i, k;
    int rc = OMPI_SUCCESS;
    MPI_Request *req=NULL, *recvreq=NULL;

    if (total_bytes_recv) {
        rbuf = malloc (total_bytes_recv);
        if (NULL == rbuf) {
            opal_output (1, "OUT OF MEMORY\n");
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
    }

    recvreq = (MPI_Request *)malloc (num_aggregators * sizeof(MPI_Request));
    if (NULL == recvreq) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    for (i=0 ; i<num_aggregators ; i++) {
        if (bytes_recv[i]) {
            rc = MCA_PML_CALL(irecv((char *)rbuf+temp_position,
                                    bytes_recv[i],
                                    MPI_BYTE,
                                    i*fh->f_aggregator_index,
                                    OMPIO_TAG_SCATTERV,
                                    fh->f_comm,
                                    &recvreq[i]));
            if (OMPI_SUCCESS != rc) {
                goto exit;
            }
            temp_position += bytes_recv[i];
        }
    }

    if (0 == fh->f_rank%fh->f_aggregator_index) {
        req = (MPI_Request *)malloc (fh->f_size * sizeof(MPI_Request));
        if (NULL == req) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        for (i=0; i<fh->f_size ; i++) {
            if (bytes_per_process[i]) {
                rc = MCA_PML_CALL(isend((char *)global_buf + displs[i],
                                        bytes_per_process[i],
                                        MPI_BYTE,
                                        i,
                                        OMPIO_TAG_SCATTERV,
                                        MCA_PML_BASE_SEND_STANDARD,
                                        fh->f_comm,
                                        &req[i]));
                if (OMPI_SUCCESS != rc) {
                    goto exit;
                }
            }
        }
    }

    for (i=0; i<num_aggregators ; i++) {
        if (bytes_recv[i]) {
            rc = ompi_request_wait (&recvreq[i], MPI_STATUS_IGNORE);
            if (OMPI_SUCCESS != rc) {
                goto exit;
            }
        }
    }
    if (0 == fh->f_rank%fh->f_aggregator_index) {
        for (i=0; i<fh->f_size ; i++) {
            if (bytes_per_process[i]) {
                rc = ompi_request_wait (&req[i], MPI_STATUS_IGNORE);
                if (OMPI_SUCCESS != rc) {
                    goto exit;
                }
            }
        }
    }
    temp_position = 0;
    if ((fh->f_flags & OMPIO_CONTIGUOUS_MEMORY) && total_bytes_recv) {
        for (i=0 ; i<num_aggregators ; i++) {
            size_t temp = bytes_recv[i];
            size_t position = 0;
            
            if (temp) {
                for (k=0 ; k<current[i] ; k++) {
                    position += broken_iovec[k].iov_len;
                }
            }
            else {
                continue;
            }

            while (temp) {
                if (part[i]) {
                    if (temp > part[i]) {
                        memcpy ((IOVBASE_TYPE *)((OPAL_PTRDIFF_TYPE)recv_buf +
                                                 position + 
                                                 (broken_iovec[current[i]].iov_len 
                                                  - part[i])),
                                (IOVBASE_TYPE *)((OPAL_PTRDIFF_TYPE)rbuf+
                                                 temp_position),
                                part[i]);
                        temp -= part[i];
                        temp_position += part[i];
                        part[i] = 0;
                        position += broken_iovec[current[i]].iov_len;
                        current[i] ++;
                    }
                    else  {
                        memcpy ((IOVBASE_TYPE *)((OPAL_PTRDIFF_TYPE)recv_buf +
                                                 position +
                                                 (broken_iovec[current[i]].iov_len 
                                                  - part[i])),
                                (IOVBASE_TYPE *)((OPAL_PTRDIFF_TYPE)rbuf+
                                                 temp_position),
                                temp);
                        temp_position += temp;
                        break;
                    }
                }
                else {
                    if (temp > broken_iovec[current[i]].iov_len) {
                        memcpy ((IOVBASE_TYPE *)((OPAL_PTRDIFF_TYPE)recv_buf +
                                                 position),
                                (IOVBASE_TYPE *)((OPAL_PTRDIFF_TYPE)rbuf+
                                                 temp_position),
                                broken_iovec[current[i]].iov_len);
                        temp -= broken_iovec[current[i]].iov_len;
                        temp_position += broken_iovec[current[i]].iov_len;
                        position += broken_iovec[current[i]].iov_len;
                        current[i] ++;
                    }
                    else {
                        memcpy ((IOVBASE_TYPE *)((OPAL_PTRDIFF_TYPE)recv_buf +
                                                 position),
                                (IOVBASE_TYPE *)((OPAL_PTRDIFF_TYPE)rbuf +
                                                 temp_position),
                                temp);
                        temp_position += temp;
                        break;
                    }
                }
            }
        }
    }
    else if (total_bytes_recv) {
        for (i=0 ; i<num_aggregators ; i++) {
            size_t temp = bytes_recv[i];
            size_t position = 0;
            size_t current_position = 0;
            size_t temp2 = 0;
            OPAL_PTRDIFF_TYPE mem_address;

            if (temp) {
                for (k=0 ; k<current[i] ; k++) {
                    position += broken_iovec[k].iov_len;
                }
                if (part[i]) {
                    position += broken_iovec[current[i]].iov_len - part[i];
                }
                for (k=0 ; k<decoded_count ; k++) {
                    if (temp2+decoded_iov[k].iov_len > position) {
                        break;
                    }
                    temp2 += decoded_iov[k].iov_len;
                }
                current_position = position - temp2;
            }
            else {
                continue;
            }

            while (temp) {
                mem_address = (OPAL_PTRDIFF_TYPE)
                    (decoded_iov[k].iov_base) + current_position;
                if (temp >= 
                    (decoded_iov[k].iov_len - current_position)) {
                    memcpy ((IOVBASE_TYPE *)mem_address,
                            (IOVBASE_TYPE *)((OPAL_PTRDIFF_TYPE)rbuf +
                                             temp_position),
                            decoded_iov[k].iov_len - current_position);
                    temp -= (decoded_iov[k].iov_len - current_position);
                    temp_position += 
                        (decoded_iov[k].iov_len - current_position);
                    k++;
                    current_position = 0;
                }
                else {
                    memcpy ((IOVBASE_TYPE *)mem_address,
                            (IOVBASE_TYPE *)((OPAL_PTRDIFF_TYPE)rbuf +
                                             temp_position),
                            temp);
                    temp_position += temp;
                    break;
                }
            }
        }
    }
 exit:
    if (NULL != req) {
        free (req);
    }
    if (NULL != recvreq) {
        free (recvreq);
    }
    if (NULL != rbuf) {
        free (rbuf);
        rbuf = NULL;
    }
    return rc;
}






#if 0

int ompi_io_ompio_receive_data (mca_io_ompio_file_t *fh,
                                void *recv_buf,
                                int *bytes_recv,
                                struct iovec *broken_iovec,
                                int *current,
                                size_t *part,
                                void *global_buf,
                                int *bytes_per_process,
                                int *displs,
                                int num_aggregators,
                                size_t stripe_size)
{
    void **rbuf = NULL;
    size_t *temp_position = NULL;
    int i, k;
    int rc = OMPI_SUCCESS;
    MPI_Request *req=NULL, *recvreq=NULL;

    rbuf = (void**) malloc (num_aggregators * sizeof(void *));
    if (NULL == rbuf) {
        opal_output (1, "OUT OF MEMORY\n");
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    temp_position = (size_t *) malloc (num_aggregators * sizeof(size_t));
    if (NULL == temp_position) {
        opal_output (1, "OUT OF MEMORY\n");
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    memset (temp_position, 0x0, num_aggregators * sizeof (size_t));

    for (i=0 ; i<num_aggregators ; i++) {
        rbuf[i] = NULL;
        if (0 != bytes_recv[i]) {
            rbuf[i] = (void *) malloc (bytes_recv[i]);
            if (NULL == rbuf[i]) {
                opal_output (1, "OUT OF MEMORY\n");
                return OMPI_ERR_OUT_OF_RESOURCE;
            }
        }
    }

    recvreq = (MPI_Request *)malloc (num_aggregators * sizeof(MPI_Request));
    if (NULL == recvreq) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    for (i=0 ; i<num_aggregators ; i++) {
        if (bytes_recv[i]) {
            rc = MCA_PML_CALL(irecv(rbuf[i],
                                    bytes_recv[i],
                                    MPI_BYTE,
                                    i*fh->f_aggregator_index,
                                    OMPIO_TAG_SCATTERV,
                                    fh->f_comm,
                                    &recvreq[i]));
            if (OMPI_SUCCESS != rc) {
                goto exit;
            }
        }
    }

    if (0 == fh->f_rank%fh->f_aggregator_index) {
        req = (MPI_Request *)malloc (fh->f_size * sizeof(MPI_Request));
        if (NULL == req) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        for (i=0; i<fh->f_size ; i++) {
            if (bytes_per_process[i]) {
                rc = MCA_PML_CALL(isend((char *)global_buf + displs[i],
                                        bytes_per_process[i],
                                        MPI_BYTE,
                                        i,
                                        OMPIO_TAG_SCATTERV,
                                        MCA_PML_BASE_SEND_STANDARD,
                                        fh->f_comm,
                                        &req[i]));
                if (OMPI_SUCCESS != rc) {
                    goto exit;
                }
            }
        }
    }

    for (i=0; i<num_aggregators ; i++) {
        if (bytes_recv[i]) {
            rc = ompi_request_wait (&recvreq[i], MPI_STATUS_IGNORE);
            if (OMPI_SUCCESS != rc) {
                goto exit;
            }
        }
    }
    if (0 == fh->f_rank%fh->f_aggregator_index) {
        for (i=0; i<fh->f_size ; i++) {
            if (bytes_per_process[i]) {
                rc = ompi_request_wait (&req[i], MPI_STATUS_IGNORE);
                if (OMPI_SUCCESS != rc) {
                    goto exit;
                }
            }
        }
    }
    /*
    for (i=0 ; i<num_aggregators ; i++) {
        fh->f_comm->c_coll.coll_scatterv (global_buf,
                                          bytes_per_process,
                                          displs,
                                          MPI_BYTE,
                                          rbuf[i],
                                          bytes_recv[i],
                                          MPI_BYTE,
                                          i*fh->f_aggregator_index,
                                          fh->f_comm,
                                          fh->f_comm->c_coll.coll_scatterv_module);
    }
    */

    for (i=0 ; i<num_aggregators ; i++) {
        size_t temp = bytes_recv[i];
        size_t position = 0;

        if (temp) {
            for (k=0 ; k<current[i] ; k++) {
                position += broken_iovec[k].iov_len;
            }
        }

        while (temp) {
            if (part[i]) {
                if (temp > part[i]) {
                    memcpy ((IOVBASE_TYPE *)((OPAL_PTRDIFF_TYPE)recv_buf +
                                             position + temp_position[i] +
                                             (broken_iovec[current[i]].iov_len - part[i])),
                            (IOVBASE_TYPE *)((OPAL_PTRDIFF_TYPE)rbuf[i]+
                                             temp_position[i]),
                            part[i]);
                    temp -= part[i];
                    temp_position[i] += part[i];
                    part[i] = 0;
                    current[i] ++;  
                }
                else  {
                    memcpy ((IOVBASE_TYPE *)((OPAL_PTRDIFF_TYPE)recv_buf +
                                             position + temp_position[i] +
                                             (broken_iovec[current[i]].iov_len - part[i])),
                            (IOVBASE_TYPE *)((OPAL_PTRDIFF_TYPE)rbuf[i]+
                                             temp_position[i]),
                            temp);
                    break;
                }
            }
            else {
                if (temp > broken_iovec[current[i]].iov_len) {
                    memcpy ((IOVBASE_TYPE *)((OPAL_PTRDIFF_TYPE)recv_buf +
                                             position + temp_position[i]),
                            (IOVBASE_TYPE *)((OPAL_PTRDIFF_TYPE)rbuf[i]+
                                             temp_position[i]),
                            broken_iovec[current[i]].iov_len);
                    temp -= broken_iovec[current[i]].iov_len;
                    temp_position[i] += broken_iovec[current[i]].iov_len;
                    current[i] ++;
                }
                else {
                    memcpy ((IOVBASE_TYPE *)((OPAL_PTRDIFF_TYPE)recv_buf +
                                             position + temp_position[i]),
                            (IOVBASE_TYPE *)((OPAL_PTRDIFF_TYPE)rbuf[i]+
                                             temp_position[i]),
                            temp);
                    break;
                }
            }
        }
    }

 exit:
    for (i=0 ; i<num_aggregators ; i++) {
        if (NULL != rbuf[i]) {
            free (rbuf[i]);
            rbuf[i] = NULL;
        }
    }
    if (NULL != req) {
        free (req);
    }
    if (NULL != recvreq) {
        free (recvreq);
    }
    if (NULL != rbuf) {
        free (rbuf);
        rbuf = NULL;
    }
    if (NULL != temp_position) {
        free (temp_position);
        temp_position = NULL;
    }

    return rc;
}


int ompi_io_ompio_send_data (mca_io_ompio_file_t *fh,
                             void *send_buf,
                             int *bytes_sent,
                             struct iovec *broken_iovec,
                             int *current,
                             size_t *part,
                             void *global_buf,
                             int *bytes_per_process,
                             int *displs,
                             int num_aggregators,
                             size_t stripe_size)
{
    void **sbuf = NULL;
    size_t *temp_position = NULL;
    int i,k;
    int rc = OMPI_SUCCESS;
    MPI_Request *req=NULL, *sendreq=NULL;

    sbuf = (void**) malloc (num_aggregators * sizeof(void *));
    if (NULL == sbuf) {
        opal_output (1, "OUT OF MEMORY\n");
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    temp_position = (size_t *) malloc (num_aggregators * sizeof(size_t));
    if (NULL == temp_position) {
        opal_output (1, "OUT OF MEMORY\n");
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    memset (temp_position, 0x0, num_aggregators * sizeof (size_t));

    for (i=0 ; i<num_aggregators ; i++) {
        sbuf[i] = NULL;
        if (0 != bytes_sent[i]) {
            sbuf[i] = (void *) malloc (bytes_sent[i]);
            if (NULL == sbuf[i]) {
                opal_output (1, "OUT OF MEMORY\n");
                return OMPI_ERR_OUT_OF_RESOURCE;
            }
        }
    }

    for (i=0 ; i<num_aggregators ; i++) {
        size_t temp = bytes_sent[i];
        size_t position = 0;

        if (temp) {
            for (k=0 ; k<current[i] ; k++) {
                position += broken_iovec[k].iov_len;
            }
        }

        while (temp) {
            if (part[i]) {
                if (temp > part[i]) {
                    memcpy ((IOVBASE_TYPE *)((OPAL_PTRDIFF_TYPE)sbuf[i]+
                                             temp_position[i]),
                            (IOVBASE_TYPE *)((OPAL_PTRDIFF_TYPE)send_buf +
                                             position + temp_position[i] +
                                             (broken_iovec[current[i]].iov_len - part[i])),
                            part[i]);
                    temp -= part[i];
                    temp_position[i] += part[i];
                    part[i] = 0;
                    current[i] ++;  
                }
                else  {
                    memcpy ((IOVBASE_TYPE *)((OPAL_PTRDIFF_TYPE)sbuf[i]+
                                             temp_position[i]),
                            (IOVBASE_TYPE *)((OPAL_PTRDIFF_TYPE)send_buf +
                                             position + temp_position[i] +
                                             (broken_iovec[current[i]].iov_len - part[i])),
                            temp);
                    break;
                }
            }
            else {
                if (temp > broken_iovec[current[i]].iov_len) {
                    memcpy ((IOVBASE_TYPE *)((OPAL_PTRDIFF_TYPE)sbuf[i]+
                                             temp_position[i]),
                            (IOVBASE_TYPE *)((OPAL_PTRDIFF_TYPE)send_buf +
                                             position + temp_position[i]),
                            broken_iovec[current[i]].iov_len);
                    temp -= broken_iovec[current[i]].iov_len;
                    temp_position[i] += broken_iovec[current[i]].iov_len;
                    current[i] ++;
                }
                else {
                    memcpy ((IOVBASE_TYPE *)((OPAL_PTRDIFF_TYPE)sbuf[i]+
                                             temp_position[i]),
                            (IOVBASE_TYPE *)((OPAL_PTRDIFF_TYPE)send_buf +
                                             position + temp_position[i]),
                            temp);
                    break;
                }
            }
        }
    }
    sendreq = (MPI_Request *)malloc (num_aggregators * sizeof(MPI_Request));
    if (NULL == sendreq) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    if (0 == fh->f_rank%fh->f_aggregator_index) {
        req = (MPI_Request *)malloc (fh->f_size * sizeof(MPI_Request));
        if (NULL == req) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        for (i=0; i<fh->f_size ; i++) {
            if (bytes_per_process[i]) {
                rc = MCA_PML_CALL(irecv((char *)global_buf + displs[i],
                                        bytes_per_process[i],
                                        MPI_BYTE,
                                        i,
                                        OMPIO_TAG_GATHERV,
                                        fh->f_comm,
                                        &req[i]));
                if (OMPI_SUCCESS != rc) {
                    goto exit;
                }
            }
        }
    }

    for (i=0 ; i<num_aggregators ; i++) {
        if (bytes_sent[i]) {
            rc = MCA_PML_CALL(isend(sbuf[i],
                                    bytes_sent[i],
                                    MPI_BYTE,
                                    i*fh->f_aggregator_index,
                                    OMPIO_TAG_GATHERV,
                                    MCA_PML_BASE_SEND_STANDARD, 
                                    fh->f_comm,
                                    &sendreq[i]));
            if (OMPI_SUCCESS != rc) {
                goto exit;
            }
        }
    }

    if (0 == fh->f_rank%fh->f_aggregator_index) {
        for (i=0; i<fh->f_size ; i++) {
            if (bytes_per_process[i]) {
                rc = ompi_request_wait (&req[i], MPI_STATUS_IGNORE);
                if (OMPI_SUCCESS != rc) {
                    goto exit;
                }
            }
        }
    }
    for (i=0; i<num_aggregators ; i++) {
        if (bytes_sent[i]) {
            rc = ompi_request_wait (&sendreq[i], MPI_STATUS_IGNORE);
            if (OMPI_SUCCESS != rc) {
                goto exit;
            }
        }
    }
    /*
    for (i=0 ; i<num_aggregators ; i++) {
        fh->f_comm->c_coll.coll_gatherv (sbuf[i],
                                         bytes_sent[i],
                                         MPI_BYTE,
                                         global_buf,
                                         bytes_per_process,
                                         displs,
                                         MPI_BYTE,
                                         i*fh->f_aggregator_index,
                                         fh->f_comm,
                                         fh->f_comm->c_coll.coll_gatherv_module);
    }
    */

 exit:
    for (i=0 ; i<num_aggregators ; i++) {
        if (NULL != sbuf[i]) {
            free (sbuf[i]);
            sbuf[i] = NULL;
        }
    }
    if (NULL != req) {
        free (req);
    }
    if (NULL != sendreq) {
        free (sendreq);
    }
    if (NULL != sbuf) {
        free (sbuf);
        sbuf = NULL;
    }
    if (NULL != temp_position) {
        free (temp_position);
        temp_position = NULL;
    }

    return rc;
}
#endif
