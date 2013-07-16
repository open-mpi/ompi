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
 *  Copyright (c) 2008-2013 University of Houston. All rights reserved.
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

#include "opal/datatype/opal_convertor.h"
#include "ompi/datatype/ompi_datatype.h"
#include <stdlib.h>
#include <stdio.h>

#include <unistd.h>
#include "io_ompio.h"

OMPI_MPI_OFFSET_TYPE get_contiguous_chunk_size (mca_io_ompio_file_t *);


int mca_io_ompio_set_view_internal(mca_io_ompio_file_t *fh,
				   OMPI_MPI_OFFSET_TYPE disp,
				   ompi_datatype_t *etype,
				   ompi_datatype_t *filetype,
				   char *datarep,
				   ompi_info_t *info)
{
    size_t max_data = 0;
    MPI_Aint lb,ub;    

    fh->f_iov_count   = 0;
    fh->f_disp        = disp;
    fh->f_offset      = disp;
    fh->f_total_bytes = 0;
    
    ompi_io_ompio_decode_datatype (fh, 
                                   filetype, 
                                   1, 
                                   NULL, 
                                   &max_data,
                                   &fh->f_decoded_iov, 
                                   &fh->f_iov_count);

    opal_datatype_get_extent(&filetype->super, &lb, &fh->f_view_extent);
    opal_datatype_type_ub   (&filetype->super, &ub);
    opal_datatype_type_size (&etype->super, &fh->f_etype_size);
    opal_datatype_type_size (&filetype->super, &fh->f_view_size);
    ompi_datatype_duplicate (etype, &fh->f_etype);
    ompi_datatype_duplicate (filetype, &fh->f_filetype);
    
    fh->f_cc_size = get_contiguous_chunk_size (fh);

    if (opal_datatype_is_contiguous_memory_layout(&etype->super,1)) {
        if (opal_datatype_is_contiguous_memory_layout(&filetype->super,1) && 
	    fh->f_view_extent == (OPAL_PTRDIFF_TYPE)fh->f_view_size ) {
            fh->f_flags |= OMPIO_CONTIGUOUS_FVIEW;
        }
    }

    return OMPI_SUCCESS;
}

int mca_io_ompio_file_set_view (ompi_file_t *fp,
                                OMPI_MPI_OFFSET_TYPE disp,
                                ompi_datatype_t *etype,
                                ompi_datatype_t *filetype,
                                char *datarep,
                                ompi_info_t *info)
{
    mca_io_ompio_data_t *data;
    mca_io_ompio_file_t *fh;

    data = (mca_io_ompio_data_t *) fp->f_io_selected_data;
    fh = &data->ompio_fh;
    
    if (NULL != fh->f_decoded_iov) {
        free (fh->f_decoded_iov);
        fh->f_decoded_iov = NULL;
    }

    if (NULL != fh->f_datarep) {
        free (fh->f_datarep);
        fh->f_datarep = NULL;
    }

    /* Reset the flags first */
    fh->f_flags = 0;

    fh->f_flags |= OMPIO_FILE_VIEW_IS_SET;
    fh->f_datarep = strdup (datarep);

    mca_io_ompio_set_view_internal (fh,
				    disp,
				    etype,
				    filetype,
				    datarep,
				    info);
    

    if (OMPI_SUCCESS != mca_fcoll_base_file_select (&data->ompio_fh,
                                                    NULL)) {
        opal_output(1, "mca_fcoll_base_file_select() failed\n");
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}

int mca_io_ompio_file_get_view (struct ompi_file_t *fp, 
                                OMPI_MPI_OFFSET_TYPE *disp,
                                struct ompi_datatype_t **etype, 
                                struct ompi_datatype_t **filetype,
                                char *datarep)
{
    mca_io_ompio_data_t *data;
    mca_io_ompio_file_t *fh;

    data = (mca_io_ompio_data_t *) fp->f_io_selected_data;
    fh = &data->ompio_fh;

    *disp = fh->f_disp;
    ompi_datatype_duplicate (fh->f_etype, etype);
    ompi_datatype_duplicate (fh->f_filetype, filetype);
    strcpy (datarep, fh->f_datarep);

    return OMPI_SUCCESS;
}

OMPI_MPI_OFFSET_TYPE get_contiguous_chunk_size (mca_io_ompio_file_t *fh)
{
    int uniform = 0, global_uniform = 0;
    int i = 0;
    OMPI_MPI_OFFSET_TYPE avg[3] = {0,0,0};
    OMPI_MPI_OFFSET_TYPE global_avg[3] = {0,0,0};

    /* This function does two things: first, it determines the average data chunk 
    ** size in the file view for each process and across all processes. 
    ** Second, it establishes whether the view across all processes is uniform. 
    ** By definition, uniform means:
    ** 1. the file view of each process has the same number of contiguous sections
    ** 2. each section in the file view has exactly the same size
    */

    for (i=0 ; i<(int)fh->f_iov_count ; i++) {
        avg[0] += fh->f_decoded_iov[i].iov_len;
        if (i && 0 == uniform) {
            if (fh->f_decoded_iov[i].iov_len != fh->f_decoded_iov[i-1].iov_len) {
                uniform = 1;
            }
        }
    }
    if ( 0 != fh->f_iov_count ) {
	avg[0] = avg[0]/fh->f_iov_count;
    }
    avg[1] = (OMPI_MPI_OFFSET_TYPE) fh->f_iov_count;
    avg[2] = (OMPI_MPI_OFFSET_TYPE) uniform;

    fh->f_comm->c_coll.coll_allreduce (avg,
                                       global_avg,
                                       3,
                                       MPI_LONG,
                                       MPI_SUM,
                                       fh->f_comm,
                                       fh->f_comm->c_coll.coll_allreduce_module);
    global_avg[0] = global_avg[0]/fh->f_size;
    global_avg[1] = global_avg[1]/fh->f_size;

    if ( global_avg[0] == avg[0] && 
	 global_avg[1] == avg[1] &&
	 0 == avg[2]             &&
	 0 == global_avg[2] ) {
	uniform = 0;
    }
    else {
	uniform = 1;
    }

    /* second confirmation round to see whether all processes agree
    ** on having a uniform file view or not 
    */
    fh->f_comm->c_coll.coll_allreduce (&uniform,
				       &global_uniform,
				       1,
				       MPI_INT,
				       MPI_MAX,
				       fh->f_comm,
				       fh->f_comm->c_coll.coll_allreduce_module);

    if ( 0 == global_uniform  ){
	/* yes, everybody agrees on having a uniform file view */
	fh->f_flags |= OMPIO_UNIFORM_FVIEW;
    }


    return global_avg[0];
}
