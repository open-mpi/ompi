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
 *  Copyright (c) 2008-2011 University of Houston. All rights reserved.
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

int mca_io_ompio_file_set_view (ompi_file_t *fp,
                                OMPI_MPI_OFFSET_TYPE disp,
                                ompi_datatype_t *etype,
                                ompi_datatype_t *filetype,
                                char *datarep,
                                ompi_info_t *info)
{
    mca_io_ompio_data_t *data;
    mca_io_ompio_file_t *fh;
    size_t max_data = 0;
    MPI_Aint lb,ub;

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

    fh->f_flags |= OMPIO_FILE_VIEW_IS_SET;
    fh->f_disp = disp;
    fh->f_offset += disp;
    fh->f_datarep = strdup (datarep);

    fh->f_iov_count = 0;

    if (opal_datatype_is_contiguous_memory_layout(&etype->super,1)) {
        if (opal_datatype_is_contiguous_memory_layout(&filetype->super,1)) {
            fh->f_flags |= OMPIO_CONTIGUOUS_FVIEW;
        }
    }

    ompi_io_ompio_decode_datatype (fh, 
                                   filetype, 
                                   1, 
                                   NULL, 
                                   &max_data,
                                   &fh->f_decoded_iov, 
                                   &fh->f_iov_count);
    /*
    if (0 == fh->f_rank) {
        int i;
        printf ("%d Entries: \n",fh->f_iov_count);
        for (i=0 ; i<fh->f_iov_count ; i++) {
            printf ("\t{%p, %lld}\n", 
                    fh->f_decoded_iov[i].iov_base, 
                    fh->f_decoded_iov[i].iov_len);
        }
    }
    */
    /* 
     * Create a derived datatype for the created iovec 
      
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
    */
    opal_datatype_get_extent(&filetype->super, &lb, &fh->f_view_extent);
    opal_datatype_type_ub   (&filetype->super, &ub);
    opal_datatype_type_size (&etype->super, &fh->f_etype_size);
    opal_datatype_type_size (&filetype->super, &fh->f_view_size);
    ompi_datatype_duplicate (etype, &fh->f_etype);
    ompi_datatype_duplicate (filetype, &fh->f_filetype);
    
    fh->f_cc_size = get_contiguous_chunk_size (fh);
    /*
    mca_fcoll_base_param = mca_base_param_find("fcoll", NULL, NULL);
    mca_base_param_lookup_string (mca_fcoll_base_param, &names);

    if (NULL == names) {        
        if ((int)cc_size >= mca_io_ompio_bytes_per_agg && 
            cc_size >= fh->f_stripe_size) {
            mca_base_param_set_string(mca_fcoll_base_param, "individual");
        }
        if ((int)cc_size < mca_io_ompio_bytes_per_agg && 
            cc_size >= fh->f_stripe_size) {
            mca_base_param_set_string(mca_fcoll_base_param, "dynamic");
        }
        else if ((int)cc_size < mca_io_ompio_bytes_per_agg && 
                 cc_size < fh->f_stripe_size) {
            mca_base_param_set_string(mca_fcoll_base_param, "two_phase");
        }
    }
    */
    if (OMPI_SUCCESS != mca_fcoll_base_file_select (&data->ompio_fh,
                                                    NULL)) {
        opal_output(1, "mca_fcoll_base_file_select() failed\n");
        return OMPI_ERROR;
    }
    /*
    printf ("%d: LB=%d  UB=%d  Extent=%d  Size=%d\n",
            fh->f_rank,lb,ub,fh->f_view_extent,fh->f_view_size);
    */
    /*
    ompi_ddt_type_extent (fh->f_etype, &fh->f_etype_extent);
    ompi_ddt_type_extent (fh->f_filetype, &fh->f_filetype_extent);
    */

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
    int uniform = 1;
    int i = 0;
    OMPI_MPI_OFFSET_TYPE avg = 0;
    OMPI_MPI_OFFSET_TYPE global_avg = 0;

    for (i=0 ; i<(int)fh->f_iov_count ; i++) {
        avg += fh->f_decoded_iov[i].iov_len;
        if (i && uniform) {
            if (fh->f_decoded_iov[i].iov_len != fh->f_decoded_iov[i-1].iov_len) {
                uniform = 0;
            }
        }
    }
    avg = avg/fh->f_iov_count;
    fh->f_comm->c_coll.coll_allreduce (&avg,
                                       &global_avg,
                                       1,
                                       MPI_LONG,
                                       MPI_SUM,
                                       fh->f_comm,
                                       fh->f_comm->c_coll.coll_allreduce_module);
    global_avg = global_avg/fh->f_size;

    if (global_avg == avg && uniform) {
        fh->f_flags |= OMPIO_UNIFORM_FVIEW;
    }

    return global_avg;
}



    /*
    opal_convertor_clone (fh->f_convertor, &convertor, 0);
    
    if (OMPI_SUCCESS != opal_convertor_prepare_for_send (&convertor, filetype, 1, NULL))
    {
        printf ("Cannot attach the datatype to a convertor\n");
        return OMPI_ERROR;
    }

    remaining_length = 1 * filetype->size;

    printf ("FILETYPE SIZE: %d\n",filetype->size);
    while (0 == opal_convertor_raw(&convertor, fh->f_decoded_iov, &fh->f_iovec_count, &max_data)) 
    {
#if 1
        printf ("New raw extraction (fh->f_iovec_count = %d, max_data = %d)\n",
                fh->f_iovec_count, max_data);
        for (i = 0; i < fh->f_iovec_count; i++) 
        {
            printf ("\t{%p, %d}\n", fh->f_decoded_iov[i].iov_base, fh->f_decoded_iov[i].iov_len);
        }
#endif
        remaining_length -= max_data;
        fh->f_iovec_count = iov_num;
    }
#if 1
    printf ("LAST extraction (fh->f_iovec_count = %d, max_data = %d)\n",
            fh->f_iovec_count, max_data);
    for (i = 0; i < fh->f_iovec_count; i++) 
    {
        printf ("\t{%p, %d}\n", fh->f_decoded_iov[i].iov_base, fh->f_decoded_iov[i].iov_len);
    }
#endif

    remaining_length -= max_data;

    if (remaining_length != 0) {
        printf( "Not all raw description was been extracted (%lu bytes missing)\n",
                (unsigned long) remaining_length );
    }
    */

/*
  
    ompi_datatype_t *pdt;
    struct iovec *iov;
    int iov_count = OMPIO_IOVEC_INITIAL_SIZE;

    remote_arch = ompi_mpi_local_arch;
    ompi_ddt_create_vector( 10,1,2, MPI_INT, &pdt );
    ompi_ddt_commit( &pdt );

    iov = (struct iovec*)malloc(iov_num * sizeof(struct iovec));

    opal_convertor_clone( fh->f_convertor, &convertor, 0 );
    
    if( OMPI_SUCCESS != opal_convertor_prepare_for_send( &convertor, pdt, 1, NULL ) ) {
        printf( "Cannot attach the datatype to a convertor\n" );
        return OMPI_ERROR;
    }

    remaining_length = 1 * pdt->size;
    printf ("PDT SIZE: %d\n",pdt->size);

    while ( 0 == opal_convertor_raw(&convertor, iov, &iov_count, &max_data) ) {
        printf( "New raw extraction (iov_count = %d, max_data = %zu)\n",
                iov_count, max_data );
        for (i = 0; i < iov_count; i++) {
            printf( "\t{%p, %d}\n", iov[i].iov_base, iov[i].iov_len );
        }
        remaining_length -= max_data;
        iov_count = iov_num;
    }
    printf( "LAST Extraction (iov_count = %d, max_data = %zu)\n",
            iov_count, max_data );
    for (i = 0; i < iov_count; i++) {
        printf( "\t{%p, %d}\n", iov[i].iov_base, iov[i].iov_len );
    }

    remaining_length -= max_data;

    if( remaining_length != 0 ) {
        printf( "Not all raw description was been extracted (%lu bytes missing)\n",
                (unsigned long) remaining_length );
    }

    sleep(3);
    exit(0);
*/
