/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2017 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008-2021 University of Houston. All rights reserved.
 * Copyright (c) 2017-2018 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2017      IBM Corporation. All rights reserved.
 * Copyright (c) 2023      Advanced Micro Devices, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "opal/datatype/opal_convertor.h"
#include "ompi/datatype/ompi_datatype.h"
#include <stdlib.h>
#include <stdio.h>

#include "common_ompio.h"
#include "common_ompio_aggregators.h"
#include "ompi/mca/fcoll/base/base.h"
#include "ompi/mca/topo/topo.h"

static OMPI_MPI_OFFSET_TYPE get_contiguous_chunk_size (ompio_file_t *, int flag);
static int datatype_duplicate (ompi_datatype_t *oldtype, ompi_datatype_t **newtype );
static int datatype_duplicate  (ompi_datatype_t *oldtype, ompi_datatype_t **newtype )
{
    ompi_datatype_t *type;
    if( ompi_datatype_is_predefined(oldtype) ) {
	OBJ_RETAIN(oldtype);
        *newtype = oldtype;
        return OMPI_SUCCESS;
    }

    if ( OMPI_SUCCESS != ompi_datatype_duplicate (oldtype, &type)){
        ompi_datatype_destroy (&type);
        return MPI_ERR_INTERN;
    }
    
    ompi_datatype_set_args( type, 0, NULL, 0, NULL, 1, &oldtype, MPI_COMBINER_DUP );

    *newtype = type;
    return OMPI_SUCCESS;
}

static void fview_clear (struct ompio_fview_t *fview) {
    if (NULL != fview->f_decoded_iov) {
        free (fview->f_decoded_iov);
        fview->f_decoded_iov = NULL;
    }

    /* Reset the flags */
    fview->f_flags &= ~OMPIO_CONTIGUOUS_FVIEW;
    fview->f_flags &= ~OMPIO_UNIFORM_FVIEW;
}

int mca_common_ompio_set_view (ompio_file_t *fh,
                               OMPI_MPI_OFFSET_TYPE disp,
                               ompi_datatype_t *etype,
                               ompi_datatype_t *filetype,
                               const char *datarep,
                               opal_info_t *info)
{
    int ret=OMPI_SUCCESS;
    size_t max_data = 0;
    int i, flag;
    int num_groups = 0;
    int num_cb_nodes=-1;
    mca_common_ompio_contg *contg_groups=NULL;

    size_t ftype_size;
    ptrdiff_t ftype_extent, lb, ub;
    ompi_datatype_t *newfiletype;

    if ( (MPI_DISPLACEMENT_CURRENT == disp) &&
         (fh->f_amode & MPI_MODE_SEQUENTIAL) ) {
        mca_sharedfp_base_module_t * shared_fp_base_module = fh->f_sharedfp;
        if ( NULL == shared_fp_base_module ){
            opal_output(0, "No shared file pointer component found for this file. Can not execute\n");
            return OMPI_ERROR;
        }
        shared_fp_base_module->sharedfp_get_position(fh, &disp);
    }
    
    fview_clear(&(fh->f_fview));
    if (NULL != fh->f_etype) {
        ompi_datatype_destroy (&fh->f_etype);
    }
    if (NULL != fh->f_filetype) {
        ompi_datatype_destroy (&fh->f_filetype);
    }
    if (NULL != fh->f_orig_filetype) {
        ompi_datatype_destroy (&fh->f_orig_filetype);
    }

    if (NULL != fh->f_datarep) {
        free (fh->f_datarep);
        fh->f_datarep = NULL;
    }

    if (NULL != fh->f_file_convertor) {
        opal_convertor_cleanup (fh->f_file_convertor);
        free (fh->f_file_convertor);
        fh->f_file_convertor = NULL;
    }

    fh->f_flags  &= ~OMPIO_DATAREP_NATIVE;
    fh->f_datarep = strdup (datarep);

    if ( !(strcmp(datarep, "external32") && strcmp(datarep, "EXTERNAL32"))) {
        fh->f_file_convertor = malloc ( sizeof(struct opal_convertor_t) );
        if ( NULL == fh->f_file_convertor ) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        opal_convertor_clone (ompi_mpi_external32_convertor, fh->f_file_convertor, 0);
    }
    else {
        fh->f_file_convertor = opal_convertor_create (opal_local_arch, 0);
        fh->f_flags |= OMPIO_DATAREP_NATIVE;
    }
    
    datatype_duplicate       (filetype, &(fh->f_orig_filetype));
    opal_datatype_get_extent (&filetype->super, &lb, &ftype_extent);
    opal_datatype_type_size  (&filetype->super, &ftype_size);

    if ( etype == filetype                             &&
	 ompi_datatype_is_predefined (filetype )       &&
	 ftype_extent == (ptrdiff_t)ftype_size ){
	ompi_datatype_create_contiguous(MCA_IO_DEFAULT_FILE_VIEW_SIZE,
					&ompi_mpi_byte.dt,
					&newfiletype);
	ompi_datatype_commit (&newfiletype);
    }
    else {
        newfiletype          = filetype;
        fh->f_fview.f_flags |= OMPIO_FILE_VIEW_IS_SET;
    }

    fh->f_fview.f_iov_count             = 0;
    fh->f_fview.f_disp                  = disp;
    fh->f_fview.f_offset                = disp;
    fh->f_fview.f_total_bytes           = 0;
    fh->f_fview.f_index_in_file_view    = 0;
    fh->f_fview.f_position_in_file_view = 0;

    mca_common_ompio_decode_datatype (fh,
                                      newfiletype,
                                      1,
                                      NULL,
                                      &max_data,
                                      fh->f_file_convertor,
                                      &fh->f_fview.f_decoded_iov,
                                      &fh->f_fview.f_iov_count);

    opal_datatype_get_extent(&newfiletype->super, &lb, &fh->f_fview.f_view_extent);
    opal_datatype_type_ub   (&newfiletype->super, &ub);
    opal_datatype_type_size (&etype->super, &fh->f_fview.f_etype_size);
    opal_datatype_type_size (&newfiletype->super, &fh->f_fview.f_view_size);
    datatype_duplicate (etype, &fh->f_etype);
    // This file type is our own representation. The original is stored
    // in orig_file type, No need to set args on this one.
    ompi_datatype_duplicate (newfiletype, &fh->f_filetype);

    if ( (fh->f_fview.f_view_size % fh->f_fview.f_etype_size) ) {
        // File view is not a multiple of the etype.
        return MPI_ERR_ARG;
    }
    
    // make sure that displacement is not negative, which could
    // lead to an illegal access.
    if ( 0 < fh->f_fview.f_iov_count && 0 > (off_t)fh->f_fview.f_decoded_iov[0].iov_base ) {
        // I think MPI_ERR_TYPE would be more appropriate, but
        // this is the error code expected in a testsuite, so I just
        // go with this.
        return MPI_ERR_IO;
    }

    
    if( SIMPLE_PLUS == OMPIO_MCA_GET(fh, grouping_option) ) {
        fh->f_cc_size = get_contiguous_chunk_size (fh, 1);
    }
    else {
        fh->f_cc_size = get_contiguous_chunk_size (fh, 0);
    }

    if (opal_datatype_is_contiguous_memory_layout(&etype->super,1)) {
        if (opal_datatype_is_contiguous_memory_layout(&filetype->super,1) &&
	    fh->f_fview.f_view_extent == (ptrdiff_t)fh->f_fview.f_view_size ) {
            fh->f_fview.f_flags |= OMPIO_CONTIGUOUS_FVIEW;
        }
    }

    contg_groups = (mca_common_ompio_contg*) calloc ( 1, fh->f_size * sizeof(mca_common_ompio_contg));
    if (NULL == contg_groups) {
        opal_output (1, "OUT OF MEMORY\n");
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    for( i = 0; i < fh->f_size; i++){
       contg_groups[i].procs_in_contg_group = (int*)calloc (1,fh->f_size * sizeof(int));
       if(NULL == contg_groups[i].procs_in_contg_group){
          int j;
          opal_output (1, "OUT OF MEMORY\n");
          for(j=0; j<i; j++) {
              free(contg_groups[j].procs_in_contg_group);
          }
          free(contg_groups);
          return OMPI_ERR_OUT_OF_RESOURCE;
       }
    }

    opal_cstring_t *stripe_str;
    /* Check the info object set during File_open */
    opal_info_get (info, "cb_nodes", &stripe_str, &flag);
    if ( flag ) {
        sscanf ( stripe_str->string, "%d", &num_cb_nodes );
        OMPIO_MCA_PRINT_INFO(fh, "cb_nodes", stripe_str->string, "");
        /* add the key/value to the file's info object */
        opal_info_set_cstring(fh->f_info, "cb_nodes", stripe_str);
        OBJ_RELEASE(stripe_str);
    }
    else {
        /* Check the info object set during file_set_view */
        opal_info_get (fh->f_info, "cb_nodes", &stripe_str, &flag);
        if ( flag ) {
            sscanf ( stripe_str->string, "%d", &num_cb_nodes );
            OMPIO_MCA_PRINT_INFO(fh, "cb_nodes", stripe_str->string, "");
            OBJ_RELEASE(stripe_str);
        }
    }
        

    if ( -1 != OMPIO_MCA_GET(fh, num_aggregators) || -1 != num_cb_nodes) {
        /* The user requested a particular number of aggregators */
        num_groups = OMPIO_MCA_GET(fh, num_aggregators);                                       
        if ( -1 != num_cb_nodes ) {
            /* A hint through an  MPI Info object trumps an mca parameter value */
            num_groups = num_cb_nodes;
        }
        if ( num_groups > fh->f_size ) {
            num_groups = fh->f_size;
        }
        mca_common_ompio_forced_grouping ( fh, num_groups, contg_groups);
    }
    else {
        if ( SIMPLE != OMPIO_MCA_GET(fh, grouping_option) && 
             SIMPLE_PLUS != OMPIO_MCA_GET(fh, grouping_option) ) {
            ret = mca_common_ompio_fview_based_grouping(fh,
                                                        &num_groups,
                                                        contg_groups);
            if ( OMPI_SUCCESS != ret ) {
                opal_output(1, "mca_common_ompio_set_view: mca_io_ompio_fview_based_grouping failed\n");
                goto exit;
            }
        }
        else {
            int done=0;
            int ndims;
            
            if ( fh->f_comm->c_flags & OMPI_COMM_CART ){
                ret = fh->f_comm->c_topo->topo.cart.cartdim_get( fh->f_comm, &ndims);
                if ( OMPI_SUCCESS != ret ){
                    goto exit;
                }
                if ( ndims > 1 ) { 
                    ret = mca_common_ompio_cart_based_grouping( fh, 
                                                                &num_groups, 
                                                                contg_groups);
                    if (OMPI_SUCCESS != ret ) {
                        opal_output(1, "mca_common_ompio_set_view: mca_io_ompio_cart_based_grouping failed\n");
                        goto exit;
                    }
                    done=1;
                }
            }
            
            if ( !done ) {
                ret = mca_common_ompio_simple_grouping(fh,
                                                       &num_groups,
                                                       contg_groups);
                if ( OMPI_SUCCESS != ret ){
                    opal_output(1, "mca_common_ompio_set_view: mca_io_ompio_simple_grouping failed\n");
                    goto exit;
                }
            }
        }
    }
#ifdef DEBUG_OMPIO
    if ( fh->f_rank == 0) {
        int ii, jj;
        printf("BEFORE finalize_init: comm size = %d num_groups = %d\n", fh->f_size, num_groups);
        for ( ii=0; ii< num_groups; ii++ ) {
            printf("contg_groups[%d].procs_per_contg_group=%d\n", ii, contg_groups[ii].procs_per_contg_group); 
            printf("contg_groups[%d].procs_in_contg_group.[", ii);

            for ( jj=0; jj< contg_groups[ii].procs_per_contg_group; jj++ ) {
                printf("%d,", contg_groups[ii].procs_in_contg_group[jj]);
            }
            printf("]\n");
        }
    }
#endif

    ret = mca_common_ompio_finalize_initial_grouping(fh,
                                                     num_groups,
                                                     contg_groups);
    if ( OMPI_SUCCESS != ret ) {
        opal_output(1, "mca_common_ompio_set_view: mca_io_ompio_finalize_initial_grouping failed\n");
        goto exit;
    }

    if ( etype == filetype                              &&
	 ompi_datatype_is_predefined (filetype )        &&
	 ftype_extent == (ptrdiff_t)ftype_size ){
	ompi_datatype_destroy ( &newfiletype );
    }

    bool info_is_set=false;
    opal_info_get (info, "collective_buffering", &stripe_str, &flag);
    if ( flag ) {
        if ( strncmp ( stripe_str->string, "false", sizeof("true") )){
            info_is_set = true;
            OMPIO_MCA_PRINT_INFO(fh, "collective_buffering", stripe_str->string, "enforcing using individual fcoll component");
        } else {
            OMPIO_MCA_PRINT_INFO(fh, "collective_buffering", stripe_str->string, "");
        }
        /* add the key/value to the file's info object */
        opal_info_set_cstring(fh->f_info, "collective_buffering", stripe_str);
        OBJ_RELEASE(stripe_str);
    } else {
        opal_info_get (fh->f_info, "collective_buffering", &stripe_str, &flag);
        if ( flag ) {
            if ( strncmp ( stripe_str->string, "false", sizeof("true") )){
                info_is_set = true;
                OMPIO_MCA_PRINT_INFO(fh, "collective_buffering", stripe_str->string, "enforcing using individual fcoll component");
            } else {
                OMPIO_MCA_PRINT_INFO(fh, "collective_buffering", stripe_str->string, "");
            }
            OBJ_RELEASE(stripe_str);
        }
    }

    mca_fcoll_base_component_t *preferred =NULL;
    if ( info_is_set ) {
        /* user requested using an info object to disable collective buffering. */
        preferred = mca_fcoll_base_component_lookup ("individual");
    }
    ret = mca_fcoll_base_file_select (fh, (mca_base_component_t *)preferred);
    if ( OMPI_SUCCESS != ret ) {
        opal_output(1, "mca_common_ompio_set_view: mca_fcoll_base_file_select() failed\n");
        goto exit;
    }


    if ( NULL != fh->f_sharedfp ) {
        ret = fh->f_sharedfp->sharedfp_seek( fh, 0, MPI_SEEK_SET);
    }

exit:
    for( i = 0; i < fh->f_size; i++){
       free(contg_groups[i].procs_in_contg_group);
    }
    free(contg_groups);

    return ret;
}

OMPI_MPI_OFFSET_TYPE get_contiguous_chunk_size (ompio_file_t *fh, int flag)
{
    OMPI_MPI_OFFSET_TYPE avg[3] = {0,0,0};
    OMPI_MPI_OFFSET_TYPE global_avg[3] = {0,0,0};
    int i = 0;

    /* This function determines the average data chunk
    ** size in the file view for each process and across all processes,
    ** and the avg. file_view size across processes.
    */

    if ( flag  ) {
        global_avg[0] = MCA_IO_DEFAULT_FILE_VIEW_SIZE;
        fh->f_avg_view_size = fh->f_fview.f_view_size;
    }
    else {
        for (i=0 ; i<(int)fh->f_fview.f_iov_count ; i++) {
            avg[0] += fh->f_fview.f_decoded_iov[i].iov_len;
        }
        if ( 0 != fh->f_fview.f_iov_count ) {
            avg[0] = avg[0]/fh->f_fview.f_iov_count;
        }
        avg[1] = (OMPI_MPI_OFFSET_TYPE) fh->f_fview.f_iov_count;
        avg[2] = (OMPI_MPI_OFFSET_TYPE) fh->f_fview.f_view_size;
        
        fh->f_comm->c_coll->coll_allreduce (avg,
                                            global_avg,
                                            3,
                                            OMPI_OFFSET_DATATYPE,
                                            MPI_SUM,
                                            fh->f_comm,
                                            fh->f_comm->c_coll->coll_allreduce_module);
        global_avg[0] = global_avg[0]/fh->f_size;
        global_avg[1] = global_avg[1]/fh->f_size;
        fh->f_avg_view_size =  global_avg[2]/fh->f_size;
    }

    return global_avg[0];
}

int mca_common_ompio_fview_duplicate (struct ompio_fview_t *outfv, struct ompio_fview_t *infv)
{
    uint32_t i;

    memset(outfv, 0, sizeof(struct ompio_fview_t));
    outfv->f_flags     = infv->f_flags;
    outfv->f_offset    = infv->f_offset;
    outfv->f_disp      = infv->f_disp;
    outfv->f_iov_count = infv->f_iov_count;

    outfv->f_decoded_iov = (struct iovec*) malloc (outfv->f_iov_count * sizeof(struct iovec));
    if (NULL == outfv->f_decoded_iov) {
        opal_output(1, "common_ompio_duplicate_fview: could not allocate memory\n");
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    for (i=0; i < outfv->f_iov_count; i++) {
        outfv->f_decoded_iov[i].iov_base = infv->f_decoded_iov[i].iov_base;
        outfv->f_decoded_iov[i].iov_len  = infv->f_decoded_iov[i].iov_len ;
    }

    outfv->f_position_in_file_view = infv->f_position_in_file_view;
    outfv->f_total_bytes           = infv->f_total_bytes;
    outfv->f_index_in_file_view    = infv->f_index_in_file_view;
    outfv->f_view_extent           = infv->f_view_extent;
    outfv->f_view_size             = infv->f_view_size;
    outfv->f_etype_size            = infv->f_etype_size;

    return OMPI_SUCCESS;
}
