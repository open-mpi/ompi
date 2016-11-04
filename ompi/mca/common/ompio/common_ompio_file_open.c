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
 * Copyright (c) 2008-2016 University of Houston. All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2016 Cisco Systems, Inc.  All rights reserved.
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
#include "ompi/mca/io/base/base.h"
#include "ompi/mca/fs/fs.h"
#include "ompi/mca/fs/base/base.h"
#include "ompi/mca/fcoll/fcoll.h"
#include "ompi/mca/fcoll/base/base.h"
#include "ompi/mca/fbtl/fbtl.h"
#include "ompi/mca/fbtl/base/base.h"
#include "ompi/mca/sharedfp/sharedfp.h"
#include "ompi/mca/sharedfp/base/base.h"

#include <unistd.h>
#include <math.h>
#include "common_ompio.h"
#include "ompi/mca/topo/topo.h"

int mca_common_ompio_file_open (ompi_communicator_t *comm,
                              const char *filename,
                              int amode,
                              ompi_info_t *info,
                              mca_io_ompio_file_t *ompio_fh, bool use_sharedfp)
{
    int ret = OMPI_SUCCESS;
    int remote_arch;


    ompio_fh->f_iov_type = MPI_DATATYPE_NULL;
    ompio_fh->f_comm     = MPI_COMM_NULL;

    if ( ((amode&MPI_MODE_RDONLY)?1:0) + ((amode&MPI_MODE_RDWR)?1:0) +
	 ((amode&MPI_MODE_WRONLY)?1:0) != 1 ) {
	return MPI_ERR_AMODE;
    }

    if ((amode & MPI_MODE_RDONLY) &&
        ((amode & MPI_MODE_CREATE) || (amode & MPI_MODE_EXCL))) {
	return  MPI_ERR_AMODE;
    }

    if ((amode & MPI_MODE_RDWR) && (amode & MPI_MODE_SEQUENTIAL)) {
	return MPI_ERR_AMODE;
    }

    ompio_fh->f_rank     = ompi_comm_rank (comm);
    ompio_fh->f_size     = ompi_comm_size (comm);
    remote_arch = opal_local_arch;
    ompio_fh->f_convertor = opal_convertor_create (remote_arch, 0);

    if ( true == use_sharedfp ) {
	ret = ompi_comm_dup (comm, &ompio_fh->f_comm);
	if ( OMPI_SUCCESS != ret )  {
	    goto fn_fail;
	}
    }
    else {
	/* No need to duplicate the communicator if the file_open is called
	   from the sharedfp component, since the comm used as an input
	   is already a dup of the user level comm. */
	ompio_fh->f_flags |= OMPIO_SHAREDFP_IS_SET;
	ompio_fh->f_comm = comm;
    }

    ompio_fh->f_fstype = NONE;
    ompio_fh->f_amode  = amode;
    ompio_fh->f_info   = info;
    ompio_fh->f_atomicity = 0;

    mca_common_ompio_set_file_defaults (ompio_fh);
    ompio_fh->f_filename = filename;

    ompio_fh->f_split_coll_req    = NULL;
    ompio_fh->f_split_coll_in_use = false;

    /*Initialize the print_queues queues here!*/
    mca_common_ompio_initialize_print_queue(&ompio_fh->f_coll_write_time);
    mca_common_ompio_initialize_print_queue(&ompio_fh->f_coll_read_time);

    /* set some function pointers required for fcoll, fbtls and sharedfp modules*/
    ompio_fh->f_decode_datatype=ompi_io_ompio_decode_datatype;
    ompio_fh->f_generate_current_file_view=ompi_io_ompio_generate_current_file_view;

    ompio_fh->f_get_num_aggregators=mca_io_ompio_get_num_aggregators;
    ompio_fh->f_get_bytes_per_agg=mca_io_ompio_get_bytes_per_agg;
    ompio_fh->f_set_aggregator_props=mca_io_ompio_set_aggregator_props;

    /* This fix is needed for data seiving to work with
       two-phase collective I/O */
     if ((amode & MPI_MODE_WRONLY)){
       amode -= MPI_MODE_WRONLY;
       amode += MPI_MODE_RDWR;
     }
     /*--------------------------------------------------*/


    if (OMPI_SUCCESS != (ret = mca_fs_base_file_select (ompio_fh,
                                                        NULL))) {
        opal_output(1, "mca_fs_base_file_select() failed\n");
        goto fn_fail;
    }
    if (OMPI_SUCCESS != (ret = mca_fbtl_base_file_select (ompio_fh,
                                                          NULL))) {
        opal_output(1, "mca_fbtl_base_file_select() failed\n");
        goto fn_fail;
    }

    if (OMPI_SUCCESS != (ret = mca_fcoll_base_file_select (ompio_fh,
                                                           NULL))) {
        opal_output(1, "mca_fcoll_base_file_select() failed\n");
        goto fn_fail;
    }

    ompio_fh->f_sharedfp_component = NULL; /*component*/
    ompio_fh->f_sharedfp           = NULL; /*module*/
    ompio_fh->f_sharedfp_data      = NULL; /*data*/

    if ( true == use_sharedfp ) {
	if (OMPI_SUCCESS != (ret = mca_sharedfp_base_file_select (ompio_fh, NULL))) {
	    opal_output ( ompi_io_base_framework.framework_output,
			  "mca_sharedfp_base_file_select() failed\n");
	    ompio_fh->f_sharedfp           = NULL; /*module*/
	    /* Its ok to not have a shared file pointer module as long as the shared file
	    ** pointer operations are not used. However, the first call to any file_read/write_shared
	    ** function will return an error code.
	    */
	}

	/* open the file once more for the shared file pointer if required.
	** Per default, the shared file pointer specific actions are however
	** only performed on first access of the shared file pointer, except
	** for the addproc sharedfp component.
	**
	** Lazy open does not work for the addproc sharedfp
	** component since it starts by spawning a process using MPI_Comm_spawn.
	** For this, the first operation has to be collective which we can
	** not guarantuee outside of the MPI_File_open operation.
	*/
	if ( NULL != ompio_fh->f_sharedfp &&
	     true == use_sharedfp &&
	     (!mca_io_ompio_sharedfp_lazy_open ||
	      !strcmp (ompio_fh->f_sharedfp_component->mca_component_name,
		       "addproc")               )) {
	    ret = ompio_fh->f_sharedfp->sharedfp_file_open(comm,
							   filename,
							   amode,
							   info,
							   ompio_fh);

	    if ( OMPI_SUCCESS != ret ) {
		goto fn_fail;
	    }
	}
    }

     /*Determine topology information if set*/
    if (ompio_fh->f_comm->c_flags & OMPI_COMM_CART){
        ret = mca_io_ompio_cart_based_grouping(ompio_fh);
	if(OMPI_SUCCESS != ret ){
	    ret = MPI_ERR_FILE;
	}
    }

    ret = ompio_fh->f_fs->fs_file_open (comm,
					filename,
					amode,
					info,
					ompio_fh);




    if ( OMPI_SUCCESS != ret ) {
	ret = MPI_ERR_FILE;
        goto fn_fail;
    }


    /* If file has been opened in the append mode, move the internal
       file pointer of OMPIO to the very end of the file. */
    if ( ompio_fh->f_amode & MPI_MODE_APPEND ) {
        OMPI_MPI_OFFSET_TYPE current_size;

        ompio_fh->f_fs->fs_file_get_size( ompio_fh,
                                          &current_size);
        mca_common_ompio_set_explicit_offset (ompio_fh, current_size);
    }



    return OMPI_SUCCESS;

    fn_fail:
        /* no need to free resources here, since the destructor
	 * is calling mca_io_ompio_file_close, which actually gets
	 *rid of all allocated memory items */

    return ret;
}

int mca_common_ompio_file_close (mca_io_ompio_file_t *ompio_fh)
{
    int ret = OMPI_SUCCESS;
    int delete_flag = 0;
    char name[256];

    ret = ompio_fh->f_comm->c_coll.coll_barrier ( ompio_fh->f_comm, ompio_fh->f_comm->c_coll.coll_barrier_module);
    if ( OMPI_SUCCESS != ret ) {
        /* Not sure what to do */
        opal_output (1,"mca_common_ompio_file_close: error in Barrier \n");
        return ret;
    }


    if(mca_io_ompio_coll_timing_info){
        strcpy (name, "WRITE");
        if (!mca_common_ompio_empty_print_queue(ompio_fh->f_coll_write_time)){
            ret = mca_common_ompio_print_time_info(ompio_fh->f_coll_write_time,
                                                   name,
                                                   ompio_fh);
            if (OMPI_SUCCESS != ret){
                printf("Error in print_time_info ");
            }

        }
        strcpy (name, "READ");
        if (!mca_common_ompio_empty_print_queue(ompio_fh->f_coll_read_time)){
            ret = mca_common_ompio_print_time_info(ompio_fh->f_coll_read_time,
                                                   name,
                                                   ompio_fh);
            if (OMPI_SUCCESS != ret){
                printf("Error in print_time_info ");
            }
        }
    }
    if ( ompio_fh->f_amode & MPI_MODE_DELETE_ON_CLOSE ) {
        delete_flag = 1;
    }

    /*close the sharedfp file*/
    if( NULL != ompio_fh->f_sharedfp ){
        ret = ompio_fh->f_sharedfp->sharedfp_file_close(ompio_fh);
    }
    if ( NULL != ompio_fh->f_fs ) {
	/* The pointer might not be set if file_close() is
	** called from the file destructor in case of an error
	** during file_open()
	*/
	ret = ompio_fh->f_fs->fs_file_close (ompio_fh);
    }
    if ( delete_flag && 0 == ompio_fh->f_rank ) {
        mca_io_ompio_file_delete ( ompio_fh->f_filename, MPI_INFO_NULL );
    }

    if ( NULL != ompio_fh->f_fs ) {
	mca_fs_base_file_unselect (ompio_fh);
    }
    if ( NULL != ompio_fh->f_fbtl ) {
	mca_fbtl_base_file_unselect (ompio_fh);
    }

    if ( NULL != ompio_fh->f_fcoll ) {
	mca_fcoll_base_file_unselect (ompio_fh);
    }
    if ( NULL != ompio_fh->f_sharedfp)  {
	mca_sharedfp_base_file_unselect (ompio_fh);
    }

    if (NULL != ompio_fh->f_io_array) {
        free (ompio_fh->f_io_array);
        ompio_fh->f_io_array = NULL;
    }

    if (NULL != ompio_fh->f_init_procs_in_group) {
        free (ompio_fh->f_init_procs_in_group);
        ompio_fh->f_init_procs_in_group = NULL;
    }
    if (NULL != ompio_fh->f_procs_in_group) {
        free (ompio_fh->f_procs_in_group);
        ompio_fh->f_procs_in_group = NULL;
    }

    if (NULL != ompio_fh->f_decoded_iov) {
        free (ompio_fh->f_decoded_iov);
        ompio_fh->f_decoded_iov = NULL;
    }

    if (NULL != ompio_fh->f_convertor) {
        free (ompio_fh->f_convertor);
        ompio_fh->f_convertor = NULL;
    }

    if (NULL != ompio_fh->f_datarep) {
        free (ompio_fh->f_datarep);
        ompio_fh->f_datarep = NULL;
    }


    if ( NULL != ompio_fh->f_coll_write_time ) {
        free ( ompio_fh->f_coll_write_time );
        ompio_fh->f_coll_write_time = NULL;
    }

    if ( NULL != ompio_fh->f_coll_read_time ) {
        free ( ompio_fh->f_coll_read_time );
        ompio_fh->f_coll_read_time = NULL;
    }

    if (MPI_DATATYPE_NULL != ompio_fh->f_iov_type) {
        ompi_datatype_destroy (&ompio_fh->f_iov_type);
    }

    if ( MPI_DATATYPE_NULL != ompio_fh->f_etype ) {
	ompi_datatype_destroy (&ompio_fh->f_etype);
    }
    if ( MPI_DATATYPE_NULL != ompio_fh->f_filetype ){
	ompi_datatype_destroy (&ompio_fh->f_filetype);
    }

    if ( MPI_DATATYPE_NULL != ompio_fh->f_orig_filetype ){
	ompi_datatype_destroy (&ompio_fh->f_orig_filetype);
    }


    if (MPI_COMM_NULL != ompio_fh->f_comm && (ompio_fh->f_flags & OMPIO_SHAREDFP_IS_SET) )  {
        ompi_comm_free (&ompio_fh->f_comm);
    }

    return ret;
}

int mca_common_ompio_file_get_size (mca_io_ompio_file_t *ompio_fh,
                                  OMPI_MPI_OFFSET_TYPE *size)
{
    int ret = OMPI_SUCCESS;

    ret = ompio_fh->f_fs->fs_file_get_size (ompio_fh, size);

    return ret;
}


int mca_common_ompio_file_get_position (mca_io_ompio_file_t *fh,
                                      OMPI_MPI_OFFSET_TYPE *offset)
{
    OMPI_MPI_OFFSET_TYPE off;

    /* No. of copies of the entire file view */
    off = (fh->f_offset - fh->f_disp)/fh->f_view_extent;

    /* No. of elements per view */
    off *= (fh->f_view_size / fh->f_etype_size);

    /* No of elements used in the current copy of the view */
    off += fh->f_total_bytes / fh->f_etype_size;

    *offset = off;
    return OMPI_SUCCESS;
}

int mca_common_ompio_set_file_defaults (mca_io_ompio_file_t *fh)
{

   if (NULL != fh) {
        ompi_datatype_t *types[2];
        int blocklen[2] = {1, 1};
        OPAL_PTRDIFF_TYPE d[2], base;
        int i;

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

        fh->f_init_procs_per_group = -1;
        fh->f_init_procs_in_group = NULL;

	fh->f_procs_per_group = -1;
        fh->f_procs_in_group = NULL;

        fh->f_init_num_aggrs = -1;
        fh->f_init_aggr_list = NULL;


        /* Default file View */
        fh->f_iov_type = MPI_DATATYPE_NULL;
        fh->f_stripe_size = mca_io_ompio_bytes_per_agg;
	/*Decoded iovec of the file-view*/
	fh->f_decoded_iov = NULL;
        fh->f_etype = NULL;
        fh->f_filetype = NULL;
        fh->f_orig_filetype = NULL;

	mca_common_ompio_set_view(fh,
                                  0,
                                  &ompi_mpi_byte.dt,
                                  &ompi_mpi_byte.dt,
                                  "native",
                                  fh->f_info);


	/*Create a derived datatype for the created iovec */
	types[0] = &ompi_mpi_long.dt;
        types[1] = &ompi_mpi_long.dt;

        d[0] = (OPAL_PTRDIFF_TYPE) fh->f_decoded_iov;
        d[1] = (OPAL_PTRDIFF_TYPE) &fh->f_decoded_iov[0].iov_len;

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

        return OMPI_SUCCESS;
    }
    else {
        return OMPI_ERROR;
    }
}


