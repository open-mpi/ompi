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
 *  Copyright (c) 2008-2015 University of Houston. All rights reserved.
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
#include "io_ompio.h"
#include "ompi/mca/topo/topo.h"

int
mca_io_ompio_file_open (ompi_communicator_t *comm,
                        char *filename,
                        int amode,
                        ompi_info_t *info,
                        ompi_file_t *fh)
{
    int ret = OMPI_SUCCESS;
    mca_io_ompio_data_t *data=NULL;
    bool use_sharedfp = true;

    data = (mca_io_ompio_data_t *) fh->f_io_selected_data;
    if ( NULL == data ) {
        return  OMPI_ERR_OUT_OF_RESOURCE;
    }

    

    ret = ompio_io_ompio_file_open(comm,filename,amode,info,&data->ompio_fh,use_sharedfp);

    if ( OMPI_SUCCESS == ret ) {
        fh->f_flags |= OMPIO_FILE_IS_OPEN;
        /*save pointer back to the file_t structure */
        data->ompio_fh.f_fh = fh;
    }




    return ret;
}

int
ompio_io_ompio_file_open (ompi_communicator_t *comm,
                        char *filename,
                        int amode,
                        ompi_info_t *info,
                        mca_io_ompio_file_t *ompio_fh, bool use_sharedfp)
{
    int ret = OMPI_SUCCESS;
    int remote_arch;
    

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

    ompio_fh->f_iov_type = MPI_DATATYPE_NULL;
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

    ompi_io_ompio_set_file_defaults (ompio_fh);
    ompio_fh->f_filename = filename;

    /*Initialize the print_queues queues here!*/
    coll_write_time = (print_queue *) malloc (sizeof(print_queue));
    coll_read_time = (print_queue *) malloc (sizeof(print_queue));

    ompi_io_ompio_initialize_print_queue(coll_write_time);
    ompi_io_ompio_initialize_print_queue(coll_read_time);

    /* set some function pointers required for fcoll, fbtls and sharedfp modules*/
    ompio_fh->f_decode_datatype=ompi_io_ompio_decode_datatype;
    ompio_fh->f_generate_current_file_view=ompi_io_ompio_generate_current_file_view;

    ompio_fh->f_sort=ompi_io_ompio_sort;
    ompio_fh->f_sort_iovec=ompi_io_ompio_sort_iovec;

    ompio_fh->f_allgather_array=ompi_io_ompio_allgather_array;
    ompio_fh->f_allgatherv_array=ompi_io_ompio_allgatherv_array;
    ompio_fh->f_gather_array=ompi_io_ompio_gather_array;
    ompio_fh->f_gatherv_array=ompi_io_ompio_gatherv_array;

    ompio_fh->f_get_num_aggregators=mca_io_ompio_get_num_aggregators;
    ompio_fh->f_get_bytes_per_agg=mca_io_ompio_get_bytes_per_agg;
    ompio_fh->f_set_aggregator_props=ompi_io_ompio_set_aggregator_props;

    ompio_fh->f_full_print_queue=ompi_io_ompio_full_print_queue;
    ompio_fh->f_register_print_entry=ompi_io_ompio_register_print_entry;   

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
        ompi_io_ompio_set_explicit_offset (ompio_fh, current_size);
    }
    


    return OMPI_SUCCESS;

    fn_fail:
        /* no need to free resources here, since the destructor
	 * is calling mca_io_ompio_file_close, which actually gets 
	 *rid of all allocated memory items */

    return ret;
}
int
mca_io_ompio_file_close (ompi_file_t *fh)
{
    int ret = OMPI_SUCCESS;
    mca_io_ompio_data_t *data;

    data = (mca_io_ompio_data_t *) fh->f_io_selected_data;
    if ( NULL == data ) {
	/* structure has already been freed, this is an erroneous call to file_close */
	return ret;
    }
    ret = ompio_io_ompio_file_close(&data->ompio_fh);

    if ( NULL != data ) {
      free ( data );
    }

    return ret;
}

int
ompio_io_ompio_file_close (mca_io_ompio_file_t *ompio_fh)
{
    int ret = OMPI_SUCCESS;
    int delete_flag = 0;
    char name[256];

    if(mca_io_ompio_coll_timing_info){
        strcpy (name, "WRITE");
        if (!ompi_io_ompio_empty_print_queue(WRITE_PRINT_QUEUE)){
            ret = ompi_io_ompio_print_time_info(WRITE_PRINT_QUEUE,
                                                name,
                                                ompio_fh);
            if (OMPI_SUCCESS != ret){
                printf("Error in print_time_info ");
            }

        }
        strcpy (name, "READ");
        if (!ompi_io_ompio_empty_print_queue(READ_PRINT_QUEUE)){
            ret = ompi_io_ompio_print_time_info(READ_PRINT_QUEUE,
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
    ret = ompio_fh->f_fs->fs_file_close (ompio_fh);
    if ( delete_flag && 0 == ompio_fh->f_rank ) {
        mca_io_ompio_file_delete ( ompio_fh->f_filename, MPI_INFO_NULL );
    }

    mca_fs_base_file_unselect (ompio_fh);
    mca_fbtl_base_file_unselect (ompio_fh);
    mca_fcoll_base_file_unselect (ompio_fh);
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

  
    if (MPI_DATATYPE_NULL != ompio_fh->f_iov_type) {
        ompi_datatype_destroy (&ompio_fh->f_iov_type);
    }

    if (MPI_COMM_NULL != ompio_fh->f_comm && (ompio_fh->f_flags & OMPIO_SHAREDFP_IS_SET) )  {
        ompi_comm_free (&ompio_fh->f_comm);
    }
    
    return ret;
}

int mca_io_ompio_file_delete (char *filename,
                              struct ompi_info_t *info) 
{
    int ret = OMPI_SUCCESS;

    ret = unlink(filename);

    if (0 > ret) {
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}

int
mca_io_ompio_file_preallocate (ompi_file_t *fh,
                               OMPI_MPI_OFFSET_TYPE diskspace)
{
    int ret = OMPI_SUCCESS, cycles, i;
    OMPI_MPI_OFFSET_TYPE tmp, current_size, size, written, len;
    mca_io_ompio_data_t *data;
    char *buf = NULL;
    ompi_status_public_t *status = NULL;

    data = (mca_io_ompio_data_t *) fh->f_io_selected_data;

    tmp = diskspace;

    data->ompio_fh.f_comm->c_coll.coll_bcast (&tmp,
                                              1,
                                              OMPI_OFFSET_DATATYPE,
                                              OMPIO_ROOT,
                                              data->ompio_fh.f_comm,
                                              data->ompio_fh.f_comm->c_coll.coll_bcast_module);

    if (tmp != diskspace) {
        return OMPI_ERROR;
    }

    /* ROMIO explanation
       On file systems with no preallocation function, we have to 
       explicitly write to allocate space. Since there could be holes in the file, 
       we need to read up to the current file size, write it back, 
       and then write beyond that depending on how much 
       preallocation is needed.
    */
    if (OMPIO_ROOT == data->ompio_fh.f_rank) {
        ret = data->ompio_fh.f_fs->fs_file_get_size (&data->ompio_fh, 
                                                     &current_size);

        size = diskspace;
        if (size > current_size) {
            size = current_size;
        }

        cycles = (size + OMPIO_PREALLOC_MAX_BUF_SIZE - 1)/
            OMPIO_PREALLOC_MAX_BUF_SIZE;
        buf = (char *) malloc (OMPIO_PREALLOC_MAX_BUF_SIZE);
        if (NULL == buf) {
            opal_output(1, "OUT OF MEMORY\n");
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        written = 0;

        for (i=0; i<cycles; i++) {
            len = OMPIO_PREALLOC_MAX_BUF_SIZE;
            if (len > size-written) {
                len = size - written;
            }
            ret = mca_io_ompio_file_read (fh, buf, len, MPI_BYTE, status);
            if (ret != OMPI_SUCCESS) {
                return OMPI_ERROR;
            }
            ret = mca_io_ompio_file_write (fh, buf, len, MPI_BYTE, status);
            if (ret != OMPI_SUCCESS) {
                return OMPI_ERROR;
            }
            written += len;
        }

        if (diskspace > current_size) {
            memset(buf, 0, OMPIO_PREALLOC_MAX_BUF_SIZE);
            size = diskspace - current_size;
            cycles = (size + OMPIO_PREALLOC_MAX_BUF_SIZE - 1) /
                OMPIO_PREALLOC_MAX_BUF_SIZE;
            for (i=0; i<cycles; i++) {
                len = OMPIO_PREALLOC_MAX_BUF_SIZE;
                if (len > diskspace-written) {
                    len = diskspace - written;
                }
                ret = mca_io_ompio_file_write (fh, buf, len, MPI_BYTE, status);
                if (ret != OMPI_SUCCESS) {
                    return OMPI_ERROR;
                }
                written += len;
            }
        }
        if (NULL != buf) {
            free (buf);
            buf = NULL;
        }
    }
    fh->f_comm->c_coll.coll_barrier (fh->f_comm,
                                     fh->f_comm->c_coll.coll_barrier_module);
    return ret;
}

int
mca_io_ompio_file_set_size (ompi_file_t *fh,
                            OMPI_MPI_OFFSET_TYPE size)
{
    int ret = OMPI_SUCCESS;
    OMPI_MPI_OFFSET_TYPE tmp;
    mca_io_ompio_data_t *data;

    data = (mca_io_ompio_data_t *) fh->f_io_selected_data;

    tmp = size;

    data->ompio_fh.f_comm->c_coll.coll_bcast (&tmp,
                                              1,
                                              OMPI_OFFSET_DATATYPE,
                                              OMPIO_ROOT,
                                              data->ompio_fh.f_comm,
                                              data->ompio_fh.f_comm->c_coll.coll_bcast_module);

    if (tmp != size) {
        return OMPI_ERROR;
    }

    ret = data->ompio_fh.f_fs->fs_file_set_size (&data->ompio_fh, size);

    return ret;
}

int
mca_io_ompio_file_get_size (ompi_file_t *fh,
                            OMPI_MPI_OFFSET_TYPE *size)
{
    int ret = OMPI_SUCCESS;
    mca_io_ompio_data_t *data;

    data = (mca_io_ompio_data_t *) fh->f_io_selected_data;
    ret = ompio_io_ompio_file_get_size(&data->ompio_fh,size);

    return ret;
}

int
ompio_io_ompio_file_get_size (mca_io_ompio_file_t *ompio_fh,
                              OMPI_MPI_OFFSET_TYPE *size)
{
    int ret = OMPI_SUCCESS;

    ret = ompio_fh->f_fs->fs_file_get_size (ompio_fh, size);

    return ret;
}


int
mca_io_ompio_file_get_amode (ompi_file_t *fh,
                             int *amode)
{
    mca_io_ompio_data_t *data;

    data = (mca_io_ompio_data_t *) fh->f_io_selected_data;

    *amode = data->ompio_fh.f_amode;

    return OMPI_SUCCESS;
}


int mca_io_ompio_file_set_info (ompi_file_t *fh,
				ompi_info_t *info)
{
    int ret = OMPI_SUCCESS;
    
    if ( MPI_INFO_NULL == fh->f_info ) {
	/* OBJ_RELEASE(MPI_INFO_NULL); */
    }
    else {
	ompi_info_free ( &fh->f_info);
	fh->f_info = OBJ_NEW(ompi_info_t);
	ret = ompi_info_dup (info, &fh->f_info);
    }

    return ret;
}


int mca_io_ompio_file_get_info (ompi_file_t *fh,
				ompi_info_t ** info_used)
{
    int ret = OMPI_SUCCESS;
    ompi_info_t *info=NULL;
    
    if ( MPI_INFO_NULL == fh->f_info  ) {
	*info_used = MPI_INFO_NULL;
    }
    else {
	info = OBJ_NEW(ompi_info_t);
	ret = ompi_info_dup (fh->f_info, &info);
	*info_used = info;
    }

    return ret;
}

int
mca_io_ompio_file_get_type_extent (ompi_file_t *fh,
                                   struct ompi_datatype_t *datatype,
                                   MPI_Aint *extent)
{
    opal_datatype_type_extent (&datatype->super, extent);
    return OMPI_SUCCESS;
}


int
mca_io_ompio_file_set_atomicity (ompi_file_t *fh,
                                 int flag)
{
    int tmp;
    mca_io_ompio_data_t *data;

    data = (mca_io_ompio_data_t *) fh->f_io_selected_data;

    if (flag) {
        flag = 1;
    }

    /* check if the atomicity flag is the same on all processes */
    tmp = flag;
    data->ompio_fh.f_comm->c_coll.coll_bcast (&tmp,
                                              1,
                                              MPI_INT,
                                              OMPIO_ROOT,
                                              data->ompio_fh.f_comm,
                                              data->ompio_fh.f_comm->c_coll.coll_bcast_module);

    if (tmp != flag) {
        return OMPI_ERROR;
    }

    data->ompio_fh.f_atomicity = flag;

    return OMPI_SUCCESS;
}

int
mca_io_ompio_file_get_atomicity (ompi_file_t *fh,
                                 int *flag)
{
    mca_io_ompio_data_t *data;

    data = (mca_io_ompio_data_t *) fh->f_io_selected_data;

    *flag = data->ompio_fh.f_atomicity;

    return OMPI_SUCCESS;
}

int
mca_io_ompio_file_sync (ompi_file_t *fh)
{
    int ret = OMPI_SUCCESS;
    mca_io_ompio_data_t *data;

    data = (mca_io_ompio_data_t *) fh->f_io_selected_data;

    ret = data->ompio_fh.f_fs->fs_file_sync (&data->ompio_fh);

    return ret;
}


int
mca_io_ompio_file_seek (ompi_file_t *fh,
                        OMPI_MPI_OFFSET_TYPE off,
                        int whence)
{
    int ret = OMPI_SUCCESS;
    mca_io_ompio_data_t *data;
    OMPI_MPI_OFFSET_TYPE offset, temp_offset;

    data = (mca_io_ompio_data_t *) fh->f_io_selected_data;

    offset = off * data->ompio_fh.f_etype_size;

    switch(whence) {
    case MPI_SEEK_SET:
        if (offset < 0) {
            return OMPI_ERROR;
        }
        break;
    case MPI_SEEK_CUR:
        offset += data->ompio_fh.f_position_in_file_view;
        offset += data->ompio_fh.f_disp;
        if (offset < 0) {
            return OMPI_ERROR;
        }
        break;
    case MPI_SEEK_END:
        ret = data->ompio_fh.f_fs->fs_file_get_size (&data->ompio_fh, 
                                                     &temp_offset);
        offset += temp_offset;
        if (offset < 0 || OMPI_SUCCESS != ret) {
            return OMPI_ERROR;
        }
        break;
    default:
        return OMPI_ERROR;
    }

    ret = ompi_io_ompio_set_explicit_offset (&data->ompio_fh, 
                                             offset/data->ompio_fh.f_etype_size);
    return ret;
}

int
mca_io_ompio_file_get_position (ompi_file_t *fd,
                                OMPI_MPI_OFFSET_TYPE *offset)
{
    int ret=OMPI_SUCCESS;
    mca_io_ompio_data_t *data=NULL;
    mca_io_ompio_file_t *fh=NULL;
    
    data = (mca_io_ompio_data_t *) fd->f_io_selected_data;
    fh = &data->ompio_fh;
    
    ret = ompio_io_ompio_file_get_position (fh, offset);
    
    return ret;
}

int
ompio_io_ompio_file_get_position (mca_io_ompio_file_t *fh,
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


int
mca_io_ompio_file_get_byte_offset (ompi_file_t *fh,
                                   OMPI_MPI_OFFSET_TYPE offset,
                                   OMPI_MPI_OFFSET_TYPE *disp)
{
    mca_io_ompio_data_t *data;
    int i, k, index;
    size_t position;
    size_t total_bytes;
    size_t temp_offset;

    data = (mca_io_ompio_data_t *) fh->f_io_selected_data;

    temp_offset = data->ompio_fh.f_view_extent * 
        (offset*data->ompio_fh.f_etype_size / data->ompio_fh.f_view_size);

    position = 0;
    total_bytes = (offset*data->ompio_fh.f_etype_size) % data->ompio_fh.f_view_size;
    index = 0;
    i = total_bytes;
    k = 0;

    while (1) {
        k += data->ompio_fh.f_decoded_iov[index].iov_len;
        if (i >= k) {
            i = i - data->ompio_fh.f_decoded_iov[index].iov_len;
            position += data->ompio_fh.f_decoded_iov[index].iov_len;
            index = index+1;
        }
        else {
            break;
        }
    }

    *disp = data->ompio_fh.f_disp + temp_offset +
        (OMPI_MPI_OFFSET_TYPE)(intptr_t)data->ompio_fh.f_decoded_iov[index].iov_base;

    return OMPI_SUCCESS;
}

int
mca_io_ompio_file_seek_shared (ompi_file_t *fp,
                               OMPI_MPI_OFFSET_TYPE offset,
                               int whence)
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
    ret = shared_fp_base_module->sharedfp_seek(fh,offset,whence);

    return ret;
}


int
mca_io_ompio_file_get_position_shared (ompi_file_t *fp,
                                       OMPI_MPI_OFFSET_TYPE * offset)
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
    ret = shared_fp_base_module->sharedfp_get_position(fh,offset);
    *offset = *offset / fh->f_etype_size;

    return ret;
}

int
mca_io_ompio_cart_based_grouping(mca_io_ompio_file_t *ompio_fh)
{
    int k = 0;
    int j = 0;
    int n = 0;
    int tmp_rank = 0;
    int coords_tmp[2] = { 0 };

    cart_topo_components cart_topo;
    
    ompio_fh->f_comm->c_topo->topo.cart.cartdim_get(ompio_fh->f_comm, &cart_topo.ndims);

    cart_topo.dims = (int*)malloc (cart_topo.ndims * sizeof(int));
    if (NULL == cart_topo.dims) {
        opal_output (1, "OUT OF MEMORY\n");
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    cart_topo.periods = (int*)malloc (cart_topo.ndims * sizeof(int));
    if (NULL == cart_topo.periods) {
        opal_output (1, "OUT OF MEMORY\n");
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    cart_topo.coords = (int*)malloc (cart_topo.ndims * sizeof(int));
    if (NULL == cart_topo.coords) {
        opal_output (1, "OUT OF MEMORY\n");
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    ompio_fh->f_comm->c_topo->topo.cart.cart_get(ompio_fh->f_comm,
                                                 cart_topo.ndims,
	                                         cart_topo.dims,
	                                         cart_topo.periods,
  	                                         cart_topo.coords);
 
    ompio_fh->f_init_procs_per_group = cart_topo.dims[1]; //number of elements per row
    ompio_fh->f_init_num_aggrs = cart_topo.dims[0];  //number of rows

    //Make an initial list of potential aggregators
    ompio_fh->f_init_aggr_list = (int *) malloc (ompio_fh->f_init_num_aggrs * sizeof(int));
    if (NULL == ompio_fh->f_init_aggr_list) {
        opal_output (1, "OUT OF MEMORY\n");
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
 
    for(k = 0; k < cart_topo.dims[0]; k++){
        coords_tmp[0] = k;
        coords_tmp[1] = k * cart_topo.dims[1];
        ompio_fh->f_comm->c_topo->topo.cart.cart_rank (ompio_fh->f_comm,coords_tmp,&tmp_rank);
        ompio_fh->f_init_aggr_list[k] = tmp_rank; //change this to use get rank
    }

    //Initial Grouping 
    ompio_fh->f_init_procs_in_group = (int*)malloc (ompio_fh->f_init_procs_per_group * sizeof(int));
    if (NULL == ompio_fh->f_init_procs_in_group) {
        opal_output (1, "OUT OF MEMORY\n");
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
                                                                                                                                                              for (j=0 ; j< ompio_fh->f_size ; j++) {
        ompio_fh->f_comm->c_topo->topo.cart.cart_coords (ompio_fh->f_comm, j, cart_topo.ndims, coords_tmp);
	if (coords_tmp[0]  == cart_topo.coords[0]) {
           if ((coords_tmp[1]/ompio_fh->f_init_procs_per_group) == 
	       (cart_topo.coords[1]/ompio_fh->f_init_procs_per_group)) {
	        
	       ompio_fh->f_init_procs_in_group[n] = j;
	       n++;
	   }
        }
    }

    /*print original group */
    /*printf("RANK%d Initial distribution \n",ompio_fh->f_rank);
    for(k = 0; k < ompio_fh->f_init_procs_per_group; k++){
       printf("%d,", ompio_fh->f_init_procs_in_group[k]);
    }
    printf("\n");*/

    if (NULL != cart_topo.dims) {
       free (cart_topo.dims);
       cart_topo.dims = NULL;
    }
    if (NULL != cart_topo.periods) {
       free (cart_topo.periods);
       cart_topo.periods = NULL;
    }
    if (NULL != cart_topo.coords) {
       free (cart_topo.coords);
       cart_topo.coords = NULL;
    }
						                                 
    return OMPI_SUCCESS;
}
