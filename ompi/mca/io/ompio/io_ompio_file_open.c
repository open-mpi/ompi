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
#include "ompi/mca/fbtl/fbtl.h"
#include "ompi/mca/fbtl/base/base.h"
#include "ompi/mca/sharedfp/sharedfp.h"
#include "ompi/mca/sharedfp/base/base.h"

#include <unistd.h>
#include "io_ompio.h"

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

    ret = ompi_comm_dup (comm, &ompio_fh->f_comm);
    if ( ret != OMPI_SUCCESS )  {
	goto fn_fail;
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

    /*
    if (MPI_INFO_NULL != info)  {
        ret = ompi_info_dup (info, &ompio_fh->f_info);
	if (OMPI_SUCCESS != ret) {
             goto fn_fail;
	}
    }
    */
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

    if (OMPI_SUCCESS != (ret = mca_sharedfp_base_file_select (ompio_fh, NULL))) {
	opal_output(1, "mca_sharedfp_base_file_select() failed\n");
	goto fn_fail;
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
    if ( true == use_sharedfp && 
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
      is calling mca_io_ompio_file_close, which actually gets 
      rid of all allocated memory items */

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
    if(ompio_fh->f_sharedfp != NULL){
        ret = ompio_fh->f_sharedfp->sharedfp_file_close(ompio_fh);
    }
    ret = ompio_fh->f_fs->fs_file_close (ompio_fh);
    if ( delete_flag && 0 == ompio_fh->f_rank ) {
        mca_io_ompio_file_delete ( ompio_fh->f_filename, MPI_INFO_NULL );
    }

    mca_fs_base_file_unselect (ompio_fh);
    mca_fbtl_base_file_unselect (ompio_fh);
    mca_fcoll_base_file_unselect (ompio_fh);
    /* mca_sharedfp_base_file_unselect (ompio_fh) ; EG?*/ 

    if (NULL != ompio_fh->f_io_array) {
        free (ompio_fh->f_io_array);
        ompio_fh->f_io_array = NULL;
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

    if (MPI_COMM_NULL != ompio_fh->f_comm)  {
        ompi_comm_free (&ompio_fh->f_comm);
    }


    /*
    if (MPI_INFO_NULL != ompio_fh->f_info)
    {
        ompi_info_free (&ompio_fh->f_info);
    }
    */
    
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
                                              MPI_LONG_LONG,
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
                                              MPI_LONG_LONG,
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


int
mca_io_ompio_file_set_info (ompi_file_t *fh,
                            ompi_info_t *info)
{
    int ret = OMPI_SUCCESS;
    mca_io_ompio_data_t *data;

    data = (mca_io_ompio_data_t *) fh->f_io_selected_data;

    ret = data->ompio_fh.f_fs->fs_file_set_info (&data->ompio_fh, info);

    return ret;
}


int
mca_io_ompio_file_get_info (ompi_file_t *fh,
                            ompi_info_t ** info_used)
{
    int ret = OMPI_SUCCESS;
    mca_io_ompio_data_t *data;

    data = (mca_io_ompio_data_t *) fh->f_io_selected_data;

    ret = ompi_info_dup (data->ompio_fh.f_info, info_used);

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
mca_io_ompio_file_get_position (ompi_file_t *fh,
                                OMPI_MPI_OFFSET_TYPE *offset)
{
    mca_io_ompio_data_t *data;

    data = (mca_io_ompio_data_t *) fh->f_io_selected_data;

    *offset = data->ompio_fh.f_position_in_file_view / data->ompio_fh.f_etype_size;

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
mca_io_ompio_file_seek_shared (ompi_file_t *fh,
                               OMPI_MPI_OFFSET_TYPE offset,
                               int whence)
{
    int ret = MPI_ERR_OTHER;
    return ret;
}


int
mca_io_ompio_file_get_position_shared (ompi_file_t *fh,
                                       OMPI_MPI_OFFSET_TYPE * offset)
{
    int ret = MPI_ERR_OTHER;
    return ret;
}
