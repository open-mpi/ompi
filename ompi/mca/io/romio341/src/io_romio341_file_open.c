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
 * Copyright (c) 2015-2021 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2016-2019 IBM Corporation. All rights reserved.
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

#include "io_romio341.h"


int
mca_io_romio341_file_open (ompi_communicator_t *comm,
                        const char *filename,
                        int amode,
                        opal_info_t *info,
                        ompi_file_t *fh)
{
    int ret;
    mca_io_romio341_data_t *data;

// An opal_info_t isn't a full ompi_info_t. so if we're using an MPI call
// below with an MPI_Info, we need to create an equivalent MPI_Info. This
// isn't ideal but it only happens a few places.
    ompi_info_t ompi_info;
    OBJ_CONSTRUCT(&ompi_info, ompi_info_t);
    opal_info_t *opal_info = &(ompi_info.super);
    opal_info_dup (info, &opal_info);

    data = (mca_io_romio341_data_t *) fh->f_io_selected_data;
//    OPAL_THREAD_LOCK (&mca_io_romio341_mutex);
    ret = ROMIO_PREFIX(MPI_File_open)(comm, filename, amode, &ompi_info,
                                      &data->romio_fh);
//    OPAL_THREAD_UNLOCK (&mca_io_romio341_mutex);

    OBJ_DESTRUCT(&ompi_info);
    return ret;
}


int
mca_io_romio341_file_close (ompi_file_t *fh)
{
    int ret;
    mca_io_romio341_data_t *data;
    int finalized;

    /* If we've already started MPI_Finalize by this point, then just
       give up (because ROMIO's file close routine calls MPI_Barrier,
       which we obviously can't do if we've started to MPI_Finalize).
       The user didn't close the file, so they should expect
       unexpected behavior. */
    PMPI_Finalized(&finalized);
    if (finalized) {
        return OMPI_SUCCESS;
    }

    /* Because ROMIO expects the MPI library to provide error handler
     * management routines but it doesn't ever participate in
     * MPI_File_close, we have to somehow inform the MPI library that
     * we no longer hold a reference to any user defined error
     * handler.  We do this by setting the errhandler at this point to
     * MPI_ERRORS_RETURN. */
    if (fh->error_handler != &ompi_mpi_errors_return.eh) {
        OBJ_RELEASE(fh->error_handler);
        fh->error_handler = &ompi_mpi_errors_return.eh;
        OBJ_RETAIN(fh->error_handler);
    }

    data = (mca_io_romio341_data_t *) fh->f_io_selected_data;

    OPAL_THREAD_LOCK (&mca_io_romio341_mutex);
    ret = ROMIO_PREFIX(MPI_File_close) (&data->romio_fh);
    OPAL_THREAD_UNLOCK (&mca_io_romio341_mutex);

    return ret;
}


int
mca_io_romio341_file_set_size (ompi_file_t *fh,
                            MPI_Offset size)
{
    int ret;
    mca_io_romio341_data_t *data;

    data = (mca_io_romio341_data_t *) fh->f_io_selected_data;
    OPAL_THREAD_LOCK (&mca_io_romio341_mutex);
    ret = ROMIO_PREFIX(MPI_File_set_size) (data->romio_fh, size);
    OPAL_THREAD_UNLOCK (&mca_io_romio341_mutex);

    return ret;
}

int
mca_io_romio341_file_preallocate (ompi_file_t *fh,
                               MPI_Offset size)
{
    int ret;
    mca_io_romio341_data_t *data;

    data = (mca_io_romio341_data_t *) fh->f_io_selected_data;
    OPAL_THREAD_LOCK (&mca_io_romio341_mutex);
    ret = ROMIO_PREFIX(MPI_File_preallocate) (data->romio_fh, size);
    OPAL_THREAD_UNLOCK (&mca_io_romio341_mutex);

    return ret;
}


int
mca_io_romio341_file_get_size (ompi_file_t *fh,
                            MPI_Offset * size)
{
    int ret;
    mca_io_romio341_data_t *data;

    data = (mca_io_romio341_data_t *) fh->f_io_selected_data;
    OPAL_THREAD_LOCK (&mca_io_romio341_mutex);
    ret = ROMIO_PREFIX(MPI_File_get_size) (data->romio_fh, size);
    OPAL_THREAD_UNLOCK (&mca_io_romio341_mutex);

    return ret;
}


int
mca_io_romio341_file_get_amode (ompi_file_t *fh,
                             int *amode)
{
    int ret;
    mca_io_romio341_data_t *data;

    data = (mca_io_romio341_data_t *) fh->f_io_selected_data;
    OPAL_THREAD_LOCK (&mca_io_romio341_mutex);
    ret = ROMIO_PREFIX(MPI_File_get_amode) (data->romio_fh, amode);
    OPAL_THREAD_UNLOCK (&mca_io_romio341_mutex);

    return ret;
}


int
mca_io_romio341_file_set_info (ompi_file_t *fh,
                            ompi_info_t *info)
{
    int ret;
    mca_io_romio341_data_t *data;

    data = (mca_io_romio341_data_t *) fh->f_io_selected_data;
    OPAL_THREAD_LOCK (&mca_io_romio341_mutex);
    ret = ROMIO_PREFIX(MPI_File_set_info) (data->romio_fh, info);
    OPAL_THREAD_UNLOCK (&mca_io_romio341_mutex);

    return ret;
}


int
mca_io_romio341_file_get_info (ompi_file_t *fh,
                            ompi_info_t ** info_used)
{
    int ret;
    mca_io_romio341_data_t *data;

    data = (mca_io_romio341_data_t *) fh->f_io_selected_data;
    OPAL_THREAD_LOCK (&mca_io_romio341_mutex);
    ret = ROMIO_PREFIX(MPI_File_get_info) (data->romio_fh, info_used);
    OPAL_THREAD_UNLOCK (&mca_io_romio341_mutex);

    return ret;
}


int
mca_io_romio341_file_set_view (ompi_file_t *fh,
                            MPI_Offset disp,
                            struct ompi_datatype_t *etype,
                            struct ompi_datatype_t *filetype,
                            const char *datarep,
                            opal_info_t *info)
{
    int ret;
    mca_io_romio341_data_t *data;

// An opal_info_t isn't a full ompi_info_t. so if we're using an MPI call
// below with an MPI_Info, we need to create an equivalent MPI_Info. This
// isn't ideal but it only happens a few places.
    ompi_info_t ompi_info;
    OBJ_CONSTRUCT(&ompi_info, ompi_info_t);
    opal_info_t *opal_info = &(ompi_info.super);
    opal_info_dup (info, &opal_info);

    data = (mca_io_romio341_data_t *) fh->f_io_selected_data;
    OPAL_THREAD_LOCK (&mca_io_romio341_mutex);
    ret =
        ROMIO_PREFIX(MPI_File_set_view) (data->romio_fh, disp, etype, filetype,
                                        datarep, &ompi_info);
    OPAL_THREAD_UNLOCK (&mca_io_romio341_mutex);

    OBJ_DESTRUCT(&ompi_info);
    return ret;
}


int
mca_io_romio341_file_get_view (ompi_file_t *fh,
                            MPI_Offset * disp,
                            struct ompi_datatype_t ** etype,
                            struct ompi_datatype_t ** filetype,
                            char *datarep)
{
    int ret;
    mca_io_romio341_data_t *data;

    data = (mca_io_romio341_data_t *) fh->f_io_selected_data;
    OPAL_THREAD_LOCK (&mca_io_romio341_mutex);
    ret =
        ROMIO_PREFIX(MPI_File_get_view) (data->romio_fh, disp, etype, filetype,
                                        datarep);
    OPAL_THREAD_UNLOCK (&mca_io_romio341_mutex);

    return ret;

}


int
mca_io_romio341_file_get_type_extent (ompi_file_t *fh,
                                   struct ompi_datatype_t *datatype,
                                   MPI_Aint * extent)
{
    int ret;
    mca_io_romio341_data_t *data;

    data = (mca_io_romio341_data_t *) fh->f_io_selected_data;
    OPAL_THREAD_LOCK (&mca_io_romio341_mutex);
    ret =
        ROMIO_PREFIX(MPI_File_get_type_extent) (data->romio_fh, datatype, extent);
    OPAL_THREAD_UNLOCK (&mca_io_romio341_mutex);

    return ret;
}


int
mca_io_romio341_file_set_atomicity (ompi_file_t *fh,
                                 int flag)
{
    int ret;
    mca_io_romio341_data_t *data;

    data = (mca_io_romio341_data_t *) fh->f_io_selected_data;
    OPAL_THREAD_LOCK (&mca_io_romio341_mutex);
    ret = ROMIO_PREFIX(MPI_File_set_atomicity) (data->romio_fh, flag);
    OPAL_THREAD_UNLOCK (&mca_io_romio341_mutex);

    return ret;
}

int
mca_io_romio341_file_get_atomicity (ompi_file_t *fh,
                                 int *flag)
{
    int ret;
    mca_io_romio341_data_t *data;

    data = (mca_io_romio341_data_t *) fh->f_io_selected_data;
    OPAL_THREAD_LOCK (&mca_io_romio341_mutex);
    ret = ROMIO_PREFIX(MPI_File_get_atomicity) (data->romio_fh, flag);
    OPAL_THREAD_UNLOCK (&mca_io_romio341_mutex);

    return ret;
}

int
mca_io_romio341_file_sync (ompi_file_t *fh)
{
    int ret;
    mca_io_romio341_data_t *data;

    data = (mca_io_romio341_data_t *) fh->f_io_selected_data;
    OPAL_THREAD_LOCK (&mca_io_romio341_mutex);
    ret = ROMIO_PREFIX(MPI_File_sync) (data->romio_fh);
    OPAL_THREAD_UNLOCK (&mca_io_romio341_mutex);

    return ret;
}


int
mca_io_romio341_file_seek_shared (ompi_file_t *fh,
                               MPI_Offset offset,
                               int whence)
{
    int ret;
    mca_io_romio341_data_t *data;

    data = (mca_io_romio341_data_t *) fh->f_io_selected_data;
    OPAL_THREAD_LOCK (&mca_io_romio341_mutex);
    ret = ROMIO_PREFIX(MPI_File_seek_shared) (data->romio_fh, offset, whence);
    OPAL_THREAD_UNLOCK (&mca_io_romio341_mutex);

    return ret;
}


int
mca_io_romio341_file_get_position_shared (ompi_file_t *fh,
                                       MPI_Offset * offset)
{
    int ret;
    mca_io_romio341_data_t *data;

    data = (mca_io_romio341_data_t *) fh->f_io_selected_data;
    OPAL_THREAD_LOCK (&mca_io_romio341_mutex);
    ret = ROMIO_PREFIX(MPI_File_get_position_shared) (data->romio_fh, offset);
    OPAL_THREAD_UNLOCK (&mca_io_romio341_mutex);

    return ret;
}


int
mca_io_romio341_file_seek (ompi_file_t *fh,
                        MPI_Offset offset,
                        int whence)
{
    int ret;
    mca_io_romio341_data_t *data;

    data = (mca_io_romio341_data_t *) fh->f_io_selected_data;
    OPAL_THREAD_LOCK (&mca_io_romio341_mutex);
    ret = ROMIO_PREFIX(MPI_File_seek) (data->romio_fh, offset, whence);
    OPAL_THREAD_UNLOCK (&mca_io_romio341_mutex);

    return ret;
}


int
mca_io_romio341_file_get_position (ompi_file_t *fh,
                                MPI_Offset * offset)
{
    int ret;
    mca_io_romio341_data_t *data;

    data = (mca_io_romio341_data_t *) fh->f_io_selected_data;
    OPAL_THREAD_LOCK (&mca_io_romio341_mutex);
    ret = ROMIO_PREFIX(MPI_File_get_position) (data->romio_fh, offset);
    OPAL_THREAD_UNLOCK (&mca_io_romio341_mutex);

    return ret;
}


int
mca_io_romio341_file_get_byte_offset (ompi_file_t *fh,
                                   MPI_Offset offset,
                                   MPI_Offset * disp)
{
    int ret;
    mca_io_romio341_data_t *data;

    data = (mca_io_romio341_data_t *) fh->f_io_selected_data;
    OPAL_THREAD_LOCK (&mca_io_romio341_mutex);
    ret = ROMIO_PREFIX(MPI_File_get_byte_offset) (data->romio_fh, offset, disp);
    OPAL_THREAD_UNLOCK (&mca_io_romio341_mutex);

    return ret;
}
