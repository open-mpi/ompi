/*
 *  Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                          All rights reserved.
 *  Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                          All rights reserved.
 *  Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                          University of Stuttgart.  All rights reserved.
 *  $COPYRIGHT$
 *  
 *  Additional copyrights may follow
 *  
 *  $HEADER$
 */

#include "ompi_config.h"
#include "mpi.h"
#include "file/file.h"
#include "io_romio.h"
#include "request/request.h"


int
mca_io_romio_file_write_at (ompi_file_t *fh,
                            MPI_Offset offset,
                            void *buf,
                            int count,
                            ompi_datatype_t *datatype,
                            ompi_status_public_t * status)
{
    int         ret;
    mca_io_romio_data_t *data;

    data = (mca_io_romio_data_t *) fh->f_io_selected_data;
    OMPI_THREAD_LOCK (&mca_io_romio_mutex);
    ret =
        ROMIO_PREFIX(MPI_File_write_at) (data->romio_fh, offset, buf, count,
                                        datatype, status);
    OMPI_THREAD_UNLOCK (&mca_io_romio_mutex);

    return ret;
}



int
mca_io_romio_file_write_at_all (ompi_file_t *fh,
                                MPI_Offset offset,
                                void *buf,
                                int count,
                                ompi_datatype_t *datatype,
                                ompi_status_public_t * status)
{
    int         ret;
    mca_io_romio_data_t *data;

    data = (mca_io_romio_data_t *) fh->f_io_selected_data;
    OMPI_THREAD_LOCK (&mca_io_romio_mutex);
    ret =
        ROMIO_PREFIX(MPI_File_write_at_all) (data->romio_fh, offset, buf, 
                                             count, datatype, status);
    OMPI_THREAD_UNLOCK (&mca_io_romio_mutex);

    return ret;
}



int
mca_io_romio_file_iwrite_at (ompi_file_t *fh,
                             MPI_Offset offset,
                             void *buf,
                             int count,
                             ompi_datatype_t *datatype,
                             mca_io_base_request_t * request)
{
    int ret, flag;
    mca_io_romio_data_t *data;
    mca_io_romio_request_t *req;

    data = (mca_io_romio_data_t *) fh->f_io_selected_data;
    req = (mca_io_romio_request_t *) request;
    OMPI_THREAD_LOCK (&mca_io_romio_mutex);
    ret =
        ROMIO_PREFIX(MPI_File_iwrite_at) (data->romio_fh, offset, buf, count,
                                          datatype, &req->romio_rq);
    if (MPI_SUCCESS == ret) {
        MCA_IO_ROMIO_REQUEST_ADD(request);
    }
    OMPI_THREAD_UNLOCK (&mca_io_romio_mutex);

    return ret;
}





int
mca_io_romio_file_write (ompi_file_t *fh,
                         void *buf,
                         int count,
                         ompi_datatype_t *datatype,
                         ompi_status_public_t * status)
{
    int         ret;
    mca_io_romio_data_t *data;

    data = (mca_io_romio_data_t *) fh->f_io_selected_data;
    OMPI_THREAD_LOCK (&mca_io_romio_mutex);
    ret =
        ROMIO_PREFIX(MPI_File_write) (data->romio_fh, buf, count, datatype,
                                     status);
    OMPI_THREAD_UNLOCK (&mca_io_romio_mutex);

    return ret;
}

int
mca_io_romio_file_write_all (ompi_file_t *fh,
                             void *buf,
                             int count,
                             ompi_datatype_t *datatype,
                             ompi_status_public_t * status)
{
    int         ret;
    mca_io_romio_data_t *data;

    data = (mca_io_romio_data_t *) fh->f_io_selected_data;
    OMPI_THREAD_LOCK (&mca_io_romio_mutex);
    ret =
        ROMIO_PREFIX(MPI_File_write_all) (data->romio_fh, buf, count, datatype,
                                         status);
    OMPI_THREAD_UNLOCK (&mca_io_romio_mutex);

    return ret;
}

int
mca_io_romio_file_iwrite (ompi_file_t *fh,
                          void *buf,
                          int count,
                          ompi_datatype_t *datatype,
                          mca_io_base_request_t * request)
{
    int ret, flag;
    mca_io_romio_data_t *data;
    mca_io_romio_request_t *req;

    data = (mca_io_romio_data_t *) fh->f_io_selected_data;
    req = (mca_io_romio_request_t *) request;
    OMPI_THREAD_LOCK (&mca_io_romio_mutex);
    ret =
        ROMIO_PREFIX(MPI_File_iwrite) (data->romio_fh, buf, count, datatype,
                                       &req->romio_rq);
    if (MPI_SUCCESS == ret) {
        MCA_IO_ROMIO_REQUEST_ADD(request);
    }
    OMPI_THREAD_UNLOCK (&mca_io_romio_mutex);

    return ret;
}


int
mca_io_romio_file_write_shared (ompi_file_t *fh,
                                void *buf,
                                int count,
                                ompi_datatype_t *datatype,
                                ompi_status_public_t * status)
{
    int         ret;
    mca_io_romio_data_t *data;

    data = (mca_io_romio_data_t *) fh->f_io_selected_data;
    OMPI_THREAD_LOCK (&mca_io_romio_mutex);
    ret =
        ROMIO_PREFIX(MPI_File_write_shared) (data->romio_fh, buf, count, 
                                             datatype, status);
    OMPI_THREAD_UNLOCK (&mca_io_romio_mutex);

    return ret;
}

int
mca_io_romio_file_iwrite_shared (ompi_file_t *fh,
                                 void *buf,
                                 int count,
                                 ompi_datatype_t *datatype,
                                 mca_io_base_request_t * request)
{
    int ret, flag;
    mca_io_romio_data_t *data;
    mca_io_romio_request_t *req;

    data = (mca_io_romio_data_t *) fh->f_io_selected_data;
    req = (mca_io_romio_request_t *) request;
    OMPI_THREAD_LOCK (&mca_io_romio_mutex);
    ret =
        ROMIO_PREFIX(MPI_File_iwrite_shared) (data->romio_fh, buf, count,
                                              datatype, &req->romio_rq);
    if (MPI_SUCCESS == ret) {
        MCA_IO_ROMIO_REQUEST_ADD(request);
    }
    OMPI_THREAD_UNLOCK (&mca_io_romio_mutex);

    return ret;
}

int
mca_io_romio_file_write_ordered (ompi_file_t *fh,
                                 void *buf,
                                 int count,
                                 ompi_datatype_t *datatype,
                                 ompi_status_public_t * status)
{
    int         ret;
    mca_io_romio_data_t *data;

    data = (mca_io_romio_data_t *) fh->f_io_selected_data;
    OMPI_THREAD_LOCK (&mca_io_romio_mutex);
    ret =
        ROMIO_PREFIX(MPI_File_write_ordered) (data->romio_fh, buf, count,
                                             datatype, status);
    OMPI_THREAD_UNLOCK (&mca_io_romio_mutex);

    return ret;
}

int
mca_io_romio_file_write_at_all_begin (ompi_file_t *fh,
                                      MPI_Offset offset,
                                      void *buf,
                                      int count,
                                      ompi_datatype_t *datatype)
{
    int         ret;
    mca_io_romio_data_t *data;

    data = (mca_io_romio_data_t *) fh->f_io_selected_data;
    OMPI_THREAD_LOCK (&mca_io_romio_mutex);
    ret = ROMIO_PREFIX(MPI_File_write_at_all_begin) (data->romio_fh, offset, 
                                                     buf, count, datatype);
    OMPI_THREAD_UNLOCK (&mca_io_romio_mutex);

    return ret;
}

int
mca_io_romio_file_write_at_all_end (ompi_file_t *fh,
                                    void *buf,
                                    ompi_status_public_t * status)
{
    int         ret;
    mca_io_romio_data_t *data;

    data = (mca_io_romio_data_t *) fh->f_io_selected_data;
    OMPI_THREAD_LOCK (&mca_io_romio_mutex);
    ret = ROMIO_PREFIX(MPI_File_write_at_all_end) (data->romio_fh, buf, 
                                                   status);
    OMPI_THREAD_UNLOCK (&mca_io_romio_mutex);

    return ret;
}

int
mca_io_romio_file_write_all_begin (ompi_file_t *fh,
                                   void *buf,
                                   int count,
                                   ompi_datatype_t *datatype)
{
    int         ret;
    mca_io_romio_data_t *data;

    data = (mca_io_romio_data_t *) fh->f_io_selected_data;
    OMPI_THREAD_LOCK (&mca_io_romio_mutex);
    ret = ROMIO_PREFIX(MPI_File_write_all_begin) (data->romio_fh, buf, count,
                                                 datatype);
    OMPI_THREAD_UNLOCK (&mca_io_romio_mutex);

    return ret;
}

int
mca_io_romio_file_write_all_end (ompi_file_t *fh,
                                 void *buf,
                                 ompi_status_public_t * status)
{
    int         ret;
    mca_io_romio_data_t *data;

    data = (mca_io_romio_data_t *) fh->f_io_selected_data;
    OMPI_THREAD_LOCK (&mca_io_romio_mutex);
    ret = ROMIO_PREFIX(MPI_File_write_all_end) (data->romio_fh, buf, status);
    OMPI_THREAD_UNLOCK (&mca_io_romio_mutex);

    return ret;
}

int
mca_io_romio_file_write_ordered_begin (ompi_file_t *fh,
                                       void *buf,
                                       int count,
                                       ompi_datatype_t *datatype)
{
    int         ret;
    mca_io_romio_data_t *data;

    data = (mca_io_romio_data_t *) fh->f_io_selected_data;
    OMPI_THREAD_LOCK (&mca_io_romio_mutex);
    ret = ROMIO_PREFIX(MPI_File_write_ordered_begin) (data->romio_fh, buf,
                                                     count, datatype);
    OMPI_THREAD_UNLOCK (&mca_io_romio_mutex);

    return ret;
}

int
mca_io_romio_file_write_ordered_end (ompi_file_t *fh,
                                     void *buf,
                                     ompi_status_public_t * status)
{
    int         ret;
    mca_io_romio_data_t *data;

    data = (mca_io_romio_data_t *) fh->f_io_selected_data;
    OMPI_THREAD_LOCK (&mca_io_romio_mutex);
    ret = ROMIO_PREFIX(MPI_File_write_ordered_end) (data->romio_fh, buf, 
                                                    status);
    OMPI_THREAD_UNLOCK (&mca_io_romio_mutex);

    return ret;
}
