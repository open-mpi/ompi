/*
 *  $HEADER$
 */

#include "mpi.h"
#include "file/file.h"
#include "request/request.h"
#include "io_romio.h"

int
mca_io_romio_File_read_at (MPI_File fh,
                           MPI_Offset offset,
                           void *buf,
                           int count,
                           MPI_Datatype datatype,
                           MPI_Status * status)
{
    int         ret;
    mca_io_romio_MPI_File romio_fh;
    mca_io_romio_file_t *mca_romio_fh;

    /* extract the ROMIO file handle: */
    mca_romio_fh = (mca_io_romio_file_t *) fh;
    romio_fh = mca_romio_fh->romio_fh;

    OMPI_THREAD_LOCK (&mca_io_romio_mutex);
    ret =
        mca_io_romio_MPI_File_read_at (romio_fh, offset, buf, count,
                                       datatype, status);
    OMPI_THREAD_UNLOCK (&mca_io_romio_mutex);

    return ret;
}


int
mca_io_romio_File_read_at_all (MPI_File fh,
                               MPI_Offset offset,
                               void *buf,
                               int count,
                               MPI_Datatype datatype,
                               MPI_Status * status)
{
    int         ret;
    mca_io_romio_MPI_File romio_fh;
    mca_io_romio_file_t *mca_romio_fh;

    /* extract the ROMIO file handle: */
    mca_romio_fh = (mca_io_romio_file_t *) fh;
    romio_fh = mca_romio_fh->romio_fh;

    OMPI_THREAD_LOCK (&mca_io_romio_mutex);
    ret =
        mca_io_romio_MPI_File_read_at_all (romio_fh, offset, buf, count,
                                           datatype, status);
    OMPI_THREAD_UNLOCK (&mca_io_romio_mutex);

    return ret;
}


int
mca_io_romio_File_iread_at (MPI_File fh,
                            MPI_Offset offset,
                            void *buf,
                            int count,
                            MPI_Datatype datatype,
                            MPI_Request * request)
{
    int         ret;
    mca_io_romio_MPI_File romio_fh;
    mca_io_romio_file_t *mca_romio_fh;

    mca_io_romio_request_t *rq;
    mca_io_romio_MPIO_Request romio_rq;

    /* create MPI_request */
    rq = malloc (sizeof (mca_io_romio_request_t));
    (*request) = (ompi_request_t *) rq;
    (*request)->req_type = OMPI_REQUEST_IO;

    /* extract the ROMIO file handle: */
    mca_romio_fh = (mca_io_romio_file_t *) fh;
    romio_fh = mca_romio_fh->romio_fh;


    OMPI_THREAD_LOCK (&mca_io_romio_mutex);
    ret =
        mca_io_romio_MPI_File_iread_at (romio_fh, offset, buf, count,
                                        datatype, &romio_rq);
    OMPI_THREAD_UNLOCK (&mca_io_romio_mutex);

    return ret;
}


int
mca_io_romio_File_read (MPI_File fh,
                        void *buf,
                        int count,
                        MPI_Datatype datatype,
                        MPI_Status * status)
{
    int         ret;
    mca_io_romio_MPI_File romio_fh;
    mca_io_romio_file_t *mca_romio_fh;

    /* extract the ROMIO file handle: */
    mca_romio_fh = (mca_io_romio_file_t *) fh;
    romio_fh = mca_romio_fh->romio_fh;

    OMPI_THREAD_LOCK (&mca_io_romio_mutex);
    ret =
        mca_io_romio_MPI_File_read (romio_fh, buf, count, datatype,
                                    status);
    OMPI_THREAD_UNLOCK (&mca_io_romio_mutex);

    return ret;
}


int
mca_io_romio_File_read_all (MPI_File fh,
                            void *buf,
                            int count,
                            MPI_Datatype datatype,
                            MPI_Status * status)
{
    int         ret;
    mca_io_romio_MPI_File romio_fh;
    mca_io_romio_file_t *mca_romio_fh;

    /* extract the ROMIO file handle: */
    mca_romio_fh = (mca_io_romio_file_t *) fh;
    romio_fh = mca_romio_fh->romio_fh;

    OMPI_THREAD_LOCK (&mca_io_romio_mutex);
    ret =
        mca_io_romio_MPI_File_read_all (romio_fh, buf, count, datatype,
                                        status);
    OMPI_THREAD_UNLOCK (&mca_io_romio_mutex);

    return ret;
}


int
mca_io_romio_File_iread (MPI_File fh,
                         void *buf,
                         int count,
                         MPI_Datatype datatype,
                         MPI_Request * request)
{
    int         ret;
    mca_io_romio_MPI_File romio_fh;
    mca_io_romio_file_t *mca_romio_fh;

    mca_io_romio_request_t *rq;
    mca_io_romio_MPIO_Request romio_rq;

    /* create MPI_request */
    rq = malloc (sizeof (mca_io_romio_request_t));
    (*request) = (ompi_request_t *) rq;
    (*request)->req_type = OMPI_REQUEST_IO;

    /* extract the ROMIO request */
    romio_rq = rq->romio_rq;

    /* extract the ROMIO file handle: */
    mca_romio_fh = (mca_io_romio_file_t *) fh;
    romio_fh = mca_romio_fh->romio_fh;


    OMPI_THREAD_LOCK (&mca_io_romio_mutex);
    ret =
        mca_io_romio_MPI_File_iread (romio_fh, buf, count, datatype,
                                     &romio_rq);
    OMPI_THREAD_UNLOCK (&mca_io_romio_mutex);

    return ret;
}


int
mca_io_romio_File_read_shared (MPI_File fh,
                               void *buf,
                               int count,
                               MPI_Datatype datatype,
                               MPI_Status * status)
{
    int         ret;
    mca_io_romio_MPI_File romio_fh;
    mca_io_romio_file_t *mca_romio_fh;

    /* extract the ROMIO file handle: */
    mca_romio_fh = (mca_io_romio_file_t *) fh;
    romio_fh = mca_romio_fh->romio_fh;

    OMPI_THREAD_LOCK (&mca_io_romio_mutex);
    ret =
        mca_io_romio_MPI_File_read_shared (romio_fh, buf, count, datatype,
                                           status);
    OMPI_THREAD_UNLOCK (&mca_io_romio_mutex);

    return ret;
}


int
mca_io_romio_File_iread_shared (MPI_File fh,
                                void *buf,
                                int count,
                                MPI_Datatype datatype,
                                MPI_Request * request)
{
    int         ret;
    mca_io_romio_MPI_File romio_fh;
    mca_io_romio_file_t *mca_romio_fh;

    mca_io_romio_request_t *rq;
    mca_io_romio_MPIO_Request romio_rq;

    /* create MPI_request */
    rq = malloc (sizeof (mca_io_romio_request_t));
    (*request) = (ompi_request_t *) rq;
    (*request)->req_type = OMPI_REQUEST_IO;

    /* extract the ROMIO request */
    romio_rq = rq->romio_rq;

    /* extract the ROMIO file handle: */
    mca_romio_fh = (mca_io_romio_file_t *) fh;
    romio_fh = mca_romio_fh->romio_fh;


    OMPI_THREAD_LOCK (&mca_io_romio_mutex);
    ret =
        mca_io_romio_MPI_File_iread_shared (romio_fh, buf, count, datatype,
                                            &romio_rq);
    OMPI_THREAD_UNLOCK (&mca_io_romio_mutex);

    return ret;
}


int
mca_io_romio_File_read_ordered (MPI_File fh,
                                void *buf,
                                int count,
                                MPI_Datatype datatype,
                                MPI_Status * status)
{
    int         ret;
    mca_io_romio_MPI_File romio_fh;
    mca_io_romio_file_t *mca_romio_fh;

    /* extract the ROMIO file handle: */
    mca_romio_fh = (mca_io_romio_file_t *) fh;
    romio_fh = mca_romio_fh->romio_fh;

    OMPI_THREAD_LOCK (&mca_io_romio_mutex);
    ret =
        mca_io_romio_MPI_File_read_ordered (romio_fh, buf, count, datatype,
                                            status);
    OMPI_THREAD_UNLOCK (&mca_io_romio_mutex);

    return ret;
}


int
mca_io_romio_File_read_at_all_begin (MPI_File fh,
                                     MPI_Offset offset,
                                     void *buf,
                                     int count,
                                     MPI_Datatype datatype)
{
    int         ret;
    mca_io_romio_MPI_File romio_fh;
    mca_io_romio_file_t *mca_romio_fh;

    /* extract the ROMIO file handle: */
    mca_romio_fh = (mca_io_romio_file_t *) fh;
    romio_fh = mca_romio_fh->romio_fh;

    OMPI_THREAD_LOCK (&mca_io_romio_mutex);
    ret =
        mca_io_romio_MPI_File_read_at_all_begin (romio_fh, offset, buf,
                                                 count, datatype);
    OMPI_THREAD_UNLOCK (&mca_io_romio_mutex);

    return ret;
}


int
mca_io_romio_File_read_at_all_end (MPI_File fh,
                                   void *buf,
                                   MPI_Status * status)
{
    int         ret;
    mca_io_romio_MPI_File romio_fh;
    mca_io_romio_file_t *mca_romio_fh;

    /* extract the ROMIO file handle: */
    mca_romio_fh = (mca_io_romio_file_t *) fh;
    romio_fh = mca_romio_fh->romio_fh;

    OMPI_THREAD_LOCK (&mca_io_romio_mutex);
    ret = mca_io_romio_MPI_File_read_at_all_end (romio_fh, buf, status);
    OMPI_THREAD_UNLOCK (&mca_io_romio_mutex);

    return ret;
}


int
mca_io_romio_File_read_all_begin (MPI_File fh,
                                  void *buf,
                                  int count,
                                  MPI_Datatype datatype)
{
    int         ret;
    mca_io_romio_MPI_File romio_fh;
    mca_io_romio_file_t *mca_romio_fh;

    /* extract the ROMIO file handle: */
    mca_romio_fh = (mca_io_romio_file_t *) fh;
    romio_fh = mca_romio_fh->romio_fh;

    OMPI_THREAD_LOCK (&mca_io_romio_mutex);
    ret =
        mca_io_romio_MPI_File_read_all_begin (romio_fh, buf, count,
                                              datatype);
    OMPI_THREAD_UNLOCK (&mca_io_romio_mutex);

    return ret;
}


int
mca_io_romio_File_read_all_end (MPI_File fh,
                                void *buf,
                                MPI_Status * status)
{
    int         ret;
    mca_io_romio_MPI_File romio_fh;
    mca_io_romio_file_t *mca_romio_fh;

    /* extract the ROMIO file handle: */
    mca_romio_fh = (mca_io_romio_file_t *) fh;
    romio_fh = mca_romio_fh->romio_fh;

    OMPI_THREAD_LOCK (&mca_io_romio_mutex);
    ret = mca_io_romio_MPI_File_read_all_end (romio_fh, buf, status);
    OMPI_THREAD_UNLOCK (&mca_io_romio_mutex);

    return ret;
}


int
mca_io_romio_File_read_ordered_begin (MPI_File fh,
                                      void *buf,
                                      int count,
                                      MPI_Datatype datatype)
{
    int         ret;
    mca_io_romio_MPI_File romio_fh;
    mca_io_romio_file_t *mca_romio_fh;

    /* extract the ROMIO file handle: */
    mca_romio_fh = (mca_io_romio_file_t *) fh;
    romio_fh = mca_romio_fh->romio_fh;

    OMPI_THREAD_LOCK (&mca_io_romio_mutex);
    ret =
        mca_io_romio_MPI_File_read_ordered_begin (romio_fh, buf, count,
                                                  datatype);
    OMPI_THREAD_UNLOCK (&mca_io_romio_mutex);

    return ret;
}


int
mca_io_romio_File_read_ordered_end (MPI_File fh,
                                    void *buf,
                                    MPI_Status * status)
{
    int         ret;
    mca_io_romio_MPI_File romio_fh;
    mca_io_romio_file_t *mca_romio_fh;

    /* extract the ROMIO file handle: */
    mca_romio_fh = (mca_io_romio_file_t *) fh;
    romio_fh = mca_romio_fh->romio_fh;

    OMPI_THREAD_LOCK (&mca_io_romio_mutex);
    ret = mca_io_romio_MPI_File_read_ordered_end (romio_fh, buf, status);
    OMPI_THREAD_UNLOCK (&mca_io_romio_mutex);

    return ret;
}
