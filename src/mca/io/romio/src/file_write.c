/*
 *  $HEADER$
 */

#include "mpi.h"
#include "file/file.h"
#include "io_romio.h"
#include "request/request.h"


int mca_io_romio_File_write_at(MPI_File fh, MPI_Offset offset, void *buf,int count, MPI_Datatype datatype, MPI_Status *status){
    int ret;
    mca_io_romio_MPI_File romio_fh;
    mca_io_romio_file_t *mca_romio_fh;  

    /* extract the ROMIO file handle: */
    mca_romio_fh = (mca_io_romio_file_t *) fh;
    romio_fh = mca_romio_fh->romio_fh;

    THREAD_LOCK(&mca_io_romio_mutex);
    ret=mca_io_romio_MPI_File_write_at(romio_fh,offset,buf,count,datatype,status);
    THREAD_UNLOCK(&mca_io_romio_mutex);
  
    return ret;
}



int mca_io_romio_File_write_at_all(MPI_File fh, MPI_Offset offset, void *buf, int count, MPI_Datatype datatype, MPI_Status *status){
    int ret;
    mca_io_romio_MPI_File romio_fh;
    mca_io_romio_file_t *mca_romio_fh;  

    /* extract the ROMIO file handle: */
    mca_romio_fh = (mca_io_romio_file_t *) fh;
    romio_fh = mca_romio_fh->romio_fh;

    THREAD_LOCK(&mca_io_romio_mutex);
    ret=mca_io_romio_MPI_File_write_at_all(romio_fh,offset,buf,count,datatype,status);
    THREAD_UNLOCK(&mca_io_romio_mutex);
  
    return ret;
}



int mca_io_romio_File_iwrite_at(MPI_File fh, MPI_Offset offset, void *buf,int count, MPI_Datatype datatype, MPI_Request *request){
    int ret;
    mca_io_romio_MPI_File romio_fh;
    mca_io_romio_file_t *mca_romio_fh;  

    mca_io_romio_request_t *rq;
    mca_io_romio_MPIO_Request romio_rq;

    /* create MPI_request */
    rq = malloc(sizeof(mca_io_romio_request_t));
    (*request) = (ompi_request_t *) rq;
    (*request)->req_type = OMPI_REQUEST_IO;
    /* extract the ROMIO request */
    romio_rq = rq->romio_rq;

    /* extract the ROMIO file handle: */
    mca_romio_fh = (mca_io_romio_file_t *) fh;
    romio_fh = mca_romio_fh->romio_fh;


    THREAD_LOCK(&mca_io_romio_mutex);
    ret=mca_io_romio_MPI_File_iwrite_at(romio_fh,offset,buf,count,datatype,
         &romio_rq);
    THREAD_UNLOCK(&mca_io_romio_mutex);
  
    return ret;
}





int mca_io_romio_File_write(MPI_File fh, void *buf, int count, MPI_Datatype datatype, MPI_Status *status){
    int ret;
    mca_io_romio_MPI_File romio_fh;
    mca_io_romio_file_t *mca_romio_fh;  

    /* extract the ROMIO file handle: */
    mca_romio_fh = (mca_io_romio_file_t *) fh;
    romio_fh = mca_romio_fh->romio_fh;

    THREAD_LOCK(&mca_io_romio_mutex);
    ret=mca_io_romio_MPI_File_write(romio_fh,buf,count,datatype,status);
    THREAD_UNLOCK(&mca_io_romio_mutex);
  
    return ret;
}

int mca_io_romio_File_write_all(MPI_File fh, void *buf, int count, MPI_Datatype datatype, MPI_Status *status){
    int ret;
    mca_io_romio_MPI_File romio_fh;
    mca_io_romio_file_t *mca_romio_fh;  

    /* extract the ROMIO file handle: */
    mca_romio_fh = (mca_io_romio_file_t *) fh;
    romio_fh = mca_romio_fh->romio_fh;

    THREAD_LOCK(&mca_io_romio_mutex);
    ret=mca_io_romio_MPI_File_write_all(romio_fh,buf,count,datatype,status);
    THREAD_UNLOCK(&mca_io_romio_mutex);
  
    return ret;
}


int mca_io_romio_File_iwrite(MPI_File fh, void *buf, int count, 
                         MPI_Datatype datatype, ompi_request_t **request)
{
    mca_io_romio_request_t *rq;
    mca_io_romio_MPIO_Request romio_rq;
    mca_io_romio_MPI_File romio_fh;
    int ret;


    /* create MPI_request */
    rq = malloc(sizeof(mca_io_romio_request_t));
    *request = (ompi_request_t*) rq;
    (*request)->req_type = OMPI_REQUEST_IO;
    /* extract the ROMIO request */
    romio_rq = rq->romio_rq;

    romio_fh = ((mca_io_romio_file_t *) fh)->romio_fh;

    THREAD_LOCK(&mca_io_romio_mutex);
    ret=mca_io_romio_MPI_File_iwrite(romio_fh, buf, count, datatype, &romio_rq);
    THREAD_UNLOCK(&mca_io_romio_mutex);
    return ret;
}


int mca_io_romio_File_write_shared(MPI_File fh, void *buf, int count, MPI_Datatype datatype, MPI_Status *status){
    int ret;
    mca_io_romio_MPI_File romio_fh;
    mca_io_romio_file_t *mca_romio_fh;  

    /* extract the ROMIO file handle: */
    mca_romio_fh = (mca_io_romio_file_t *) fh;
    romio_fh = mca_romio_fh->romio_fh;

    THREAD_LOCK(&mca_io_romio_mutex);
    ret=mca_io_romio_MPI_File_write_shared(romio_fh,buf,count,datatype,status);
    THREAD_UNLOCK(&mca_io_romio_mutex);
  
    return ret;
}
int mca_io_romio_File_iwrite_shared(MPI_File fh, void *buf, int count, 
                                    MPI_Datatype datatype, MPI_Request *request){
    int ret;
    mca_io_romio_MPI_File romio_fh;
    mca_io_romio_file_t *mca_romio_fh;  

    mca_io_romio_request_t *rq;
    mca_io_romio_MPIO_Request romio_rq;

    /* create MPI_request */
    rq = malloc(sizeof(mca_io_romio_request_t));
    (*request) = (ompi_request_t *) rq;
    (*request)->req_type = OMPI_REQUEST_IO;
    /* extract the ROMIO request */
    romio_rq = rq->romio_rq;


    /* extract the ROMIO file handle: */
    mca_romio_fh = (mca_io_romio_file_t *) fh;
    romio_fh = mca_romio_fh->romio_fh;


    THREAD_LOCK(&mca_io_romio_mutex);
    ret= mca_io_romio_MPI_File_iwrite_shared(romio_fh,buf,count,datatype,&romio_rq);
    THREAD_UNLOCK(&mca_io_romio_mutex);
  
    return ret;
}
int mca_io_romio_File_write_ordered(MPI_File fh, void *buf, int count, 
                                    MPI_Datatype datatype, MPI_Status *status){
    int ret;
    mca_io_romio_MPI_File romio_fh;
    mca_io_romio_file_t *mca_romio_fh;  

    /* extract the ROMIO file handle: */
    mca_romio_fh = (mca_io_romio_file_t *) fh;
    romio_fh = mca_romio_fh->romio_fh;

    THREAD_LOCK(&mca_io_romio_mutex);
    ret= mca_io_romio_MPI_File_write_ordered(romio_fh,buf,count,datatype,status);
    THREAD_UNLOCK(&mca_io_romio_mutex);
  
    return ret;
}
int mca_io_romio_File_write_at_all_begin(MPI_File fh, MPI_Offset offset, void *buf,
                                         int count, MPI_Datatype datatype){
    int ret;
    mca_io_romio_MPI_File romio_fh;
    mca_io_romio_file_t *mca_romio_fh;  

    /* extract the ROMIO file handle: */
    mca_romio_fh = (mca_io_romio_file_t *) fh;
    romio_fh = mca_romio_fh->romio_fh;

    THREAD_LOCK(&mca_io_romio_mutex);
    ret= mca_io_romio_MPI_File_write_at_all_begin(romio_fh,offset, buf,
                                         count, datatype);
    THREAD_UNLOCK(&mca_io_romio_mutex);
  
    return ret;
}
int mca_io_romio_File_write_at_all_end(MPI_File fh, void *buf, MPI_Status *status){
    int ret;
    mca_io_romio_MPI_File romio_fh;
    mca_io_romio_file_t *mca_romio_fh;  

    /* extract the ROMIO file handle: */
    mca_romio_fh = (mca_io_romio_file_t *) fh;
    romio_fh = mca_romio_fh->romio_fh;

    THREAD_LOCK(&mca_io_romio_mutex);
    ret=mca_io_romio_MPI_File_write_at_all_end(romio_fh,buf,status);
    THREAD_UNLOCK(&mca_io_romio_mutex);
  
    return ret;
}
int mca_io_romio_File_write_all_begin(MPI_File fh, void *buf, int count, 
                                      MPI_Datatype datatype){
    int ret;
    mca_io_romio_MPI_File romio_fh;
    mca_io_romio_file_t *mca_romio_fh;  

    /* extract the ROMIO file handle: */
    mca_romio_fh = (mca_io_romio_file_t *) fh;
    romio_fh = mca_romio_fh->romio_fh;

    THREAD_LOCK(&mca_io_romio_mutex);
    ret= mca_io_romio_MPI_File_write_all_begin(romio_fh, buf, count, 
                                      datatype);
    THREAD_UNLOCK(&mca_io_romio_mutex);
  
    return ret;
}
int mca_io_romio_File_write_all_end(MPI_File fh, void *buf, MPI_Status *status){
    int ret;
    mca_io_romio_MPI_File romio_fh;
    mca_io_romio_file_t *mca_romio_fh;  

    /* extract the ROMIO file handle: */
    mca_romio_fh = (mca_io_romio_file_t *) fh;
    romio_fh = mca_romio_fh->romio_fh;

    THREAD_LOCK(&mca_io_romio_mutex);
    ret= mca_io_romio_MPI_File_write_all_end(romio_fh,buf,status);
    THREAD_UNLOCK(&mca_io_romio_mutex);
  
    return ret;
}
int mca_io_romio_File_write_ordered_begin(MPI_File fh, void *buf, int count,MPI_Datatype datatype){
    int ret;
    mca_io_romio_MPI_File romio_fh;
    mca_io_romio_file_t *mca_romio_fh;  

    /* extract the ROMIO file handle: */
    mca_romio_fh = (mca_io_romio_file_t *) fh;
    romio_fh = mca_romio_fh->romio_fh;

    THREAD_LOCK(&mca_io_romio_mutex);
    ret=mca_io_romio_MPI_File_write_ordered_begin(romio_fh,buf,count,datatype);
    THREAD_UNLOCK(&mca_io_romio_mutex);
  
    return ret;
}
int mca_io_romio_File_write_ordered_end(MPI_File fh, void *buf, MPI_Status *status){
    int ret;
    mca_io_romio_MPI_File romio_fh;
    mca_io_romio_file_t *mca_romio_fh;  

    /* extract the ROMIO file handle: */
    mca_romio_fh = (mca_io_romio_file_t *) fh;
    romio_fh = mca_romio_fh->romio_fh;

    THREAD_LOCK(&mca_io_romio_mutex);
    ret=mca_io_romio_MPI_File_write_ordered_end(romio_fh,buf,status);
    THREAD_UNLOCK(&mca_io_romio_mutex);
  
    return ret;
}




