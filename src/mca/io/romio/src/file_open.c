/*
 *  $HEADER$
 */

#include "mpi.h"
#include "file/file.h"
#include "io_romio.h"
#include "request/request.h"
#include <string.h>

int mca_io_romio_File_open(MPI_Comm comm, char *filename, int amode,
                         MPI_Info info, MPI_File *fh){

    int ret;
    mca_io_romio_MPI_File romio_fh;
    mca_io_romio_file_t *mca_romio_fh;  

    /* create a new ompi_file_t */
    mca_romio_fh = malloc(sizeof(mca_io_romio_file_t));
    (*fh) = (ompi_file_t *) mca_romio_fh;
    strncpy((*fh)->f_name,filename,MPI_MAX_OBJECT_NAME);
    (*fh)->ompi_io_version=OMPI_IO_1_0_0;
    (*fh)->mca_io_functions.f_io=romio_actions;

    romio_fh = mca_romio_fh->romio_fh;

    OMPI_THREAD_LOCK(&mca_io_romio_mutex);
    ret=mca_io_romio_MPI_File_open(comm,filename,amode,info,&romio_fh);
    OMPI_THREAD_UNLOCK(&mca_io_romio_mutex);

    return ret;
}
    

int mca_io_romio_File_close(MPI_File *fh) {
    int ret;
    mca_io_romio_MPI_File romio_fh;
    mca_io_romio_file_t *mca_romio_fh;  

    /* extract the ROMIO file handle: */
    mca_romio_fh = (mca_io_romio_file_t *)(*fh);
    romio_fh = mca_romio_fh->romio_fh;

    OMPI_THREAD_LOCK(&mca_io_romio_mutex);
    ret=mca_io_romio_MPI_File_close(&romio_fh);
    OMPI_THREAD_UNLOCK(&mca_io_romio_mutex);
  
    free(*fh);

    return ret;
}



int mca_io_romio_File_delete(char *filename, MPI_Info info) {
    int ret;

    OMPI_THREAD_LOCK(&mca_io_romio_mutex);
    ret=mca_io_romio_MPI_File_delete(filename, info);
    OMPI_THREAD_UNLOCK(&mca_io_romio_mutex);

    return ret;
}


int mca_io_romio_File_set_size(MPI_File fh, MPI_Offset size){
    int ret;
    mca_io_romio_MPI_File romio_fh;
    mca_io_romio_file_t *mca_romio_fh;  

    /* extract the ROMIO file handle: */
    mca_romio_fh = (mca_io_romio_file_t *) fh;
    romio_fh = mca_romio_fh->romio_fh;

    OMPI_THREAD_LOCK(&mca_io_romio_mutex);
    ret=mca_io_romio_MPI_File_set_size(romio_fh, size);
    OMPI_THREAD_UNLOCK(&mca_io_romio_mutex);
  
    return ret;


}

int mca_io_romio_File_preallocate(MPI_File fh, MPI_Offset size){
    int ret;
    mca_io_romio_MPI_File romio_fh;
    mca_io_romio_file_t *mca_romio_fh;  

    /* extract the ROMIO file handle: */
    mca_romio_fh = (mca_io_romio_file_t *) fh;
    romio_fh = mca_romio_fh->romio_fh;

    OMPI_THREAD_LOCK(&mca_io_romio_mutex);
    ret=mca_io_romio_MPI_File_preallocate(romio_fh,size);
    OMPI_THREAD_UNLOCK(&mca_io_romio_mutex);
  
    return ret;
}


int mca_io_romio_File_get_size(MPI_File fh, MPI_Offset *size){
    int ret;
    mca_io_romio_MPI_File romio_fh;
    mca_io_romio_file_t *mca_romio_fh;  

    /* extract the ROMIO file handle: */
    mca_romio_fh = (mca_io_romio_file_t *) fh;
    romio_fh = mca_romio_fh->romio_fh;

    OMPI_THREAD_LOCK(&mca_io_romio_mutex);
    ret=mca_io_romio_MPI_File_get_size(romio_fh,size);
    OMPI_THREAD_UNLOCK(&mca_io_romio_mutex);
  
    return ret;
}


int mca_io_romio_File_get_group(MPI_File fh, MPI_Group *group){
    int ret;
    mca_io_romio_MPI_File romio_fh;
    mca_io_romio_file_t *mca_romio_fh;  

    /* extract the ROMIO file handle: */
    mca_romio_fh = (mca_io_romio_file_t *) fh;
    romio_fh = mca_romio_fh->romio_fh;

    OMPI_THREAD_LOCK(&mca_io_romio_mutex);
    ret=mca_io_romio_MPI_File_get_group(romio_fh,group);
    OMPI_THREAD_UNLOCK(&mca_io_romio_mutex);
  
    return ret;
}


int mca_io_romio_File_get_amode(MPI_File fh, int *amode){
    int ret;
    mca_io_romio_MPI_File romio_fh;
    mca_io_romio_file_t *mca_romio_fh;  

    /* extract the ROMIO file handle: */
    mca_romio_fh = (mca_io_romio_file_t *) fh;
    romio_fh = mca_romio_fh->romio_fh;

    OMPI_THREAD_LOCK(&mca_io_romio_mutex);
    ret=mca_io_romio_MPI_File_get_amode(romio_fh, amode);
    OMPI_THREAD_UNLOCK(&mca_io_romio_mutex);
  
    return ret;
}


int mca_io_romio_File_set_info(MPI_File fh, MPI_Info info){
    int ret;
    mca_io_romio_MPI_File romio_fh;
    mca_io_romio_file_t *mca_romio_fh;  

    /* extract the ROMIO file handle: */
    mca_romio_fh = (mca_io_romio_file_t *) fh;
    romio_fh = mca_romio_fh->romio_fh;

    OMPI_THREAD_LOCK(&mca_io_romio_mutex);
    ret=mca_io_romio_MPI_File_set_info(romio_fh,info);
    OMPI_THREAD_UNLOCK(&mca_io_romio_mutex);
  
    return ret;
}


int mca_io_romio_File_get_info(MPI_File fh, MPI_Info *info_used){
    int ret;
    mca_io_romio_MPI_File romio_fh;
    mca_io_romio_file_t *mca_romio_fh;  

    /* extract the ROMIO file handle: */
    mca_romio_fh = (mca_io_romio_file_t *) fh;
    romio_fh = mca_romio_fh->romio_fh;

    OMPI_THREAD_LOCK(&mca_io_romio_mutex);
    ret=mca_io_romio_MPI_File_get_info(romio_fh,info_used);
    OMPI_THREAD_UNLOCK(&mca_io_romio_mutex);
  
    return ret;
}


int mca_io_romio_File_set_view(MPI_File fh, MPI_Offset disp, MPI_Datatype etype,
                               MPI_Datatype filetype, char *datarep, MPI_Info info){

    int ret;
    mca_io_romio_MPI_File romio_fh;
    mca_io_romio_file_t *mca_romio_fh;  

    /* extract the ROMIO file handle: */
    mca_romio_fh = (mca_io_romio_file_t *) fh;
    romio_fh = mca_romio_fh->romio_fh;

    OMPI_THREAD_LOCK(&mca_io_romio_mutex);
    ret=mca_io_romio_MPI_File_set_view(romio_fh,disp,etype,filetype,datarep,info);
    OMPI_THREAD_UNLOCK(&mca_io_romio_mutex);
  
    return ret;


}
int mca_io_romio_File_get_view(MPI_File fh, MPI_Offset *disp, MPI_Datatype *etype, MPI_Datatype *filetype, char *datarep){
    int ret;
    mca_io_romio_MPI_File romio_fh;
    mca_io_romio_file_t *mca_romio_fh;  

    /* extract the ROMIO file handle: */
    mca_romio_fh = (mca_io_romio_file_t *) fh;
    romio_fh = mca_romio_fh->romio_fh;

    OMPI_THREAD_LOCK(&mca_io_romio_mutex);
    ret=mca_io_romio_MPI_File_get_view(romio_fh,disp,etype,filetype,datarep);
    OMPI_THREAD_UNLOCK(&mca_io_romio_mutex);
  
    return ret;

}




int mca_io_romio_File_get_type_extent(MPI_File fh, MPI_Datatype datatype, MPI_Aint *extent){ 
    int ret;
    mca_io_romio_MPI_File romio_fh;
    mca_io_romio_file_t *mca_romio_fh;  

    /* extract the ROMIO file handle: */
    mca_romio_fh = (mca_io_romio_file_t *) fh;
    romio_fh = mca_romio_fh->romio_fh;

    OMPI_THREAD_LOCK(&mca_io_romio_mutex);
    ret=mca_io_romio_MPI_File_get_type_extent(romio_fh,datatype,extent);
    OMPI_THREAD_UNLOCK(&mca_io_romio_mutex);
  
    return ret;
}


int mca_io_romio_File_set_atomicity(MPI_File fh, int flag){ 
    int ret;
    mca_io_romio_MPI_File romio_fh;
    mca_io_romio_file_t *mca_romio_fh;  

    /* extract the ROMIO file handle: */
    mca_romio_fh = (mca_io_romio_file_t *) fh;
    romio_fh = mca_romio_fh->romio_fh;

    OMPI_THREAD_LOCK(&mca_io_romio_mutex);
    ret=mca_io_romio_MPI_File_set_atomicity(romio_fh,flag);
    OMPI_THREAD_UNLOCK(&mca_io_romio_mutex);
  
    return ret;
}

int mca_io_romio_File_get_atomicity(MPI_File fh, int *flag){ 
    int ret;
    mca_io_romio_MPI_File romio_fh;
    mca_io_romio_file_t *mca_romio_fh;  

    /* extract the ROMIO file handle: */
    mca_romio_fh = (mca_io_romio_file_t *) fh;
    romio_fh = mca_romio_fh->romio_fh;

    OMPI_THREAD_LOCK(&mca_io_romio_mutex);
    ret=mca_io_romio_MPI_File_get_atomicity(romio_fh,flag);
    OMPI_THREAD_UNLOCK(&mca_io_romio_mutex);
  
    return ret;
}

int mca_io_romio_File_sync(MPI_File fh){ 
    int ret;
    mca_io_romio_MPI_File romio_fh;
    mca_io_romio_file_t *mca_romio_fh;  

    /* extract the ROMIO file handle: */
    mca_romio_fh = (mca_io_romio_file_t *) fh;
    romio_fh = mca_romio_fh->romio_fh;

    OMPI_THREAD_LOCK(&mca_io_romio_mutex);
    ret=mca_io_romio_MPI_File_sync(romio_fh);
    OMPI_THREAD_UNLOCK(&mca_io_romio_mutex);
  
    return ret;
}





int mca_io_romio_File_seek_shared(MPI_File fh, MPI_Offset offset, int whence){ 
    int ret;
    mca_io_romio_MPI_File romio_fh;
    mca_io_romio_file_t *mca_romio_fh;  

    /* extract the ROMIO file handle: */
    mca_romio_fh = (mca_io_romio_file_t *) fh;
    romio_fh = mca_romio_fh->romio_fh;

    OMPI_THREAD_LOCK(&mca_io_romio_mutex);
    ret=mca_io_romio_MPI_File_seek_shared(romio_fh, offset, whence);
    OMPI_THREAD_UNLOCK(&mca_io_romio_mutex);
  
    return ret;
}
int mca_io_romio_File_get_position_shared(MPI_File fh, MPI_Offset *offset){ 
    int ret;
    mca_io_romio_MPI_File romio_fh;
    mca_io_romio_file_t *mca_romio_fh;  

    /* extract the ROMIO file handle: */
    mca_romio_fh = (mca_io_romio_file_t *) fh;
    romio_fh = mca_romio_fh->romio_fh;

    OMPI_THREAD_LOCK(&mca_io_romio_mutex);
    ret=mca_io_romio_MPI_File_get_position_shared(romio_fh,offset);
    OMPI_THREAD_UNLOCK(&mca_io_romio_mutex);
  
    return ret;
}

int mca_io_romio_File_seek(MPI_File fh, MPI_Offset offset, int whence){ 
    int ret;
    mca_io_romio_MPI_File romio_fh;
    mca_io_romio_file_t *mca_romio_fh;  

    /* extract the ROMIO file handle: */
    mca_romio_fh = (mca_io_romio_file_t *) fh;
    romio_fh = mca_romio_fh->romio_fh;

    OMPI_THREAD_LOCK(&mca_io_romio_mutex);
    ret=mca_io_romio_MPI_File_seek(romio_fh,offset,whence);
    OMPI_THREAD_UNLOCK(&mca_io_romio_mutex);
  
    return ret;
}
int mca_io_romio_File_get_position(MPI_File fh, MPI_Offset *offset){ 
    int ret;
    mca_io_romio_MPI_File romio_fh;
    mca_io_romio_file_t *mca_romio_fh;  

    /* extract the ROMIO file handle: */
    mca_romio_fh = (mca_io_romio_file_t *) fh;
    romio_fh = mca_romio_fh->romio_fh;

    OMPI_THREAD_LOCK(&mca_io_romio_mutex);
    ret=mca_io_romio_MPI_File_get_position(romio_fh,offset);
    OMPI_THREAD_UNLOCK(&mca_io_romio_mutex);
  
    return ret;
}
int mca_io_romio_File_get_byte_offset(MPI_File fh, MPI_Offset offset, MPI_Offset *disp){ 
    int ret;
    mca_io_romio_MPI_File romio_fh;
    mca_io_romio_file_t *mca_romio_fh;  

    /* extract the ROMIO file handle: */
    mca_romio_fh = (mca_io_romio_file_t *) fh;
    romio_fh = mca_romio_fh->romio_fh;

    OMPI_THREAD_LOCK(&mca_io_romio_mutex);
    ret=mca_io_romio_MPI_File_get_byte_offset(romio_fh,offset,disp);
    OMPI_THREAD_UNLOCK(&mca_io_romio_mutex);
  
    return ret;
}


int mca_io_romio_File_set_errhandler(MPI_File fh, MPI_Errhandler eh){ 
    int ret;
    mca_io_romio_MPI_File romio_fh;
    mca_io_romio_file_t *mca_romio_fh;  

    /* extract the ROMIO file handle: */
    mca_romio_fh = (mca_io_romio_file_t *) fh;
    romio_fh = mca_romio_fh->romio_fh;

    OMPI_THREAD_LOCK(&mca_io_romio_mutex);
    ret=mca_io_romio_MPI_File_set_errhandler(romio_fh,eh);
    OMPI_THREAD_UNLOCK(&mca_io_romio_mutex);
  
    return ret;
}
int mca_io_romio_File_get_errhandler(MPI_File fh, MPI_Errhandler *eh ){ 
    int ret;
    mca_io_romio_MPI_File romio_fh;
    mca_io_romio_file_t *mca_romio_fh;  

    /* extract the ROMIO file handle: */
    mca_romio_fh = (mca_io_romio_file_t *) fh;
    romio_fh = mca_romio_fh->romio_fh;

    OMPI_THREAD_LOCK(&mca_io_romio_mutex);
    ret=mca_io_romio_MPI_File_get_errhandler(romio_fh,eh );
    OMPI_THREAD_UNLOCK(&mca_io_romio_mutex);
  
    return ret;
}


