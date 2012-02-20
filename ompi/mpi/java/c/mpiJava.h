/*
 * Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 */


#include "mpi.h"

typedef struct {
    jfieldID CommhandleID;
    jfieldID ErrhandleID;
    jfieldID GrouphandleID;
    jfieldID DatatypehandleID;
    jfieldID DatatypebaseTypeID;
    jfieldID DatatypebaseSizeID;
    jfieldID OphandleID;
    jfieldID stathandleID;
    jfieldID sourceID;
    jfieldID tagID;
    jfieldID indexID;
    jfieldID elementsID;
    jfieldID reqhandleID;
    jfieldID opTagID;
    jfieldID bufSaveID;
    jfieldID countSaveID;
    jfieldID offsetSaveID;
    jfieldID baseTypeSaveID;
    jfieldID bufbaseSaveID;
    jfieldID bufptrSaveID;
    jfieldID commSaveID;
    jfieldID typeSaveID;
    int *dt_sizes;
} ompi_java_globals_t;
extern ompi_java_globals_t ompi_java;

void ompi_java_clearFreeList(JNIEnv*);

void ompi_java_init_native_Datatype(void);

void* ompi_java_getBufPtr(void** bufbase,
                          JNIEnv *env, jobject buf,
                          int baseType, int offset);

void ompi_java_releaseBufPtr(JNIEnv *env, jobject buf,
                             void* bufbase, int baseType);

void* ompi_java_getMPIWriteBuf(int* bsize, int count,
                               MPI_Datatype type, MPI_Comm comm);

#ifndef GC_DOES_PINNING

void* ompi_java_getMPIBuf(int* size, JNIEnv *env, jobject buf, int offset,
                          int count, MPI_Datatype type, MPI_Comm comm,
                          int baseType);

void ompi_java_releaseMPIBuf(JNIEnv *env, jobject buf, int offset,
                             int count, MPI_Datatype type, MPI_Comm comm,
                             void* bufptr, int size, int baseType);

void ompi_java_releaseMPIRecvBuf(int* elements, JNIEnv *env, jobject buf, int offset,
                                 int count, MPI_Datatype type, MPI_Comm comm,
                                 void* bufptr, MPI_Status* status,
                                 int baseType);

void ompi_java_releaseMPIReadBuf(void* bufptr);

#endif  /* GC_DOES_PINNING */
