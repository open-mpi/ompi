/*
 * Copyright (c) 2011-2013 Cisco Systems, Inc.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef _MPIJAVA_H_
#define _MPIJAVA_H_

#include "mpi.h"

typedef struct {
    jfieldID  CommHandle;
    jfieldID  GroupHandle;
    jclass    CartParmsClass;
    jmethodID CartParmsInit;
    jclass    ShiftParmsClass;
    jmethodID ShiftParmsInit;
    jclass    GraphParmsClass;
    jmethodID GraphParmsInit;
    jclass    DistGraphNeighborsClass;
    jmethodID DistGraphNeighborsInit;
    jfieldID  DatatypeHandle;
    jfieldID  DatatypeBaseType;
    jfieldID  DatatypeBaseSize;
    jfieldID  MessageHandle;
    jfieldID  OpHandle;
    jfieldID  OpCommute;
    jmethodID OpCall;
    jfieldID  ReqHandle;
    jfieldID  ReqStatus;
    jclass    ExceptionClass;
    jmethodID ExceptionInit;
    jclass    IntegerClass;
    jmethodID IntegerValueOf;
    jclass    LongClass;
    jmethodID LongValueOf;
    int dtSizes[12];
} ompi_java_globals_t;

extern ompi_java_globals_t ompi_java;

void ompi_java_init_native_Datatype(JNIEnv *env);

void* ompi_java_getBufPtr(
        void** bufBase, JNIEnv *env,
        jobject buf, jboolean db, int baseType, int offset);

void ompi_java_releaseBufPtr(
        JNIEnv *env, jobject buf, jboolean db, void* bufBase, int baseType);

void ompi_java_releaseReadBufPtr(
        JNIEnv *env, jobject buf, jboolean db, void *bufBase, int baseType);

void* ompi_java_getDirectBufferAddress(JNIEnv *env, jobject buf);

void ompi_java_setStaticLongField(JNIEnv *env, jclass c,
                                  char *field, jlong value);

void ompi_java_setIntField(JNIEnv *env, jclass c, jobject obj,
                           char *field, jint value);

void   ompi_java_findClasses(JNIEnv *env);
jclass ompi_java_findClass(JNIEnv *env, const char *className);
void   ompi_java_deleteClasses(JNIEnv *env);

jobject ompi_java_Integer_valueOf(JNIEnv *env, jint i);
jobject ompi_java_Long_valueOf(JNIEnv *env, jlong i);

void ompi_java_getIntArray(
        JNIEnv *env, jintArray array, jint **jptr, int **cptr);
void ompi_java_releaseIntArray(
        JNIEnv *env, jintArray array, jint *jptr, int *cptr);
void ompi_java_forgetIntArray(
        JNIEnv *env, jintArray array, jint *jptr, int *cptr);

void ompi_java_getBooleanArray(
        JNIEnv *env, jbooleanArray array, jboolean **jptr, int **cptr);
void ompi_java_releaseBooleanArray(
        JNIEnv *env, jbooleanArray array, jboolean *jptr, int *cptr);
void ompi_java_forgetBooleanArray(
        JNIEnv *env, jbooleanArray array, jboolean *jptr, int *cptr);

void ompi_java_getPtrArray(
        JNIEnv *env, jlongArray array, jlong **jptr, void ***cptr);
void ompi_java_releasePtrArray(
        JNIEnv *env, jlongArray array, jlong *jptr, void **cptr);

jboolean ompi_java_exceptionCheck(JNIEnv *env, int rc);

void*      ompi_java_attrSet(JNIEnv *env, jbyteArray jval);
jbyteArray ompi_java_attrGet(JNIEnv *env, void *cval);
int        ompi_java_attrCopy(void *attrValIn, void *attrValOut, int *flag);
int        ompi_java_attrDelete(void *attrVal);

MPI_Op ompi_java_op_getHandle(
        JNIEnv *env, jobject jOp, jlong hOp, int baseType);

jlongArray ompi_java_status_new(
        JNIEnv *env, MPI_Status *status);
void ompi_java_status_set(
        JNIEnv *env, jlongArray jData, MPI_Status *status);
void ompi_java_status_setIndex(
        JNIEnv *env, jlongArray jData, MPI_Status *status, int index);

#endif /* _MPIJAVA_H_ */
