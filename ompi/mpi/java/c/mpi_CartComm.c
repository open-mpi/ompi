/*
    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.
*/
/*
 * File         : mpi_CartComm.c
 * Headerfile   : mpi_CartComm.h
 * Author       : Sung-Hoon Ko, Xinying Li
 * Created      : Thu Apr  9 12:22:15 1998
 * Revision     : $Revision: 1.6 $
 * Updated      : $Date: 2003/01/16 16:39:34 $
 * Copyright: Northeast Parallel Architectures Center
 *            at Syracuse University 1998
 */
#include "ompi_config.h"

#include <stdlib.h>
#ifdef HAVE_TARGETCONDITIONALS_H
#include <TargetConditionals.h>
#endif

#include "mpi.h"
#include "mpi_CartComm.h"
#include "mpiJava.h"

JNIEXPORT void JNICALL Java_mpi_CartComm_init(JNIEnv *env, jclass clazz)
{
    ompi_java.CartParmsInit = (*env)->GetMethodID(env,
            ompi_java.CartParmsClass, "<init>", "([I[Z[I)V");
    
    ompi_java.ShiftParmsInit = (*env)->GetMethodID(env,
            ompi_java.ShiftParmsClass, "<init>", "(II)V");
}

JNIEXPORT jobject JNICALL Java_mpi_CartComm_getTopo(
        JNIEnv *env, jobject jthis, jlong comm)
{
    int maxdims;
    int rc = MPI_Cartdim_get((MPI_Comm)comm, &maxdims);

    if(ompi_java_exceptionCheck(env, rc))
        return NULL;

    jintArray     dims    = (*env)->NewIntArray(env, maxdims);
    jbooleanArray periods = (*env)->NewBooleanArray(env, maxdims);
    jintArray     coords  = (*env)->NewIntArray(env, maxdims);

    if(maxdims != 0)
    {
        jint *jDims, *jCoords;
        jboolean *jPeriods;
        int  *cDims, *cCoords, *cPeriods;

        ompi_java_getIntArray(env, dims, &jDims, &cDims);
        ompi_java_getIntArray(env, coords, &jCoords, &cCoords);
        ompi_java_getBooleanArray(env, periods, &jPeriods, &cPeriods);

        rc = MPI_Cart_get((MPI_Comm)comm, maxdims, cDims, cPeriods, cCoords);
        ompi_java_exceptionCheck(env, rc);

        ompi_java_releaseIntArray(env, dims, jDims, cDims);
        ompi_java_releaseIntArray(env, coords, jCoords, cCoords);
        ompi_java_releaseBooleanArray(env, periods, jPeriods, cPeriods);
    }

    return (*env)->NewObject(env, ompi_java.CartParmsClass,
                             ompi_java.CartParmsInit, dims, periods, coords);
}

JNIEXPORT jobject JNICALL Java_mpi_CartComm_shift(
        JNIEnv *env, jobject jthis, jlong comm, jint direction, jint disp)
{
    int sr, dr;
    int rc = MPI_Cart_shift((MPI_Comm)comm, direction, disp, &sr, &dr);
    ompi_java_exceptionCheck(env, rc);

    return (*env)->NewObject(env, ompi_java.ShiftParmsClass,
                             ompi_java.ShiftParmsInit, sr, dr);
}

JNIEXPORT jintArray JNICALL Java_mpi_CartComm_getCoords(
        JNIEnv *env, jobject jthis, jlong comm, jint rank)
{
    int maxdims;
    int rc = MPI_Cartdim_get((MPI_Comm)comm, &maxdims);

    if(ompi_java_exceptionCheck(env, rc))
        return NULL;

    jintArray coords = (*env)->NewIntArray(env, maxdims);
    jint *jCoords;
    int  *cCoords;
    ompi_java_getIntArray(env, coords, &jCoords, &cCoords);

    rc = MPI_Cart_coords((MPI_Comm)comm, rank, maxdims, cCoords);
    ompi_java_exceptionCheck(env, rc);

    ompi_java_releaseIntArray(env, coords, jCoords, cCoords);
    return coords;
}

JNIEXPORT jint JNICALL Java_mpi_CartComm_map(
        JNIEnv *env, jobject jthis, jlong comm,
        jintArray dims, jbooleanArray periods)
{
    int nDims = (*env)->GetArrayLength(env, dims);
    jint *jDims;
    jboolean *jPeriods;
    int *cDims, *cPeriods;
    ompi_java_getIntArray(env, dims, &jDims, &cDims);
    ompi_java_getBooleanArray(env, periods, &jPeriods, &cPeriods);

    int newrank;
    int rc = MPI_Cart_map((MPI_Comm)comm, nDims, cDims, cPeriods, &newrank);
    ompi_java_exceptionCheck(env, rc);

    ompi_java_forgetIntArray(env, dims, jDims, cDims);
    ompi_java_forgetBooleanArray(env, periods, jPeriods, cPeriods);
    return newrank;
}

JNIEXPORT jint JNICALL Java_mpi_CartComm_getRank(
        JNIEnv *env, jobject jthis, jlong comm, jintArray coords)
{
    jint *jCoords;
    int  *cCoords;
    ompi_java_getIntArray(env, coords, &jCoords, &cCoords);

    int rank;
    int rc = MPI_Cart_rank((MPI_Comm)comm, cCoords, &rank);
    ompi_java_exceptionCheck(env, rc);

    ompi_java_forgetIntArray(env, coords, jCoords, cCoords);
    return rank;
}

JNIEXPORT jlong JNICALL Java_mpi_CartComm_sub(
        JNIEnv *env, jobject jthis, jlong comm, jbooleanArray remainDims)
{
    jboolean *jRemainDims;
    int      *cRemainDims;
    ompi_java_getBooleanArray(env, remainDims, &jRemainDims, &cRemainDims);

    MPI_Comm newcomm;
    int rc = MPI_Cart_sub((MPI_Comm)comm, cRemainDims, &newcomm);
    ompi_java_exceptionCheck(env, rc);

    ompi_java_forgetBooleanArray(env, remainDims, jRemainDims, cRemainDims);
    return (jlong)newcomm;
}

JNIEXPORT void JNICALL Java_mpi_CartComm_createDims_1jni(
        JNIEnv *env, jclass jthis, jint nNodes, jintArray dims)
{
    int   nDims = (*env)->GetArrayLength(env, dims);
    jint *jDims;
    int  *cDims;
    ompi_java_getIntArray(env, dims, &jDims, &cDims);

    int rc = MPI_Dims_create(nNodes, nDims, cDims);
    ompi_java_exceptionCheck(env, rc);
    ompi_java_releaseIntArray(env, dims, jDims, cDims);
}
