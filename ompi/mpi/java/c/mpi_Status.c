/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/*
 * This file is almost a complete re-write for Open MPI compared to the
 * original mpiJava package. Its license and copyright are listed below.
 * See <path to ompi/mpi/java/README> for more information.
 */
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
 * File         : mpi_Status.c
 * Headerfile   : mpi_Status.h
 * Author       : Sung-Hoon Ko, Xinying Li
 * Created      : Thu Apr  9 12:22:15 1998
 * Revision     : $Revision: 1.9 $
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
#include "mpi_Status.h"
#include "mpiJava.h"

static void getStatus(MPI_Status *status, jint source, jint tag,
                      jint error, jint cancelled, jlong ucount)
{
    /* Copy the whole thing to C */
    status->MPI_SOURCE = source;
    status->MPI_TAG    = tag;
    status->MPI_ERROR  = error;
    status->_cancelled = cancelled;
    status->_ucount    = ucount;
}

JNIEXPORT void JNICALL Java_mpi_Status_init(JNIEnv *env, jclass clazz)
{
    ompi_java.StatusData = (*env)->GetFieldID(env, clazz, "data", "[J");
}

JNIEXPORT jint JNICALL Java_mpi_Status_getCount(
        JNIEnv *env, jobject jthis, jint source, jint tag,
        jint error, jint cancelled, jlong ucount, jlong jType)
{
    int count;
    MPI_Status stat;
    getStatus(&stat, source, tag, error, cancelled, ucount);
    MPI_Datatype datatype = (MPI_Datatype)jType;
    int rc = MPI_Get_count(&stat, datatype, &count);
    ompi_java_exceptionCheck(env, rc);
    return count;
}

JNIEXPORT jboolean JNICALL Java_mpi_Status_isCancelled(
        JNIEnv *env, jobject jthis, jint source, jint tag,
        jint error, jint cancelled, jlong ucount)
{
    int flag;
    MPI_Status stat;
    getStatus(&stat, source, tag, error, cancelled, ucount);
    int rc = MPI_Test_cancelled(&stat, &flag);
    ompi_java_exceptionCheck(env, rc);
    return flag==0 ? JNI_FALSE : JNI_TRUE;
}

JNIEXPORT jint JNICALL Java_mpi_Status_getElements(
        JNIEnv *env, jobject jthis, jint source, jint tag,
        jint error, jint cancelled, jlong ucount, jlong jType)
{
    int count;
    MPI_Status stat;
    getStatus(&stat, source, tag, error, cancelled, ucount);
    MPI_Datatype datatype = (MPI_Datatype)jType;
    int rc = MPI_Get_elements(&stat, datatype, &count);
    ompi_java_exceptionCheck(env, rc);
    return count;
}

JNIEXPORT jobject JNICALL Java_mpi_Status_getElementsX(
        JNIEnv *env, jobject jthis, jint source, jint tag,
        jint error, jint cancelled, jlong ucount, jlong jType)
{
    MPI_Count count;
    MPI_Status stat;
    getStatus(&stat, source, tag, error, cancelled, ucount);
    MPI_Datatype datatype = (MPI_Datatype)jType;
    int rc = MPI_Get_elements_x(&stat, datatype, &count);
    ompi_java_exceptionCheck(env, rc);

	return (*env)->NewObject(env, ompi_java.CountClass,
				ompi_java.CountInit, (jlong)count);
}

JNIEXPORT jint JNICALL Java_mpi_Status_setElements(
        JNIEnv *env, jobject jthis, jint source, jint tag,
        jint error, jint cancelled, jlong ucount, jlong jType, int count)
{
    MPI_Status stat;
    getStatus(&stat, source, tag, error, cancelled, ucount);
    MPI_Datatype datatype = (MPI_Datatype)jType;
    int rc = MPI_Status_set_elements(&stat, datatype, count);
    ompi_java_exceptionCheck(env, rc);
    return stat._ucount;
}

JNIEXPORT jlong JNICALL Java_mpi_Status_setElementsX(
        JNIEnv *env, jobject jthis, jint source, jint tag,
        jint error, jint cancelled, jlong ucount, jlong jType, jlong jcount)
{
    MPI_Status stat;
    MPI_Count count = (long)jcount;
    getStatus(&stat, source, tag, error, cancelled, ucount);
    MPI_Datatype datatype = (MPI_Datatype)jType;
    int rc = MPI_Status_set_elements_x(&stat, datatype, count);
    ompi_java_exceptionCheck(env, rc);
    return (jlong)stat._ucount;
}

JNIEXPORT void JNICALL Java_mpi_Status_setCancelled(
        JNIEnv *env, jobject jthis, jint source, jint tag,
        jint error, jint cancelled, jlong ucount, int flag)
{
    MPI_Status stat;
    getStatus(&stat, source, tag, error, cancelled, ucount);
    int rc = MPI_Status_set_cancelled(&stat, flag);
    ompi_java_exceptionCheck(env, rc);
}

jobject ompi_java_status_new(JNIEnv *env, MPI_Status *status)
{
    jlongArray jData = (*env)->NewLongArray(env, 6);
    ompi_java_status_set(env, jData, status);
    jobject jStatus = (*env)->AllocObject(env, ompi_java.StatusClass);
    (*env)->SetObjectField(env, jStatus, ompi_java.StatusData, jData);
    return jStatus;
}

jobject ompi_java_status_newIndex(JNIEnv *env, MPI_Status *status, int index)
{
    jlongArray jData = (*env)->NewLongArray(env, 6);
    ompi_java_status_setIndex(env, jData, status, index);
    jobject jStatus = (*env)->AllocObject(env, ompi_java.StatusClass);
    (*env)->SetObjectField(env, jStatus, ompi_java.StatusData, jData);
    return jStatus;
}

void ompi_java_status_set(JNIEnv *env, jlongArray jData, MPI_Status *status)
{
    /* Copy the whole thing to Java */
    int i = 0;
    jlong *data = (*env)->GetPrimitiveArrayCritical(env, jData, NULL);
    data[i++] = status->MPI_SOURCE;
    data[i++] = status->MPI_TAG;
    data[i++] = status->MPI_ERROR;
    data[i++] = status->_cancelled;
    data[i++] = status->_ucount;
    (*env)->ReleasePrimitiveArrayCritical(env, jData, data, 0);
}

void ompi_java_status_setIndex(
        JNIEnv *env, jlongArray jData, MPI_Status *status, int index)
{
    /* Copy the whole thing to Java */
    int i = 0;
    jlong *data = (*env)->GetPrimitiveArrayCritical(env, jData, NULL);
    data[i++] = status->MPI_SOURCE;
    data[i++] = status->MPI_TAG;
    data[i++] = status->MPI_ERROR;
    data[i++] = status->_cancelled;
    data[i++] = status->_ucount;
    data[i++] = index;
    (*env)->ReleasePrimitiveArrayCritical(env, jData, data, 0);
}
