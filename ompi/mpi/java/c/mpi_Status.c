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

JNIEXPORT void JNICALL Java_mpi_Status_init(JNIEnv *env, jclass c)
{
    ompi_java.StSource     = (*env)->GetFieldID(env, c, "source",     "I");
    ompi_java.StTag        = (*env)->GetFieldID(env, c, "tag",        "I");
    ompi_java.StError      = (*env)->GetFieldID(env, c, "error",      "I");
    ompi_java.St_cancelled = (*env)->GetFieldID(env, c, "_cancelled", "I");
    ompi_java.St_ucount    = (*env)->GetFieldID(env, c, "_ucount",    "J");
    ompi_java.StIndex      = (*env)->GetFieldID(env, c, "index",      "I");
}

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

void ompi_java_status_set(MPI_Status *status, JNIEnv *env, jobject obj)
{
    /* Copy the whole thing to Java */
    (*env)->SetIntField(env, obj, ompi_java.StSource, status->MPI_SOURCE);
    (*env)->SetIntField(env, obj, ompi_java.StTag,    status->MPI_TAG);
    (*env)->SetIntField(env, obj, ompi_java.StError,  status->MPI_ERROR);
    (*env)->SetIntField(env, obj, ompi_java.St_cancelled, status->_cancelled);
    (*env)->SetLongField(env, obj, ompi_java.St_ucount,   status->_ucount);
}
