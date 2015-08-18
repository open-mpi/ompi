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
 * File         : mpi_Intercomm.c
 * Headerfile   : mpi_Intercomm.h
 * Author       : Xinying Li
 * Created      : Thu Apr  9 12:22:15 1998
 * Revision     : $Revision: 1.3 $
 * Updated      : $Date: 2003/01/16 16:39:34 $
 * Copyright: Northeast Parallel Architectures Center
 *            at Syracuse University 1998
 */

#include "ompi_config.h"

#ifdef HAVE_TARGETCONDITIONALS_H
#include <TargetConditionals.h>
#endif

#include "mpi.h"
#include "mpi_Intercomm.h"
#include "mpiJava.h"


/*
 * Class:     mpi_Intercomm
 * Method:    getRemoteSize_jni
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_mpi_Intercomm_getRemoteSize_1jni(
                       JNIEnv *env, jobject jthis)
{
    int size, rc;

    rc = MPI_Comm_remote_size(
         (MPI_Comm)((*env)->GetLongField(env,jthis,ompi_java.CommHandle)),
         &size);

    ompi_java_exceptionCheck(env, rc);
    return size;
}

/*
 * Class:     mpi_Intercomm
 * Method:    getRemoteGroup_jni
 * Signature: ()J
 */
JNIEXPORT jlong JNICALL Java_mpi_Intercomm_getRemoteGroup_1jni(
                        JNIEnv *env, jobject jthis)
{
    MPI_Group group;

    int rc = MPI_Comm_remote_group(
             (MPI_Comm)((*env)->GetLongField(env,jthis,ompi_java.CommHandle)),
             &group);

    ompi_java_exceptionCheck(env, rc);
    return (jlong)group;
}

/*
 * Class:     mpi_Intercomm
 * Method:    merge_jni
 * Signature: (Z)Lmpi/Intracomm;
 */
JNIEXPORT jlong JNICALL Java_mpi_Intercomm_merge_1jni(
                        JNIEnv *env, jobject jthis, jboolean high)
{
    MPI_Comm newintracomm;

    int rc = MPI_Intercomm_merge(
             (MPI_Comm)((*env)->GetLongField(env,jthis,ompi_java.CommHandle)),
             high, &newintracomm);

    ompi_java_exceptionCheck(env, rc);
    return (jlong)newintracomm;
}

/*
 * Class:     mpi_Intercomm
 * Method:    getParent_jni
 * Signature: ()J
 */
JNIEXPORT jlong JNICALL Java_mpi_Intercomm_getParent_1jni(
                        JNIEnv *env, jclass clazz)
{
    MPI_Comm parent;
    int rc = MPI_Comm_get_parent(&parent);
    ompi_java_exceptionCheck(env, rc);
    return (jlong)parent;
}
