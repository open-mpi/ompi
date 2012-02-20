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


/*jmethodID handleConstructorID ;*/

/* jclass status_class ; */

/*
 * Class:     mpi_Status
 * Method:    init
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_mpi_Status_init(JNIEnv *env, jclass jthis)
{
    ompi_java.stathandleID = (*env)->GetFieldID(env,jthis,"handle","J");

    ompi_java.sourceID     = (*env)->GetFieldID(env,jthis,"source","I");
    ompi_java.tagID        = (*env)->GetFieldID(env,jthis,"tag","I");
    ompi_java.indexID      = (*env)->GetFieldID(env,jthis,"index","I");
    ompi_java.elementsID   = (*env)->GetFieldID(env,jthis,"elements","I");

    /* handleConstructorID = (*env)->GetMethodID(env, jthis, "<init>", "()V");*/

    /* status_class = (*env)->NewGlobalRef(env, jthis) ; */
}

/*
 * Class:     mpi_Status
 * Method:    alloc
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_mpi_Status_alloc(JNIEnv *env, jobject jthis)
{
    MPI_Status *status = (MPI_Status*) malloc(sizeof(MPI_Status));

    (*env)->SetLongField(env, jthis, ompi_java.stathandleID, (jlong)status);
}

/*
 * Class:     mpi_Status
 * Method:    free
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_mpi_Status_free(JNIEnv *env, jobject jthis)
{
    MPI_Status *status =
        (MPI_Status *)((*env)->GetLongField(env,jthis,ompi_java.stathandleID));
    free(status) ;
}

/*
 * Class:     mpi_Status
 * Method:    get_count
 * Signature: (Lmpi/Datatype;)I
 */
JNIEXPORT jint JNICALL Java_mpi_Status_get_1count(JNIEnv *env, jobject jthis,
                                                  jobject type)
{
    int count;

    MPI_Datatype datatype =
        (MPI_Datatype)((*env)->GetLongField(env,type,ompi_java.DatatypehandleID)) ;

    MPI_Status *stat =
        (MPI_Status*)((*env)->GetLongField(env,jthis,ompi_java.stathandleID));

#ifdef GC_DOES_PINNING

    ompi_java_clearFreeList(env) ;

    MPI_Get_count(stat, datatype, &count) ;
    return count;

#else

    int elements = (*env)->GetIntField(env, jthis, ompi_java.elementsID) ;

    int dt_size;

    ompi_java_clearFreeList(env) ;

    MPI_Type_size(datatype, &dt_size) ;

    if (elements != -1) {
        count = elements / dt_size ;  /* Cached at start of send buffer. */

        if (count * dt_size == elements) {
            return count ;
        } else {
            return MPI_UNDEFINED;
        }
    }
    else {
        /* Status object returned by IPROBE or PROBE.
         *
         * Didn't have access to data buffer to find `elements' value,
         * so only way to find `count' is to invert `MPI_PACK_SIZE'.
         */

        int bsize, bsizeTrial ;
        MPI_Get_count(stat, MPI_BYTE, &bsize) ;

        bsize -= sizeof(int) ;

        count = bsize / dt_size ;
        MPI_Pack_size(count, datatype, MPI_COMM_WORLD, &bsizeTrial) ;
        /* Strictly, we should use the communicator the message was
         * received on, but I'm too lazy to cache it.
         */

        while(bsizeTrial > bsize) {
            count-- ;
            MPI_Pack_size(count, datatype, MPI_COMM_WORLD, &bsizeTrial) ;
        }

        if (bsizeTrial == bsize) {
            return count ;
        } else {
            return MPI_UNDEFINED;
        }
    }

#endif  /* GC_DOES_PINNING */
}

/*
 * Class:     mpi_Status
 * Method:    Test_cancelled
 * Signature: ()Z
 */
JNIEXPORT jboolean JNICALL Java_mpi_Status_Test_1cancelled(JNIEnv *env, jobject jthis)
{
    int flag;
    MPI_Status *stat;  /*shko*/

    ompi_java_clearFreeList(env) ;

    stat=(MPI_Status *)((*env)->GetLongField(env,jthis,ompi_java.stathandleID));/*shko*/

    MPI_Test_cancelled(stat, &flag);
    if (flag==0) {
        return JNI_FALSE;
    } else {
        return JNI_TRUE;
    }
}

/*
 * Class:     mpi_Status
 * Method:    get_elements
 * Signature: (Lmpi/Datatype;)I
 */
JNIEXPORT jint JNICALL Java_mpi_Status_get_1elements(JNIEnv *env,
                                                     jobject jthis, jobject type)
{
    int count;

    MPI_Datatype datatype =
        (MPI_Datatype)((*env)->GetLongField(env,type,ompi_java.DatatypehandleID)) ;

    MPI_Status *stat =
        (MPI_Status*)((*env)->GetLongField(env,jthis,ompi_java.stathandleID));

#ifdef GC_DOES_PINNING

    ompi_java_clearFreeList(env) ;

    MPI_Get_elements(stat, datatype, &count) ;
    return count;

#else

    int elements = (*env)->GetIntField(env, jthis, ompi_java.elementsID) ;
    int baseType = (*env)->GetIntField(env, type, ompi_java.DatatypebaseTypeID) ;
 
    int dt_size = ompi_java.dt_sizes[baseType] ;
 
    ompi_java_clearFreeList(env) ;

    if(elements != -1) {
        count = elements / dt_size ;
 
        if(count * dt_size == elements)
            return count ;
        else
            return MPI_UNDEFINED ;
        /* Can only happen if illegal base type mismatch between
         * sender and receiver?
         */
    }
    else {
        /* Status object returned by IPROBE or PROBE.
         * 
         * Didn't have access to data buffer to find `elements' value,
         * so only way to find `count' is to invert `MPI_PACK_SIZE'.
         */
 
        int bsize, bsizeTrial ;
        MPI_Get_count(stat, MPI_BYTE, &bsize) ;
 
        bsize -= sizeof(int) ;
 
        count = bsize / dt_size ;
        MPI_Pack_size(count, datatype, MPI_COMM_WORLD, &bsizeTrial) ;
        /* Strictly, we should use the communicator the message was
         * received on, but I'm too lazy to cache it.
         */
 
        while(bsizeTrial > bsize) {
            count-- ;
            MPI_Pack_size(count, datatype, MPI_COMM_WORLD, &bsizeTrial) ;
        }
 
        if(bsizeTrial == bsize) 
            return count ;
        else
            return MPI_UNDEFINED ;
        /* Can only happen if illegal base type mismatch between
         * sender and receiver?
         */
    }

#endif /* GC_DOES_PINNING */
}

