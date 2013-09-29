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
 * File         : mpi_Comm.c
 * Headerfile   : mpi_Comm.h
 * Author       : Sung-Hoon Ko, Xinying Li, Sang Lim, Bryan Carpenter
 * Created      : Thu Apr  9 12:22:15 1998
 * Revision     : $Revision: 1.17 $
 * Updated      : $Date: 2003/01/16 16:39:34 $
 * Copyright: Northeast Parallel Architectures Center
 *            at Syracuse University 1998
 */
#include "ompi_config.h"
#include <stdlib.h>
#include <assert.h>
#ifdef HAVE_TARGETCONDITIONALS_H
#include <TargetConditionals.h>
#endif

#include "mpi.h"
#include "mpi_Comm.h"
#include "mpiJava.h"  /* must come AFTER the related .h so JNI is included */

static void* getArrayPtr(void** bufBase, JNIEnv *env,
                         jobject buf, int baseType, int offset)
{
    switch(baseType)
    {
        case 0:   /* NULL */
            *bufBase = NULL;
            return NULL;

        case 1: {
            jbyte* els = (*env)->GetByteArrayElements(env, buf, NULL);
            *bufBase = els;
            return els + offset;
        }
        case 2: {
            jchar* els = (*env)->GetCharArrayElements(env, buf, NULL);
            *bufBase = els;
            return els + offset;
        }
        case 3: {
            jshort* els = (*env)->GetShortArrayElements(env, buf, NULL);
            *bufBase = els;
            return els + offset;
        }
        case 4: {
            jboolean* els = (*env)->GetBooleanArrayElements(env, buf, NULL);
            *bufBase = els;
            return els + offset;
        }
        case 5: {
            jint* els = (*env)->GetIntArrayElements(env, buf, NULL);
            *bufBase = els;
            return els + offset;
        }
        case 6: {
            jlong* els = (*env)->GetLongArrayElements(env, buf, NULL);
            *bufBase = els;
            return els + offset;
        }
        case 7: {
            jfloat* els = (*env)->GetFloatArrayElements(env, buf, NULL);
            *bufBase = els;
            return els + offset;
        }
        case 8: {
            jdouble* els = (*env)->GetDoubleArrayElements(env, buf, NULL);
            *bufBase = els;
            return els + offset;
        }
        case 9: {
            jbyte* els = (*env)->GetByteArrayElements(env, buf, NULL);
            *bufBase = els;
            return els + offset;
        }
        default:
            *bufBase = NULL;
            return NULL;  /* 'UNDEFINED' */
    }
}

static void releaseArrayPtr(JNIEnv *e, jobject buf, void *bufBase,
                            int baseType, jint mode)
{
    switch(baseType)
    {
        case 0:
            break;
        case 1:
            (*e)->ReleaseByteArrayElements(e, buf, (jbyte*)bufBase, mode);
            break;
        case 2:
            (*e)->ReleaseCharArrayElements(e, buf, (jchar*)bufBase, mode);
            break;
        case 3:
            (*e)->ReleaseShortArrayElements(e, buf, (jshort*)bufBase, mode);
            break;
        case 4:
            (*e)->ReleaseBooleanArrayElements(e, buf, (jboolean*)bufBase, mode);
            break;
        case 5:
            (*e)->ReleaseIntArrayElements(e, buf, (jint*)bufBase, mode);
            break;
        case 6:
            (*e)->ReleaseLongArrayElements(e, buf, (jlong*)bufBase, mode);
            break;
        case 7:
            (*e)->ReleaseFloatArrayElements(e, buf, (jfloat*)bufBase, mode);
            break;
        case 8:
            (*e)->ReleaseDoubleArrayElements(e, buf, (jdouble*)bufBase, mode);
            break;
        case 9:
            (*e)->ReleaseByteArrayElements(e, buf, (jbyte*)bufBase, mode);
            break;
        default:
            break;
    }
}

void* ompi_java_getBufPtr(void** bufBase, JNIEnv *env, jobject buf,
                          int baseType, int offset)
{
    if(buf == NULL)
    {
        /* Allow NULL buffers to send/recv 0 items as control messages. */
        *bufBase = NULL;
        return NULL;
    }
    else
    {
        *bufBase = (*env)->GetDirectBufferAddress(env, buf);

        if(*bufBase != NULL)
        {
            assert(offset == 0);
            return *bufBase;
        }
        else
        {
            return getArrayPtr(bufBase, env, buf, baseType, offset);
        }
    }
}

void ompi_java_releaseBufPtr(JNIEnv *env, jobject buf,
                             void* bufBase, int baseType)
{
    if(buf != NULL && (*env)->GetDirectBufferAddress(env, buf) == NULL)
        releaseArrayPtr(env, buf, bufBase, baseType, 0);
}

void ompi_java_releaseReadBufPtr(
        JNIEnv *env, jobject buf, void *bufBase, int baseType)
{
    if(buf != NULL && (*env)->GetDirectBufferAddress(env, buf) == NULL)
        releaseArrayPtr(env, buf, bufBase, baseType, JNI_ABORT);
}

void* ompi_java_getDirectBufferAddress(JNIEnv *env, jobject buf)
{
    /* Allow NULL buffers to send/recv 0 items as control messages. */
    return buf == NULL ? NULL : (*env)->GetDirectBufferAddress(env, buf);
}

/* `getArrayCritical' is used in
     getMPIBuf, releaseMPIBuf...
   for getting pointer from `jobject buf'
*/
static void* getArrayCritical(void** bufBase, JNIEnv *env,
                              jobject buf, int baseType, int offset)
{
    *bufBase = (jbyte*)(*env)->GetPrimitiveArrayCritical(env, buf, NULL);

    switch(baseType)
    {
        case 0:
            return NULL;
        case 1:
            return ((jbyte*)*bufBase) + offset;
        case 2:
            return ((jchar*)*bufBase) + offset;
        case 3:
            return ((jshort*)*bufBase) + offset;
        case 4:
            return ((jboolean*)*bufBase) + offset;
        case 5:
            return ((jint*)*bufBase) + offset;
        case 6:
            return ((jlong*)*bufBase) + offset;
        case 7:
            return ((jfloat*)*bufBase) + offset;
        case 8:
            return ((jdouble*)*bufBase) + offset;
        case 9:
            return ((jbyte*)*bufBase) + offset;
        default:
            return NULL;
    }
}

static void* getBufCritical(void** bufBase, JNIEnv *env,
                            jobject buf, int baseType, int offset)
{
    *bufBase = (*env)->GetDirectBufferAddress(env, buf);

    if(*bufBase != NULL)
    {
        assert(offset == 0);
        return *bufBase;
    }
    else
    {
        return getArrayCritical(bufBase, env, buf, baseType, offset);
    }
}

static void releaseBufCritical(JNIEnv *env, jobject buf, void* bufBase)
{
    if((*env)->GetDirectBufferAddress(env, buf) == NULL)
        (*env)->ReleasePrimitiveArrayCritical(env, buf, bufBase, 0);
}

static int isInter(JNIEnv *env, MPI_Comm comm)
{
    int rc, flag;
    rc = MPI_Comm_test_inter(comm, &flag);
    ompi_java_exceptionCheck(env, rc);
    return flag;
}

JNIEXPORT void JNICALL Java_mpi_Comm_init(JNIEnv *env, jclass clazz)
{
    jfieldID nullHandleID = (*env)->GetStaticFieldID(
                            env, clazz, "nullHandle", "J");

    (*env)->SetStaticLongField(env, clazz, nullHandleID, (jlong)MPI_COMM_NULL);
    ompi_java.CommHandle = (*env)->GetFieldID(env,clazz,"handle","J");
}

JNIEXPORT void JNICALL Java_mpi_Comm_getComm(JNIEnv *env, jobject jthis,
                                             jint type)
{
    switch (type) {
    case 0:
        (*env)->SetLongField(env,jthis, ompi_java.CommHandle,(jlong)MPI_COMM_NULL);
        break;
    case 1:
        (*env)->SetLongField(env,jthis, ompi_java.CommHandle,(jlong)MPI_COMM_SELF);
        break;
    case 2:
        (*env)->SetLongField(env,jthis, ompi_java.CommHandle,(jlong)MPI_COMM_WORLD);
        break;
    }
}

JNIEXPORT jlong JNICALL Java_mpi_Comm_dup(JNIEnv *env, jobject jthis)
{
    MPI_Comm comm, newcomm;
    comm = (MPI_Comm)(*env)->GetLongField(env, jthis, ompi_java.CommHandle);
    int rc = MPI_Comm_dup(comm, &newcomm);
    ompi_java_exceptionCheck(env, rc);
    return (jlong)newcomm;
}

JNIEXPORT jint JNICALL Java_mpi_Comm_getSize(
        JNIEnv *env, jobject jthis, jlong comm)
{
    int rc, size;
    rc = MPI_Comm_size((MPI_Comm)comm, &size);
    ompi_java_exceptionCheck(env, rc);
    return size;
}

JNIEXPORT jint JNICALL Java_mpi_Comm_getRank(
        JNIEnv *env, jobject jthis, jlong comm)
{
    int rc, rank;
    rc = MPI_Comm_rank((MPI_Comm)comm, &rank);
    ompi_java_exceptionCheck(env, rc);
    return rank;
}

JNIEXPORT jint JNICALL Java_mpi_Comm_compare(
        JNIEnv *env, jclass jthis, jlong comm1, jlong comm2)
{
    int rc, result;
    rc = MPI_Comm_compare((MPI_Comm)comm1, (MPI_Comm)comm2, &result);
    ompi_java_exceptionCheck(env, rc);
    return result;
}

JNIEXPORT jlong JNICALL Java_mpi_Comm_free(
        JNIEnv *env, jobject jthis, jlong handle)
{
    MPI_Comm comm = (MPI_Comm)handle;
    int rc = MPI_Comm_free(&comm);
    ompi_java_exceptionCheck(env, rc);
    return (jlong)comm;
}

JNIEXPORT jlong JNICALL Java_mpi_Comm_disconnect(
        JNIEnv *env, jobject jthis, jlong handle)
{
    MPI_Comm comm = (MPI_Comm)handle;
    int rc = MPI_Comm_disconnect(&comm);
    ompi_java_exceptionCheck(env, rc);
    return (jlong)comm;
}

JNIEXPORT jboolean JNICALL Java_mpi_Comm_isNull(JNIEnv *env, jobject jthis)
{
    MPI_Comm comm = (MPI_Comm)((*env)->GetLongField(
                    env, jthis, ompi_java.CommHandle));

    return comm == MPI_COMM_NULL ? JNI_TRUE : JNI_FALSE;
}

JNIEXPORT jlong JNICALL Java_mpi_Comm_getGroup(
        JNIEnv *env, jobject jthis, jlong comm)
{
    MPI_Group group;
    int rc = MPI_Comm_group((MPI_Comm)comm, &group);
    ompi_java_exceptionCheck(env, rc);
    return (jlong)group;
}

JNIEXPORT jboolean JNICALL Java_mpi_Comm_isInter(
        JNIEnv *env, jobject jthis, jlong comm)
{
    return isInter(env, (MPI_Comm)comm) ? JNI_TRUE : JNI_FALSE;
}

JNIEXPORT jlong JNICALL Java_mpi_Comm_createIntercomm(
        JNIEnv *env, jobject jthis, jlong comm, jlong localComm,
        jint localLeader, jint remoteLeader, jint tag)
{
    MPI_Comm newintercomm;

    int rc = MPI_Intercomm_create(
             (MPI_Comm)localComm, localLeader,
             (MPI_Comm)comm, remoteLeader, tag, &newintercomm);

    ompi_java_exceptionCheck(env, rc);
    return (jlong)newintercomm;
}

JNIEXPORT void JNICALL Java_mpi_Comm_send(
        JNIEnv *env, jobject jthis, jobject buf, jint offset,
        jint count, jobject jType, jint dest, jint tag)
{
    MPI_Comm comm = (MPI_Comm)((*env)->GetLongField(
                    env, jthis, ompi_java.CommHandle));

    MPI_Datatype type = (MPI_Datatype)((*env)->GetLongField(
                        env, jType, ompi_java.DatatypeHandle));

    int baseType = (*env)->GetIntField(env, jType, ompi_java.DatatypeBaseType);
    void *bufPtr, *bufBase;
    bufPtr = ompi_java_getBufPtr(&bufBase, env, buf, baseType, offset);
    int rc = MPI_Send(bufPtr, count, type, dest, tag, comm);
    ompi_java_exceptionCheck(env, rc);
    ompi_java_releaseReadBufPtr(env, buf, bufBase, baseType);
}

JNIEXPORT void JNICALL Java_mpi_Comm_recv(
        JNIEnv *env, jobject jthis,
        jobject buf, jint offset, jint count, jobject jType,
        jint source, jint tag, jobject jStatus)
{
    MPI_Comm comm = (MPI_Comm)((*env)->GetLongField(
                    env, jthis, ompi_java.CommHandle));

    MPI_Datatype type = (MPI_Datatype)((*env)->GetLongField(
                        env, jType, ompi_java.DatatypeHandle));

    int baseType = (*env)->GetIntField(env,
                   jType, ompi_java.DatatypeBaseType);

    void *bufPtr, *bufBase;
    bufPtr = ompi_java_getBufPtr(&bufBase, env, buf, baseType, offset);

    MPI_Status status;
    int rc = MPI_Recv(bufPtr, count, type, source, tag, comm, &status);
    ompi_java_exceptionCheck(env, rc);
    ompi_java_releaseBufPtr(env, buf, bufBase, baseType);
    ompi_java_status_set(&status, env, jStatus);
}

JNIEXPORT void JNICALL Java_mpi_Comm_sendRecv(
        JNIEnv *env, jobject jthis,
        jobject sBuf, jint sOffset, jint sCount,
        jobject sjType, jint dest, jint sTag,
        jobject rBuf, jint rOffset, jint rCount,
        jobject rjType, jint source, jint rTag, jobject jStatus)
{
    MPI_Comm comm = (MPI_Comm)((*env)->GetLongField(
                    env, jthis, ompi_java.CommHandle));

    MPI_Datatype sType = (MPI_Datatype)((*env)->GetLongField(
                         env, sjType, ompi_java.DatatypeHandle));
    MPI_Datatype rType = (MPI_Datatype)((*env)->GetLongField(
                         env, rjType, ompi_java.DatatypeHandle));

    int sBaseType = (*env)->GetIntField(env,
                    sjType, ompi_java.DatatypeBaseType);
    int rBaseType = (*env)->GetIntField(env,
                    rjType, ompi_java.DatatypeBaseType);

    void *sBufPtr, *sBufBase,
         *rBufPtr, *rBufBase;

    sBufPtr = ompi_java_getBufPtr(&sBufBase, env, sBuf, sBaseType, sOffset);
    rBufPtr = ompi_java_getBufPtr(&rBufBase, env, rBuf, rBaseType, rOffset);
    MPI_Status status;

    int rc = MPI_Sendrecv(sBufPtr, sCount, sType, dest, sTag,
                          rBufPtr, rCount, rType, source, rTag,
                          comm, &status);

    ompi_java_exceptionCheck(env, rc);
    ompi_java_releaseReadBufPtr(env, sBuf, sBufBase, sBaseType);
    ompi_java_releaseBufPtr(env, rBuf, rBufBase, rBaseType);
    ompi_java_status_set(&status, env, jStatus);
}

JNIEXPORT void JNICALL Java_mpi_Comm_sendRecvReplace(
        JNIEnv *env, jobject jthis,
        jobject buf, jint offset, jint count, jobject jType,
        jint dest, jint sTag, jint source, jint rTag, jobject jStatus)
{
    MPI_Comm comm = (MPI_Comm)((*env)->GetLongField(
                    env, jthis, ompi_java.CommHandle));

    MPI_Datatype type = (MPI_Datatype)((*env)->GetLongField(
                        env, jType, ompi_java.DatatypeHandle));

    int baseType = (*env)->GetIntField(env,
                   jType, ompi_java.DatatypeBaseType);

    MPI_Status status;
    void *bufPtr, *bufBase;
    bufPtr = ompi_java_getBufPtr(&bufBase, env, buf, baseType, offset);

    int rc = MPI_Sendrecv_replace(bufPtr, count, type, dest,
                                  sTag, source, rTag, comm, &status);

    ompi_java_exceptionCheck(env, rc);
    ompi_java_releaseBufPtr(env, buf, bufBase, baseType);
    ompi_java_status_set(&status, env, jStatus);
}

JNIEXPORT void JNICALL Java_mpi_Comm_bSend(
        JNIEnv *env, jobject jthis, jobject buf, jint offset,
        jint count, jobject jType, jint dest, jint tag)
{
    MPI_Comm comm = (MPI_Comm)((*env)->GetLongField(
                    env, jthis, ompi_java.CommHandle));

    MPI_Datatype type = (MPI_Datatype)((*env)->GetLongField(
                        env, jType, ompi_java.DatatypeHandle));

    int baseType = (*env)->GetIntField(env,
                   jType, ompi_java.DatatypeBaseType);

    void *bufPtr, *bufBase;
    bufPtr = ompi_java_getBufPtr(&bufBase, env, buf, baseType, offset);

    int rc = MPI_Bsend(bufPtr, count, type, dest, tag, comm);
    ompi_java_exceptionCheck(env, rc);
    ompi_java_releaseReadBufPtr(env, buf, bufBase, baseType);
}

JNIEXPORT void JNICALL Java_mpi_Comm_sSend(
                       JNIEnv *env, jobject jthis,
                       jobject buf, jint offset, jint count,
                       jobject jType, jint dest, jint tag)
{
    MPI_Comm comm = (MPI_Comm)((*env)->GetLongField(
                    env, jthis, ompi_java.CommHandle));

    MPI_Datatype type = (MPI_Datatype)((*env)->GetLongField(
                        env, jType, ompi_java.DatatypeHandle));

    int baseType = (*env)->GetIntField(env,
                   jType, ompi_java.DatatypeBaseType);

    void *bufPtr, *bufBase;
    bufPtr = ompi_java_getBufPtr(&bufBase, env, buf, baseType, offset);

    int rc = MPI_Ssend(bufPtr, count, type, dest, tag, comm);
    ompi_java_exceptionCheck(env, rc);
    ompi_java_releaseReadBufPtr(env, buf, bufBase, baseType);
}

JNIEXPORT void JNICALL Java_mpi_Comm_rSend(
                       JNIEnv *env, jobject jthis,
                       jobject buf, jint offset, jint count,
                       jobject jType, jint dest, jint tag)
{
    MPI_Comm comm = (MPI_Comm)((*env)->GetLongField(
                    env, jthis, ompi_java.CommHandle));

    MPI_Datatype type = (MPI_Datatype)((*env)->GetLongField(
                        env, jType, ompi_java.DatatypeHandle));

    int baseType = (*env)->GetIntField(env,
                   jType, ompi_java.DatatypeBaseType);

    void *bufPtr, *bufBase;
    bufPtr = ompi_java_getBufPtr(&bufBase, env, buf, baseType, offset);

    int rc = MPI_Rsend(bufPtr, count, type, dest, tag, comm);
    ompi_java_exceptionCheck(env, rc);
    ompi_java_releaseReadBufPtr(env, buf, bufBase, baseType);
}

JNIEXPORT jlong JNICALL Java_mpi_Comm_iSend(
        JNIEnv *env, jobject jthis, jlong comm,
        jobject buf, jint count, jlong type, jint dest, jint tag)
{
    void *ptr = ompi_java_getDirectBufferAddress(env, buf);
    MPI_Request request;

    int rc = MPI_Isend(ptr, count, (MPI_Datatype)type,
                       dest, tag, (MPI_Comm)comm, &request);

    ompi_java_exceptionCheck(env, rc);
    return (jlong)request;
}

JNIEXPORT jlong JNICALL Java_mpi_Comm_ibSend(
        JNIEnv *env, jobject jthis, jlong comm,
        jobject buf, jint count, jlong type, jint dest, jint tag)
{
    void *ptr = ompi_java_getDirectBufferAddress(env, buf);
    MPI_Request request;

    int rc = MPI_Ibsend(ptr, count, (MPI_Datatype)type,
                        dest, tag, (MPI_Comm)comm, &request);

    ompi_java_exceptionCheck(env, rc);
    return (jlong)request;
}

JNIEXPORT jlong JNICALL Java_mpi_Comm_isSend(
        JNIEnv *env, jobject jthis, jlong comm,
        jobject buf, jint count, jlong type, jint dest, jint tag)
{
    void *ptr = ompi_java_getDirectBufferAddress(env, buf);
    MPI_Request request;

    int rc = MPI_Issend(ptr, count, (MPI_Datatype)type,
                        dest, tag, (MPI_Comm)comm, &request);

    ompi_java_exceptionCheck(env, rc);
    return (jlong)request;
}

JNIEXPORT jlong JNICALL Java_mpi_Comm_irSend(
        JNIEnv *env, jobject jthis, jlong comm,
        jobject buf, jint count, jlong type, jint dest, jint tag)
{
    void *ptr = ompi_java_getDirectBufferAddress(env, buf);
    MPI_Request request;

    int rc = MPI_Irsend(ptr, count, (MPI_Datatype)type,
                        dest, tag, (MPI_Comm)comm, &request);

    ompi_java_exceptionCheck(env, rc);
    return (jlong)request;
}

JNIEXPORT jlong JNICALL Java_mpi_Comm_iRecv(
        JNIEnv *env, jobject jthis, jlong comm,
        jobject buf, jint count, jlong type, jint source, jint tag)
{
    void *ptr = ompi_java_getDirectBufferAddress(env, buf);
    MPI_Request request;

    int rc = MPI_Irecv(ptr, count, (MPI_Datatype)type,
                       source, tag, (MPI_Comm)comm, &request);

    ompi_java_exceptionCheck(env, rc);
    return (jlong)request;
}

JNIEXPORT jlong JNICALL Java_mpi_Comm_sendInit(
        JNIEnv *env, jobject jthis, jlong comm,
        jobject buf, jint count, jlong type, jint dest, jint tag)
{
    void *ptr = ompi_java_getDirectBufferAddress(env, buf);
    MPI_Request request;

    int rc = MPI_Send_init(ptr, count, (MPI_Datatype)type,
                           dest, tag, (MPI_Comm)comm, &request);

    ompi_java_exceptionCheck(env, rc);
    return (jlong)request;
}

JNIEXPORT jlong JNICALL Java_mpi_Comm_bSendInit(
        JNIEnv *env, jobject jthis, jlong comm,
        jobject buf, jint count, jlong type, jint dest, jint tag)
{
    void *ptr = ompi_java_getDirectBufferAddress(env, buf);
    MPI_Request request;

    int rc = MPI_Bsend_init(ptr, count, (MPI_Datatype)type,
                            dest, tag, (MPI_Comm)comm, &request);

    ompi_java_exceptionCheck(env, rc);
    return (jlong)request;
}

JNIEXPORT jlong JNICALL Java_mpi_Comm_sSendInit(
        JNIEnv *env, jobject jthis, jlong comm,
        jobject buf, jint count, jlong type, jint dest, jint tag)
{
    void *ptr = ompi_java_getDirectBufferAddress(env, buf);
    MPI_Request request;

    int rc = MPI_Ssend_init(ptr, count, (MPI_Datatype)type,
                            dest, tag, (MPI_Comm)comm, &request);

    ompi_java_exceptionCheck(env, rc);
    return (jlong)request;
}

JNIEXPORT jlong JNICALL Java_mpi_Comm_rSendInit(
        JNIEnv *env, jobject jthis, jlong comm,
        jobject buf, jint count, jlong type, jint dest, jint tag)
{
    void *ptr = ompi_java_getDirectBufferAddress(env, buf);
    MPI_Request request;

    int rc = MPI_Rsend_init(ptr, count, (MPI_Datatype)type,
                            dest, tag, (MPI_Comm)comm, &request);

    ompi_java_exceptionCheck(env, rc);
    return (jlong)request;
}

JNIEXPORT jlong JNICALL Java_mpi_Comm_recvInit(
        JNIEnv *env, jobject jthis, jlong comm,
        jobject buf, jint count, jlong type, jint source, jint tag)
{
    void *ptr = ompi_java_getDirectBufferAddress(env, buf);
    MPI_Request request;

    int rc = MPI_Recv_init(ptr, count, (MPI_Datatype)type,
                           source, tag, (MPI_Comm)comm, &request);

    ompi_java_exceptionCheck(env, rc);
    return (jlong)request;
}

JNIEXPORT jint JNICALL Java_mpi_Comm_pack(
        JNIEnv *env, jobject jthis, jobject inBuf, jint offset,
        jint inCount, jobject jType, jbyteArray outBuf, jint position)
{
    MPI_Comm comm = (MPI_Comm)((*env)->GetLongField(
                    env, jthis, ompi_java.CommHandle));

    MPI_Datatype type = (MPI_Datatype)((*env)->GetLongField(
                        env, jType, ompi_java.DatatypeHandle));

    int bType   = (*env)->GetIntField(env, jType, ompi_java.DatatypeBaseType),
        outSize = (*env)->GetArrayLength(env,outBuf);

    void *oBufPtr, *iBufPtr, *iBufBase;
    oBufPtr = (*env)->GetPrimitiveArrayCritical(env, outBuf, NULL);
    iBufPtr = getBufCritical(&iBufBase, env, inBuf, bType, offset);

    if(inCount != 0 && outSize != position)
    {
        /* LAM doesn't like count = 0 */
        int rc = MPI_Pack(iBufPtr, inCount, type,
                          oBufPtr, outSize, &position, comm);

        ompi_java_exceptionCheck(env, rc);
    }

    releaseBufCritical(env, inBuf, iBufBase);
    (*env)->ReleasePrimitiveArrayCritical(env, outBuf, oBufPtr, 0);
    return position;
}

JNIEXPORT jint JNICALL Java_mpi_Comm_unpack(
        JNIEnv *env, jobject jthis, jbyteArray inBuf, jint position,
        jobject outBuf, jint offset, jint outCount, jobject jType)
{
    MPI_Comm comm = (MPI_Comm)((*env)->GetLongField(
                    env, jthis, ompi_java.CommHandle));

    MPI_Datatype type = (MPI_Datatype)((*env)->GetLongField(
                        env, jType, ompi_java.DatatypeHandle));

    int bType  = (*env)->GetIntField(env, jType, ompi_java.DatatypeBaseType),
        inSize = (*env)->GetArrayLength(env, inBuf);

    void *iBufPtr, *oBufPtr, *oBufBase;
    iBufPtr = (*env)->GetPrimitiveArrayCritical(env, inBuf, NULL);
    oBufPtr = getBufCritical(&oBufBase, env, outBuf, bType, offset);

    int rc = MPI_Unpack(iBufPtr, inSize, &position,
                        oBufPtr, outCount, type, comm);

    ompi_java_exceptionCheck(env, rc);
    (*env)->ReleasePrimitiveArrayCritical(env, inBuf, iBufPtr, 0);
    releaseBufCritical(env, outBuf, oBufBase);
    return position;
}

JNIEXPORT jint JNICALL Java_mpi_Comm_packSize(
        JNIEnv *env, jobject jthis, jlong comm, jint incount, jlong type)
{
    int rc, size;
    rc = MPI_Pack_size(incount, (MPI_Datatype)type, (MPI_Comm)comm, &size);
    ompi_java_exceptionCheck(env, rc);
    return size;
}

JNIEXPORT jobject JNICALL Java_mpi_Comm_iProbe(
        JNIEnv *env, jobject jthis, jlong comm, jint source, jint tag)
{
    int flag;
    MPI_Status status;
    int rc = MPI_Iprobe(source, tag, (MPI_Comm)comm, &flag, &status);
    ompi_java_exceptionCheck(env, rc);

    if(flag == 0)
        return NULL;

    jobject stat = ompi_java_status_new(&status, env);
    (*env)->SetIntField(env, stat, ompi_java.StElements, -1);
    return stat;
}

JNIEXPORT void JNICALL Java_mpi_Comm_probe(
        JNIEnv *env, jobject jthis, jlong comm,
        jint source, jint tag, jobject stat)
{
    MPI_Status status;
    int rc = MPI_Probe(source, tag, (MPI_Comm)comm, &status);
    ompi_java_exceptionCheck(env, rc);
    ompi_java_status_set(&status, env, stat);
    (*env)->SetIntField(env, stat, ompi_java.StElements, -1);
}

JNIEXPORT jint JNICALL Java_mpi_Comm_getTopology(
        JNIEnv *env, jobject jthis, jlong comm)
{
    int rc, status;
    rc = MPI_Topo_test((MPI_Comm)comm, &status);
    ompi_java_exceptionCheck(env, rc);
    return status;
}

JNIEXPORT void JNICALL Java_mpi_Comm_abort(
        JNIEnv *env, jobject jthis, jlong comm, jint errorcode)
{
    int rc = MPI_Abort((MPI_Comm)comm, errorcode);
    ompi_java_exceptionCheck(env, rc);
}

JNIEXPORT void JNICALL Java_mpi_Comm_setErrhandler(
        JNIEnv *env, jobject jthis, jlong comm, jlong errhandler)
{
    int rc = MPI_Errhandler_set((MPI_Comm)comm, (MPI_Errhandler)errhandler);
    ompi_java_exceptionCheck(env, rc);
}

JNIEXPORT jlong JNICALL Java_mpi_Comm_getErrhandler(
        JNIEnv *env, jobject jthis, jlong comm)
{
    MPI_Errhandler errhandler;
    int rc = MPI_Errhandler_get((MPI_Comm)comm, &errhandler);
    ompi_java_exceptionCheck(env, rc);
    return (jlong)errhandler;
}

static int commCopyAttr(MPI_Comm oldcomm, int keyval, void *extraState,
                        void *attrValIn, void *attrValOut, int *flag)
{
    return ompi_java_attrCopy(attrValIn, attrValOut, flag);
}

static int commDeleteAttr(MPI_Comm oldcomm, int keyval,
                          void *attrVal, void *extraState)
{
    return ompi_java_attrDelete(attrVal);
}

JNIEXPORT jint JNICALL Java_mpi_Comm_createKeyval_1jni(
                       JNIEnv *env, jclass clazz)
{
    int rc, keyval;
    rc = MPI_Comm_create_keyval(commCopyAttr, commDeleteAttr, &keyval, NULL);
    ompi_java_exceptionCheck(env, rc);
    return keyval;
}

JNIEXPORT void JNICALL Java_mpi_Comm_freeKeyval_1jni(
                       JNIEnv *env, jclass clazz, jint keyval)
{
    int rc = MPI_Comm_free_keyval((int*)(&keyval));
    ompi_java_exceptionCheck(env, rc);
}

JNIEXPORT void JNICALL Java_mpi_Comm_setAttr(
        JNIEnv *env, jobject jthis, jlong comm, jint keyval, jbyteArray jval)
{
    void *cval = ompi_java_attrSet(env, jval);
    int rc = MPI_Comm_set_attr((MPI_Comm)comm, keyval, cval);
    ompi_java_exceptionCheck(env, rc);
}

JNIEXPORT jobject JNICALL Java_mpi_Comm_getAttr_1predefined(
        JNIEnv *env, jobject jthis, jlong comm, jint keyval)
{
    int flag, *val;
    int rc = MPI_Comm_get_attr((MPI_Comm)comm, keyval, &val, &flag);

    if(ompi_java_exceptionCheck(env, rc) || !flag)
        return NULL;

    return ompi_java_Integer_valueOf(env, (jint)(*val));
}

JNIEXPORT jbyteArray JNICALL Java_mpi_Comm_getAttr(
        JNIEnv *env, jobject jthis, jlong comm, jint keyval)
{
    int flag;
    void *cval;
    int rc = MPI_Comm_get_attr((MPI_Comm)comm, keyval, &cval, &flag);

    if(ompi_java_exceptionCheck(env, rc) || !flag)
        return NULL;

    return ompi_java_attrGet(env, cval);
}

JNIEXPORT void JNICALL Java_mpi_Comm_deleteAttr(
        JNIEnv *env, jobject jthis, jlong comm, jint keyval)
{
    int rc = MPI_Comm_delete_attr((MPI_Comm)comm, keyval);
    ompi_java_exceptionCheck(env, rc);
}

JNIEXPORT void JNICALL Java_mpi_Comm_barrier(
        JNIEnv *env, jobject jthis, jlong comm)
{
    int rc = MPI_Barrier((MPI_Comm)comm);
    ompi_java_exceptionCheck(env, rc);
}

JNIEXPORT jlong JNICALL Java_mpi_Comm_iBarrier(
        JNIEnv *env, jobject jthis, jlong comm)
{
    MPI_Request request;
    int rc = MPI_Ibarrier((MPI_Comm)comm, &request);
    ompi_java_exceptionCheck(env, rc);
    return (jlong)request;
}

JNIEXPORT void JNICALL Java_mpi_Comm_bcast(
        JNIEnv *env, jobject jthis, jobject buf, jint offset,
        jint count, jobject type, jint root)
{
    MPI_Comm comm = (MPI_Comm)((*env)->GetLongField(
                    env, jthis, ompi_java.CommHandle));

    MPI_Datatype mpiType = (MPI_Datatype)((*env)->GetLongField(
                           env, type, ompi_java.DatatypeHandle));

    void *bufPtr, *bufBase;
    int baseType = (*env)->GetIntField(env, type, ompi_java.DatatypeBaseType);
    bufPtr = ompi_java_getBufPtr(&bufBase, env, buf, baseType, offset);

    int rc = MPI_Bcast(bufPtr, count, mpiType, root, comm);
    ompi_java_exceptionCheck(env, rc);
    ompi_java_releaseBufPtr(env, buf, bufBase, baseType);
}

JNIEXPORT jlong JNICALL Java_mpi_Comm_iBcast(
        JNIEnv *env, jobject jthis, jlong comm,
        jobject buf, jint count, jlong type, jint root)
{
    void *ptr = ompi_java_getDirectBufferAddress(env, buf);
    MPI_Request request;

    int rc = MPI_Ibcast(ptr, count, (MPI_Datatype)type,
                        root, (MPI_Comm)comm, &request);

    ompi_java_exceptionCheck(env, rc);
    return (jlong)request;
}

JNIEXPORT void JNICALL Java_mpi_Comm_gather(
        JNIEnv *env, jobject jthis,
        jobject sendBuf, jint sOffset, jint sCount, jobject sJType,
        jobject recvBuf, jint rOffset, jint rCount, jobject rJType,
        jint root)
{
    MPI_Comm comm = (MPI_Comm)((*env)->GetLongField(
                    env, jthis, ompi_java.CommHandle));
    int id;
    int rc = MPI_Comm_rank(comm, &id);
    int rootOrInter = id == root || isInter(env, comm);

    if(ompi_java_exceptionCheck(env, rc))
        return;

    void *sPtr, *sBase, *rPtr = NULL, *rBase;
    MPI_Datatype sType;
    int sBType, rBType;

    if(sJType == NULL)
    {
        assert(sendBuf == NULL);
        sType  = MPI_DATATYPE_NULL;
        sBType = 0;
        sPtr   = MPI_IN_PLACE;
        sBase  = NULL;
    }
    else
    {
        sType = (MPI_Datatype)((*env)->GetLongField(
                env, sJType, ompi_java.DatatypeHandle));

        sBType = (*env)->GetIntField(env, sJType, ompi_java.DatatypeBaseType);
        sPtr   = ompi_java_getBufPtr(&sBase, env, sendBuf, sBType, sOffset);
    }

    MPI_Datatype rType = (MPI_Datatype)((*env)->GetLongField(
                         env, rJType, ompi_java.DatatypeHandle));

    if(rootOrInter || sPtr == MPI_IN_PLACE)
    {
        /*
         * In principle need the "id == root" check here and elsewere for
         * correctness, in case arguments that are not supposed to be
         * significant except on root are legitimately passed in as `null',
         * say.  Shouldn't produce null pointer exception.
         *
         * (However in this case MPICH complains if `mpi_rtype' is not defined
         * in all processes, notwithstanding what the spec says.)
         */

        rBType = (*env)->GetIntField(env, rJType, ompi_java.DatatypeBaseType);
        rPtr   = ompi_java_getBufPtr(&rBase, env, recvBuf, rBType, rOffset);

        if(!rootOrInter)
        {
            /* The receive buffer is ignored for all non-root processes.
             * As we are using MPI_IN_PLACE version, we use the receive
             * buffer as the send buffer.
             */
            assert(sendBuf == NULL);
            sPtr   = rPtr;
            sCount = rCount;
            sType  = rType;
        }
    }

    rc = MPI_Gather(sPtr, sCount, sType, rPtr, rCount, rType, root, comm);
    ompi_java_exceptionCheck(env, rc);

    if(rootOrInter || sendBuf == NULL)
        ompi_java_releaseBufPtr(env, recvBuf, rBase, rBType);

    if(sendBuf != NULL)
        ompi_java_releaseReadBufPtr(env, sendBuf, sBase, sBType);
}

JNIEXPORT jlong JNICALL Java_mpi_Comm_iGather(
        JNIEnv *env, jobject jthis, jlong comm,
        jobject sendBuf, jint sCount, jlong sType,
        jobject recvBuf, jint rCount, jlong rType, jint root)
{
    int id;
    int rc = MPI_Comm_rank((MPI_Comm)comm, &id);
    int rootOrInter = id == root || isInter(env, (MPI_Comm)comm);

    if(ompi_java_exceptionCheck(env, rc))
        return (jlong)MPI_REQUEST_NULL;

    MPI_Request request;
    void *sPtr, *rPtr = NULL;

    if(sType == 0)
    {
        assert(sendBuf == NULL);
        sType = (jlong)MPI_DATATYPE_NULL;
        sPtr  = MPI_IN_PLACE;
    }
    else
    {
        sPtr = ompi_java_getDirectBufferAddress(env, sendBuf);
    }

    if(rootOrInter || sPtr == MPI_IN_PLACE)
    {
        /*
         * In principle need the "id == root" check here and elsewere for
         * correctness, in case arguments that are not supposed to be
         * significant except on root are legitimately passed in as `null',
         * say.  Shouldn't produce null pointer exception.
         *
         * (However in this case MPICH complains if `mpi_rtype' is not defined
         * in all processes, notwithstanding what the spec says.)
         */

        rPtr = ompi_java_getDirectBufferAddress(env, recvBuf);

        if(!rootOrInter)
        {
            /* The receive buffer is ignored for all non-root processes.
             * As we are using MPI_IN_PLACE version, we use the receive
             * buffer as the send buffer.
             */
            assert(sendBuf == NULL);
            sPtr   = rPtr;
            sCount = rCount;
            sType  = rType;
        }
    }

    rc = MPI_Igather(sPtr, sCount, (MPI_Datatype)sType,
                     rPtr, rCount, (MPI_Datatype)rType,
                     root, (MPI_Comm)comm, &request);

    ompi_java_exceptionCheck(env, rc);
    return (jlong)request;
}

JNIEXPORT void JNICALL Java_mpi_Comm_gatherv(
        JNIEnv *env, jobject jthis,
        jobject sendBuf, jint sOffset, jint sCount, jobject sJType,
        jobject recvBuf, jint rOffset, jintArray rCounts,
        jintArray displs, jobject rJType, jint root)
{
    MPI_Comm comm = (MPI_Comm)((*env)->GetLongField(
                    env, jthis, ompi_java.CommHandle));
    int id;
    int rc = MPI_Comm_rank(comm, &id);
    int rootOrInter = id == root || isInter(env, comm);

    if(ompi_java_exceptionCheck(env, rc))
        return;

    void *sPtr, *sBase, *rPtr = NULL, *rBase;
    MPI_Datatype sType;
    int sBType;

    if(sJType == NULL)
    {
        assert(sendBuf == NULL);
        sType  = MPI_DATATYPE_NULL;
        sBType = 0;
        sPtr   = MPI_IN_PLACE;
        sBase  = NULL;
    }
    else
    {
        sType = (MPI_Datatype)((*env)->GetLongField(
                env, sJType, ompi_java.DatatypeHandle));

        sBType = (*env)->GetIntField(env, sJType, ompi_java.DatatypeBaseType);
        sPtr   = ompi_java_getBufPtr(&sBase, env, sendBuf, sBType, sOffset);
    }

    jint *jRCounts = NULL, *jDispls = NULL;
    int  *cRCounts = NULL, *cDispls = NULL;
    MPI_Datatype rType = sType;
    int rBType;

    if(rootOrInter)
    {
        ompi_java_getIntArray(env, rCounts, &jRCounts, &cRCounts);
        ompi_java_getIntArray(env, displs, &jDispls, &cDispls);

        rType = (MPI_Datatype)((*env)->GetLongField(
                env, rJType, ompi_java.DatatypeHandle));

        rBType = (*env)->GetIntField(env, rJType, ompi_java.DatatypeBaseType);
        rPtr   = ompi_java_getBufPtr(&rBase, env, recvBuf, rBType, rOffset);
    }

    rc = MPI_Gatherv(sPtr, sCount, sType, rPtr, cRCounts,
                     cDispls, rType, root, comm);

    ompi_java_exceptionCheck(env, rc);

    if(sendBuf != NULL)
        ompi_java_releaseReadBufPtr(env, sendBuf, sBase, sBType);

    if(rootOrInter)
    {
        ompi_java_releaseBufPtr(env, recvBuf, rBase, rBType);
        ompi_java_forgetIntArray(env, rCounts, jRCounts, cRCounts);
        ompi_java_forgetIntArray(env, displs, jDispls, cDispls);
    }
}

JNIEXPORT jlong JNICALL Java_mpi_Comm_iGatherv(
        JNIEnv *env, jobject jthis, jlong comm,
        jobject sendBuf, jint sCount, jlong sType,
        jobject recvBuf, jintArray rCounts,
        jintArray displs, jlong rType, jint root)
{
    int id;
    int rc = MPI_Comm_rank((MPI_Comm)comm, &id);
    int rootOrInter = id == root || isInter(env, (MPI_Comm)comm);

    if(ompi_java_exceptionCheck(env, rc))
        return (jlong)MPI_REQUEST_NULL;

    MPI_Request request;
    void *sPtr, *rPtr = NULL;

    if(sType == 0)
    {
        assert(sendBuf == NULL);
        sType = (jlong)MPI_DATATYPE_NULL;
        sPtr  = MPI_IN_PLACE;
    }
    else
    {
        sPtr = ompi_java_getDirectBufferAddress(env, sendBuf);
    }

    jint *jRCounts, *jDispls;
    int  *cRCounts, *cDispls;

    if(rootOrInter)
    {
        ompi_java_getIntArray(env, rCounts, &jRCounts, &cRCounts);
        ompi_java_getIntArray(env, displs, &jDispls, &cDispls);
        rPtr = ompi_java_getDirectBufferAddress(env, recvBuf);
    }
    else
    {
        jRCounts = jDispls = NULL;
        cRCounts = cDispls = NULL;
        rType = sType;
    }

    rc = MPI_Igatherv(sPtr, sCount, (MPI_Datatype)sType, rPtr,
                      cRCounts, cDispls, (MPI_Datatype)rType,
                      root, (MPI_Comm)comm, &request);

    ompi_java_exceptionCheck(env, rc);

    if(rootOrInter)
    {
        ompi_java_forgetIntArray(env, rCounts, jRCounts, cRCounts);
        ompi_java_forgetIntArray(env, displs, jDispls, cDispls);
    }

    return (jlong)request;
}

JNIEXPORT void JNICALL Java_mpi_Comm_scatter(
        JNIEnv *env, jobject jthis,
        jobject sendBuf, jint sOffset, jint sCount, jobject sJType,
        jobject recvBuf, jint rOffset, jint rCount, jobject rJType, jint root)
{
    MPI_Comm comm = (MPI_Comm)((*env)->GetLongField(
                    env, jthis, ompi_java.CommHandle));
    int id;
    int rc = MPI_Comm_rank(comm, &id);
    int rootOrInter = id == root || isInter(env, comm);

    if(ompi_java_exceptionCheck(env, rc))
        return;

    void *sPtr = NULL, *sBase, *rPtr, *rBase;
    MPI_Datatype sType, rType;
    int sBType, rBType;

    if(rJType == NULL)
    {
        assert(recvBuf == NULL);
        rType  = MPI_DATATYPE_NULL;
        rPtr   = MPI_IN_PLACE;
        rBType = 0;
    }
    else
    {
        rType = (MPI_Datatype)((*env)->GetLongField(
                env, rJType, ompi_java.DatatypeHandle));

        rBType = (*env)->GetIntField(env, rJType, ompi_java.DatatypeBaseType);
        rPtr   = ompi_java_getBufPtr(&rBase, env, recvBuf, rBType, rOffset);
    }

    sType = (MPI_Datatype)((*env)->GetLongField(
            env, sJType, ompi_java.DatatypeHandle));

    if(rootOrInter || rPtr == MPI_IN_PLACE)
    {
        sBType = (*env)->GetIntField(env, sJType, ompi_java.DatatypeBaseType);
        sPtr   = ompi_java_getBufPtr(&sBase, env, sendBuf, sBType, sOffset);

        if(!rootOrInter)
        {
            /* The send buffer is ignored for all non-root processes.
             * As we are using MPI_IN_PLACE version, we use the send
             * buffer as the receive buffer.
             */
            assert(recvBuf == NULL);
            rPtr   = sPtr;
            rCount = sCount;
            rType  = sType;
        }
    }

    rc = MPI_Scatter(sPtr, sCount, sType, rPtr, rCount, rType, root, comm);
    ompi_java_exceptionCheck(env, rc);

    if(rootOrInter || recvBuf == NULL)
        ompi_java_releaseBufPtr(env, sendBuf, sBase, sBType);

    if(recvBuf != NULL)
        ompi_java_releaseBufPtr(env, recvBuf, rBase, rBType);
}

JNIEXPORT jlong JNICALL Java_mpi_Comm_iScatter(
        JNIEnv *env, jobject jthis, jlong comm,
        jobject sendBuf, jint sCount, jlong sType,
        jobject recvBuf, jint rCount, jlong rType, jint root)
{
    int id;
    int rc = MPI_Comm_rank((MPI_Comm)comm, &id);
    int rootOrInter = id == root || isInter(env, (MPI_Comm)comm);

    if(ompi_java_exceptionCheck(env, rc))
        return (jlong)MPI_REQUEST_NULL;

    void *sPtr = NULL, *rPtr;
    MPI_Request request;

    if(rType == 0)
    {
        assert(recvBuf == NULL);
        rType = (jlong)MPI_DATATYPE_NULL;
        rPtr  = MPI_IN_PLACE;
    }
    else
    {
        rPtr = ompi_java_getDirectBufferAddress(env, recvBuf);
    }

    if(rootOrInter || rPtr == MPI_IN_PLACE)
    {
        sPtr = ompi_java_getDirectBufferAddress(env, sendBuf);

        if(!rootOrInter)
        {
            /* The send buffer is ignored for all non-root processes.
             * As we are using MPI_IN_PLACE version, we use the send
             * buffer as the receive buffer.
             */
            assert(recvBuf == NULL);
            rPtr   = sPtr;
            rCount = sCount;
            rType  = sType;
        }
    }

    rc = MPI_Iscatter(sPtr, sCount, (MPI_Datatype)sType,
                      rPtr, rCount, (MPI_Datatype)rType,
                      root, (MPI_Comm)comm, &request);

    ompi_java_exceptionCheck(env, rc);
    return (jlong)request;
}

JNIEXPORT void JNICALL Java_mpi_Comm_scatterv(
        JNIEnv *env, jobject jthis,
        jobject sendBuf, jint sOffset, jintArray sCounts,
        jintArray displs, jobject sJType,
        jobject recvBuf, jint rOffset, jint rCount, jobject rJType,
        jint root)
{
    MPI_Comm comm = (MPI_Comm)((*env)->GetLongField(
                    env, jthis, ompi_java.CommHandle));
    int id;
    int rc = MPI_Comm_rank(comm, &id);
    int rootOrInter = id == root || isInter(env, comm);

    if(ompi_java_exceptionCheck(env, rc))
        return;

    void *sPtr = NULL, *sBase, *rPtr, *rBase;
    MPI_Datatype rType;
    int rBType;

    if(rJType == NULL)
    {
        assert(recvBuf == NULL);
        rType  = MPI_DATATYPE_NULL;
        rBType = 0;
        rPtr   = MPI_IN_PLACE;
        rBase  = NULL;
    }
    else
    {
        rType = (MPI_Datatype)((*env)->GetLongField(
                env, rJType, ompi_java.DatatypeHandle));

        rBType = (*env)->GetIntField(env, rJType, ompi_java.DatatypeBaseType);
        rPtr   = ompi_java_getBufPtr(&rBase, env, recvBuf, rBType, rOffset);
    }

    jint *jSCounts = NULL, *jDispls = NULL;
    int  *cSCounts = NULL, *cDispls = NULL;
    MPI_Datatype sType = rType;
    int sBType;

    if(rootOrInter)
    {
        ompi_java_getIntArray(env, sCounts, &jSCounts, &cSCounts);
        ompi_java_getIntArray(env, displs, &jDispls, &cDispls);

        sType = (MPI_Datatype)((*env)->GetLongField(
                env, sJType, ompi_java.DatatypeHandle));

        sBType = (*env)->GetIntField(env, sJType, ompi_java.DatatypeBaseType);
        sPtr   = ompi_java_getBufPtr(&sBase, env, sendBuf, sBType, sOffset);
    }

    rc = MPI_Scatterv(sPtr, cSCounts, cDispls, sType,
                      rPtr, rCount, rType, root, comm);

    ompi_java_exceptionCheck(env, rc);

    if(recvBuf != NULL)
        ompi_java_releaseBufPtr(env, recvBuf, rBase, rBType);

    if(rootOrInter)
    {
        ompi_java_releaseBufPtr(env, sendBuf, sBase, sBType);
        ompi_java_forgetIntArray(env, sCounts, jSCounts, cSCounts);
        ompi_java_forgetIntArray(env, displs, jDispls, cDispls);
    }
}

JNIEXPORT jlong JNICALL Java_mpi_Comm_iScatterv(
        JNIEnv *env, jobject jthis, jlong comm,
        jobject sendBuf, jintArray sCounts, jintArray displs, jlong sType,
        jobject recvBuf, jint rCount, jlong rType, jint root)
{
    int id;
    int rc = MPI_Comm_rank((MPI_Comm)comm, &id);
    int rootOrInter = id == root || isInter(env, (MPI_Comm)comm);

    if(ompi_java_exceptionCheck(env, rc))
        return (jlong)MPI_REQUEST_NULL;

    MPI_Request request;
    void *sPtr = NULL, *rPtr;

    if(rType == 0)
    {
        assert(recvBuf == NULL);
        rType = (jlong)MPI_DATATYPE_NULL;
        rPtr  = MPI_IN_PLACE;
    }
    else
    {
        rPtr = ompi_java_getDirectBufferAddress(env, recvBuf);
    }

    jint *jSCounts, *jDispls;
    int  *cSCounts, *cDispls;

    if(rootOrInter)
    {
        ompi_java_getIntArray(env, sCounts, &jSCounts, &cSCounts);
        ompi_java_getIntArray(env, displs, &jDispls, &cDispls);
        sPtr = ompi_java_getDirectBufferAddress(env, sendBuf);
    }
    else
    {
        jSCounts = jDispls = NULL;
        cSCounts = cDispls = NULL;
        sType = rType;
    }

    rc = MPI_Iscatterv(sPtr, cSCounts, cDispls, (MPI_Datatype)sType,
                       rPtr, rCount, (MPI_Datatype)rType, root, 
                       (MPI_Comm)comm, &request);

    ompi_java_exceptionCheck(env, rc);

    if(rootOrInter)
    {
        ompi_java_forgetIntArray(env, sCounts, jSCounts, cSCounts);
        ompi_java_forgetIntArray(env, displs, jDispls, cDispls);
    }

    return (jlong)request;
}

JNIEXPORT void JNICALL Java_mpi_Comm_allGather(
        JNIEnv *env, jobject jthis,
        jobject sendBuf, jint sOffset, jint sCount, jobject sendtype,
        jobject recvBuf, jint rOffset, jint rCount, jobject recvtype)
{
    MPI_Comm comm = (MPI_Comm)((*env)->GetLongField(
                    env, jthis, ompi_java.CommHandle));
    int sBType;
    MPI_Datatype sType;
    void *sPtr, *sBase, *rPtr, *rBase;

    if(sendtype == NULL)
    {
        assert(sendBuf == NULL);
        sType  = MPI_DATATYPE_NULL;
        sBType = 0;
        sPtr   = MPI_IN_PLACE;
        sBase  = NULL;
    }
    else
    {
        sType = (MPI_Datatype)((*env)->GetLongField(
                env, sendtype, ompi_java.DatatypeHandle));

        sBType = (*env)->GetIntField(env,
                 sendtype, ompi_java.DatatypeBaseType);

        sPtr = ompi_java_getBufPtr(&sBase, env, sendBuf, sBType, sOffset);
    }

    MPI_Datatype rType = (MPI_Datatype)((*env)->GetLongField(
                         env, recvtype, ompi_java.DatatypeHandle));

    int rBType = (*env)->GetIntField(env,
                 recvtype, ompi_java.DatatypeBaseType);

    rPtr = ompi_java_getBufPtr(&rBase, env, recvBuf, rBType, rOffset);
    int rc = MPI_Allgather(sPtr, sCount, sType, rPtr, rCount, rType, comm);
    ompi_java_exceptionCheck(env, rc);

    ompi_java_releaseBufPtr(env, recvBuf, rBase, rBType);

    if(sendBuf != NULL)
        ompi_java_releaseReadBufPtr(env, sendBuf, sBase, sBType);
}

JNIEXPORT jlong JNICALL Java_mpi_Comm_iAllGather(
        JNIEnv *env, jobject jthis, jlong comm,
        jobject sendBuf, jint sCount, jlong sType,
        jobject recvBuf, jint rCount, jlong rType)
{
    void *sPtr, *rPtr;
    MPI_Request request;

    if(sType == 0)
    {
        assert(sendBuf == NULL);
        sType = (jlong)MPI_DATATYPE_NULL;
        sPtr  = MPI_IN_PLACE;
    }
    else
    {
        sPtr = ompi_java_getDirectBufferAddress(env, sendBuf);
    }

    rPtr = ompi_java_getDirectBufferAddress(env, recvBuf);

    int rc = MPI_Iallgather(sPtr, sCount, (MPI_Datatype)sType,
                            rPtr, rCount, (MPI_Datatype)rType,
                            (MPI_Comm)comm, &request);

    ompi_java_exceptionCheck(env, rc);
    return (jlong)request;
}

JNIEXPORT void JNICALL Java_mpi_Comm_allGatherv(
        JNIEnv *env, jobject jthis,
        jobject sendBuf, jint sOffset, jint sCount, jobject sJType,
        jobject recvBuf, jint rOffset, jintArray rCounts,
        jintArray displs, jobject rJType)
{
    MPI_Comm comm = (MPI_Comm)((*env)->GetLongField(
                    env, jthis, ompi_java.CommHandle));

    void *sPtr, *sBase, *rPtr, *rBase;
    MPI_Datatype sType;
    int sBType;

    if(sJType == NULL)
    {
        assert(sendBuf == NULL);
        sType  = MPI_DATATYPE_NULL;
        sBType = 0;
        sPtr   = MPI_IN_PLACE;
        sBase  = NULL;
    }
    else
    {
        sType = (MPI_Datatype)((*env)->GetLongField(
                env, sJType, ompi_java.DatatypeHandle));

        sBType = (*env)->GetIntField(env, sJType, ompi_java.DatatypeBaseType);
        sPtr   = ompi_java_getBufPtr(&sBase, env, sendBuf, sBType, sOffset);
    }

    MPI_Datatype rType = (MPI_Datatype)((*env)->GetLongField(
                         env, rJType, ompi_java.DatatypeHandle));

    int rBType = (*env)->GetIntField(env, rJType, ompi_java.DatatypeBaseType);

    jint *jRCounts, *jDispls;
    int  *cRCounts, *cDispls;
    ompi_java_getIntArray(env, rCounts, &jRCounts, &cRCounts);
    ompi_java_getIntArray(env, displs, &jDispls, &cDispls);

    rPtr = ompi_java_getBufPtr(&rBase, env, recvBuf, rBType, rOffset);

    int rc = MPI_Allgatherv(sPtr, sCount, sType, rPtr,
                            cRCounts, cDispls, rType, comm);

    ompi_java_exceptionCheck(env, rc);
    ompi_java_releaseBufPtr(env, recvBuf, rBase, rBType);

    if(sendBuf != NULL)
        ompi_java_releaseReadBufPtr(env, sendBuf, sBase, sBType);

    ompi_java_forgetIntArray(env, rCounts, jRCounts, cRCounts);
    ompi_java_forgetIntArray(env, displs, jDispls, cDispls);
}

JNIEXPORT jlong JNICALL Java_mpi_Comm_iAllGatherv(
        JNIEnv *env, jobject jthis, jlong comm,
        jobject sendBuf, jint sCount, jlong sType,
        jobject recvBuf, jintArray rCounts, jintArray displs, jlong rType)
{
    MPI_Request request;
    void *sPtr, *rPtr;

    if(sType == 0)
    {
        assert(sendBuf == NULL);
        sType = (jlong)MPI_DATATYPE_NULL;
        sPtr  = MPI_IN_PLACE;
    }
    else
    {
        sPtr = ompi_java_getDirectBufferAddress(env, sendBuf);
    }

    jint *jRCounts, *jDispls;
    int  *cRCounts, *cDispls;
    ompi_java_getIntArray(env, rCounts, &jRCounts, &cRCounts);
    ompi_java_getIntArray(env, displs, &jDispls, &cDispls);

    rPtr = ompi_java_getDirectBufferAddress(env, recvBuf);

    int rc = MPI_Iallgatherv(sPtr, sCount, (MPI_Datatype)sType,
                             rPtr, cRCounts, cDispls, (MPI_Datatype)rType,
                             (MPI_Comm)comm, &request);

    ompi_java_exceptionCheck(env, rc);
    ompi_java_forgetIntArray(env, rCounts, jRCounts, cRCounts);
    ompi_java_forgetIntArray(env, displs, jDispls, cDispls);
    return (jlong)request;
}

JNIEXPORT void JNICALL Java_mpi_Comm_allToAll(
        JNIEnv *env, jobject jthis,
        jobject sendBuf, jint sOffset, jint sCount, jobject sJType,
        jobject recvBuf, jint rOffset, jint rCount, jobject rJType)
{
    MPI_Comm comm = (MPI_Comm)((*env)->GetLongField(
                    env, jthis, ompi_java.CommHandle));

    MPI_Datatype sType = (MPI_Datatype)((*env)->GetLongField(
                         env, sJType, ompi_java.DatatypeHandle));
    MPI_Datatype rType = (MPI_Datatype)((*env)->GetLongField(
                         env, rJType, ompi_java.DatatypeHandle));

    int sBType = (*env)->GetIntField(env, sJType, ompi_java.DatatypeBaseType);
    int rBType = (*env)->GetIntField(env, rJType, ompi_java.DatatypeBaseType);

    void *sPtr, *sBase, *rPtr, *rBase;
    rPtr = ompi_java_getBufPtr(&rBase, env, recvBuf, rBType, rOffset);
    sPtr = ompi_java_getBufPtr(&sBase, env, sendBuf, sBType, sOffset);

    int rc = MPI_Alltoall(sPtr, sCount, sType, rPtr, rCount, rType, comm);
    ompi_java_exceptionCheck(env, rc);

    ompi_java_releaseReadBufPtr(env, sendBuf, sBase, sBType);
    ompi_java_releaseBufPtr(env, recvBuf, rBase, rBType);
}

JNIEXPORT jlong JNICALL Java_mpi_Comm_iAllToAll(
        JNIEnv *env, jobject jthis, jlong comm,
        jobject sendBuf, jint sCount, jlong sType,
        jobject recvBuf, jint rCount, jlong rType)
{
    void *sPtr = ompi_java_getDirectBufferAddress(env, sendBuf),
         *rPtr = ompi_java_getDirectBufferAddress(env, recvBuf);

    MPI_Request request;

    int rc = MPI_Ialltoall(sPtr, sCount, (MPI_Datatype)sType,
                           rPtr, rCount, (MPI_Datatype)rType,
                           (MPI_Comm)comm, &request);

    ompi_java_exceptionCheck(env, rc);
    return (jlong)request;
}

JNIEXPORT void JNICALL Java_mpi_Comm_allToAllv(
        JNIEnv *env, jobject jthis,
        jobject sendBuf, jint sOffset, jintArray sCount,
        jintArray sDispls, jobject sJType,
        jobject recvBuf, jint rOffset, jintArray rCount,
        jintArray rDispls, jobject rJType)
{
    MPI_Comm comm = (MPI_Comm)((*env)->GetLongField(
                    env, jthis, ompi_java.CommHandle));

    MPI_Datatype sType = (MPI_Datatype)((*env)->GetLongField(
                         env, sJType, ompi_java.DatatypeHandle));
    MPI_Datatype rType = (MPI_Datatype)((*env)->GetLongField(
                         env, rJType, ompi_java.DatatypeHandle));

    int sBType = (*env)->GetIntField(env, sJType, ompi_java.DatatypeBaseType),
        rBType = (*env)->GetIntField(env, rJType, ompi_java.DatatypeBaseType);

    jint *jSCount, *jRCount, *jSDispls, *jRDispls;
    int  *cSCount, *cRCount, *cSDispls, *cRDispls;
    ompi_java_getIntArray(env, sCount, &jSCount, &cSCount);
    ompi_java_getIntArray(env, rCount, &jRCount, &cRCount);
    ompi_java_getIntArray(env, sDispls, &jSDispls, &cSDispls);
    ompi_java_getIntArray(env, rDispls, &jRDispls, &cRDispls);

    void *sPtr, *sBase, *rPtr, *rBase;
    rPtr = ompi_java_getBufPtr(&rBase, env, recvBuf, rBType, rOffset);
    sPtr = ompi_java_getBufPtr(&sBase, env, sendBuf, sBType, sOffset);

    int rc = MPI_Alltoallv(sPtr, cSCount, cSDispls, sType,
                           rPtr, cRCount, cRDispls, rType, comm);

    ompi_java_exceptionCheck(env, rc);
    ompi_java_releaseReadBufPtr(env, sendBuf, sBase, sBType);
    ompi_java_releaseBufPtr(env, recvBuf, rBase, rBType);

    ompi_java_forgetIntArray(env, sCount,  jSCount,  cSCount);
    ompi_java_forgetIntArray(env, rCount,  jRCount,  cRCount);
    ompi_java_forgetIntArray(env, sDispls, jSDispls, cSDispls);
    ompi_java_forgetIntArray(env, rDispls, jRDispls, cRDispls);
}

JNIEXPORT jlong JNICALL Java_mpi_Comm_iAllToAllv(
        JNIEnv *env, jobject jthis, jlong comm,
        jobject sendBuf, jintArray sCount, jintArray sDispls, jlong sType,
        jobject recvBuf, jintArray rCount, jintArray rDispls, jlong rType)
{
    jint *jSCount, *jRCount, *jSDispls, *jRDispls;
    int  *cSCount, *cRCount, *cSDispls, *cRDispls;
    ompi_java_getIntArray(env, sCount, &jSCount, &cSCount);
    ompi_java_getIntArray(env, rCount, &jRCount, &cRCount);
    ompi_java_getIntArray(env, sDispls, &jSDispls, &cSDispls);
    ompi_java_getIntArray(env, rDispls, &jRDispls, &cRDispls);

    void *sPtr = ompi_java_getDirectBufferAddress(env, sendBuf),
         *rPtr = ompi_java_getDirectBufferAddress(env, recvBuf);

    MPI_Request request;

    int rc = MPI_Ialltoallv(sPtr, cSCount, cSDispls, (MPI_Datatype)sType,
                            rPtr, cRCount, cRDispls, (MPI_Datatype)rType,
                            (MPI_Comm)comm, &request);

    ompi_java_exceptionCheck(env, rc);
    ompi_java_forgetIntArray(env, sCount,  jSCount,  cSCount);
    ompi_java_forgetIntArray(env, rCount,  jRCount,  cRCount);
    ompi_java_forgetIntArray(env, sDispls, jSDispls, cSDispls);
    ompi_java_forgetIntArray(env, rDispls, jRDispls, cRDispls);
    return (jlong)request;
}

JNIEXPORT void JNICALL Java_mpi_Comm_reduce(
        JNIEnv *env, jobject jthis, jobject sendBuf, jint sOffset,
        jobject recvBuf, jint rOffset, jint count, jobject type,
        jobject op, jint root)
{
    MPI_Comm comm = (MPI_Comm)((*env)->GetLongField(
                    env, jthis, ompi_java.CommHandle));
    int id;
    int rc = MPI_Comm_rank(comm, &id);
    int rootOrInter = id == root || isInter(env, comm);

    if(ompi_java_exceptionCheck(env, rc))
        return;

    MPI_Datatype mpiType = (MPI_Datatype)((*env)->GetLongField(
                           env, type, ompi_java.DatatypeHandle));

    int baseType = (*env)->GetIntField(env, type, ompi_java.DatatypeBaseType);
    void *sPtr, *sBase, *rPtr = NULL, *rBase;

    if(sendBuf == NULL)
        sPtr = MPI_IN_PLACE;
    else
        sPtr = ompi_java_getBufPtr(&sBase, env, sendBuf, baseType, sOffset);

    if(rootOrInter || sendBuf == NULL)
    {
        rPtr = ompi_java_getBufPtr(&rBase, env, recvBuf, baseType, rOffset);

        if(!rootOrInter)
        {
            /* The receive buffer is ignored for all non-root processes.
             * As we are using MPI_IN_PLACE version, we use the receive
             * buffer as the send buffer.
             */
            assert(sendBuf == NULL);
            sPtr = rPtr;
        }
    }

    MPI_Op mpiOp = ompi_java_op_getHandle(env, op, baseType);
    rc = MPI_Reduce(sPtr, rPtr, count, mpiType, mpiOp, root, comm);
    ompi_java_exceptionCheck(env, rc);

    if(sendBuf != NULL)
        ompi_java_releaseReadBufPtr(env, sendBuf, sBase, baseType);

    if(rootOrInter || sendBuf == NULL)
        ompi_java_releaseBufPtr(env, recvBuf, rBase, baseType);
}

JNIEXPORT jlong JNICALL Java_mpi_Comm_iReduce(
        JNIEnv *env, jobject jthis, jlong comm,
        jobject sendBuf, jobject recvBuf, int count,
        jlong type, jint baseType, jobject op, jint root)
{
    int id;
    int rc = MPI_Comm_rank((MPI_Comm)comm, &id);
    int rootOrInter = id == root || isInter(env, (MPI_Comm)comm);

    if(ompi_java_exceptionCheck(env, rc))
        return (jlong)MPI_REQUEST_NULL;

    void *sPtr, *rPtr = NULL;
    MPI_Request request;

    if(sendBuf == NULL)
        sPtr = MPI_IN_PLACE;
    else
        sPtr = (*env)->GetDirectBufferAddress(env, sendBuf);

    if(rootOrInter || sendBuf == NULL)
    {
        rPtr = (*env)->GetDirectBufferAddress(env, recvBuf);

        if(!rootOrInter)
        {
            /* The receive buffer is ignored for all non-root processes.
             * As we are using MPI_IN_PLACE version, we use the receive
             * buffer as the send buffer.
             */
            assert(sendBuf == NULL);
            sPtr = rPtr;
        }
    }

    MPI_Op mpiOp = ompi_java_op_getHandle(env, op, baseType);

    rc = MPI_Ireduce(sPtr, rPtr, count, (MPI_Datatype)type,
                     mpiOp, root, (MPI_Comm)comm, &request);

    ompi_java_exceptionCheck(env, rc);
    return (jlong)request;
}

JNIEXPORT void JNICALL Java_mpi_Comm_allReduce(
                       JNIEnv *env, jobject jthis,
                       jobject sendBuf, jint sendOffset,
                       jobject recvBuf, jint recvOffset,
                       jint count, jobject type, jobject op)
{
    MPI_Comm comm = (MPI_Comm)((*env)->GetLongField(
                    env, jthis, ompi_java.CommHandle));

    MPI_Datatype mpiType = (MPI_Datatype)((*env)->GetLongField(
                           env, type, ompi_java.DatatypeHandle));

    int baseType = (*env)->GetIntField(env, type, ompi_java.DatatypeBaseType);
    void *sPtr, *sBase, *rPtr, *rBase;

    if(sendBuf == NULL)
        sPtr = MPI_IN_PLACE;
    else
        sPtr = ompi_java_getBufPtr(&sBase, env, sendBuf, baseType, sendOffset);

    rPtr = ompi_java_getBufPtr(&rBase, env, recvBuf, baseType, recvOffset);
    MPI_Op mpiOp = ompi_java_op_getHandle(env, op, baseType);
    int rc = MPI_Allreduce(sPtr, rPtr, count, mpiType, mpiOp, comm);
    ompi_java_exceptionCheck(env, rc);

    ompi_java_releaseBufPtr(env, recvBuf, rBase, baseType);

    if(sendBuf != NULL)
        ompi_java_releaseReadBufPtr(env, sendBuf, sBase, baseType);
}

JNIEXPORT jlong JNICALL Java_mpi_Comm_iAllReduce(
        JNIEnv *env, jobject jthis, jlong comm,
        jobject sendBuf, jobject recvBuf,
        jint count, jlong type, jint baseType, jobject op)
{
    MPI_Request request;
    void *sPtr, *rPtr;

    if(sendBuf == NULL)
        sPtr = MPI_IN_PLACE;
    else
        sPtr = (*env)->GetDirectBufferAddress(env, sendBuf);

    rPtr = (*env)->GetDirectBufferAddress(env, recvBuf);
    MPI_Op mpiOp = ompi_java_op_getHandle(env, op, baseType);

    int rc = MPI_Iallreduce(sPtr, rPtr, count, (MPI_Datatype)type,
                            mpiOp, (MPI_Comm)comm, &request);

    ompi_java_exceptionCheck(env, rc);
    return (jlong)request;
}

JNIEXPORT void JNICALL Java_mpi_Comm_reduceScatter(
        JNIEnv *env, jobject jthis, jobject sendBuf, jint sOffset,
        jobject recvBuf, jint rOffset, jintArray rCounts,
        jobject jType, jobject op)
{
    MPI_Comm comm = (MPI_Comm)((*env)->GetLongField(
                    env, jthis, ompi_java.CommHandle));

    MPI_Datatype type = (MPI_Datatype)((*env)->GetLongField(
                        env, jType, ompi_java.DatatypeHandle));

    int bType = (*env)->GetIntField(env, jType, ompi_java.DatatypeBaseType);
    void *sPtr, *sBase, *rPtr, *rBase;

    if(sendBuf == NULL)
        sPtr = MPI_IN_PLACE;
    else
        sPtr = ompi_java_getBufPtr(&sBase, env, sendBuf, bType, sOffset);

    rPtr = ompi_java_getBufPtr(&rBase, env, recvBuf, bType, rOffset);
    MPI_Op mpiOp = ompi_java_op_getHandle(env, op, bType);

    jint *jRCounts;
    int  *cRCounts;
    ompi_java_getIntArray(env, rCounts, &jRCounts, &cRCounts);

    int rc = MPI_Reduce_scatter(sPtr, rPtr, cRCounts, type, mpiOp, comm);
    ompi_java_exceptionCheck(env, rc);

    ompi_java_releaseBufPtr(env, recvBuf, rBase, bType);
    ompi_java_forgetIntArray(env, rCounts, jRCounts, cRCounts);

    if(sendBuf != NULL)
        ompi_java_releaseReadBufPtr(env, sendBuf, sBase, bType);
}

JNIEXPORT jlong JNICALL Java_mpi_Comm_iReduceScatter(
        JNIEnv *env, jobject jthis, jlong comm,
        jobject sendBuf, jobject recvBuf, jintArray rCounts,
        jlong type, int bType, jobject op)
{
    void *sPtr, *rPtr;

    if(sendBuf == NULL)
        sPtr = MPI_IN_PLACE;
    else
        sPtr = (*env)->GetDirectBufferAddress(env, sendBuf);

    rPtr = (*env)->GetDirectBufferAddress(env, recvBuf);
    MPI_Op mpiOp = ompi_java_op_getHandle(env, op, bType);
    MPI_Request request;

    jint *jRCounts;
    int  *cRCounts;
    ompi_java_getIntArray(env, rCounts, &jRCounts, &cRCounts);

    int rc = MPI_Ireduce_scatter(sPtr, rPtr, cRCounts, (MPI_Datatype)type,
                                 mpiOp, (MPI_Comm)comm, &request);

    ompi_java_exceptionCheck(env, rc);
    ompi_java_forgetIntArray(env, rCounts, jRCounts, cRCounts);
    return (jlong)request;
}

JNIEXPORT void JNICALL Java_mpi_Comm_reduceScatterBlock(
        JNIEnv *env, jobject jthis, jobject sendBuf, jint sOffset,
        jobject recvBuf, jint rOffset, jint count, jobject jType, jobject op)
{
    MPI_Comm comm = (MPI_Comm)((*env)->GetLongField(
                    env, jthis, ompi_java.CommHandle));

    MPI_Datatype type = (MPI_Datatype)((*env)->GetLongField(
                        env, jType, ompi_java.DatatypeHandle));

    int bType = (*env)->GetIntField(env, jType, ompi_java.DatatypeBaseType);
    void *sPtr, *sBase, *rPtr, *rBase;

    if(sendBuf == NULL)
        sPtr = MPI_IN_PLACE;
    else
        sPtr = ompi_java_getBufPtr(&sBase, env, sendBuf, bType, sOffset);

    rPtr = ompi_java_getBufPtr(&rBase, env, recvBuf, bType, rOffset);
    MPI_Op mpiOp = ompi_java_op_getHandle(env, op, bType);

    int rc = MPI_Reduce_scatter_block(sPtr, rPtr, count, type, mpiOp, comm);
    ompi_java_exceptionCheck(env, rc);

    ompi_java_releaseBufPtr(env, recvBuf, rBase, bType);

    if(sendBuf != NULL)
        ompi_java_releaseReadBufPtr(env, sendBuf, sBase, bType);
}

JNIEXPORT jlong JNICALL Java_mpi_Comm_iReduceScatterBlock(
        JNIEnv *env, jobject jthis, jlong comm, jobject sendBuf,
        jobject recvBuf, jint count, jlong type, jint bType, jobject op)
{
    void *sPtr, *rPtr;

    if(sendBuf == NULL)
        sPtr = MPI_IN_PLACE;
    else
        sPtr = (*env)->GetDirectBufferAddress(env, sendBuf);

    rPtr = (*env)->GetDirectBufferAddress(env, recvBuf);
    MPI_Op mpiOp = ompi_java_op_getHandle(env, op, bType);
    MPI_Request request;

    int rc = MPI_Ireduce_scatter_block(sPtr, rPtr, count, (MPI_Datatype)type,
                                       mpiOp, (MPI_Comm)comm, &request);

    ompi_java_exceptionCheck(env, rc);
    return (jlong)request;
}

JNIEXPORT void JNICALL Java_mpi_Comm_reduceLocal_1jni(
                       JNIEnv *env, jobject jthis,
                       jobject inbuf, jobject inoutbuf,
                       jint count, jobject type, jobject op)
{
    MPI_Datatype mpi_type =
        (MPI_Datatype)((*env)->GetLongField(env,type,ompi_java.DatatypeHandle));

    int baseType = (*env)->GetIntField(env, type, ompi_java.DatatypeBaseType);
    void *inptr, *inbase, *inoutptr, *inoutbase;
    inptr = ompi_java_getBufPtr(&inbase, env, inbuf, baseType, 0);
    inoutptr = ompi_java_getBufPtr(&inoutbase, env, inoutbuf, baseType, 0);

    int rc = MPI_Reduce_local(inptr, inoutptr, count, mpi_type,
                              ompi_java_op_getHandle(env, op, baseType));

    ompi_java_exceptionCheck(env, rc);
    ompi_java_releaseReadBufPtr(env, inbuf, inbase, baseType);
    ompi_java_releaseBufPtr(env, inoutbuf, inoutbase, baseType);
}

JNIEXPORT void JNICALL Java_mpi_Comm_setName(
        JNIEnv *env, jobject jthis, jlong handle, jstring jname)
{
    const char *name = (*env)->GetStringUTFChars(env, jname, NULL);
    int rc = MPI_Comm_set_name((MPI_Comm)handle, (char*)name);
    ompi_java_exceptionCheck(env, rc);
    (*env)->ReleaseStringUTFChars(env, jname, name);
}

JNIEXPORT jstring JNICALL Java_mpi_Comm_getName(
        JNIEnv *env, jobject jthis, jlong handle)
{
    char name[MPI_MAX_OBJECT_NAME];
    int len;
    int rc = MPI_Comm_get_name((MPI_Comm)handle, name, &len);

    if(ompi_java_exceptionCheck(env, rc))
        return NULL;

    return (*env)->NewStringUTF(env, name);
}
