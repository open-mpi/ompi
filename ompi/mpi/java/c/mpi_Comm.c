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

static void* getBufCritical(void** bufBase, JNIEnv *env,
                            jobject buf, jboolean db, int offset)
{
    if(buf == NULL)
    {
        /* Allow NULL buffers to send/recv 0 items as control messages. */
        *bufBase = NULL;
        return NULL;
    }
    else if(db)
    {
        *bufBase = (*env)->GetDirectBufferAddress(env, buf);
        assert(offset == 0);
        return *bufBase;
    }
    else
    {
        return ompi_java_getArrayCritical(bufBase, env, buf, offset);
    }
}

static void releaseBufCritical(
        JNIEnv *env, jobject buf, jboolean db, void* bufBase)
{
    if(!db && buf)
        (*env)->ReleasePrimitiveArrayCritical(env, buf, bufBase, 0);
}

static int isInter(JNIEnv *env, MPI_Comm comm)
{
    int rc, flag;
    rc = MPI_Comm_test_inter(comm, &flag);
    ompi_java_exceptionCheck(env, rc);
    return flag;
}

static int getSize(JNIEnv *env, MPI_Comm comm, int inter)
{
    int rc, size;

    if(inter)
        rc = MPI_Comm_remote_size(comm, &size);
    else
        rc = MPI_Comm_size(comm, &size);

    ompi_java_exceptionCheck(env, rc);
    return size;
}

static int getGroupSize(JNIEnv *env, MPI_Comm comm)
{
    int rc, size;
    rc = MPI_Comm_size(comm, &size);
    ompi_java_exceptionCheck(env, rc);
    return size;
}

static int getRank(JNIEnv *env, MPI_Comm comm)
{
    int rc, rank;
    rc = MPI_Comm_rank(comm, &rank);
    ompi_java_exceptionCheck(env, rc);
    return rank;
}

static int getTopo(JNIEnv *env, MPI_Comm comm)
{
    int rc, status;
    rc = MPI_Topo_test(comm, &status);
    ompi_java_exceptionCheck(env, rc);
    return status;
}

static void getNeighbors(JNIEnv *env, MPI_Comm comm, int *out, int *in)
{
    int rc, weighted;

    switch(getTopo(env, comm))
    {
        case MPI_CART:
            rc = MPI_Cartdim_get(comm, in);
            *in *= 2;
            *out = *in;
            break;
        case MPI_GRAPH:
            rc = MPI_Graph_neighbors_count(comm, getRank(env, comm), in);
            *out = *in;
            break;
        case MPI_DIST_GRAPH:
            rc = MPI_Dist_graph_neighbors_count(comm, in, out, &weighted);
            break;
        default:
            assert(0);
    }

    ompi_java_exceptionCheck(env, rc);
}

static int getSum(int *counts, int size)
{
    int i, s = 0;

    for(i = 0; i < size; i++)
        s += counts[i];

    return s;
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

JNIEXPORT jlong JNICALL Java_mpi_Comm_dup(
        JNIEnv *env, jobject jthis, jlong comm)
{
    MPI_Comm newcomm;
    int rc = MPI_Comm_dup((MPI_Comm)comm, &newcomm);
    ompi_java_exceptionCheck(env, rc);
    return (jlong)newcomm;
}

JNIEXPORT jlongArray JNICALL Java_mpi_Comm_iDup(
        JNIEnv *env, jobject jthis, jlong comm)
{
    MPI_Comm newcomm;
    MPI_Request request;
    int rc = MPI_Comm_idup((MPI_Comm)comm, &newcomm, &request);
    ompi_java_exceptionCheck(env, rc);
    jlongArray jcr = (*env)->NewLongArray(env, 2);
    jlong *cr = (jlong*)(*env)->GetPrimitiveArrayCritical(env, jcr, NULL);
    cr[0] = (jlong)newcomm;
    cr[1] = (jlong)request;
    (*env)->ReleasePrimitiveArrayCritical(env, jcr, cr, 0);
    return jcr;
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
    return getRank(env, (MPI_Comm)comm);
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

JNIEXPORT void JNICALL Java_mpi_Comm_setInfo(
        JNIEnv *env, jobject jthis, jlong comm, jlong info)
{
    int rc = MPI_Comm_set_info((MPI_Comm)comm, (MPI_Info)info);
    ompi_java_exceptionCheck(env, rc);
}

JNIEXPORT jlong JNICALL Java_mpi_Comm_getInfo(
        JNIEnv *env, jobject jthis, jlong comm)
{
    MPI_Info info;
    int rc = MPI_Comm_get_info((MPI_Comm)comm, &info);
    ompi_java_exceptionCheck(env, rc);
    return (jlong)info;
}

JNIEXPORT jlong JNICALL Java_mpi_Comm_disconnect(
        JNIEnv *env, jobject jthis, jlong handle)
{
    MPI_Comm comm = (MPI_Comm)handle;
    int rc = MPI_Comm_disconnect(&comm);
    ompi_java_exceptionCheck(env, rc);
    return (jlong)comm;
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
        JNIEnv *env, jobject jthis, jlong jComm,
        jobject buf, jboolean db, jint offset, jint count,
        jlong jType, jint bType, jint dest, jint tag)
{
    MPI_Comm     comm = (MPI_Comm)jComm;
    MPI_Datatype type = (MPI_Datatype)jType;

    void *ptr;
    ompi_java_buffer_t *item;
    ompi_java_getReadPtr(&ptr, &item, env, buf, db, offset, count, type, bType);

    int rc = MPI_Send(ptr, count, type, dest, tag, comm);
    ompi_java_exceptionCheck(env, rc);
    ompi_java_releaseReadPtr(ptr, item, buf, db);
}

JNIEXPORT void JNICALL Java_mpi_Comm_recv(
        JNIEnv *env, jobject jthis, jlong jComm,
        jobject buf, jboolean db, jint offset, jint count,
        jlong jType, jint bType, jint source, jint tag, jlongArray jStatus)
{
    MPI_Comm     comm = (MPI_Comm)jComm;
    MPI_Datatype type = (MPI_Datatype)jType;

    void *ptr;
    ompi_java_buffer_t *item;
    ompi_java_getWritePtr(&ptr, &item, env, buf, db, count, type);

    MPI_Status status;
    int rc = MPI_Recv(ptr, count, type, source, tag, comm, &status);
    ompi_java_exceptionCheck(env, rc);

    ompi_java_releaseWritePtr(ptr,item,env,buf,db,offset,count,type,bType);
    ompi_java_status_set(env, jStatus, &status);
}

JNIEXPORT void JNICALL Java_mpi_Comm_sendRecv(
        JNIEnv *env, jobject jthis, jlong jComm,
        jobject sBuf, jboolean sdb, jint sOff, jint sCount,
        jlong sjType, jint sBType, jint dest, jint sTag,
        jobject rBuf, jboolean rdb, jint rOff, jint rCount,
        jlong rjType, jint rBType, jint source, jint rTag,
        jlongArray jStatus)
{
    MPI_Comm     comm  = (MPI_Comm)jComm;
    MPI_Datatype sType = (MPI_Datatype)sjType;
    MPI_Datatype rType = (MPI_Datatype)rjType;

    void *sPtr, *rPtr;
    ompi_java_buffer_t *sItem, *rItem;
    MPI_Status status;

    ompi_java_getReadPtr(&sPtr,&sItem, env, sBuf,sdb,sOff,sCount,sType,sBType);
    ompi_java_getWritePtr(&rPtr, &rItem, env, rBuf, rdb, rCount, rType);

    int rc = MPI_Sendrecv(sPtr, sCount, sType, dest, sTag,
                          rPtr, rCount, rType, source, rTag, comm, &status);

    ompi_java_exceptionCheck(env, rc);
    ompi_java_releaseReadPtr(sPtr, sItem, sBuf, sdb);
    ompi_java_releaseWritePtr(rPtr,rItem,env,rBuf,rdb,rOff,rCount,rType,rBType);
    ompi_java_status_set(env, jStatus, &status);
}

JNIEXPORT void JNICALL Java_mpi_Comm_sendRecvReplace(
        JNIEnv *env, jobject jthis, jlong jComm,
        jobject buf, jboolean db, jint offset,
        jint count, jlong jType, jint bType,
        jint dest, jint sTag, jint source, jint rTag, jlongArray jStatus)
{
    MPI_Comm     comm = (MPI_Comm)jComm;
    MPI_Datatype type = (MPI_Datatype)jType;

    void *ptr;
    ompi_java_buffer_t *item;
    ompi_java_getReadPtr(&ptr, &item, env, buf, db, offset, count, type, bType);
    MPI_Status status;

    int rc = MPI_Sendrecv_replace(ptr, count, type, dest,
                                  sTag, source, rTag, comm, &status);

    ompi_java_exceptionCheck(env, rc);
    ompi_java_status_set(env, jStatus, &status);
    ompi_java_releaseWritePtr(ptr,item,env,buf,db,offset,count,type,bType);
}

JNIEXPORT void JNICALL Java_mpi_Comm_bSend(
        JNIEnv *env, jobject jthis, jlong jComm,
        jobject buf, jboolean db, jint offset,
        jint count, jlong jType, jint bType, jint dest, jint tag)
{
    MPI_Comm     comm = (MPI_Comm)jComm;
    MPI_Datatype type = (MPI_Datatype)jType;

    void *ptr;
    ompi_java_buffer_t *item;
    ompi_java_getReadPtr(&ptr, &item, env, buf, db, offset, count, type, bType);

    int rc = MPI_Bsend(ptr, count, type, dest, tag, comm);
    ompi_java_exceptionCheck(env, rc);
    ompi_java_releaseReadPtr(ptr, item, buf, db);
}

JNIEXPORT void JNICALL Java_mpi_Comm_sSend(
        JNIEnv *env, jobject jthis, jlong jComm,
        jobject buf, jboolean db, jint offset,
        jint count, jlong jType, jint bType, jint dest, jint tag)
{
    MPI_Comm     comm = (MPI_Comm)jComm;
    MPI_Datatype type = (MPI_Datatype)jType;

    void *ptr;
    ompi_java_buffer_t *item;
    ompi_java_getReadPtr(&ptr, &item, env, buf, db, offset, count, type, bType);

    int rc = MPI_Ssend(ptr, count, type, dest, tag, comm);
    ompi_java_exceptionCheck(env, rc);
    ompi_java_releaseReadPtr(ptr, item, buf, db);
}

JNIEXPORT void JNICALL Java_mpi_Comm_rSend(
        JNIEnv *env, jobject jthis, jlong jComm,
        jobject buf, jboolean db, jint offset,
        jint count, jlong jType, jint bType, jint dest, jint tag)
{
    MPI_Comm     comm = (MPI_Comm)jComm;
    MPI_Datatype type = (MPI_Datatype)jType;

    void *ptr;
    ompi_java_buffer_t *item;
    ompi_java_getReadPtr(&ptr, &item, env, buf, db, offset, count, type, bType);

    int rc = MPI_Rsend(ptr, count, type, dest, tag, comm);
    ompi_java_exceptionCheck(env, rc);
    ompi_java_releaseReadPtr(ptr, item, buf, db);
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
        JNIEnv *env, jobject jthis, jlong jComm,
        jobject inBuf, jboolean indb, jint offset,
        jint inCount, jlong jType, jbyteArray outBuf, jint position)
{
    MPI_Comm     comm = (MPI_Comm)jComm;
    MPI_Datatype type = (MPI_Datatype)jType;
    int outSize = (*env)->GetArrayLength(env, outBuf);

    void *oBufPtr, *iBufPtr, *iBufBase;
    oBufPtr = (*env)->GetPrimitiveArrayCritical(env, outBuf, NULL);
    iBufPtr = getBufCritical(&iBufBase, env, inBuf, indb, offset);

    if(inCount != 0 && outSize != position)
    {
        /* LAM doesn't like count = 0 */
        int rc = MPI_Pack(iBufPtr, inCount, type,
                          oBufPtr, outSize, &position, comm);

        ompi_java_exceptionCheck(env, rc);
    }

    releaseBufCritical(env, inBuf, indb, iBufBase);
    (*env)->ReleasePrimitiveArrayCritical(env, outBuf, oBufPtr, 0);
    return position;
}

JNIEXPORT jint JNICALL Java_mpi_Comm_unpack(
        JNIEnv *env, jobject jthis, jlong jComm,
        jbyteArray inBuf, jint position, jobject outBuf, jboolean outdb,
        jint offset, jint outCount, jlong jType)
{
    MPI_Comm     comm = (MPI_Comm)jComm;
    MPI_Datatype type = (MPI_Datatype)jType;
    int inSize = (*env)->GetArrayLength(env, inBuf);

    void *iBufPtr, *oBufPtr, *oBufBase;
    iBufPtr = (*env)->GetPrimitiveArrayCritical(env, inBuf, NULL);
    oBufPtr = getBufCritical(&oBufBase, env, outBuf, outdb, offset);

    int rc = MPI_Unpack(iBufPtr, inSize, &position,
                        oBufPtr, outCount, type, comm);

    ompi_java_exceptionCheck(env, rc);
    (*env)->ReleasePrimitiveArrayCritical(env, inBuf, iBufPtr, 0);
    releaseBufCritical(env, outBuf, outdb, oBufBase);
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
    return flag ? ompi_java_status_new(env, &status) : NULL;
}

JNIEXPORT void JNICALL Java_mpi_Comm_probe(
        JNIEnv *env, jobject jthis, jlong comm,
        jint source, jint tag, jlongArray jStatus)
{
    MPI_Status status;
    int rc = MPI_Probe(source, tag, (MPI_Comm)comm, &status);
    ompi_java_exceptionCheck(env, rc);
    ompi_java_status_set(env, jStatus, &status);
}

JNIEXPORT jint JNICALL Java_mpi_Comm_getTopology(
        JNIEnv *env, jobject jthis, jlong comm)
{
    return getTopo(env, (MPI_Comm)comm);
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
        JNIEnv *env, jobject jthis, jlong jComm, jobject buf, jboolean db,
        jint offset, jint count, jlong jType, jint bType, jint root)
{
    MPI_Comm     comm = (MPI_Comm)jComm;
    MPI_Datatype type = (MPI_Datatype)jType;

    void *ptr;
    ompi_java_buffer_t *item;
    ompi_java_getReadPtr(&ptr, &item, env, buf, db, offset, count, type, bType);

    int rc = MPI_Bcast(ptr, count, type, root, comm);
    ompi_java_exceptionCheck(env, rc);
    ompi_java_releaseWritePtr(ptr,item,env,buf,db,offset,count,type,bType);
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
        JNIEnv *env, jobject jthis, jlong jComm,
        jobject sBuf, jboolean sdb, jint sOff, jint sCount,
        jlong sjType, jint sBType,
        jobject rBuf, jboolean rdb, jint rOff, jint rCount,
        jlong rjType, jint rBType, jint root)
{
    MPI_Comm comm = (MPI_Comm)jComm;
    int rank  = getRank(env, comm);
    int inter = isInter(env, comm);
    int rootOrInter = rank == root || inter;

    void *sPtr, *rPtr = NULL;
    ompi_java_buffer_t *sItem, *rItem;
    MPI_Datatype sType;

    if(sjType == 0)
    {
        assert(sBuf == NULL);
        sType = MPI_DATATYPE_NULL;
        sPtr  = MPI_IN_PLACE;
    }
    else
    {
        sType = (MPI_Datatype)sjType;

        ompi_java_getReadPtr(&sPtr, &sItem, env, sBuf, sdb,
                             sOff, sCount, sType, sBType);
    }

    MPI_Datatype rType = (MPI_Datatype)rjType;
    int rCountTotal = rootOrInter ? rCount * getSize(env, comm, inter) : rCount;

    if(rootOrInter || sPtr == MPI_IN_PLACE)
    {
        if(sPtr == MPI_IN_PLACE)
        {
            /* We use the receive buffer as the send buffer. */
            ompi_java_getReadPtr(&rPtr, &rItem, env, rBuf, rdb,
                                 rOff, rCountTotal, rType, rBType);
        }
        else
        {
            ompi_java_getWritePtr(&rPtr, &rItem, env, rBuf, rdb,
                                  rCountTotal, rType);
        }

        if(!rootOrInter)
        {
            /* The receive buffer is ignored for all non-root processes.
             * As we are using MPI_IN_PLACE version, we use the receive
             * buffer as the send buffer.
             */
            assert(sBuf == NULL);
            sPtr   = rPtr;
            sCount = rCount;
            sType  = rType;
        }
    }

    int rc = MPI_Gather(sPtr, sCount, sType, rPtr, rCount, rType, root, comm);
    ompi_java_exceptionCheck(env, rc);

    if(rootOrInter)
    {
        ompi_java_releaseWritePtr(rPtr, rItem, env, rBuf, rdb,
                                  rOff, rCountTotal, rType, rBType);
    }
    else if(sBuf == NULL)
    {
        ompi_java_releaseReadPtr(rPtr, rItem, rBuf, rdb);
    }

    if(sBuf != NULL)
        ompi_java_releaseReadPtr(sPtr, sItem, sBuf, sdb);
}

JNIEXPORT jlong JNICALL Java_mpi_Comm_iGather(
        JNIEnv *env, jobject jthis, jlong jComm,
        jobject sendBuf, jint sCount, jlong sType,
        jobject recvBuf, jint rCount, jlong rType, jint root)
{
    MPI_Comm comm = (MPI_Comm)jComm;
    int rank = getRank(env, comm);
    int rootOrInter = rank == root || isInter(env, comm);

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

    int rc = MPI_Igather(sPtr, sCount, (MPI_Datatype)sType,
                         rPtr, rCount, (MPI_Datatype)rType,
                         root, comm, &request);

    ompi_java_exceptionCheck(env, rc);
    return (jlong)request;
}

JNIEXPORT void JNICALL Java_mpi_Comm_gatherv(
        JNIEnv *env, jobject jthis, jlong jComm,
        jobject sBuf, jboolean sdb, jint sOff,
        jint sCount, jlong sjType, jint sBType,
        jobject rBuf, jboolean rdb, jint rOff, jintArray rCounts,
        jintArray displs, jlong rjType, jint rBType, jint root)
{
    MPI_Comm comm = (MPI_Comm)jComm;
    int rank  = getRank(env, comm);
    int inter = isInter(env, comm);
    int rootOrInter = rank == root || inter;
    int size = rootOrInter ? getSize(env, comm, inter) : 0;

    void *sPtr, *rPtr = NULL;
    ompi_java_buffer_t *sItem, *rItem;
    MPI_Datatype sType;

    if(sjType == 0)
    {
        assert(sBuf == NULL);
        sType = MPI_DATATYPE_NULL;
        sPtr  = MPI_IN_PLACE;
    }
    else
    {
        sType = (MPI_Datatype)sjType;

        ompi_java_getReadPtr(&sPtr, &sItem, env, sBuf, sdb,
                             sOff, sCount, sType, sBType);
    }

    jint *jRCounts = NULL, *jDispls = NULL;
    int  *cRCounts = NULL, *cDispls = NULL;
    MPI_Datatype rType = sType;

    if(rootOrInter)
    {
        ompi_java_getIntArray(env, rCounts, &jRCounts, &cRCounts);
        ompi_java_getIntArray(env, displs, &jDispls, &cDispls);
        rType = (MPI_Datatype)rjType;

        if(sPtr == MPI_IN_PLACE)
        {
            /* We use the receive buffer as the send buffer. */
            ompi_java_getReadPtrv(&rPtr, &rItem, env, rBuf, rdb, rOff,
                                  cRCounts, cDispls, size, root, rType, rBType);
        }
        else
        {
            ompi_java_getWritePtrv(&rPtr, &rItem, env, rBuf, rdb,
                                   cRCounts, cDispls, size, rType);
        }
    }

    int rc = MPI_Gatherv(sPtr, sCount, sType, rPtr, cRCounts,
                         cDispls, rType, root, comm);

    ompi_java_exceptionCheck(env, rc);

    if(sBuf != NULL)
        ompi_java_releaseReadPtr(sPtr, sItem, sBuf, sdb);

    if(rootOrInter)
    {
        ompi_java_releaseWritePtrv(rPtr, rItem, env, rBuf, rdb, rOff,
                                   cRCounts, cDispls, size, rType, rBType);

        ompi_java_forgetIntArray(env, rCounts, jRCounts, cRCounts);
        ompi_java_forgetIntArray(env, displs, jDispls, cDispls);
    }
}

JNIEXPORT jlong JNICALL Java_mpi_Comm_iGatherv(
        JNIEnv *env, jobject jthis, jlong jComm,
        jobject sendBuf, jint sCount, jlong sType,
        jobject recvBuf, jintArray rCounts,
        jintArray displs, jlong rType, jint root)
{
    MPI_Comm comm = (MPI_Comm)jComm;
    int rank = getRank(env, comm);
    int rootOrInter = rank == root || isInter(env, comm);

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

    int rc = MPI_Igatherv(sPtr, sCount, (MPI_Datatype)sType, rPtr,
                          cRCounts, cDispls, (MPI_Datatype)rType,
                          root, comm, &request);

    ompi_java_exceptionCheck(env, rc);

    if(rootOrInter)
    {
        ompi_java_forgetIntArray(env, rCounts, jRCounts, cRCounts);
        ompi_java_forgetIntArray(env, displs, jDispls, cDispls);
    }

    return (jlong)request;
}

JNIEXPORT void JNICALL Java_mpi_Comm_scatter(
        JNIEnv *env, jobject jthis, jlong jComm,
        jobject sBuf, jboolean sdb, jint sOff, jint sCount,
        jlong sjType, jint sBType,
        jobject rBuf, jboolean rdb, jint rOff, jint rCount,
        jlong rjType, jint rBType, jint root)
{
    MPI_Comm comm = (MPI_Comm)jComm;
    int rank  = getRank(env, comm);
    int inter = isInter(env, comm);
    int rootOrInter = rank == root || inter;

    void *sPtr = NULL, *rPtr;
    ompi_java_buffer_t *sItem, *rItem;
    MPI_Datatype rType;

    if(rjType == 0)
    {
        assert(rBuf == NULL);
        rType = MPI_DATATYPE_NULL;
        rPtr  = MPI_IN_PLACE;
    }
    else
    {
        rType = (MPI_Datatype)rjType;
        ompi_java_getWritePtr(&rPtr, &rItem, env, rBuf, rdb, rCount, rType);
    }

    MPI_Datatype sType = (MPI_Datatype)sjType;
    int sCountTotal = rootOrInter ? sCount * getSize(env, comm, inter) : sCount;

    if(rootOrInter || rPtr == MPI_IN_PLACE)
    {
        ompi_java_getReadPtr(&sPtr, &sItem, env, sBuf, sdb, sOff,
                             sCountTotal, sType, sBType);
        if(!rootOrInter)
        {
            /* The send buffer is ignored for all non-root processes.
             * As we are using MPI_IN_PLACE version, we use the send
             * buffer as the receive buffer.
             */
            assert(rBuf == NULL);
            rPtr   = sPtr;
            rCount = sCount;
            rType  = sType;
        }
    }

    int rc = MPI_Scatter(sPtr, sCount, sType, rPtr, rCount, rType, root, comm);
    ompi_java_exceptionCheck(env, rc);

    if(rootOrInter)
    {
        ompi_java_releaseReadPtr(sPtr, sItem, sBuf, sdb);
    }
    else if(rBuf == NULL)
    {
        ompi_java_releaseWritePtr(sPtr, sItem, env, sBuf, sdb,
                                  sOff, sCount, sType, sBType);
    }

    if(rBuf != NULL)
    {
        ompi_java_releaseWritePtr(rPtr, rItem, env, rBuf, rdb,
                                  rOff, rCount, rType, rBType);
    }
}

JNIEXPORT jlong JNICALL Java_mpi_Comm_iScatter(
        JNIEnv *env, jobject jthis, jlong jComm,
        jobject sendBuf, jint sCount, jlong sType,
        jobject recvBuf, jint rCount, jlong rType, jint root)
{
    MPI_Comm comm = (MPI_Comm)jComm;
    int rank = getRank(env, comm);
    int rootOrInter = rank == root || isInter(env, comm);

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

    int rc = MPI_Iscatter(sPtr, sCount, (MPI_Datatype)sType,
                          rPtr, rCount, (MPI_Datatype)rType,
                          root, comm, &request);

    ompi_java_exceptionCheck(env, rc);
    return (jlong)request;
}

JNIEXPORT void JNICALL Java_mpi_Comm_scatterv(
        JNIEnv *env, jobject jthis, jlong jComm,
        jobject sBuf, jboolean sdb, jint sOff, jintArray sCounts,
        jintArray displs, jlong sjType, jint sBType,
        jobject rBuf, jboolean rdb, jint rOff, jint rCount,
        jlong rjType, jint rBType, jint root)
{
    MPI_Comm comm = (MPI_Comm)jComm;
    int rank  = getRank(env, comm);
    int inter = isInter(env, comm);
    int rootOrInter = rank == root || inter;
    int size = rootOrInter ? getSize(env, comm, inter) : 0;

    void *sPtr = NULL, *rPtr;
    ompi_java_buffer_t *sItem, *rItem;
    MPI_Datatype rType;

    if(rjType == 0)
    {
        assert(rBuf == NULL);
        rType = MPI_DATATYPE_NULL;
        rPtr  = MPI_IN_PLACE;
    }
    else
    {
        rType = (MPI_Datatype)rjType;
        ompi_java_getWritePtr(&rPtr, &rItem, env, rBuf, rdb, rCount, rType);
    }

    jint *jSCounts = NULL, *jDispls = NULL;
    int  *cSCounts = NULL, *cDispls = NULL;
    MPI_Datatype sType = rType;

    if(rootOrInter)
    {
        ompi_java_getIntArray(env, sCounts, &jSCounts, &cSCounts);
        ompi_java_getIntArray(env, displs, &jDispls, &cDispls);
        sType = (MPI_Datatype)sjType;

        ompi_java_getReadPtrv(&sPtr, &sItem, env, sBuf, sdb, sOff,
                              cSCounts, cDispls, size, -1, sType, sBType);
    }

    int rc = MPI_Scatterv(sPtr, cSCounts, cDispls, sType,
                          rPtr, rCount, rType, root, comm);

    ompi_java_exceptionCheck(env, rc);

    if(rBuf != NULL)
    {
        ompi_java_releaseWritePtr(rPtr, rItem, env, rBuf, rdb,
                                  rOff, rCount, rType, rBType);
    }

    if(rootOrInter)
    {
        ompi_java_releaseReadPtr(sPtr, sItem, sBuf, sdb);
        ompi_java_forgetIntArray(env, sCounts, jSCounts, cSCounts);
        ompi_java_forgetIntArray(env, displs, jDispls, cDispls);
    }
}

JNIEXPORT jlong JNICALL Java_mpi_Comm_iScatterv(
        JNIEnv *env, jobject jthis, jlong jComm,
        jobject sendBuf, jintArray sCounts, jintArray displs, jlong sType,
        jobject recvBuf, jint rCount, jlong rType, jint root)
{
    MPI_Comm comm = (MPI_Comm)jComm;
    int rank = getRank(env, comm);
    int rootOrInter = rank == root || isInter(env, comm);

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

    int rc = MPI_Iscatterv(sPtr, cSCounts, cDispls, (MPI_Datatype)sType,
                           rPtr, rCount, (MPI_Datatype)rType, root,
                           comm, &request);

    ompi_java_exceptionCheck(env, rc);

    if(rootOrInter)
    {
        ompi_java_forgetIntArray(env, sCounts, jSCounts, cSCounts);
        ompi_java_forgetIntArray(env, displs, jDispls, cDispls);
    }

    return (jlong)request;
}

JNIEXPORT void JNICALL Java_mpi_Comm_allGather(
        JNIEnv *env, jobject jthis, jlong jComm,
        jobject sBuf, jboolean sdb, jint sOff,
        jint sCount, jlong sjType, jint sBType,
        jobject rBuf, jboolean rdb, jint rOff,
        jint rCount, jlong rjType, jint rBType)
{
    MPI_Comm     comm  = (MPI_Comm)jComm;
    MPI_Datatype sType,
                 rType = (MPI_Datatype)rjType;

    int inter  = isInter(env, comm),
        size   = getSize(env, comm, inter),
        rTotal = rCount * size;

    void *sPtr, *rPtr;
    ompi_java_buffer_t *sItem, *rItem;

    if(sjType == 0)
    {
        assert(sBuf == NULL);
        sType = MPI_DATATYPE_NULL;
        sPtr  = MPI_IN_PLACE;
        int rank = getRank(env, comm);

        ompi_java_getReadPtrRank(&rPtr, &rItem, env, rBuf, rdb, rOff,
                                 rCount, size, rank, rType, rBType);
    }
    else
    {
        sType = (MPI_Datatype)sjType;

        ompi_java_getReadPtr(&sPtr, &sItem, env, sBuf, sdb,
                             sOff, sCount, sType, sBType);

        ompi_java_getWritePtr(&rPtr, &rItem, env, rBuf, rdb, rTotal, rType);
    }

    int rc = MPI_Allgather(sPtr, sCount, sType, rPtr, rCount, rType, comm);
    ompi_java_exceptionCheck(env, rc);

    ompi_java_releaseWritePtr(rPtr, rItem, env, rBuf, rdb,
                              rOff, rTotal, rType, rBType);
    if(sBuf != NULL)
        ompi_java_releaseReadPtr(sPtr, sItem, sBuf, sdb);
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
        JNIEnv *env, jobject jthis, jlong jComm,
        jobject sBuf, jboolean sdb, jint sOff,
        jint sCount, jlong sjType, jint sBType,
        jobject rBuf, jboolean rdb, jint rOff,
        jintArray rCounts, jintArray displs, jlong rjType, jint rBType)
{
    MPI_Comm comm = (MPI_Comm)jComm;
    int inter = isInter(env, comm),
        size  = getSize(env, comm, inter);

    MPI_Datatype sType,
                 rType = (MPI_Datatype)rjType;

    void *sPtr, *rPtr;
    ompi_java_buffer_t *sItem, *rItem;
    jint *jRCounts, *jDispls;
    int  *cRCounts, *cDispls;
    ompi_java_getIntArray(env, rCounts, &jRCounts, &cRCounts);
    ompi_java_getIntArray(env, displs, &jDispls, &cDispls);

    if(sjType == 0)
    {
        assert(sBuf == NULL);
        sType = MPI_DATATYPE_NULL;
        sPtr  = MPI_IN_PLACE;
        int rank = getRank(env, comm);

        ompi_java_getReadPtrv(&rPtr, &rItem, env, rBuf, rdb, rOff,
                              cRCounts, cDispls, size, rank, rType, rBType);
    }
    else
    {
        sType = (MPI_Datatype)sjType;

        ompi_java_getReadPtr(&sPtr, &sItem, env, sBuf, sdb,
                             sOff, sCount, sType, sBType);

        ompi_java_getWritePtrv(&rPtr, &rItem, env, rBuf, rdb,
                               cRCounts, cDispls, size, rType);
    }

    int rc = MPI_Allgatherv(sPtr, sCount, sType, rPtr,
                            cRCounts, cDispls, rType, comm);

    ompi_java_exceptionCheck(env, rc);

    ompi_java_releaseWritePtrv(rPtr, rItem, env, rBuf, rdb, rOff,
                               cRCounts, cDispls, size, rType, rBType);
    if(sBuf != NULL)
        ompi_java_releaseReadPtr(sPtr, sItem, sBuf, sdb);

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
        JNIEnv *env, jobject jthis, jlong jComm,
        jobject sBuf, jboolean sdb, jint sOff,
        jint sCount, jlong sjType, jint sBType,
        jobject rBuf, jboolean rdb, jint rOff,
        jint rCount, jlong rjType, jint rBType)
{
    MPI_Comm     comm  = (MPI_Comm)jComm;
    MPI_Datatype sType = (MPI_Datatype)sjType;
    MPI_Datatype rType = (MPI_Datatype)rjType;

    int inter  = isInter(env, comm),
        size   = getSize(env, comm, inter),
        sTotal = sCount * size,
        rTotal = rCount * size;

    void *sPtr, *rPtr;
    ompi_java_buffer_t *sItem, *rItem;
    ompi_java_getReadPtr(&sPtr, &sItem, env,sBuf,sdb,sOff,sTotal,sType,sBType);
    ompi_java_getWritePtr(&rPtr, &rItem, env, rBuf, rdb, rTotal, rType);

    int rc = MPI_Alltoall(sPtr, sCount, sType, rPtr, rCount, rType, comm);
    ompi_java_exceptionCheck(env, rc);

    ompi_java_releaseReadPtr(sPtr, sItem, sBuf, sdb);
    ompi_java_releaseWritePtr(rPtr,rItem,env,rBuf,rdb,rOff,rTotal,rType,rBType);
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
        JNIEnv *env, jobject jthis, jlong jComm,
        jobject sBuf, jboolean sdb, jint sOff, jintArray sCount,
        jintArray sDispl, jlong sjType, jint sBType,
        jobject rBuf, jboolean rdb, jint rOff, jintArray rCount,
        jintArray rDispl, jlong rjType, jint rBType)
{
    MPI_Comm     comm  = (MPI_Comm)jComm;
    MPI_Datatype sType = (MPI_Datatype)sjType;
    MPI_Datatype rType = (MPI_Datatype)rjType;

    int inter = isInter(env, comm),
        size  = getSize(env, comm, inter);

    jint *jSCount, *jRCount, *jSDispl, *jRDispl;
    int  *cSCount, *cRCount, *cSDispl, *cRDispl;
    ompi_java_getIntArray(env, sCount, &jSCount, &cSCount);
    ompi_java_getIntArray(env, rCount, &jRCount, &cRCount);
    ompi_java_getIntArray(env, sDispl, &jSDispl, &cSDispl);
    ompi_java_getIntArray(env, rDispl, &jRDispl, &cRDispl);

    void *sPtr, *rPtr;
    ompi_java_buffer_t *sItem, *rItem;

    ompi_java_getReadPtrv(&sPtr, &sItem, env, sBuf, sdb, sOff,
                          cSCount, cSDispl, size, -1, sType, sBType);
    ompi_java_getWritePtrv(&rPtr, &rItem, env, rBuf, rdb,
                           cRCount, cRDispl, size, rType);

    int rc = MPI_Alltoallv(sPtr, cSCount, cSDispl, sType,
                           rPtr, cRCount, cRDispl, rType, comm);

    ompi_java_exceptionCheck(env, rc);
    ompi_java_releaseReadPtr(sPtr, sItem, sBuf, sdb);

    ompi_java_releaseWritePtrv(rPtr, rItem, env, rBuf, rdb, rOff,
                               cRCount, cRDispl, size, rType, rBType);

    ompi_java_forgetIntArray(env, sCount, jSCount, cSCount);
    ompi_java_forgetIntArray(env, rCount, jRCount, cRCount);
    ompi_java_forgetIntArray(env, sDispl, jSDispl, cSDispl);
    ompi_java_forgetIntArray(env, rDispl, jRDispl, cRDispl);
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

JNIEXPORT void JNICALL Java_mpi_Comm_neighborAllGather(
        JNIEnv *env, jobject jthis, jlong jComm,
        jobject sBuf, jboolean sdb, jint sOff,
        jint sCount, jlong sjType, jint sBType,
        jobject rBuf, jboolean rdb, jint rOff,
        jint rCount, jlong rjType, jint rBType)
{
    MPI_Comm     comm  = (MPI_Comm)jComm;
    MPI_Datatype sType = (MPI_Datatype)sjType;
    MPI_Datatype rType = (MPI_Datatype)rjType;

    int sSize, rSize;
    getNeighbors(env, comm, &sSize, &rSize);
    int rTotal = rCount * rSize;

    void *sPtr, *rPtr;
    ompi_java_buffer_t *sItem, *rItem;
    ompi_java_getReadPtr(&sPtr,&sItem,env,sBuf,sdb,sOff,sCount,sType,sBType);
    ompi_java_getWritePtr(&rPtr, &rItem, env, rBuf, rdb, rTotal, rType);

    int rc = MPI_Neighbor_allgather(
             sPtr, sCount, sType, rPtr, rCount, rType, comm);

    ompi_java_exceptionCheck(env, rc);
    ompi_java_releaseReadPtr(sPtr, sItem, sBuf, sdb);
    ompi_java_releaseWritePtr(rPtr,rItem,env,rBuf,rdb,rOff,rTotal,rType,rBType);
}

JNIEXPORT jlong JNICALL Java_mpi_Comm_iNeighborAllGather(
        JNIEnv *env, jobject jthis, jlong jComm,
        jobject sendBuf, jint sCount, jlong sjType,
        jobject recvBuf, jint rCount, jlong rjType)
{
    MPI_Comm     comm  = (MPI_Comm)jComm;
    MPI_Datatype sType = (MPI_Datatype)sjType;
    MPI_Datatype rType = (MPI_Datatype)rjType;

    void *sPtr = ompi_java_getDirectBufferAddress(env, sendBuf),
         *rPtr = ompi_java_getDirectBufferAddress(env, recvBuf);

    MPI_Request request;

    int rc = MPI_Ineighbor_allgather(
             sPtr, sCount, sType, rPtr, rCount, rType, comm, &request);

    ompi_java_exceptionCheck(env, rc);
    return (jlong)request;
}

JNIEXPORT void JNICALL Java_mpi_Comm_neighborAllGatherv(
        JNIEnv *env, jobject jthis, jlong jComm,
        jobject sBuf, jboolean sdb, jint sOff,
        jint sCount, jlong sjType, jint sBType,
        jobject rBuf, jboolean rdb, jint rOff,
        jintArray rCount, jintArray displs, jlong rjType, jint rBType)
{
    MPI_Comm     comm  = (MPI_Comm)jComm;
    MPI_Datatype sType = (MPI_Datatype)sjType;
    MPI_Datatype rType = (MPI_Datatype)rjType;

    int sSize, rSize;
    getNeighbors(env, comm, &sSize, &rSize);

    jint *jRCount, *jDispls;
    int  *cRCount, *cDispls;
    ompi_java_getIntArray(env, rCount, &jRCount, &cRCount);
    ompi_java_getIntArray(env, displs, &jDispls, &cDispls);

    void *sPtr, *rPtr;
    ompi_java_buffer_t *sItem, *rItem;
    ompi_java_getReadPtr(&sPtr,&sItem,env,sBuf,sdb,sOff,sCount,sType,sBType);

    ompi_java_getWritePtrv(&rPtr, &rItem, env, rBuf, rdb,
                           cRCount, cDispls, rSize, rType);

    int rc = MPI_Neighbor_allgatherv(
             sPtr, sCount, sType, rPtr, cRCount, cDispls, rType, comm);

    ompi_java_exceptionCheck(env, rc);
    ompi_java_releaseReadPtr(sPtr, sItem, sBuf, sdb);

    ompi_java_releaseWritePtrv(rPtr, rItem, env, rBuf, rdb, rOff,
                               cRCount, cDispls, rSize, rType, rBType);

    ompi_java_forgetIntArray(env, rCount, jRCount, cRCount);
    ompi_java_forgetIntArray(env, displs, jDispls, cDispls);
}

JNIEXPORT jlong JNICALL Java_mpi_Comm_iNeighborAllGatherv(
        JNIEnv *env, jobject jthis, jlong jComm,
        jobject sendBuf, jint sCount, jlong sjType,
        jobject recvBuf, jintArray rCount, jintArray displs, jlong rjType)
{
    MPI_Comm     comm  = (MPI_Comm)jComm;
    MPI_Datatype sType = (MPI_Datatype)sjType;
    MPI_Datatype rType = (MPI_Datatype)rjType;

    jint *jRCount, *jDispls;
    int  *cRCount, *cDispls;
    ompi_java_getIntArray(env, rCount, &jRCount, &cRCount);
    ompi_java_getIntArray(env, displs, &jDispls, &cDispls);

    void *sPtr = ompi_java_getDirectBufferAddress(env, sendBuf),
         *rPtr = ompi_java_getDirectBufferAddress(env, recvBuf);

    MPI_Request request;

    int rc = MPI_Ineighbor_allgatherv(sPtr, sCount, sType, rPtr, cRCount,
                                      cDispls, rType, comm, &request);

    ompi_java_exceptionCheck(env, rc);
    ompi_java_forgetIntArray(env, rCount, jRCount, cRCount);
    ompi_java_forgetIntArray(env, displs, jDispls, cDispls);
    return (jlong)request;
}

JNIEXPORT void JNICALL Java_mpi_Comm_neighborAllToAll(
        JNIEnv *env, jobject jthis, jlong jComm,
        jobject sBuf, jboolean sdb, jint sOff,
        jint sCount, jlong sjType, jint sBType,
        jobject rBuf, jboolean rdb, jint rOff,
        jint rCount, jlong rjType, jint rBType)
{
    MPI_Comm     comm  = (MPI_Comm)jComm;
    MPI_Datatype sType = (MPI_Datatype)sjType;
    MPI_Datatype rType = (MPI_Datatype)rjType;

    int sSize, rSize;
    getNeighbors(env, comm, &sSize, &rSize);
    int sTotal = sCount * sSize;
    int rTotal = rCount * rSize;

    void *sPtr, *rPtr;
    ompi_java_buffer_t *sItem, *rItem;
    ompi_java_getReadPtr(&sPtr, &sItem, env,sBuf,sdb,sOff,sTotal,sType,sBType);
    ompi_java_getWritePtr(&rPtr, &rItem, env, rBuf, rdb, rTotal, rType);

    int rc = MPI_Neighbor_alltoall(
             sPtr, sCount, sType, rPtr, rCount, rType, comm);

    ompi_java_exceptionCheck(env, rc);
    ompi_java_releaseReadPtr(sPtr, sItem, sBuf, sdb);
    ompi_java_releaseWritePtr(rPtr,rItem,env,rBuf,rdb,rOff,rTotal,rType,rBType);
}

JNIEXPORT jlong JNICALL Java_mpi_Comm_iNeighborAllToAll(
        JNIEnv *env, jobject jthis, jlong jComm,
        jobject sendBuf, jint sCount, jlong sjType,
        jobject recvBuf, jint rCount, jlong rjType)
{
    MPI_Comm     comm  = (MPI_Comm)jComm;
    MPI_Datatype sType = (MPI_Datatype)sjType;
    MPI_Datatype rType = (MPI_Datatype)rjType;

    void *sPtr = ompi_java_getDirectBufferAddress(env, sendBuf),
         *rPtr = ompi_java_getDirectBufferAddress(env, recvBuf);

    MPI_Request request;

    int rc = MPI_Ineighbor_alltoall(
             sPtr, sCount, sType, rPtr, rCount, rType, comm, &request);

    ompi_java_exceptionCheck(env, rc);
    return (jlong)request;
}

JNIEXPORT void JNICALL Java_mpi_Comm_neighborAllToAllv(
        JNIEnv *env, jobject jthis, jlong jComm,
        jobject sBuf, jboolean sdb, jint sOff,
        jintArray sCount, jintArray sDispl, jlong sjType, jint sBType,
        jobject rBuf, jboolean rdb, jint rOff,
        jintArray rCount, jintArray rDispl, jlong rjType, jint rBType)
{
    MPI_Comm     comm  = (MPI_Comm)jComm;
    MPI_Datatype sType = (MPI_Datatype)sjType;
    MPI_Datatype rType = (MPI_Datatype)rjType;

    int sSize, rSize;
    getNeighbors(env, comm, &sSize, &rSize);

    jint *jSCount, *jRCount, *jSDispl, *jRDispl;
    int  *cSCount, *cRCount, *cSDispl, *cRDispl;
    ompi_java_getIntArray(env, sCount, &jSCount, &cSCount);
    ompi_java_getIntArray(env, rCount, &jRCount, &cRCount);
    ompi_java_getIntArray(env, sDispl, &jSDispl, &cSDispl);
    ompi_java_getIntArray(env, rDispl, &jRDispl, &cRDispl);

    void *sPtr, *rPtr;
    ompi_java_buffer_t *sItem, *rItem;

    ompi_java_getReadPtrv(&sPtr, &sItem, env, sBuf, sdb, sOff,
                          cSCount, cSDispl, sSize, -1, sType, sBType);
    ompi_java_getWritePtrv(&rPtr, &rItem, env, rBuf, rdb,
                           cRCount, cRDispl, rSize, rType);

    int rc = MPI_Neighbor_alltoallv(sPtr, cSCount, cSDispl, sType,
                                    rPtr, cRCount, cRDispl, rType, comm);

    ompi_java_exceptionCheck(env, rc);
    ompi_java_releaseReadPtr(sPtr, sItem, sBuf, sdb);

    ompi_java_releaseWritePtrv(rPtr, rItem, env, rBuf, rdb, rOff,
                               cRCount, cRDispl, rSize, rType, rBType);

    ompi_java_forgetIntArray(env, sCount, jSCount, cSCount);
    ompi_java_forgetIntArray(env, rCount, jRCount, cRCount);
    ompi_java_forgetIntArray(env, sDispl, jSDispl, cSDispl);
    ompi_java_forgetIntArray(env, rDispl, jRDispl, cRDispl);
}

JNIEXPORT jlong JNICALL Java_mpi_Comm_iNeighborAllToAllv(
        JNIEnv *env, jobject jthis, jlong jComm,
        jobject sendBuf, jintArray sCount, jintArray sDispls, jlong sjType,
        jobject recvBuf, jintArray rCount, jintArray rDispls, jlong rjType)
{
    MPI_Comm     comm  = (MPI_Comm)jComm;
    MPI_Datatype sType = (MPI_Datatype)sjType;
    MPI_Datatype rType = (MPI_Datatype)rjType;

    jint *jSCount, *jRCount, *jSDispls, *jRDispls;
    int  *cSCount, *cRCount, *cSDispls, *cRDispls;
    ompi_java_getIntArray(env, sCount,  &jSCount,  &cSCount);
    ompi_java_getIntArray(env, rCount,  &jRCount,  &cRCount);
    ompi_java_getIntArray(env, sDispls, &jSDispls, &cSDispls);
    ompi_java_getIntArray(env, rDispls, &jRDispls, &cRDispls);

    void *sPtr = ompi_java_getDirectBufferAddress(env, sendBuf),
         *rPtr = ompi_java_getDirectBufferAddress(env, recvBuf);

    MPI_Request request;

    int rc = MPI_Ineighbor_alltoallv(
             sPtr, cSCount, cSDispls, sType,
             rPtr, cRCount, cRDispls, rType, comm, &request);

    ompi_java_exceptionCheck(env, rc);
    ompi_java_forgetIntArray(env, sCount,  jSCount,  cSCount);
    ompi_java_forgetIntArray(env, rCount,  jRCount,  cRCount);
    ompi_java_forgetIntArray(env, sDispls, jSDispls, cSDispls);
    ompi_java_forgetIntArray(env, rDispls, jRDispls, cRDispls);
    return (jlong)request;
}

JNIEXPORT void JNICALL Java_mpi_Comm_reduce(
        JNIEnv *env, jobject jthis, jlong jComm,
        jobject sBuf, jboolean sdb, jint sOff,
        jobject rBuf, jboolean rdb, jint rOff, jint count,
        jlong jType, jint bType, jobject jOp, jlong hOp, jint root)
{
    MPI_Comm     comm = (MPI_Comm)jComm;
    MPI_Datatype type = (MPI_Datatype)jType;

    int rank = getRank(env, comm);
    int rootOrInter = rank == root || isInter(env, comm);

    void *sPtr, *rPtr = NULL;
    ompi_java_buffer_t *sItem, *rItem;

    if(sBuf == NULL)
    {
        ompi_java_getReadPtr(&rPtr,&rItem,env,rBuf,rdb,rOff,count,type,bType);
        sPtr = rootOrInter ? MPI_IN_PLACE : rPtr;
        /* The receive buffer is ignored for all non-root processes.
         * On MPI_IN_PLACE version we use receive buffer as the send buffer.
         */
    }
    else
    {
        ompi_java_getReadPtr(&sPtr,&sItem,env,sBuf,sdb,sOff,count,type,bType);

        if(rootOrInter)
            ompi_java_getWritePtr(&rPtr, &rItem, env, rBuf, rdb, count, type);
    }

    MPI_Op op = ompi_java_op_getHandle(env, jOp, hOp, bType);
    int rc = MPI_Reduce(sPtr, rPtr, count, type, op, root, comm);
    ompi_java_exceptionCheck(env, rc);

    if(sBuf != NULL)
        ompi_java_releaseReadPtr(sPtr, sItem, sBuf, sdb);

    if(rootOrInter)
        ompi_java_releaseWritePtr(rPtr,rItem,env,rBuf,rdb,rOff,count,type,bType);
    else if(sBuf == NULL)
        ompi_java_releaseReadPtr(rPtr, rItem, rBuf, rdb);
}

JNIEXPORT jlong JNICALL Java_mpi_Comm_iReduce(
        JNIEnv *env, jobject jthis, jlong jComm,
        jobject sendBuf, jobject recvBuf, int count,
        jlong type, jint baseType, jobject jOp, jlong hOp, jint root)
{
    MPI_Comm comm = (MPI_Comm)jComm;
    int rank = getRank(env, comm);
    int rootOrInter = rank == root || isInter(env, comm);

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

    MPI_Op op = ompi_java_op_getHandle(env, jOp, hOp, baseType);

    int rc = MPI_Ireduce(sPtr, rPtr, count, (MPI_Datatype)type,
                         op, root, comm, &request);

    ompi_java_exceptionCheck(env, rc);
    return (jlong)request;
}

JNIEXPORT void JNICALL Java_mpi_Comm_allReduce(
        JNIEnv *env, jobject jthis, jlong jComm,
        jobject sBuf, jboolean sdb, jint sOff,
        jobject rBuf, jboolean rdb, jint rOff,
        jint count, jlong jType, jint bType, jobject jOp, jlong hOp)
{
    MPI_Comm     comm = (MPI_Comm)jComm;
    MPI_Datatype type = (MPI_Datatype)jType;

    void *sPtr, *rPtr;
    ompi_java_buffer_t *sItem, *rItem;

    if(sBuf == NULL)
    {
        sPtr = MPI_IN_PLACE;
        ompi_java_getReadPtr(&rPtr,&rItem,env,rBuf,rdb,rOff,count,type,bType);
    }
    else
    {
        ompi_java_getReadPtr(&sPtr,&sItem,env,sBuf,sdb,sOff,count,type,bType);
        ompi_java_getWritePtr(&rPtr, &rItem, env, rBuf, rdb, count, type);
    }

    MPI_Op op = ompi_java_op_getHandle(env, jOp, hOp, bType);
    int rc = MPI_Allreduce(sPtr, rPtr, count, type, op, comm);
    ompi_java_exceptionCheck(env, rc);

    if(sBuf != NULL)
        ompi_java_releaseReadPtr(sPtr, sItem, sBuf, sdb);

    ompi_java_releaseWritePtr(rPtr,rItem,env,rBuf,rdb,rOff,count,type,bType);
}

JNIEXPORT jlong JNICALL Java_mpi_Comm_iAllReduce(
        JNIEnv *env, jobject jthis, jlong comm,
        jobject sendBuf, jobject recvBuf, jint count,
        jlong type, jint baseType, jobject jOp, jlong hOp)
{
    MPI_Request request;
    void *sPtr, *rPtr;

    if(sendBuf == NULL)
        sPtr = MPI_IN_PLACE;
    else
        sPtr = (*env)->GetDirectBufferAddress(env, sendBuf);

    rPtr = (*env)->GetDirectBufferAddress(env, recvBuf);
    MPI_Op op = ompi_java_op_getHandle(env, jOp, hOp, baseType);

    int rc = MPI_Iallreduce(sPtr, rPtr, count, (MPI_Datatype)type,
                            op, (MPI_Comm)comm, &request);

    ompi_java_exceptionCheck(env, rc);
    return (jlong)request;
}

JNIEXPORT void JNICALL Java_mpi_Comm_reduceScatter(
        JNIEnv *env, jobject jthis, jlong jComm,
        jobject sBuf, jboolean sdb, jint sOff,
        jobject rBuf, jboolean rdb, jint rOff,
        jintArray rCounts, jlong jType, jint bType, jobject jOp, jlong hOp)
{
    MPI_Comm     comm = (MPI_Comm)jComm;
    MPI_Datatype type = (MPI_Datatype)jType;

    jint *jRCounts;
    int  *cRCounts;
    ompi_java_getIntArray(env, rCounts, &jRCounts, &cRCounts);

    int size  = getGroupSize(env, comm),
        count = getSum(cRCounts, size),
        rbCnt; /* Receive buffer count */

    void *sPtr, *rPtr;
    ompi_java_buffer_t *sItem, *rItem;

    if(sBuf == NULL)
    {
        sPtr  = MPI_IN_PLACE;
        rbCnt = count;
        ompi_java_getReadPtr(&rPtr,&rItem,env,rBuf,rdb,rOff,count,type,bType);
    }
    else
    {
        ompi_java_getReadPtr(&sPtr,&sItem,env,sBuf,sdb,sOff,count,type,bType);
        rbCnt = cRCounts[getRank(env, comm)];
        ompi_java_getWritePtr(&rPtr, &rItem, env, rBuf, rdb, rbCnt, type);
    }

    MPI_Op op = ompi_java_op_getHandle(env, jOp, hOp, bType);
    int rc = MPI_Reduce_scatter(sPtr, rPtr, cRCounts, type, op, comm);
    ompi_java_exceptionCheck(env, rc);

    if(sBuf != NULL)
        ompi_java_releaseReadPtr(sPtr, sItem, sBuf, sdb);

    ompi_java_releaseWritePtr(rPtr,rItem,env,rBuf,rdb,rOff,rbCnt,type,bType);
    ompi_java_forgetIntArray(env, rCounts, jRCounts, cRCounts);
}

JNIEXPORT jlong JNICALL Java_mpi_Comm_iReduceScatter(
        JNIEnv *env, jobject jthis, jlong comm,
        jobject sendBuf, jobject recvBuf, jintArray rCounts,
        jlong type, int bType, jobject jOp, jlong hOp)
{
    void *sPtr, *rPtr;

    if(sendBuf == NULL)
        sPtr = MPI_IN_PLACE;
    else
        sPtr = (*env)->GetDirectBufferAddress(env, sendBuf);

    rPtr = (*env)->GetDirectBufferAddress(env, recvBuf);
    MPI_Op op = ompi_java_op_getHandle(env, jOp, hOp, bType);
    MPI_Request request;

    jint *jRCounts;
    int  *cRCounts;
    ompi_java_getIntArray(env, rCounts, &jRCounts, &cRCounts);

    int rc = MPI_Ireduce_scatter(sPtr, rPtr, cRCounts, (MPI_Datatype)type,
                                 op, (MPI_Comm)comm, &request);

    ompi_java_exceptionCheck(env, rc);
    ompi_java_forgetIntArray(env, rCounts, jRCounts, cRCounts);
    return (jlong)request;
}

JNIEXPORT void JNICALL Java_mpi_Comm_reduceScatterBlock(
        JNIEnv *env, jobject jthis, jlong jComm,
        jobject sBuf, jboolean sdb, jint sOff,
        jobject rBuf, jboolean rdb, jint rOff,
        jint rCount, jlong jType, jint bType, jobject jOp, jlong hOp)
{
    MPI_Comm     comm = (MPI_Comm)jComm;
    MPI_Datatype type = (MPI_Datatype)jType;

    void *sPtr, *rPtr;
    ompi_java_buffer_t *sItem, *rItem;

    int count = rCount * getGroupSize(env, comm),
        rbCnt; /* Receive buffer count */

    if(sBuf == NULL)
    {
        sPtr  = MPI_IN_PLACE;
        rbCnt = count;
        ompi_java_getReadPtr(&rPtr,&rItem,env,rBuf,rdb,rOff,count,type,bType);
    }
    else
    {
        ompi_java_getReadPtr(&sPtr,&sItem,env,sBuf,sdb,sOff,count,type,bType);
        rbCnt = rCount;
        ompi_java_getWritePtr(&rPtr, &rItem, env, rBuf, rdb, rbCnt, type);
    }

    MPI_Op op = ompi_java_op_getHandle(env, jOp, hOp, bType);
    int rc = MPI_Reduce_scatter_block(sPtr, rPtr, rCount, type, op, comm);
    ompi_java_exceptionCheck(env, rc);

    if(sBuf != NULL)
        ompi_java_releaseReadPtr(sPtr, sItem, sBuf, sdb);

    ompi_java_releaseWritePtr(rPtr,rItem,env,rBuf,rdb,rOff,rbCnt,type,bType);
}

JNIEXPORT jlong JNICALL Java_mpi_Comm_iReduceScatterBlock(
        JNIEnv *env, jobject jthis, jlong comm, jobject sendBuf,
        jobject recvBuf, jint count, jlong type, jint bType,
        jobject jOp, jlong hOp)
{
    void *sPtr, *rPtr;

    if(sendBuf == NULL)
        sPtr = MPI_IN_PLACE;
    else
        sPtr = (*env)->GetDirectBufferAddress(env, sendBuf);

    rPtr = (*env)->GetDirectBufferAddress(env, recvBuf);
    MPI_Op op = ompi_java_op_getHandle(env, jOp, hOp, bType);
    MPI_Request request;

    int rc = MPI_Ireduce_scatter_block(sPtr, rPtr, count, (MPI_Datatype)type,
                                       op, (MPI_Comm)comm, &request);

    ompi_java_exceptionCheck(env, rc);
    return (jlong)request;
}

JNIEXPORT void JNICALL Java_mpi_Comm_reduceLocal(
        JNIEnv *env, jclass clazz, jobject inBuf, jboolean idb, jint inOff,
        jobject inOutBuf, jboolean iodb, jint inOutOff, jint count,
        jlong jType, jlong op)
{
    MPI_Datatype type = (MPI_Datatype)jType;
    void *inPtr, *inBase, *inOutPtr, *inOutBase;
    inPtr = getBufCritical(&inBase, env, inBuf, idb, inOff);
    inOutPtr = getBufCritical(&inOutBase, env, inOutBuf, iodb, inOutOff);
    int rc = MPI_Reduce_local(inPtr, inOutPtr, count, type, (MPI_Op)op);
    ompi_java_exceptionCheck(env, rc);
    releaseBufCritical(env, inBuf, idb, inBase);
    releaseBufCritical(env, inOutBuf, iodb, inOutBase);
}

JNIEXPORT void JNICALL Java_mpi_Comm_reduceLocalUf(
        JNIEnv *env, jclass clazz, jobject inBuf, jboolean idb, jint inOff,
        jobject inOutBuf, jboolean iodb, jint inOutOff, jint count,
        jlong jType, jint bType, jobject jOp, jlong hOp)
{
    MPI_Datatype type = (MPI_Datatype)jType;
    void *inPtr, *inOutPtr;
    ompi_java_buffer_t *inItem, *inOutItem;

    ompi_java_getReadPtr(&inPtr, &inItem, env, inBuf,
                         idb, inOff, count, type, bType);
    ompi_java_getReadPtr(&inOutPtr, &inOutItem, env, inOutBuf,
                         iodb, inOutOff, count, type, bType);

    MPI_Op op = ompi_java_op_getHandle(env, jOp, hOp, bType);
    int rc = MPI_Reduce_local(inPtr, inOutPtr, count, type, op);

    ompi_java_exceptionCheck(env, rc);
    ompi_java_releaseReadPtr(inPtr, inItem, inBuf, idb);

    ompi_java_releaseWritePtr(inOutPtr, inOutItem, env, inOutBuf,
                              iodb, inOutOff, count, type, bType);
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
