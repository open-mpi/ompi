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
 * File         : mpi_Intracomm.c
 * Headerfile   : mpi_Intracomm.h
 * Author       : Xinying Li, Bryan Carpenter
 * Created      : Thu Apr  9 12:22:15 1998
 * Revision     : $Revision: 1.10 $
 * Updated      : $Date: 2003/01/16 16:39:34 $
 * Copyright: Northeast Parallel Architectures Center
 *            at Syracuse University 1998
 */

#include "ompi_config.h"

#include <stdlib.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_TARGETCONDITIONALS_H
#include <TargetConditionals.h>
#endif

#include "mpi.h"
#include "mpi_Comm.h"
#include "mpi_Intracomm.h"
#include "mpiJava.h"

JNIEXPORT jlong JNICALL Java_mpi_Intracomm_split(
        JNIEnv *env, jobject jthis, jlong comm, jint colour, jint key)
{
    MPI_Comm newcomm;
    int rc = MPI_Comm_split((MPI_Comm)comm, colour, key, &newcomm);
    ompi_java_exceptionCheck(env, rc);
    return (jlong)newcomm;
}

JNIEXPORT jlong JNICALL Java_mpi_Intracomm_create(
        JNIEnv *env, jobject jthis, jlong comm, jlong group)
{
    MPI_Comm newcomm;
    int rc = MPI_Comm_create((MPI_Comm)comm, (MPI_Group)group, &newcomm);
    ompi_java_exceptionCheck(env, rc);
    return (jlong)newcomm;
}

JNIEXPORT jlong JNICALL Java_mpi_Intracomm_createCart(
        JNIEnv *env, jobject jthis, jlong comm,
        jintArray dims, jbooleanArray periods, jboolean reorder)
{
    jint *jDims;
    int  *cDims;
    ompi_java_getIntArray(env, dims, &jDims, &cDims);

    jboolean *jPeriods;
    int      *cPeriods;
    ompi_java_getBooleanArray(env, periods, &jPeriods, &cPeriods);

    int ndims = (*env)->GetArrayLength(env, dims);
    MPI_Comm cart;

    int rc = MPI_Cart_create((MPI_Comm)comm, ndims, cDims,
                             cPeriods, reorder, &cart);

    ompi_java_exceptionCheck(env, rc);
    ompi_java_forgetIntArray(env, dims, jDims, cDims);
    ompi_java_forgetBooleanArray(env, periods, jPeriods, cPeriods);
    return (jlong)cart;
}

JNIEXPORT jlong JNICALL Java_mpi_Intracomm_createGraph(
        JNIEnv *env, jobject jthis, jlong comm,
        jintArray index, jintArray edges, jboolean reorder)
{
    MPI_Comm graph;
    int nnodes = (*env)->GetArrayLength(env, index);

    jint *jIndex, *jEdges;
    int  *cIndex, *cEdges;
    ompi_java_getIntArray(env, index, &jIndex, &cIndex);
    ompi_java_getIntArray(env, edges, &jEdges, &cEdges);

    int rc = MPI_Graph_create((MPI_Comm)comm,
             nnodes, cIndex, cEdges, reorder, &graph);

    ompi_java_exceptionCheck(env, rc);
    ompi_java_forgetIntArray(env, index, jIndex, cIndex);
    ompi_java_forgetIntArray(env, edges, jEdges, cEdges);
    return (jlong)graph;
}

JNIEXPORT jlong JNICALL Java_mpi_Intracomm_createDistGraph(
        JNIEnv *env, jobject jthis, jlong comm, jintArray sources,
        jintArray degrees, jintArray destins, jintArray weights,
        jlong info, jboolean reorder, jboolean weighted)
{
    MPI_Comm graph;
    int nnodes = (*env)->GetArrayLength(env, sources);
    
    jint *jSources, *jDegrees, *jDestins, *jWeights = NULL;
    int  *cSources, *cDegrees, *cDestins, *cWeights = MPI_UNWEIGHTED;
    ompi_java_getIntArray(env, sources, &jSources, &cSources);
    ompi_java_getIntArray(env, degrees, &jDegrees, &cDegrees);
    ompi_java_getIntArray(env, destins, &jDestins, &cDestins);

    if(weighted)
        ompi_java_getIntArray(env, weights, &jWeights, &cWeights);

    int rc = MPI_Dist_graph_create((MPI_Comm)comm,
             nnodes, cSources, cDegrees, cDestins, cWeights,
             (MPI_Info)info, reorder, &graph);

    ompi_java_exceptionCheck(env, rc);
    ompi_java_forgetIntArray(env, sources, jSources, cSources);
    ompi_java_forgetIntArray(env, degrees, jDegrees, cDegrees);
    ompi_java_forgetIntArray(env, destins, jDestins, cDestins);

    if(weighted)
        ompi_java_forgetIntArray(env, weights, jWeights, cWeights);

    return (jlong)graph;
}

JNIEXPORT jlong JNICALL Java_mpi_Intracomm_createDistGraphAdjacent(
        JNIEnv *env, jobject jthis, jlong comm, jintArray sources,
        jintArray srcWeights, jintArray destins, jintArray desWeights,
        jlong info, jboolean reorder, jboolean weighted)
{
    MPI_Comm graph;

    int inDegree  = (*env)->GetArrayLength(env, sources),
        outDegree = (*env)->GetArrayLength(env, destins);
    
    jint *jSources, *jDestins, *jSrcWeights, *jDesWeights;
    int  *cSources, *cDestins, *cSrcWeights, *cDesWeights;
    ompi_java_getIntArray(env, sources, &jSources, &cSources);
    ompi_java_getIntArray(env, destins, &jDestins, &cDestins);
    
    if(weighted)
    {
        ompi_java_getIntArray(env, srcWeights, &jSrcWeights, &cSrcWeights);
        ompi_java_getIntArray(env, desWeights, &jDesWeights, &cDesWeights);
    }
    else
    {
        jSrcWeights = jDesWeights = NULL;
        cSrcWeights = cDesWeights = MPI_UNWEIGHTED;
    }
    
    int rc = MPI_Dist_graph_create_adjacent((MPI_Comm)comm,
             inDegree, cSources, cSrcWeights, outDegree, cDestins,
             cDesWeights, (MPI_Info)info, reorder, &graph);

    ompi_java_exceptionCheck(env, rc);
    ompi_java_forgetIntArray(env, sources, jSources, cSources);
    ompi_java_forgetIntArray(env, destins, jDestins, cDestins);

    if(weighted)
    {
        ompi_java_forgetIntArray(env, srcWeights, jSrcWeights, cSrcWeights);
        ompi_java_forgetIntArray(env, desWeights, jDesWeights, cDesWeights);
    }

    return (jlong)graph;
}

JNIEXPORT void JNICALL Java_mpi_Intracomm_scan(
        JNIEnv *env, jobject jthis, jlong jComm,
        jobject sBuf, jboolean sdb, jint sOff,
        jobject rBuf, jboolean rdb, jint rOff, jint count,
        jlong jType, jint bType, jobject jOp, jlong hOp)
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
    int rc = MPI_Scan(sPtr, rPtr, count, type, op, comm);
    ompi_java_exceptionCheck(env, rc);

    if(sBuf != NULL)
        ompi_java_releaseReadPtr(sPtr, sItem, sBuf, sdb);
    
    ompi_java_releaseWritePtr(rPtr,rItem,env,rBuf,rdb,rOff,count,type,bType);
}

JNIEXPORT jlong JNICALL Java_mpi_Intracomm_iScan(
        JNIEnv *env, jobject jthis, jlong comm,
        jobject sendBuf, jobject recvBuf, jint count,
        jlong type, int baseType, jobject jOp, jlong hOp)
{
    void *sPtr, *rPtr;
    MPI_Request request;

    if(sendBuf == NULL)
        sPtr = MPI_IN_PLACE;
    else
        sPtr = (*env)->GetDirectBufferAddress(env, sendBuf);

    rPtr = (*env)->GetDirectBufferAddress(env, recvBuf);

    int rc = MPI_Iscan(sPtr, rPtr, count, (MPI_Datatype)type,
                       ompi_java_op_getHandle(env, jOp, hOp, baseType),
                       (MPI_Comm)comm, &request);

    ompi_java_exceptionCheck(env, rc);
    return (jlong)request;
}

JNIEXPORT void JNICALL Java_mpi_Intracomm_exScan(
        JNIEnv *env, jobject jthis, jlong jComm,
        jobject sBuf, jboolean sdb, jint sOff,
        jobject rBuf, jboolean rdb, jint rOff, jint count,
        jlong jType, int bType, jobject jOp, jlong hOp)
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
    int rc = MPI_Exscan(sPtr, rPtr, count, type, op, comm);
    ompi_java_exceptionCheck(env, rc);

    if(sBuf != NULL)
        ompi_java_releaseReadPtr(sPtr, sItem, sBuf, sdb);

    ompi_java_releaseWritePtr(rPtr,rItem,env,rBuf,rdb,rOff,count,type,bType);
}

JNIEXPORT jlong JNICALL Java_mpi_Intracomm_iExScan(
        JNIEnv *env, jobject jthis, jlong comm,
        jobject sendBuf, jobject recvBuf, jint count,
        jlong type, int bType, jobject jOp, jlong hOp)
{
    void *sPtr, *rPtr;

    if(sendBuf == NULL)
        sPtr = MPI_IN_PLACE;
    else
        sPtr = (*env)->GetDirectBufferAddress(env, sendBuf);

    rPtr = (*env)->GetDirectBufferAddress(env, recvBuf);
    MPI_Request request;

    int rc = MPI_Iexscan(sPtr, rPtr, count, (MPI_Datatype)type,
                         ompi_java_op_getHandle(env, jOp, hOp, bType),
                         (MPI_Comm)comm, &request);

    ompi_java_exceptionCheck(env, rc);
    return (jlong)request;
}

JNIEXPORT jstring JNICALL Java_mpi_Intracomm_openPort(
                          JNIEnv *env, jclass clazz, jlong info)
{
    char port[MPI_MAX_PORT_NAME + 1];
    int rc = MPI_Open_port((MPI_Info)info, port);

    return ompi_java_exceptionCheck(env, rc)
           ? NULL : (*env)->NewStringUTF(env, port);
}

JNIEXPORT void JNICALL Java_mpi_Intracomm_closePort_1jni(
                       JNIEnv *env, jclass clazz, jstring jport)
{
    const char *port = (*env)->GetStringUTFChars(env, jport, NULL);
    int rc = MPI_Close_port((char*)port);
    ompi_java_exceptionCheck(env, rc);
    (*env)->ReleaseStringUTFChars(env, jport, port);
}

JNIEXPORT jlong JNICALL Java_mpi_Intracomm_accept(
        JNIEnv *env, jobject jthis, jlong comm,
        jstring jport, jlong info, jint root)
{
    const char *port = jport == NULL ? NULL :
                       (*env)->GetStringUTFChars(env, jport, NULL);
    MPI_Comm newComm;

    int rc = MPI_Comm_accept((char*)port, (MPI_Info)info,
                             root, (MPI_Comm)comm, &newComm);

    ompi_java_exceptionCheck(env, rc);
    
    if(jport != NULL)
        (*env)->ReleaseStringUTFChars(env, jport, port);

    return (jlong)newComm;
}

JNIEXPORT jlong JNICALL Java_mpi_Intracomm_connect(
        JNIEnv *env, jobject jthis, jlong comm,
        jstring jport, jlong info, jint root)
{
    const char *port = jport == NULL ? NULL :
                       (*env)->GetStringUTFChars(env, jport, NULL);
    MPI_Comm newComm;

    int rc = MPI_Comm_connect((char*)port, (MPI_Info)info,
                              root, (MPI_Comm)comm, &newComm);

    ompi_java_exceptionCheck(env, rc);

    if(jport != NULL)
        (*env)->ReleaseStringUTFChars(env, jport, port);

    return (jlong)newComm;
}

JNIEXPORT void JNICALL Java_mpi_Intracomm_publishName(
        JNIEnv *env, jclass clazz, jstring jservice, jlong info, jstring jport)
{
    const char *service = (*env)->GetStringUTFChars(env, jservice, NULL),
               *port    = (*env)->GetStringUTFChars(env, jport,    NULL);

    int rc = MPI_Publish_name((char*)service, (MPI_Info)info, (char*)port);
    ompi_java_exceptionCheck(env, rc);

    (*env)->ReleaseStringUTFChars(env, jservice, service);
    (*env)->ReleaseStringUTFChars(env, jport,    port);
}

JNIEXPORT void JNICALL Java_mpi_Intracomm_unpublishName(
        JNIEnv *env, jclass clazz, jstring jservice, jlong info, jstring jport)
{
    const char *service = (*env)->GetStringUTFChars(env, jservice, NULL),
               *port    = (*env)->GetStringUTFChars(env, jport,    NULL);

    int rc = MPI_Unpublish_name((char*)service, (MPI_Info)info, (char*)port);
    ompi_java_exceptionCheck(env, rc);

    (*env)->ReleaseStringUTFChars(env, jservice, service);
    (*env)->ReleaseStringUTFChars(env, jport,    port);
}

JNIEXPORT jstring JNICALL Java_mpi_Intracomm_lookupName(
        JNIEnv *env, jclass clazz, jstring jservice, jlong info)
{
    char port[MPI_MAX_PORT_NAME + 1];
    const char *service = (*env)->GetStringUTFChars(env, jservice, NULL);

    int rc = MPI_Lookup_name((char*)service, (MPI_Info)info, port);
    (*env)->ReleaseStringUTFChars(env, jservice, service);

    return ompi_java_exceptionCheck(env, rc)
           ? NULL : (*env)->NewStringUTF(env, port);
}

JNIEXPORT jlong JNICALL Java_mpi_Intracomm_spawn(
        JNIEnv *env, jobject jthis, jlong comm, jstring jCommand,
        jobjectArray jArgv, jint maxprocs, jlong info, jint root,
        jintArray errCodes)
{
    int i, rc;
    MPI_Comm intercomm;
    const char* command = (*env)->GetStringUTFChars(env, jCommand, NULL);

    jint *jErrCodes;
    int  *cErrCodes = MPI_ERRCODES_IGNORE;

    if(errCodes != NULL)
        ompi_java_getIntArray(env, errCodes, &jErrCodes, &cErrCodes);

    char **argv = MPI_ARGV_NULL;

    if(jArgv != NULL)
    {
        jsize argvLength = (*env)->GetArrayLength(env, jArgv);
        argv = (char**)calloc(argvLength + 1, sizeof(char*));

        for(i = 0; i < argvLength; i++)
        {
            jstring a = (*env)->GetObjectArrayElement(env, jArgv, i);
            argv[i] = strdup((*env)->GetStringUTFChars(env, a, NULL));
            (*env)->DeleteLocalRef(env, a);
        }

        argv[argvLength] = NULL;
    }

    rc = MPI_Comm_spawn((char*)command, argv, maxprocs, (MPI_Info)info,
                        root, (MPI_Comm)comm, &intercomm, cErrCodes);

    ompi_java_exceptionCheck(env, rc);

    if(jArgv != NULL)
    {
        jsize argvLength = (*env)->GetArrayLength(env, jArgv);

        for(i = 0; i < argvLength; i++)
        {
            jstring a = (*env)->GetObjectArrayElement(env, jArgv, i);
            (*env)->ReleaseStringUTFChars(env, a, argv[i]);
            (*env)->DeleteLocalRef(env, a);
        }

        free(argv);
    }

    if(errCodes != NULL)
        ompi_java_releaseIntArray(env, errCodes, jErrCodes, cErrCodes);

    (*env)->ReleaseStringUTFChars(env, jCommand, command);
    return (jlong)intercomm;
}

JNIEXPORT jlong JNICALL Java_mpi_Intracomm_spawnMultiple(
        JNIEnv *env, jobject jthis, jlong comm, jobjectArray jCommands,
        jobjectArray jArgv, jintArray maxProcs, jlongArray info,
        jint root, jintArray errCodes)
{
    int i, rc;
    MPI_Comm intercomm;
    jlong *jInfo = (*env)->GetLongArrayElements(env, info, NULL);

    jint *jMaxProcs, *jErrCodes;
    int  *cMaxProcs, *cErrCodes = MPI_ERRCODES_IGNORE;
    ompi_java_getIntArray(env, maxProcs, &jMaxProcs, &cMaxProcs);

    if(errCodes != NULL)
        ompi_java_getIntArray(env, errCodes, &jErrCodes, &cErrCodes);

    int commandsLength = (*env)->GetArrayLength(env, jCommands),
        infoLength     = (*env)->GetArrayLength(env, info);

    char **commands = calloc(commandsLength, sizeof(char*)),
         ***argv    = MPI_ARGVS_NULL;
    MPI_Info *cInfo = calloc(infoLength, sizeof(MPI_Info));

    for(i = 0; i < infoLength; i++)
        cInfo[i] = (MPI_Info)jInfo[i];

    for(i = 0; i < commandsLength; i++)
    {
        jstring a = (*env)->GetObjectArrayElement(env, jCommands, i);
        commands[i] = (char*)(*env)->GetStringUTFChars(env, a, NULL);
        (*env)->DeleteLocalRef(env, a);
    }

    if(jArgv != NULL)
    {
        int argvLength = (*env)->GetArrayLength(env, jArgv);
        argv = calloc(argvLength, sizeof(char**));

        for(i = 0; i < argvLength; i++)
        {
            jobjectArray arr = (*env)->GetObjectArrayElement(env, jArgv, i);
            int j, length = (*env)->GetArrayLength(env, arr);
            argv[i] = calloc(length + 1, sizeof(char*));

            for(j = 0; j < length; j++)
            {
                jstring a = (*env)->GetObjectArrayElement(env, arr, j);
                argv[i][j] = (char*)(*env)->GetStringUTFChars(env, a, NULL);
                (*env)->DeleteLocalRef(env, a);
            }

            argv[i][length] = NULL;
            (*env)->DeleteLocalRef(env, arr);
        }
    }

    rc = MPI_Comm_spawn_multiple(
            commandsLength, commands, argv, cMaxProcs, cInfo,
            root, (MPI_Comm)comm, &intercomm, cErrCodes);

    ompi_java_exceptionCheck(env, rc);

    if(jArgv != NULL)
    {
        int argvLength = (*env)->GetArrayLength(env, jArgv);

        for(i = 0; i < argvLength; i++)
        {
            jobjectArray arr = (*env)->GetObjectArrayElement(env, jArgv, i);
            int j, length = (*env)->GetArrayLength(env, arr);

            for(j = 0; j < length; j++)
            {
                jstring a = (*env)->GetObjectArrayElement(env, arr, j);
                (*env)->ReleaseStringUTFChars(env, a, argv[i][j]);
                (*env)->DeleteLocalRef(env, a);
            }

            (*env)->DeleteLocalRef(env, arr);
            free(argv[i]);
        }

        free(argv);
    }

    for(i = 0; i < commandsLength; i++)
    {
        jstring a = (*env)->GetObjectArrayElement(env, jCommands, i);
        (*env)->ReleaseStringUTFChars(env, a, commands[i]);
        (*env)->DeleteLocalRef(env, a);
    }

    if(errCodes != NULL)
        ompi_java_releaseIntArray(env, errCodes, jErrCodes, cErrCodes);

    free(cInfo);
    free(commands);
    (*env)->ReleaseLongArrayElements(env, info, jInfo, JNI_ABORT);
    ompi_java_forgetIntArray(env, maxProcs, jMaxProcs, cMaxProcs);
    return (jlong)intercomm;
}
