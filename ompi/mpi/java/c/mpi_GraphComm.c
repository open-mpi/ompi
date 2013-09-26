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
 * File         : mpi_GraphComm.c
 * Headerfile   : mpi_GraphComm.h 
 * Author       : Xinying Li
 * Created      : Thu Apr  9 12:22:15 1998
 * Revision     : $Revision: 1.2 $
 * Updated      : $Date: 2003/01/16 16:39:34 $
 * Copyright: Northeast Parallel Architectures Center
 *            at Syracuse University 1998
 */
#include "ompi_config.h"

#ifdef HAVE_TARGETCONDITIONALS_H
#include <TargetConditionals.h>
#endif

#include "mpi.h"
#include "mpi_GraphComm.h"
#include "mpiJava.h"

JNIEXPORT void JNICALL Java_mpi_GraphComm_init(JNIEnv *env, jclass clazz)
{
    ompi_java.GraphParmsInit = (*env)->GetMethodID(env,
            ompi_java.GraphParmsClass, "<init>", "([I[I)V");
    ompi_java.DistGraphNeighborsInit = (*env)->GetMethodID(env,
            ompi_java.DistGraphNeighborsClass, "<init>", "([I[I[I[IZ)V");
}

JNIEXPORT jobject JNICALL Java_mpi_GraphComm_getDims(
        JNIEnv *env, jobject jthis, jlong comm)
{
    int maxInd, maxEdg;
    int rc = MPI_Graphdims_get((MPI_Comm)comm, &maxInd, &maxEdg);

    if(ompi_java_exceptionCheck(env, rc))
        return NULL;

    jintArray index = (*env)->NewIntArray(env, maxInd),
              edges = (*env)->NewIntArray(env, maxEdg);

    jint *jIndex, *jEdges;
    int  *cIndex, *cEdges;
    ompi_java_getIntArray(env, index, &jIndex, &cIndex);
    ompi_java_getIntArray(env, edges, &jEdges, &cEdges);

    rc = MPI_Graph_get((MPI_Comm)comm, maxInd, maxEdg, cIndex, cEdges);
    ompi_java_exceptionCheck(env, rc);

    ompi_java_releaseIntArray(env, index, jIndex, cIndex);
    ompi_java_releaseIntArray(env, edges, jEdges, cEdges);

    return (*env)->NewObject(env, ompi_java.GraphParmsClass,
                             ompi_java.GraphParmsInit, index, edges);
}

JNIEXPORT jintArray JNICALL Java_mpi_GraphComm_getNeighbors(
        JNIEnv *env, jobject jthis, jlong comm, jint rank)
{
    int maxNs;
    int rc = MPI_Graph_neighbors_count((MPI_Comm)comm, rank, &maxNs);

    if(ompi_java_exceptionCheck(env, rc))
        return NULL;

    jintArray neighbors = (*env)->NewIntArray(env, maxNs);
    jint *jNeighbors;
    int  *cNeighbors;
    ompi_java_getIntArray(env, neighbors, &jNeighbors, &cNeighbors);

    rc = MPI_Graph_neighbors((MPI_Comm)comm, rank, maxNs, cNeighbors);
    ompi_java_exceptionCheck(env, rc);

    ompi_java_releaseIntArray(env, neighbors, jNeighbors, cNeighbors);
    return neighbors;
}

JNIEXPORT jobject JNICALL Java_mpi_GraphComm_getDistGraphNeighbors(
        JNIEnv *env, jobject jthis, jlong comm)
{
    int inDegree, outDegree, weighted;

    int rc = MPI_Dist_graph_neighbors_count(
             (MPI_Comm)comm, &inDegree, &outDegree, &weighted);

    if(ompi_java_exceptionCheck(env, rc))
        return NULL;

    jintArray sources      = (*env)->NewIntArray(env, inDegree),
              srcWeights   = (*env)->NewIntArray(env, inDegree),
              destinations = (*env)->NewIntArray(env, outDegree),
              destWeights  = (*env)->NewIntArray(env, outDegree);

    jint *jSources, *jSrcWeights, *jDestinations, *jDestWeights;
    int  *cSources, *cSrcWeights, *cDestinations, *cDestWeights;

    ompi_java_getIntArray(env, sources,      &jSources,      &cSources);
    ompi_java_getIntArray(env, srcWeights,   &jSrcWeights,   &cSrcWeights);
    ompi_java_getIntArray(env, destinations, &jDestinations, &cDestinations);
    ompi_java_getIntArray(env, destWeights,  &jDestWeights,  &cDestWeights);

    rc = MPI_Dist_graph_neighbors((MPI_Comm)comm,
            inDegree, cSources, cSrcWeights,
            outDegree, cDestinations, cDestWeights);

    ompi_java_exceptionCheck(env, rc);
    ompi_java_releaseIntArray(env, sources,      jSources,      cSources);
    ompi_java_releaseIntArray(env, srcWeights,   jSrcWeights,   cSrcWeights);
    ompi_java_releaseIntArray(env, destinations, jDestinations, cDestinations);
    ompi_java_releaseIntArray(env, destWeights,  jDestWeights,  cDestWeights);

    return (*env)->NewObject(env,
           ompi_java.DistGraphNeighborsClass, ompi_java.DistGraphNeighborsInit,
           sources, srcWeights, destinations, destWeights,
           weighted ? JNI_TRUE : JNI_FALSE);
}

JNIEXPORT jint JNICALL Java_mpi_GraphComm_map(
        JNIEnv *env, jobject jthis, jlong comm,
        jintArray index, jintArray edges)
{
    int nNodes = (*env)->GetArrayLength(env, index);
    jint *jIndex, *jEdges;
    int  *cIndex, *cEdges;
    ompi_java_getIntArray(env, index, &jIndex, &cIndex);
    ompi_java_getIntArray(env, edges, &jEdges, &cEdges);

    int newrank;
    int rc = MPI_Graph_map((MPI_Comm)comm, nNodes, cIndex, cEdges, &newrank);
    ompi_java_exceptionCheck(env, rc);

    ompi_java_releaseIntArray(env, index, jIndex, cIndex);
    ompi_java_releaseIntArray(env, edges, jEdges, cEdges);
    return newrank;
}
