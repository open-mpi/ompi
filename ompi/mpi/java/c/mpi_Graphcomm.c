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
 * File         : mpi_Graphcomm.c
 * Headerfile   : mpi_Graphcomm.h 
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
#include "mpi_Graphcomm.h"
#include "mpiJava.h"


/*
 * Class:     mpi_Graphcomm
 * Method:    Get
 * Signature: ()Lmpi/GraphParms;
 */
JNIEXPORT jobject JNICALL Java_mpi_Graphcomm_Get(JNIEnv *env, jobject jthis)
{
    jintArray index, edges;
    jint *ind, *edg;
    jboolean isCopy=JNI_TRUE;
    int maxind, maxedg;

    jclass graphparms_class=(*env)->FindClass(env,"mpi/GraphParms");
    jfieldID indexID,edgesID;
    jmethodID handleConstructorID = (*env)->GetMethodID(env,
                                                        graphparms_class, "<init>", "()V");
    jobject graphparms=(*env)->NewObject(env,graphparms_class, handleConstructorID);

    ompi_java_clearFreeList(env) ;

    MPI_Graphdims_get((MPI_Comm)((*env)->GetLongField(env,jthis,ompi_java.CommhandleID)),&maxind,&maxedg);
    index=(*env)->NewIntArray(env,maxind);
    edges=(*env)->NewIntArray(env,maxedg);
    ind=(*env)->GetIntArrayElements(env,index,&isCopy);
    edg=(*env)->GetIntArrayElements(env,edges,&isCopy);

    MPI_Graph_get((MPI_Comm)((*env)->GetLongField(env,jthis,ompi_java.CommhandleID)),
                  maxind,maxedg, (int*)ind, (int*)edg);

    (*env)->ReleaseIntArrayElements(env,index,ind,0);
    (*env)->ReleaseIntArrayElements(env,edges,edg,0);

    indexID=(*env)->GetFieldID(env,graphparms_class,"index","[I");
    edgesID=(*env)->GetFieldID(env,graphparms_class , "edges", "[I");

    (*env)->SetObjectField(env, graphparms, indexID, index);
    (*env)->SetObjectField(env, graphparms, edgesID, edges);

    /* printf("Graphcomm Get finished.\n"); */
    return graphparms;

}

/*
 * Class:     mpi_Graphcomm
 * Method:    Neighbours
 * Signature: (I)[I
 */
JNIEXPORT jintArray JNICALL Java_mpi_Graphcomm_Neighbours(JNIEnv *env, jobject jthis, jint rank)
{
    jint *neighbors;
    jboolean isCopy=JNI_TRUE;
    jintArray jneighbors;
    int maxns;

    ompi_java_clearFreeList(env) ;

    MPI_Graph_neighbors_count((MPI_Comm)((*env)->GetLongField(env,jthis,ompi_java.CommhandleID)),rank,&maxns);
    jneighbors=(*env)->NewIntArray(env,maxns);
    neighbors=(*env)->GetIntArrayElements(env,jneighbors,&isCopy);
    MPI_Graph_neighbors((MPI_Comm)((*env)->GetLongField(env,jthis,ompi_java.CommhandleID)),
                        rank,maxns,(int*)neighbors);
    (*env)->ReleaseIntArrayElements(env,jneighbors,neighbors,0);
    return jneighbors;
}

/*
 * Class:     mpi_Graphcomm
 * Method:    Map
 * Signature: ([I[I)I
 */
JNIEXPORT jint JNICALL Java_mpi_Graphcomm_Map(JNIEnv *env, jobject jthis, jintArray index, jintArray edges)
{
    int newrank;
    jint *ind, *edg;
    jboolean isCopy=JNI_TRUE;
    int nnodes;

    ompi_java_clearFreeList(env) ;

    nnodes=(*env)->GetArrayLength(env,index);
    ind=(*env)->GetIntArrayElements(env,index,&isCopy);
    edg=(*env)->GetIntArrayElements(env,edges,&isCopy);

    MPI_Graph_map((MPI_Comm)((*env)->GetLongField(env,jthis,ompi_java.CommhandleID)),
                  nnodes,(int*)index,(int*)edges, &newrank);
    (*env)->ReleaseIntArrayElements(env,index,ind,0);
    (*env)->ReleaseIntArrayElements(env,edges,edg,0);
    return newrank;
}


