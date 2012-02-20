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
 * File         : mpi_Cartcomm.c
 * Headerfile   : mpi_Cartcomm.h 
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
#include "mpi_Cartcomm.h"
#include "mpiJava.h"

/*
 * Class:     mpi_Cartcomm
 * Method:    Get
 * Signature: (I)Lmpi/CartParms;
 */
JNIEXPORT jobject JNICALL Java_mpi_Cartcomm_Get(JNIEnv *env, jobject jthis)
{
    jintArray dims, coords;
    jbooleanArray periods;
    jint *ds, *cs; 
    jboolean *ps;
    int *ips ;
    jboolean isCopy1=JNI_TRUE, isCopy2=JNI_TRUE ,isCopy3=JNI_TRUE;
    int maxdims;
    int i ;

    jclass cartparms_class=(*env)->FindClass(env,"mpi/CartParms");
    jfieldID dimsID,periodsID,coordsID;
    jmethodID handleConstructorID = 
        (*env)->GetMethodID(env, cartparms_class, "<init>", "()V");
    jobject cartparms = 
        (*env)->NewObject(env,cartparms_class, handleConstructorID);

    ompi_java_clearFreeList(env) ;

    MPI_Cartdim_get((MPI_Comm)((*env)->GetLongField(env,jthis,ompi_java.CommhandleID)),&maxdims);

    dims=(*env)->NewIntArray(env,maxdims);
    periods=(*env)->NewBooleanArray(env,maxdims);
    coords=(*env)->NewIntArray(env,maxdims);

    ips = (int*) malloc(sizeof(int) * maxdims) ;

    ds=(*env)->GetIntArrayElements(env,dims,&isCopy1);

    cs=(*env)->GetIntArrayElements(env,coords,&isCopy3);

    MPI_Cart_get((MPI_Comm)((*env)->GetLongField(env,jthis,ompi_java.CommhandleID)),
                 maxdims, (int*)ds, ips, (int*)cs);

    ps=(*env)->GetBooleanArrayElements(env,periods,&isCopy2);

    for (i = 0 ; i < maxdims ; i++) {
        ps [i] = ips [i] ? JNI_TRUE : JNI_FALSE ;
    }

    dimsID=(*env)->GetFieldID(env,cartparms_class,"dims","[I");
    periodsID=(*env)->GetFieldID(env,cartparms_class,"periods","[Z");
    coordsID=(*env)->GetFieldID(env,cartparms_class , "coords", "[I");

    (*env)->SetObjectField(env, cartparms, dimsID, dims);
    (*env)->SetObjectField(env, cartparms, periodsID, periods);
    (*env)->SetObjectField(env, cartparms, coordsID, coords);

    (*env)->ReleaseIntArrayElements(env,dims,ds,0);
    (*env)->ReleaseBooleanArrayElements(env,periods,ps,0);
    (*env)->ReleaseIntArrayElements(env,coords,cs,0);

    return cartparms;
}

/*
 * Class:     mpi_Cartcomm
 * Method:    Shift
 * Signature: (II)Lmpi/ShiftParms;
 */
JNIEXPORT jobject JNICALL Java_mpi_Cartcomm_Shift(JNIEnv *env, jobject jthis,
                                                  jint direction, jint disp)
{
    int sr, dr;
    jclass shiftparms_class=(*env)->FindClass(env,"mpi/ShiftParms");
    jfieldID rsID,rdID;
    jmethodID handleConstructorID = (*env)->GetMethodID(env,
                                                        shiftparms_class, "<init>", "()V");
    jobject shiftparms=(*env)->NewObject(env,shiftparms_class,
                                         handleConstructorID);

    ompi_java_clearFreeList(env) ;

    MPI_Cart_shift((MPI_Comm)((*env)->GetLongField(env,jthis,ompi_java.CommhandleID)), 
                   direction, disp, &sr, &dr);
    rsID=(*env)->GetFieldID(env,shiftparms_class,"rank_source","I");
    rdID=(*env)->GetFieldID(env,shiftparms_class,"rank_dest", "I");
    (*env)->SetIntField(env, shiftparms, rsID, sr);
    (*env)->SetIntField(env, shiftparms, rdID, dr);
    /* printf("Shift finished.\n"); */
    return shiftparms;
}

/*
 * Class:     mpi_Cartcomm
 * Method:    Coords
 * Signature: (I)[I
 */
JNIEXPORT jintArray JNICALL Java_mpi_Cartcomm_Coords(JNIEnv *env, jobject jthis, jint rank)
{
    jint *coords;
    jboolean isCopy=JNI_TRUE;
    jintArray jcoords;
    int maxdims;
    /*
      jclass jthis_class=(*env)->FindClass(env,"mpi/Cartcomm");
      jfieldID maxdimsID=(*env)->GetFieldID(env,jthis_class,"maxdims","I");
      maxdims=(*env)->GetIntField(env,jthis, maxdimsID);
    */

    ompi_java_clearFreeList(env) ;

    MPI_Cartdim_get((MPI_Comm)((*env)->GetLongField(env,jthis,ompi_java.CommhandleID)),
                    &maxdims);
    jcoords=(*env)->NewIntArray(env,maxdims);
    coords=(*env)->GetIntArrayElements(env,jcoords,&isCopy);
    MPI_Cart_coords((MPI_Comm)((*env)->GetLongField(env,jthis,ompi_java.CommhandleID)),
                    rank,maxdims,(int*)coords);
    (*env)->ReleaseIntArrayElements(env,jcoords,coords,0);
    return jcoords;
}

/*
 * Class:     mpi_Cartcomm
 * Method:    Map
 * Signature: ([I[Z)I
 */
JNIEXPORT jint JNICALL Java_mpi_Cartcomm_Map(JNIEnv *env, jobject jthis,
                                             jintArray dims, jbooleanArray periods)
{
    int newrank;
    jint *ds;
    jboolean *ps;
    jboolean isCopy=JNI_TRUE;
    int ndims;
    int *int_re_ds=(int*)calloc((*env)->GetArrayLength(env,periods), sizeof(int));
    int i;

    ompi_java_clearFreeList(env) ;

    ndims=(*env)->GetArrayLength(env,dims);
    ds=(*env)->GetIntArrayElements(env,dims,&isCopy);
    ps=(*env)->GetBooleanArrayElements(env,periods,&isCopy);

    for (i=0;i<=(*env)->GetArrayLength(env,periods);i++)
        if(ps[i]==JNI_TRUE)
	    int_re_ds[i]=1;
        else
	    int_re_ds[i]=0;

    MPI_Cart_map((MPI_Comm)((*env)->GetLongField(env,jthis,ompi_java.CommhandleID)),
                 ndims,(int*)ds,int_re_ds, &newrank);
    (*env)->ReleaseIntArrayElements(env,dims,ds,0);
    (*env)->ReleaseBooleanArrayElements(env,periods,ps,0);
    free(int_re_ds);
    return newrank;
}

/*
 * Class:     mpi_Cartcomm
 * Method:    Rank
 * Signature: ([I)I
 */
JNIEXPORT jint JNICALL Java_mpi_Cartcomm_Rank(JNIEnv *env, jobject jthis, jintArray coords)
{
    int rank;
    jint *crds;
    jboolean isCopy=JNI_TRUE;

    ompi_java_clearFreeList(env) ;

    crds=(*env)->GetIntArrayElements(env,coords,&isCopy);
    MPI_Cart_rank((MPI_Comm)((*env)->GetLongField(env,jthis,ompi_java.CommhandleID)),
                  (int*)crds, &rank);
    (*env)->ReleaseIntArrayElements(env,coords,crds,0);
    return rank;
}

/*
 * Class:     mpi_Cartcomm
 * Method:    Sub
 * Signature: ([Z)Lmpi/Cartcomm;
 */
JNIEXPORT jlong JNICALL Java_mpi_Cartcomm_sub(JNIEnv *env, jobject jthis,
                                              jbooleanArray remain_dims)
{
    MPI_Comm newcomm;
    jboolean *re_ds;
    jboolean isCopy=JNI_TRUE;
    int *int_re_ds=(int*)calloc((*env)->GetArrayLength(env,remain_dims), sizeof(int));
    int i;

    ompi_java_clearFreeList(env) ;

    re_ds=(*env)->GetBooleanArrayElements(env,remain_dims,&isCopy);
    for(i=0;i<=(*env)->GetArrayLength(env,remain_dims);i++)
        if(re_ds[i]==JNI_TRUE)
	    int_re_ds[i]=1;
        else
	    int_re_ds[i]=0;

    MPI_Cart_sub((MPI_Comm)((*env)->GetLongField(env,jthis,ompi_java.CommhandleID)),
                 int_re_ds, &newcomm);
    (*env)->ReleaseBooleanArrayElements(env,remain_dims,re_ds,0);

    free(int_re_ds);
    return (jlong)newcomm;
}

/*
 * Class:     mpi_Cartcomm
 * Method:    Dims_create
 * Signature: (I[I)V
 */
JNIEXPORT void JNICALL Java_mpi_Cartcomm_Dims_1create(JNIEnv *env, jclass jthis,
                                                      jint nnodes, jintArray dims )
{
    jint *cdims;
    jboolean isCopy=JNI_TRUE;
    int ndims = (*env)->GetArrayLength(env,dims) ;

    ompi_java_clearFreeList(env) ;

    cdims=(*env)->GetIntArrayElements(env,dims,&isCopy);
    MPI_Dims_create(nnodes,ndims,(int*)cdims);
    (*env)->ReleaseIntArrayElements(env,dims,cdims,0);
}
