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
#ifdef HAVE_TARGETCONDITIONALS_H
#include <TargetConditionals.h>
#endif

#include "mpi.h"
#include "mpi_Comm.h"
#include "mpi_Intracomm.h"
#include "mpiJava.h"


/* Collectives are not particularly amenable to the strategies used
 * in point-to-point to reduce copying when the GC does not support pinning.
 *
 * It's possibly doable, but may too complex to be worth the effort.
 * A general problem is that the relation between positions in the
 * original buffer and positions in a packed buffer is not very
 * well-defined.  
 *
 * Collectives that use `Op' have an additional problem that
 * `MPI_User_function' prototype expects the actual user-specified
 * datatype as an argument.  Packing, then operating on data transferred
 * as a more primitive datatype is not generally correct.
 */


extern MPI_Datatype Dts[] ;


/*
 * Class:     mpi_Intracomm
 * Method:    split
 * Signature: (II)J
 */
JNIEXPORT jlong JNICALL Java_mpi_Intracomm_split(JNIEnv *env, jobject jthis,
                                                 jint colour, jint key)
{
    MPI_Comm newcomm;

    ompi_java_clearFreeList(env) ;

    MPI_Comm_split((MPI_Comm)((*env)->GetLongField(env,jthis,ompi_java.CommhandleID)),
                   colour, key, &newcomm);

    return (jlong)newcomm;
}

/*
 * Class:     mpi_Intracomm
 * Method:    creat
 * Signature: (Lmpi/Group;)J
 */
JNIEXPORT jlong JNICALL Java_mpi_Intracomm_creat(JNIEnv *env, jobject jthis,
                                                 jobject group)
{
    MPI_Comm newcomm;

    ompi_java_clearFreeList(env) ;

    MPI_Comm_create((MPI_Comm)((*env)->GetLongField(env,jthis,ompi_java.CommhandleID)),
                    (MPI_Group)((*env)->GetLongField(env,group,ompi_java.GrouphandleID)),
                    &newcomm);
    return (jlong)newcomm;
}

/*
 * Class:     mpi_Intracomm
 * Method:    Barrier
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_mpi_Intracomm_Barrier(JNIEnv *env, jobject jthis)
{
    ompi_java_clearFreeList(env) ;

    MPI_Barrier((MPI_Comm)((*env)->GetLongField(env,jthis,ompi_java.CommhandleID)));
}

/*
 * Class:     mpi_Intracomm
 * Method:    GetCart
 * Signature: ([I[ZZ)J
 */
JNIEXPORT jlong JNICALL Java_mpi_Intracomm_GetCart(JNIEnv *env, jobject jthis,
                                                   jintArray dims, jbooleanArray periods,
                                                   jboolean reorder)
{
    MPI_Comm cart;
    int ndims=(*env)->GetArrayLength(env,dims);
    jboolean isCopy=JNI_TRUE;
    jint *ds; jboolean *ps;
    int i;
    int *int_re_ds=(int*)calloc((*env)->GetArrayLength(env,periods),
                                sizeof(int));

    ompi_java_clearFreeList(env) ;

    ds=(*env)->GetIntArrayElements(env,dims,&isCopy);
    ps=(*env)->GetBooleanArrayElements(env,periods,&isCopy);

    for(i=0;i<=(*env)->GetArrayLength(env,periods);i++)
        if(ps[i]==JNI_TRUE)
            int_re_ds[i]=1;
        else
            int_re_ds[i]=0;

    MPI_Cart_create((MPI_Comm)((*env)->GetLongField(env,jthis,ompi_java.CommhandleID)), 
                    ndims, (int*)ds, int_re_ds, reorder, &cart);
    (*env)->ReleaseIntArrayElements(env,dims,ds,0);
    (*env)->ReleaseBooleanArrayElements(env,periods,ps,0);
    free(int_re_ds);
    return (jlong)cart;
}

/*
 * Class:     mpi_Intracomm
 * Method:    GetGraph
 * Signature: ([I[IZ)J
 */
JNIEXPORT jlong JNICALL Java_mpi_Intracomm_GetGraph(JNIEnv *env, jobject jthis,
                                                    jintArray index, jintArray edges,
                                                    jboolean reorder)
{
    MPI_Comm graph;
    int nnodes=(*env)->GetArrayLength(env,index);
    jboolean isCopy=JNI_TRUE;
    jint *ind, *edg;

    ompi_java_clearFreeList(env) ;

    ind=(*env)->GetIntArrayElements(env,index,&isCopy);
    edg=(*env)->GetIntArrayElements(env,edges,&isCopy);
    MPI_Graph_create((MPI_Comm)((*env)->GetLongField(env,jthis,ompi_java.CommhandleID)),
                     nnodes, (int*)ind, (int*)edg, reorder, &graph);
    (*env)->ReleaseIntArrayElements(env,index,ind,0);
    (*env)->ReleaseIntArrayElements(env,edges,edg,0);
    return (jlong)graph;
}

/*
 * Class:     mpi_Intracomm
 * Method:    bcast
 * Signature: (Ljava/lang/Object;IILmpi/Datatype;I)V
 */
JNIEXPORT void JNICALL Java_mpi_Intracomm_bcast(JNIEnv *env, jobject jthis,
                                                jobject buf, jint offset,
                                                jint count, jobject type, jint root)
{
    MPI_Comm mpi_comm =
        (MPI_Comm)((*env)->GetLongField(env,jthis,ompi_java.CommhandleID)) ;
    MPI_Datatype mpi_type =
        (MPI_Datatype)((*env)->GetLongField(env,type,ompi_java.DatatypehandleID)) ;

    int baseType = (*env)->GetIntField(env, type, ompi_java.DatatypebaseTypeID) ;

    void *bufptr ;

#ifdef GC_DOES_PINNING

    void *bufbase ;

    ompi_java_clearFreeList(env) ;

    bufptr = ompi_java_getBufPtr(&bufbase, env, buf, baseType, offset) ;
    MPI_Bcast(bufptr, count, mpi_type, root, mpi_comm) ;
    ompi_java_releaseBufPtr(env, buf, bufbase, baseType) ;

#else

    int size ;

    ompi_java_clearFreeList(env) ;

    bufptr = ompi_java_getMPIBuf(&size, env, buf, offset,
                                    count, mpi_type, mpi_comm, baseType) ;

    MPI_Bcast(bufptr, size, MPI_BYTE, root, mpi_comm) ;

    ompi_java_releaseMPIBuf(env, buf, offset, count, mpi_type, mpi_comm,
                               bufptr, size, baseType) ;

#endif  /* GC_DOES_PINNING */
}


/*
 * Class:     mpi_Intracomm
 * Method:    Gather
 * Signature:
 (Ljava/lang/Object;IILmpi/Datatype;Ljava/lang/Object;IILmpi/Datatype;I)V
*/
JNIEXPORT void JNICALL Java_mpi_Intracomm_gather(JNIEnv *env, jobject jthis,
                                                 jobject sendbuf, jint sendoffset,
                                                 jint sendcount, jobject sendtype,
                                                 jobject recvbuf, jint recvoffset,
                                                 jint recvcount, jobject recvtype,
                                                 jint root)
{
    int id ;

    MPI_Comm mpi_comm =
        (MPI_Comm)((*env)->GetLongField(env,jthis,ompi_java.CommhandleID)) ;

    MPI_Datatype mpi_stype =
        (MPI_Datatype)((*env)->GetLongField(env,sendtype,ompi_java.DatatypehandleID)) ;
    MPI_Datatype mpi_rtype = (MPI_Datatype)
        ((*env)->GetLongField(env, recvtype, ompi_java.DatatypehandleID)) ;

    int sbaseType = (*env)->GetIntField(env, sendtype, ompi_java.DatatypebaseTypeID) ;
    int rbaseType = -1;

    void *sendptr, *recvptr = NULL;
    void *sbufbase, *rbufbase ;

    ompi_java_clearFreeList(env) ;

    MPI_Comm_rank(mpi_comm, &id) ;
    if(id == root) {
        /* 
         * In principle need the "id == root" check here and elsewere for
         * correctness, in case arguments that are not supposed to be
         * significant except on root are legitimately passed in as `null',
         * say.  Shouldn't produce null pointer exception.
         *
         * (However in this case MPICH complains if `mpi_rtype' is not defined
         * in all processes, notwithstanding what the spec says.)
         */

        rbaseType = (*env)->GetIntField(env, recvtype, ompi_java.DatatypebaseTypeID) ;

        recvptr = ompi_java_getBufPtr(&rbufbase,
                                         env, recvbuf, rbaseType, recvoffset) ;
    }

    sendptr = ompi_java_getBufPtr(&sbufbase, env, sendbuf, sbaseType, sendoffset) ;

    MPI_Gather(sendptr, sendcount, mpi_stype,
               recvptr, recvcount, mpi_rtype, root, mpi_comm) ;

    ompi_java_releaseBufPtr(env, sendbuf, sbufbase, sbaseType) ;

    if (id == root) {
        ompi_java_releaseBufPtr(env, recvbuf, rbufbase, rbaseType);
    }
}

/*
 * Class:     mpi_Intracomm
 * Method:    Gatherv
 * Signature:
 (Ljava/lang/Object;IILmpi/Datatype;Ljava/lang/Object;I[I[ILmpi/Datatype;I)V
*/
JNIEXPORT void JNICALL Java_mpi_Intracomm_gatherv(JNIEnv *env, jobject jthis,
                                                  jobject sendbuf, jint sendoffset,
                                                  jint sendcount, jobject sendtype,
                                                  jobject recvbuf, jint recvoffset,
                                                  jintArray recvcounts, jintArray displs,
                                                  jobject recvtype, jint root)
{
    int id ;
    jint *rcount = NULL, *dps = NULL;
    jboolean isCopy ;

    MPI_Comm mpi_comm =
        (MPI_Comm) ((*env)->GetLongField(env,jthis,ompi_java.CommhandleID)) ;

    MPI_Datatype mpi_stype = (MPI_Datatype)
        ((*env)->GetLongField(env,sendtype,ompi_java.DatatypehandleID)) ;
    MPI_Datatype mpi_rtype = mpi_stype;

    int sbaseType = (*env)->GetIntField(env, sendtype, ompi_java.DatatypebaseTypeID) ;
    int rbaseType = 0;

    void *sendptr, *recvptr = NULL;
    void *sbufbase, *rbufbase ;

    ompi_java_clearFreeList(env) ;

    MPI_Comm_rank(mpi_comm, &id) ;
    if(id == root) {
        rcount=(*env)->GetIntArrayElements(env,recvcounts,&isCopy);
        dps=(*env)->GetIntArrayElements(env,displs,&isCopy);

        mpi_rtype = (MPI_Datatype)
            ((*env)->GetLongField(env,recvtype,ompi_java.DatatypehandleID)) ;

        rbaseType = (*env)->GetIntField(env, recvtype, ompi_java.DatatypebaseTypeID) ;

        recvptr = ompi_java_getBufPtr(&rbufbase,
                                         env, recvbuf, rbaseType, recvoffset) ;
    }

    sendptr = ompi_java_getBufPtr(&sbufbase, env, sendbuf, sbaseType, sendoffset) ;

    MPI_Gatherv(sendptr, sendcount, mpi_stype,
                recvptr, (int*) rcount, (int*) dps, mpi_rtype,
                root, mpi_comm) ;

    ompi_java_releaseBufPtr(env, sendbuf, sbufbase, sbaseType) ;
    if (id == root) {
        ompi_java_releaseBufPtr(env, recvbuf, rbufbase, rbaseType);
    }

    if (id == root) {
        (*env)->ReleaseIntArrayElements(env,recvcounts,rcount,JNI_ABORT);
        (*env)->ReleaseIntArrayElements(env,displs,dps,JNI_ABORT);
    }
}

/*
 * Class:     mpi_Intracomm
 * Method:    Scatter
 * Signature:
 (Ljava/lang/Object;IILmpi/Datatype;Ljava/lang/Object;IILmpi/Datatype;I)V
*/
JNIEXPORT void JNICALL Java_mpi_Intracomm_scatter(JNIEnv *env, jobject jthis,
                                                  jobject sendbuf, jint sendoffset,
                                                  jint sendcount, jobject sendtype,
                                                  jobject recvbuf, jint recvoffset,
                                                  jint recvcount, jobject recvtype,
                                                  jint root)
{
    int id ;
    MPI_Comm mpi_comm =
        (MPI_Comm)((*env)->GetLongField(env,jthis,ompi_java.CommhandleID)) ;

    MPI_Datatype mpi_stype =
        (MPI_Datatype) ((*env)->GetLongField(env,sendtype,ompi_java.DatatypehandleID)) ;
    /* MPICH complains if `mpi_stype' is not defined
     * in all processes, notwithstanding what the spec says. */

    MPI_Datatype mpi_rtype =
        (MPI_Datatype)((*env)->GetLongField(env,recvtype,ompi_java.DatatypehandleID)) ;


    int sbaseType = -1;
    int rbaseType = (*env)->GetIntField(env, recvtype, ompi_java.DatatypebaseTypeID) ;

    void *sendptr = NULL, *recvptr ;
    void *sbufbase, *rbufbase ;

    ompi_java_clearFreeList(env) ;

    MPI_Comm_rank(mpi_comm, &id) ;
    if (id == root) {
        sbaseType = (*env)->GetIntField(env, sendtype, ompi_java.DatatypebaseTypeID) ;
    }

    recvptr = ompi_java_getBufPtr(&rbufbase, env, recvbuf, rbaseType, recvoffset) ;

    if (id == root) {
        sendptr = ompi_java_getBufPtr(&sbufbase,
                                         env, sendbuf, sbaseType, sendoffset);
    }

    MPI_Scatter(sendptr, sendcount, mpi_stype,
                recvptr, recvcount, mpi_rtype, root, mpi_comm) ;

    if (id == root) {
        ompi_java_releaseBufPtr(env, sendbuf, sbufbase, sbaseType);
    }

    ompi_java_releaseBufPtr(env, recvbuf, rbufbase, rbaseType);
}



/*
 * Class:     mpi_Intracomm
 * Method:    Scatterv
 * Signature:
 (Ljava/lang/Object;II[ILmpi/Datatype;Ljava/lang/Object;I[ILmpi/Datatype;I)V
*/
JNIEXPORT void JNICALL Java_mpi_Intracomm_scatterv(JNIEnv *env, jobject jthis,
                                                   jobject sendbuf, jint sendoffset,
                                                   jintArray sendcount, jintArray displs,
                                                   jobject sendtype,
                                                   jobject recvbuf, jint recvoffset,
                                                   jint recvcount, jobject recvtype,
                                                   jint root)
{
    int id ;
    jint *scount = NULL, *dps = NULL;
    jboolean isCopy ;

    MPI_Comm mpi_comm =
        (MPI_Comm)((*env)->GetLongField(env,jthis,ompi_java.CommhandleID)) ;

    MPI_Datatype mpi_rtype =
        (MPI_Datatype)((*env)->GetLongField(env,recvtype,ompi_java.DatatypehandleID)) ;
    MPI_Datatype mpi_stype = mpi_rtype;

    int sbaseType = -1;
    int rbaseType = (*env)->GetIntField(env, recvtype, ompi_java.DatatypebaseTypeID) ;

    void *sendptr = NULL, *recvptr ;
    void *sbufbase, *rbufbase ;

    ompi_java_clearFreeList(env) ;

    MPI_Comm_rank(mpi_comm, &id) ;
    if(id == root) {
        mpi_stype = (MPI_Datatype)
            ((*env)->GetLongField(env,sendtype,ompi_java.DatatypehandleID)) ;

        sbaseType = (*env)->GetIntField(env, sendtype, ompi_java.DatatypebaseTypeID) ;

        scount = (*env)->GetIntArrayElements(env,sendcount,&isCopy);
        dps    = (*env)->GetIntArrayElements(env,displs,&isCopy);
    }

    recvptr = ompi_java_getBufPtr(&rbufbase, env, recvbuf, rbaseType, recvoffset) ;

    if (id == root) {
        sendptr = ompi_java_getBufPtr(&sbufbase,
                                         env, sendbuf, sbaseType, sendoffset);
    }

    MPI_Scatterv(sendptr, (int*) scount, (int*) dps, mpi_stype,
                 recvptr, recvcount, mpi_rtype,
                 root, mpi_comm) ;

    if (id == root) {
        ompi_java_releaseBufPtr(env, sendbuf, sbufbase, sbaseType);
    }

    ompi_java_releaseBufPtr(env, recvbuf, rbufbase, rbaseType) ;

    if (id == root) {
        (*env)->ReleaseIntArrayElements(env, sendcount, scount, JNI_ABORT);
        (*env)->ReleaseIntArrayElements(env, displs, dps, JNI_ABORT);
    }
}

/*
 * Class:     mpi_Intracomm
 * Method:    Allgather
 * Signature:
 (Ljava/lang/Object;IILmpi/Datatype;Ljava/lang/Object;IILmpi/Datatype;)V
*/
JNIEXPORT void JNICALL Java_mpi_Intracomm_allgather(JNIEnv *env, jobject jthis,
                                                    jobject sendbuf, jint sendoffset,
                                                    jint sendcount, jobject sendtype,
                                                    jobject recvbuf, jint recvoffset,
                                                    jint recvcount, jobject recvtype)
{
    MPI_Comm mpi_comm =
        (MPI_Comm)((*env)->GetLongField(env,jthis,ompi_java.CommhandleID)) ;

    MPI_Datatype mpi_stype = (MPI_Datatype)
        ((*env)->GetLongField(env,sendtype,ompi_java.DatatypehandleID)) ;
    MPI_Datatype mpi_rtype = (MPI_Datatype)
        ((*env)->GetLongField(env, recvtype, ompi_java.DatatypehandleID)) ;

    int sbaseType = (*env)->GetIntField(env, sendtype, ompi_java.DatatypebaseTypeID) ;
    int rbaseType = (*env)->GetIntField(env, recvtype, ompi_java.DatatypebaseTypeID) ;

    void *sendptr, *recvptr ;
    void *sbufbase, *rbufbase ;

    ompi_java_clearFreeList(env) ;

    recvptr = ompi_java_getBufPtr(&rbufbase, env, recvbuf, rbaseType, recvoffset) ;
    sendptr = ompi_java_getBufPtr(&sbufbase, env, sendbuf, sbaseType, sendoffset) ;

    MPI_Allgather(sendptr, sendcount, mpi_stype,
                  recvptr, recvcount, mpi_rtype, mpi_comm) ;

    ompi_java_releaseBufPtr(env, sendbuf, sbufbase, sbaseType) ;
    ompi_java_releaseBufPtr(env, recvbuf, rbufbase, rbaseType) ;
}

/*
 * Class:     mpi_Intracomm
 * Method:    Allgatherv
 * Signature:
 (Ljava/lang/Object;IILmpi/Datatype;Ljava/lang/Object;I[I[ILmpi/Datatype;)V
*/
JNIEXPORT void JNICALL Java_mpi_Intracomm_allgatherv(JNIEnv *env, jobject jthis,
                                                     jobject sendbuf, jint sendoffset,
                                                     jint sendcount,jobject sendtype,
                                                     jobject recvbuf, jint recvoffset,
                                                     jintArray recvcount, jintArray displs,
                                                     jobject recvtype)
{
    jint *rcount, *dps;
    jboolean isCopy ;

    MPI_Comm mpi_comm =
        (MPI_Comm)((*env)->GetLongField(env,jthis,ompi_java.CommhandleID)) ;

    MPI_Datatype mpi_stype = (MPI_Datatype)
        ((*env)->GetLongField(env,sendtype,ompi_java.DatatypehandleID)) ;
    MPI_Datatype mpi_rtype = (MPI_Datatype)
        ((*env)->GetLongField(env, recvtype, ompi_java.DatatypehandleID)) ;

    int sbaseType = (*env)->GetIntField(env, sendtype, ompi_java.DatatypebaseTypeID) ;
    int rbaseType = (*env)->GetIntField(env, recvtype, ompi_java.DatatypebaseTypeID) ;

    void *sendptr, *recvptr ;
    void *sbufbase, *rbufbase ;

    ompi_java_clearFreeList(env) ;

    rcount = (*env)->GetIntArrayElements(env, recvcount, &isCopy);
    dps = (*env)->GetIntArrayElements(env, displs, &isCopy);

    recvptr = ompi_java_getBufPtr(&rbufbase, env, recvbuf, rbaseType, recvoffset) ;
    sendptr = ompi_java_getBufPtr(&sbufbase, env, sendbuf, sbaseType, sendoffset) ;

    MPI_Allgatherv(sendptr, sendcount, mpi_stype,
                   recvptr, (int*) rcount, (int*) dps, mpi_rtype,
                   mpi_comm) ;

    ompi_java_releaseBufPtr(env, sendbuf, sbufbase, sbaseType) ;
    ompi_java_releaseBufPtr(env, recvbuf, rbufbase, rbaseType) ;

    (*env)->ReleaseIntArrayElements(env, recvcount, rcount, JNI_ABORT);
    (*env)->ReleaseIntArrayElements(env, displs, dps, JNI_ABORT);
}
 
/*
 * Class:     mpi_Intracomm
 * Method:    Alltoall
 * Signature:
 (Ljava/lang/Object;IILmpi/Datatype;Ljava/lang/Object;IILmpi/Datatype;)V
*/
JNIEXPORT void JNICALL Java_mpi_Intracomm_alltoall(JNIEnv *env, jobject jthis,
                                                   jobject sendbuf, jint sendoffset,
                                                   jint sendcount, jobject sendtype,
                                                   jobject recvbuf, jint recvoffset,
                                                   jint recvcount, jobject recvtype)
{
    MPI_Comm mpi_comm =
        (MPI_Comm)((*env)->GetLongField(env,jthis,ompi_java.CommhandleID)) ;

    MPI_Datatype mpi_stype = (MPI_Datatype)
        ((*env)->GetLongField(env,sendtype,ompi_java.DatatypehandleID)) ;
    MPI_Datatype mpi_rtype = (MPI_Datatype)
        ((*env)->GetLongField(env, recvtype, ompi_java.DatatypehandleID)) ;

    int sbaseType = (*env)->GetIntField(env, sendtype, ompi_java.DatatypebaseTypeID) ;
    int rbaseType = (*env)->GetIntField(env, recvtype, ompi_java.DatatypebaseTypeID) ;

    void *sendptr, *recvptr ;
    void *sbufbase, *rbufbase ;

    ompi_java_clearFreeList(env) ;

    recvptr = ompi_java_getBufPtr(&rbufbase, env, recvbuf, rbaseType, recvoffset) ;
    sendptr = ompi_java_getBufPtr(&sbufbase, env, sendbuf, sbaseType, sendoffset) ;

    MPI_Alltoall(sendptr, sendcount, mpi_stype,
                 recvptr, recvcount, mpi_rtype, mpi_comm) ;

    ompi_java_releaseBufPtr(env, sendbuf, sbufbase, sbaseType) ;
    ompi_java_releaseBufPtr(env, recvbuf, rbufbase, rbaseType) ;
}

/*
 * Class:     mpi_Intracomm
 * Method:    Alltoallv
 * Signature:
 (Ljava/lang/Object;II[ILmpi/Datatype;Ljava/lang/Object;I[I[ILmpi/Datatype;)V
*/
JNIEXPORT void JNICALL Java_mpi_Intracomm_alltoallv(JNIEnv *env, jobject jthis,
                                                    jobject sendbuf, jint sendoffset, jintArray sendcount, 
                                                    jintArray sdispls, jobject sendtype,
                                                    jobject recvbuf, jint recvoffset, jintArray recvcount,
                                                    jintArray rdispls, jobject recvtype)
{
    jint *rcount, *scount, *sdps, *rdps ;
    jboolean isCopy ;

    MPI_Comm mpi_comm =
        (MPI_Comm)((*env)->GetLongField(env,jthis,ompi_java.CommhandleID)) ;

    MPI_Datatype mpi_stype = (MPI_Datatype)
        ((*env)->GetLongField(env,sendtype,ompi_java.DatatypehandleID)) ;
    MPI_Datatype mpi_rtype = (MPI_Datatype)
        ((*env)->GetLongField(env, recvtype, ompi_java.DatatypehandleID)) ;

    int sbaseType = (*env)->GetIntField(env, sendtype, ompi_java.DatatypebaseTypeID) ;
    int rbaseType = (*env)->GetIntField(env, recvtype, ompi_java.DatatypebaseTypeID) ;

    void *sendptr, *recvptr ;
    void *sbufbase, *rbufbase ;

    ompi_java_clearFreeList(env) ;

    scount=(*env)->GetIntArrayElements(env,sendcount,&isCopy);
    rcount=(*env)->GetIntArrayElements(env,recvcount,&isCopy);
    sdps=(*env)->GetIntArrayElements(env,sdispls,&isCopy);
    rdps=(*env)->GetIntArrayElements(env,rdispls,&isCopy);

    recvptr = ompi_java_getBufPtr(&rbufbase, env, recvbuf, rbaseType, recvoffset) ;
    sendptr = ompi_java_getBufPtr(&sbufbase, env, sendbuf, sbaseType, sendoffset) ;

    MPI_Alltoallv(sendptr, (int*) scount, (int*) sdps, mpi_stype,
                  recvptr, (int*) rcount, (int*) rdps, mpi_rtype,
                  mpi_comm) ;

    ompi_java_releaseBufPtr(env, sendbuf, sbufbase, sbaseType) ;
    ompi_java_releaseBufPtr(env, recvbuf, rbufbase, rbaseType) ;

    (*env)->ReleaseIntArrayElements(env,recvcount,rcount,JNI_ABORT);
    (*env)->ReleaseIntArrayElements(env,sendcount,scount,JNI_ABORT);
    (*env)->ReleaseIntArrayElements(env,sdispls,sdps,JNI_ABORT);
    (*env)->ReleaseIntArrayElements(env,rdispls,rdps,JNI_ABORT);
}
 
/*
 * Class:     mpi_Intracomm
 * Method:    Reduce
 * Signature:
 (Ljava/lang/Object;ILjava/lang/Object;IILmpi/Datatype;Lmpi/Op;I)V
*/
JNIEXPORT void JNICALL Java_mpi_Intracomm_reduce(JNIEnv *env, jobject jthis,
                                                 jobject sendbuf, jint sendoffset,
                                                 jobject recvbuf, jint recvoffset,
                                                 jint count, jobject type, jobject op, jint root)
{
    int id ;

    MPI_Comm mpi_comm =
        (MPI_Comm)((*env)->GetLongField(env,jthis,ompi_java.CommhandleID)) ;

    MPI_Datatype mpi_type =
        (MPI_Datatype)((*env)->GetLongField(env,type,ompi_java.DatatypehandleID)) ;

    int baseType = (*env)->GetIntField(env, type, ompi_java.DatatypebaseTypeID) ;

    void *sendptr, *recvptr = NULL;
    void *sbufbase, *rbufbase ;

    ompi_java_clearFreeList(env) ;

    MPI_Comm_rank(mpi_comm, &id) ;

    if (id == root) {
        recvptr = ompi_java_getBufPtr(&rbufbase,
                                         env, recvbuf, baseType, recvoffset);
    }

    sendptr = ompi_java_getBufPtr(&sbufbase, env, sendbuf, baseType, sendoffset) ;

    MPI_Reduce(sendptr, recvptr, count, mpi_type,
               (MPI_Op)((*env)->GetLongField(env,op,ompi_java.OphandleID)),
               root, mpi_comm) ;

    ompi_java_releaseBufPtr(env, sendbuf, sbufbase, baseType) ;

    if (id == root) {
        ompi_java_releaseBufPtr(env, recvbuf, rbufbase, baseType);
    }
}

/*
 * Class:     mpi_Intracomm
 * Method:    Allreduce
 * Signature:
 (Ljava/lang/Object;ILjava/lang/Object;IILmpi/Datatype;Lmpi/Op;)V
*/
JNIEXPORT void JNICALL Java_mpi_Intracomm_allreduce(JNIEnv *env, jobject jthis,
                                                    jobject sendbuf, jint sendoffset,
                                                    jobject recvbuf, jint recvoffset,
                                                    jint count, jobject type, jobject op)
{
    MPI_Comm mpi_comm =
        (MPI_Comm)((*env)->GetLongField(env,jthis,ompi_java.CommhandleID)) ;

    MPI_Datatype mpi_type =
        (MPI_Datatype)((*env)->GetLongField(env,type,ompi_java.DatatypehandleID)) ;

    int baseType = (*env)->GetIntField(env, type, ompi_java.DatatypebaseTypeID) ;

    void *sendptr, *recvptr ;
    void *sbufbase, *rbufbase ;

    ompi_java_clearFreeList(env) ;

    recvptr = ompi_java_getBufPtr(&rbufbase, env, recvbuf, baseType, recvoffset) ;
    sendptr = ompi_java_getBufPtr(&sbufbase, env, sendbuf, baseType, sendoffset) ;

    MPI_Allreduce(sendptr, recvptr, count, mpi_type,
                  (MPI_Op)((*env)->GetLongField(env,op,ompi_java.OphandleID)),
                  mpi_comm) ;

    ompi_java_releaseBufPtr(env, sendbuf, sbufbase, baseType) ;
    ompi_java_releaseBufPtr(env, recvbuf, rbufbase, baseType) ;
}

/*
 * Class:     mpi_Intracomm
 * Method:    Reduce_scatter
 * Signature:
 (Ljava/lang/Object;ILjava/lang/Object;I[ILmpi/Datatype;Lmpi/Op;)V
*/
JNIEXPORT void JNICALL Java_mpi_Intracomm_reduce_1scatter(JNIEnv *env,
                                                          jobject jthis,
                                                          jobject sendbuf, jint sendoffset,
                                                          jobject recvbuf, jint recvoffset,
                                                          jintArray recvcount,
                                                          jobject type, jobject op)
{
    jint *rcount;
    jboolean isCopy ;

    MPI_Comm mpi_comm =
        (MPI_Comm)((*env)->GetLongField(env,jthis,ompi_java.CommhandleID)) ;

    MPI_Datatype mpi_type =
        (MPI_Datatype)((*env)->GetLongField(env,type,ompi_java.DatatypehandleID)) ;

    int baseType = (*env)->GetIntField(env, type, ompi_java.DatatypebaseTypeID) ;

    void *sendptr, *recvptr ;
    void *sbufbase, *rbufbase ;

    ompi_java_clearFreeList(env) ;

    rcount=(*env)->GetIntArrayElements(env,recvcount,&isCopy);

    recvptr = ompi_java_getBufPtr(&rbufbase, env, recvbuf, baseType, recvoffset) ;
    sendptr = ompi_java_getBufPtr(&sbufbase, env, sendbuf, baseType, sendoffset) ;

    MPI_Reduce_scatter(sendptr, recvptr, (int*) rcount, mpi_type,
                       (MPI_Op)((*env)->GetLongField(env,op,ompi_java.OphandleID)),
                       mpi_comm) ;

    ompi_java_releaseBufPtr(env, sendbuf, sbufbase, baseType) ;
    ompi_java_releaseBufPtr(env, recvbuf, rbufbase, baseType) ;

    (*env)->ReleaseIntArrayElements(env,recvcount,rcount,JNI_ABORT);
}

/*
 * Class:     mpi_Intracomm
 * Method:    Reduce_local
 * Signature:
 (Ljava/lang/Object;ILjava/lang/Object;IILmpi/Datatype;Lmpi/Op;I)V
*/
JNIEXPORT void JNICALL Java_mpi_Intracomm_reduce_1local(JNIEnv *env, jobject jthis,
                                                        jobject inbuf, jobject inoutbuf,
                                                        jint count, jobject type,
                                                        jobject op)
{
    MPI_Datatype mpi_type =
        (MPI_Datatype)((*env)->GetLongField(env,type,ompi_java.DatatypehandleID)) ;

    int baseType = (*env)->GetIntField(env, type, ompi_java.DatatypebaseTypeID) ;

    void *inptr, *inoutptr = NULL;
    void *inbase, *inoutbase ;

    ompi_java_clearFreeList(env) ;

    inptr = ompi_java_getBufPtr(&inbase, env, inbuf, baseType, 0) ;
    inoutptr = ompi_java_getBufPtr(&inoutbase, env, inoutbuf, baseType, 0) ;

    MPI_Reduce_local(inptr, inoutptr, count, mpi_type,
               (MPI_Op)((*env)->GetLongField(env,op,ompi_java.OphandleID))) ;

    ompi_java_releaseBufPtr(env, inbuf, inbase, baseType) ;
    ompi_java_releaseBufPtr(env, inoutbuf, inoutbase, baseType) ;
}

/*
 * Class:     mpi_Intracomm
 * Method:    Scan
 * Signature:
 (Ljava/lang/Object;ILjava/lang/Object;IILmpi/Datatype;Lmpi/Op;)V
*/
JNIEXPORT void JNICALL Java_mpi_Intracomm_scan(JNIEnv *env, jobject jthis,
                                               jobject sendbuf, jint sendoffset,
                                               jobject recvbuf, jint recvoffset,
                                               jint count, jobject type, jobject op)
{
    MPI_Comm mpi_comm =
        (MPI_Comm)((*env)->GetLongField(env,jthis,ompi_java.CommhandleID)) ;

    MPI_Datatype mpi_type =
        (MPI_Datatype)((*env)->GetLongField(env,type,ompi_java.DatatypehandleID)) ;

    int baseType = (*env)->GetIntField(env, type, ompi_java.DatatypebaseTypeID) ;

    void *sendptr, *recvptr ;
    void *sbufbase, *rbufbase ;

    ompi_java_clearFreeList(env) ;

    recvptr = ompi_java_getBufPtr(&rbufbase, env, recvbuf, baseType, recvoffset) ;
    sendptr = ompi_java_getBufPtr(&sbufbase, env, sendbuf, baseType, sendoffset) ;

    MPI_Scan(sendptr, recvptr, count, mpi_type,
             (MPI_Op)((*env)->GetLongField(env,op,ompi_java.OphandleID)),
             mpi_comm) ;

    ompi_java_releaseBufPtr(env, sendbuf, sbufbase, baseType) ;
    ompi_java_releaseBufPtr(env, recvbuf, rbufbase, baseType) ;
}

