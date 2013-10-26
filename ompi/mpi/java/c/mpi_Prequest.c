#include "ompi_config.h"
#include <stdlib.h>
#include <assert.h>
#ifdef HAVE_TARGETCONDITIONALS_H
#include <TargetConditionals.h>
#endif

#include "mpi.h"
#include "mpi_Prequest.h"
#include "mpiJava.h"

/*
 * Class:     mpi_Prequest
 * Method:    start_jni
 * Signature: (J)V
 */
JNIEXPORT void JNICALL Java_mpi_Prequest_start_1jni(JNIEnv *env, jobject jthis)
{
    MPI_Request request = (MPI_Request)(*env)->GetLongField(
                          env, jthis, ompi_java.ReqHandle);

    int rc = MPI_Start(&request);
    ompi_java_exceptionCheck(env, rc);
    (*env)->SetLongField(env, jthis, ompi_java.ReqHandle, (jlong)request);
}

/*
 * Class:     mpi_Prequest
 * Method:    startAll_jni
 * Signature: ([Lmpi/Prequest;)V
 */
JNIEXPORT void JNICALL Java_mpi_Prequest_startAll_1jni(
                       JNIEnv *env, jclass clazz, jobjectArray prequests)
{
    int i, count = (*env)->GetArrayLength(env, prequests);
    MPI_Request *requests = calloc(count, sizeof(MPI_Request));

    for(i = 0; i < count; i++)
    {
        jobject r = (*env)->GetObjectArrayElement(env, prequests, i);

        requests[i] = (MPI_Request)(*env)->GetLongField(
                      env, r, ompi_java.ReqHandle);

        (*env)->DeleteLocalRef(env, r);
    }

    int rc = MPI_Startall(count, requests);
    ompi_java_exceptionCheck(env, rc);

    for(i = 0; i < count; i++)
    {
        jobject r = (*env)->GetObjectArrayElement(env, prequests, i);
        (*env)->SetLongField(env, r, ompi_java.ReqHandle, (long)requests[i]);
        (*env)->DeleteLocalRef(env, r);
    }

    free(requests);
}
