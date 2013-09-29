#include "ompi_config.h"

#ifdef HAVE_TARGETCONDITIONALS_H
#include <TargetConditionals.h>
#endif

#include "mpi.h"
#include "mpi_Message.h"
#include "mpiJava.h"

JNIEXPORT void JNICALL Java_mpi_Message_init(JNIEnv *e, jclass c)
{
    ompi_java_setStaticLongField(e, c, "NULL",    (jlong)MPI_MESSAGE_NULL);
    ompi_java_setStaticLongField(e, c, "NO_PROC", (jlong)MPI_MESSAGE_NO_PROC);
    ompi_java.MessageHandle = (*e)->GetFieldID(e, c, "handle", "J");
}

JNIEXPORT jobject JNICALL Java_mpi_Message_mProbe(
        JNIEnv *env, jobject jthis, jint source, jint tag, jlong comm)
{
    MPI_Message message = (MPI_Message)((*env)->GetLongField(
                          env, jthis, ompi_java.MessageHandle));
    int rc;
    MPI_Status status;
    rc = MPI_Mprobe(source, tag, (MPI_Comm)comm, &message, &status);

    if(ompi_java_exceptionCheck(env, rc))
        return NULL;

    (*env)->SetLongField(env, jthis, ompi_java.MessageHandle, (jlong)message);
    jobject stat = ompi_java_status_new(&status, env);
    return stat;
}

JNIEXPORT jobject JNICALL Java_mpi_Message_imProbe(
        JNIEnv *env, jobject jthis, jint source, jint tag, jlong comm)
{
    MPI_Message message = (MPI_Message)((*env)->GetLongField(
                          env, jthis, ompi_java.MessageHandle));
    int rc, flag;
    MPI_Status status;
    rc = MPI_Improbe(source, tag, (MPI_Comm)comm, &flag, &message, &status);

    if(ompi_java_exceptionCheck(env, rc) || !flag)
        return NULL;

    (*env)->SetLongField(env, jthis, ompi_java.MessageHandle, (jlong)message);
    jobject stat = ompi_java_status_new(&status, env);
    return stat;
}

JNIEXPORT void JNICALL Java_mpi_Message_mRecv(
        JNIEnv *env, jobject jthis, jobject buf, jint offset, jint count,
        jobject jType, jobject stat)
{
    MPI_Message msg = (MPI_Message)((*env)->GetLongField(
                      env, jthis, ompi_java.MessageHandle));

    MPI_Datatype type = (MPI_Datatype)((*env)->GetLongField(
                        env, jType, ompi_java.DatatypeHandle));

    int bType = (*env)->GetIntField(env, jType, ompi_java.DatatypeBaseType);
    void *bufPtr, *bufBase;
    bufPtr = ompi_java_getBufPtr(&bufBase, env, buf, bType, offset);

    MPI_Status status;
    int rc = MPI_Mrecv(bufPtr, count, type, &msg, &status);

    if(!ompi_java_exceptionCheck(env, rc))
    {
        (*env)->SetLongField(env, jthis, ompi_java.MessageHandle, (jlong)msg);
        ompi_java_status_set(&status, env, stat);
    }

    ompi_java_releaseBufPtr(env, buf, bufBase, bType);
}

JNIEXPORT jlong JNICALL Java_mpi_Message_imRecv(
        JNIEnv *env, jobject jthis, jobject buf, jint count, jlong type)
{
    MPI_Message msg = (MPI_Message)((*env)->GetLongField(
                      env, jthis, ompi_java.MessageHandle));

    void *ptr = ompi_java_getDirectBufferAddress(env, buf);
    MPI_Request request;

    int rc = MPI_Imrecv(ptr, count, (MPI_Datatype)type, &msg, &request);
    ompi_java_exceptionCheck(env, rc);
    (*env)->SetLongField(env, jthis, ompi_java.MessageHandle, (jlong)msg);
    return (jlong)request;
}
