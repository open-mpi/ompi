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

JNIEXPORT jlong JNICALL Java_mpi_Message_mProbe(
        JNIEnv *env, jobject jthis,
        jint source, jint tag, jlong jComm, jlongArray jStatus)
{
    MPI_Comm comm = (MPI_Comm)jComm;
    MPI_Message message;
    MPI_Status  status;
    int rc = MPI_Mprobe(source, tag, comm, &message, &status);
    ompi_java_exceptionCheck(env, rc);
    ompi_java_status_set(env, jStatus, &status);
    return (jlong)message;
}

JNIEXPORT jobject JNICALL Java_mpi_Message_imProbe(
        JNIEnv *env, jobject jthis, jint source, jint tag, jlong jComm)
{
    MPI_Comm comm = (MPI_Comm)jComm;
    MPI_Message message;
    MPI_Status  status;
    int rc, flag;
    rc = MPI_Improbe(source, tag, comm, &flag, &message, &status);

    if(ompi_java_exceptionCheck(env, rc) || !flag)
        return NULL;

    (*env)->SetLongField(env, jthis, ompi_java.MessageHandle, (jlong)message);
    return ompi_java_status_new(env, &status);
}

JNIEXPORT jlong JNICALL Java_mpi_Message_mRecv(
        JNIEnv *env, jobject jthis, jlong jMessage, jobject buf, jboolean db,
        jint off, jint count, jlong jType, jint bType, jlongArray jStatus)
{
    MPI_Message  message = (MPI_Message)jMessage;
    MPI_Datatype type    = (MPI_Datatype)jType;

    void *ptr;
    ompi_java_buffer_t *item;
    ompi_java_getWritePtr(&ptr, &item, env, buf, db, count, type);

    MPI_Status status;
    int rc = MPI_Mrecv(ptr, count, type, &message, &status);
    ompi_java_exceptionCheck(env, rc);

    ompi_java_status_set(env, jStatus, &status);
    ompi_java_releaseWritePtr(ptr, item, env, buf, db, off, count, type, bType);
    return (jlong)message;
}

JNIEXPORT jlong JNICALL Java_mpi_Message_imRecv(
        JNIEnv *env, jobject jthis, jlong jMessage,
        jobject buf, jint count, jlong jType)
{
    MPI_Message  message = (MPI_Message)jMessage;
    MPI_Datatype type    = (MPI_Datatype)jType;
    void *ptr = ompi_java_getDirectBufferAddress(env, buf);

    MPI_Request request;
    int rc = MPI_Imrecv(ptr, count, type, &message, &request);
    ompi_java_exceptionCheck(env, rc);
    (*env)->SetLongField(env, jthis, ompi_java.MessageHandle, (jlong)message);
    return (jlong)request;
}
