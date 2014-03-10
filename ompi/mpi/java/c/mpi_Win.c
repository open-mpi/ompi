#include "ompi_config.h"

#include <stdlib.h>
#ifdef HAVE_TARGETCONDITIONALS_H
#include <TargetConditionals.h>
#endif

#include "mpi.h"
#include "mpi_Win.h"
#include "mpiJava.h"

JNIEXPORT jlong JNICALL Java_mpi_Win_createWin(
        JNIEnv *env, jobject jthis, jobject jBase,
        jint size, jint dispUnit, jlong info, jlong comm)
{
    void *base = (*env)->GetDirectBufferAddress(env, jBase);
    MPI_Win win;

    int rc = MPI_Win_create(base, (MPI_Aint)size, dispUnit,
                            (MPI_Info)info, (MPI_Comm)comm, &win);

    ompi_java_exceptionCheck(env, rc);
    return (jlong)win;
}

JNIEXPORT jlong JNICALL Java_mpi_Win_getGroup(
        JNIEnv *env, jobject jthis, jlong win)
{
    MPI_Group group;
    int rc = MPI_Win_get_group((MPI_Win)win, &group);
    ompi_java_exceptionCheck(env, rc);
    return (jlong)group;
}

JNIEXPORT void JNICALL Java_mpi_Win_put(
        JNIEnv *env, jobject jthis, jlong win, jobject origin,
        jint orgCount, jlong orgType, jint targetRank, jint targetDisp,
        jint targetCount, jlong targetType, jint baseType)
{
    void *orgPtr = (*env)->GetDirectBufferAddress(env, origin);

    int rc = MPI_Put(orgPtr, orgCount, (MPI_Datatype)orgType,
                     targetRank, (MPI_Aint)targetDisp, targetCount,
                     (MPI_Datatype)targetType, (MPI_Win)win);

    ompi_java_exceptionCheck(env, rc);
}

JNIEXPORT void JNICALL Java_mpi_Win_get(
        JNIEnv *env, jobject jthis, jlong win, jobject origin,
        jint orgCount, jlong orgType, jint targetRank, jint targetDisp,
        jint targetCount, jlong targetType, jint baseType)
{
    void *orgPtr = (*env)->GetDirectBufferAddress(env, origin);

    int rc = MPI_Get(orgPtr, orgCount, (MPI_Datatype)orgType,
                     targetRank, (MPI_Aint)targetDisp, targetCount,
                     (MPI_Datatype)targetType, (MPI_Win)win);

    ompi_java_exceptionCheck(env, rc);
}

JNIEXPORT void JNICALL Java_mpi_Win_accumulate(
        JNIEnv *env, jobject jthis, jlong win,
        jobject origin, jint orgCount, jlong orgType,
        jint targetRank, jint targetDisp, jint targetCount, jlong targetType,
        jobject jOp, jlong hOp, jint baseType)
{
    void *orgPtr = (*env)->GetDirectBufferAddress(env, origin);
    MPI_Op op = ompi_java_op_getHandle(env, jOp, hOp, baseType);

    int rc = MPI_Accumulate(orgPtr, orgCount, (MPI_Datatype)orgType,
                            targetRank, (MPI_Aint)targetDisp, targetCount,
                            (MPI_Datatype)targetType, op, (MPI_Win)win);

    ompi_java_exceptionCheck(env, rc);
}

JNIEXPORT void JNICALL Java_mpi_Win_fence(
        JNIEnv *env, jobject jthis, jlong win, jint assertion)
{
    int rc = MPI_Win_fence(assertion, (MPI_Win)win);
    ompi_java_exceptionCheck(env, rc);
}

JNIEXPORT void JNICALL Java_mpi_Win_start(
        JNIEnv *env, jobject jthis, jlong win, jlong group, jint assertion)
{
    int rc = MPI_Win_start((MPI_Group)group, assertion, (MPI_Win)win);
    ompi_java_exceptionCheck(env, rc);
}

JNIEXPORT void JNICALL Java_mpi_Win_complete(
        JNIEnv *env, jobject jthis, jlong win)
{
    int rc = MPI_Win_complete((MPI_Win)win);
    ompi_java_exceptionCheck(env, rc);
}

JNIEXPORT void JNICALL Java_mpi_Win_post(
        JNIEnv *env, jobject jthis, jlong win, jlong group, jint assertion)
{
    int rc = MPI_Win_post((MPI_Group)group, assertion, (MPI_Win)win);
    ompi_java_exceptionCheck(env, rc);
}

JNIEXPORT void JNICALL Java_mpi_Win_waitFor(
        JNIEnv *env, jobject jthis, jlong win)
{
    int rc = MPI_Win_wait((MPI_Win)win);
    ompi_java_exceptionCheck(env, rc);
}

JNIEXPORT jboolean JNICALL Java_mpi_Win_test(
        JNIEnv *env, jobject jthis, jlong win)
{
    int flag;
    int rc = MPI_Win_test((MPI_Win)win, &flag);
    ompi_java_exceptionCheck(env, rc);
    return flag ? JNI_TRUE : JNI_FALSE;
}

JNIEXPORT void JNICALL Java_mpi_Win_lock(
        JNIEnv *env, jobject jthis, jlong win,
        jint lockType, jint rank, jint assertion)
{
    int rc = MPI_Win_lock(lockType, rank, assertion, (MPI_Win)win);
    ompi_java_exceptionCheck(env, rc);
}

JNIEXPORT void JNICALL Java_mpi_Win_unlock(
        JNIEnv *env, jobject jthis, jlong win, jint rank)
{
    int rc = MPI_Win_unlock(rank, (MPI_Win)win);
    ompi_java_exceptionCheck(env, rc);
}

JNIEXPORT void JNICALL Java_mpi_Win_setErrhandler(
        JNIEnv *env, jobject jthis, jlong win, jlong errhandler)
{
    int rc = MPI_Win_set_errhandler(
             (MPI_Win)win, (MPI_Errhandler)MPI_ERRORS_RETURN);

    ompi_java_exceptionCheck(env, rc);
}

JNIEXPORT void JNICALL Java_mpi_Win_callErrhandler(
        JNIEnv *env, jobject jthis, jlong win, jint errorCode)
{
    int rc = MPI_Win_call_errhandler((MPI_Win)win, errorCode);
    ompi_java_exceptionCheck(env, rc);
}

static int winCopyAttr(MPI_Win oldwin, int keyval, void *extraState,
                       void *attrValIn, void *attrValOut, int *flag)
{
    return ompi_java_attrCopy(attrValIn, attrValOut, flag);
}

static int winDeleteAttr(MPI_Win oldwin, int keyval,
                         void *attrVal, void *extraState)
{
    return ompi_java_attrDelete(attrVal);
}

JNIEXPORT jint JNICALL Java_mpi_Win_createKeyval_1jni(JNIEnv *env, jclass clazz)
{
    int rc, keyval;
    rc = MPI_Win_create_keyval(winCopyAttr, winDeleteAttr, &keyval, NULL);
    ompi_java_exceptionCheck(env, rc);
    return keyval;
}

JNIEXPORT void JNICALL Java_mpi_Win_freeKeyval_1jni(
        JNIEnv *env, jclass clazz, jint keyval)
{
    int rc = MPI_Win_free_keyval((int*)(&keyval));
    ompi_java_exceptionCheck(env, rc);
}

JNIEXPORT void JNICALL Java_mpi_Win_setAttr(
        JNIEnv *env, jobject jthis, jlong win, jint keyval, jbyteArray jval)
{
    void *cval = ompi_java_attrSet(env, jval);
    int rc = MPI_Win_set_attr((MPI_Win)win, keyval, cval);
    ompi_java_exceptionCheck(env, rc);
}

JNIEXPORT jobject JNICALL Java_mpi_Win_getAttr(
        JNIEnv *env, jobject jthis, jlong win, jint keyval)
{
    int flag;
    void *val;
    int rc = MPI_Win_get_attr((MPI_Win)win, keyval, &val, &flag);

    if(ompi_java_exceptionCheck(env, rc) || !flag)
        return NULL;

    switch(keyval)
    {
        case MPI_WIN_SIZE:
            return ompi_java_Integer_valueOf(env, (jint)(*((MPI_Aint*)val)));
        case MPI_WIN_DISP_UNIT:
            return ompi_java_Integer_valueOf(env, (jint)(*((int*)val)));
        case MPI_WIN_BASE:
            return ompi_java_Long_valueOf(env, (jlong)val);
        default:
            return ompi_java_attrGet(env, val);
    }
}

JNIEXPORT void JNICALL Java_mpi_Win_deleteAttr(
        JNIEnv *env, jobject jthis, jlong win, jint keyval)
{
    int rc = MPI_Win_delete_attr((MPI_Win)win, keyval);
    ompi_java_exceptionCheck(env, rc);
}

JNIEXPORT jlong JNICALL Java_mpi_Win_free(
        JNIEnv *env, jobject jthis, jlong handle)
{
    MPI_Win win = (MPI_Win)handle;
    int rc = MPI_Win_free(&win);
    ompi_java_exceptionCheck(env, rc);
    return (jlong)win;
}
