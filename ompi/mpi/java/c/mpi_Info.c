#include "ompi_config.h"

#include <stdlib.h>
#ifdef HAVE_TARGETCONDITIONALS_H
#include <TargetConditionals.h>
#endif

#include "mpi.h"
#include "mpi_Info.h"
#include "mpiJava.h"

/*
 * Class:     mpi_Info
 * Method:    create
 * Signature: ()J
 */
JNIEXPORT jlong JNICALL Java_mpi_Info_create(JNIEnv *env, jobject jthis)
{
    MPI_Info info;
    int rc = MPI_Info_create(&info);
    ompi_java_exceptionCheck(env, rc);
    return (jlong)info;
}

/*
 * Class:     mpi_Info
 * Method:    getEnv
 * Signature: ()J
 */
JNIEXPORT jlong JNICALL Java_mpi_Info_getEnv(JNIEnv *env, jclass clazz)
{
    return (jlong)MPI_INFO_ENV;
}

/*
 * Class:     mpi_Info
 * Method:    getNull
 * Signature: ()J
 */
JNIEXPORT jlong JNICALL Java_mpi_Info_getNull(JNIEnv *env, jclass clazz)
{
    return (jlong)MPI_INFO_NULL;
}

/*
 * Class:     mpi_Info
 * Method:    set
 * Signature: (JLjava/lang/String;Ljava/lang/String;)V
 */
JNIEXPORT void JNICALL Java_mpi_Info_set(
        JNIEnv *env, jobject jthis, jlong handle, jstring jkey, jstring jvalue)
{
    const char *key   = (*env)->GetStringUTFChars(env, jkey,   NULL),
               *value = (*env)->GetStringUTFChars(env, jvalue, NULL);

    int rc = MPI_Info_set((MPI_Info)handle, (char*)key, (char*)value);
    ompi_java_exceptionCheck(env, rc);

    (*env)->ReleaseStringUTFChars(env, jkey,   key);
    (*env)->ReleaseStringUTFChars(env, jvalue, value);
}

/*
 * Class:     mpi_Info
 * Method:    get
 * Signature: (JLjava/lang/String;)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_mpi_Info_get(
        JNIEnv *env, jobject jthis, jlong handle, jstring jkey)
{
    MPI_Info info = (MPI_Info)handle;
    const char *key = (*env)->GetStringUTFChars(env, jkey, NULL);

    int rc, valueLen, flag;
    rc = MPI_Info_get_valuelen(info, (char*)key, &valueLen, &flag);

    if(ompi_java_exceptionCheck(env, rc) || !flag)
    {
        (*env)->ReleaseStringUTFChars(env, jkey, key);
        return NULL;
    }

    char *value = (char*)calloc(valueLen + 1, sizeof(char));
    rc = MPI_Info_get((MPI_Info)info, (char*)key, valueLen, value, &flag);
    (*env)->ReleaseStringUTFChars(env, jkey, key);

    if(ompi_java_exceptionCheck(env, rc) || !flag)
    {
        free(value);
        return NULL;
    }

    jstring jvalue = (*env)->NewStringUTF(env, value);
    free(value);
    return jvalue;
}

/*
 * Class:     mpi_Info
 * Method:    delete
 * Signature: (JLjava/lang/String;)V
 */
JNIEXPORT void JNICALL Java_mpi_Info_delete(
        JNIEnv *env, jobject jthis, jlong handle, jstring jkey)
{
    const char *key = (*env)->GetStringUTFChars(env, jkey, NULL);
    int rc = MPI_Info_delete((MPI_Info)handle, (char*)key);
    ompi_java_exceptionCheck(env, rc);
    (*env)->ReleaseStringUTFChars(env, jkey, key);
}

/*
 * Class:     mpi_Info
 * Method:    size
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL Java_mpi_Info_size(
        JNIEnv *env, jobject jthis, jlong handle)
{
    int rc, nkeys;
    rc = MPI_Info_get_nkeys((MPI_Info)handle, &nkeys);
    ompi_java_exceptionCheck(env, rc);
    return (jint)nkeys;
}

/*
 * Class:     mpi_Info
 * Method:    getKey
 * Signature: (JI)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_mpi_Info_getKey(
        JNIEnv *env, jobject jthis, jlong handle, jint i)
{
    char key[MPI_MAX_INFO_KEY + 1];
    int rc = MPI_Info_get_nthkey((MPI_Info)handle, i, key);

    return ompi_java_exceptionCheck(env, rc)
           ? NULL : (*env)->NewStringUTF(env, key);
}

/*
 * Class:     mpi_Info
 * Method:    clone
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL Java_mpi_Info_clone(
        JNIEnv *env, jobject jthis, jlong handle)
{
    MPI_Info newInfo;
    int rc = MPI_Info_dup((MPI_Info)handle, &newInfo);
    ompi_java_exceptionCheck(env, rc);
    return (jlong)newInfo;
}

/*
 * Class:     mpi_Info
 * Method:    free
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL Java_mpi_Info_free(
        JNIEnv *env, jobject jthis, jlong handle)
{
    MPI_Info info = (MPI_Info)handle;
    int rc = MPI_Info_free(&info);
    ompi_java_exceptionCheck(env, rc);
    return (jlong)info;
}

/*
 * Class:     mpi_Info
 * Method:    isNull
 * Signature: (J)Z
 */
JNIEXPORT jboolean JNICALL Java_mpi_Info_isNull(
        JNIEnv *env, jobject jthis, jlong handle)
{
    return (MPI_Info)handle == MPI_INFO_NULL ? JNI_TRUE : JNI_FALSE;
}
