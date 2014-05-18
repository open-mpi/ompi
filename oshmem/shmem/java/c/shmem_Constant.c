#include "oshmem_config.h"

#include <stdio.h>
#include <stdlib.h>
#include <dlfcn.h>

#ifdef HAVE_TARGETCONDITIONALS_H
#include <TargetConditionals.h>
#endif

#include "shmem.h"
#include "shmem_Constant.h"
#include "shmemJava.h"  /* must come AFTER the related .h so JNI is included */

static void setIntField(JNIEnv *env, jclass c, jobject obj,
                        char *field, jint value)
{
    jfieldID id = (*env)->GetFieldID(env, c, field, "I");
    (*env)->SetIntField(env, obj, id, value);
}

JNIEXPORT void JNICALL Java_shmem_Constant_setConstant(JNIEnv *env, jobject obj)
{
    jclass c = (*env)->GetObjectClass(env, obj);
    setIntField(env, c, obj, "CMP_EQ", SHMEM_CMP_EQ);
    setIntField(env, c, obj, "CMP_GE", SHMEM_CMP_GE);
    setIntField(env, c, obj, "CMP_GT", SHMEM_CMP_GT);
    setIntField(env, c, obj, "CMP_LE", SHMEM_CMP_LE);
    setIntField(env, c, obj, "CMP_LT", SHMEM_CMP_LT);
    setIntField(env, c, obj, "CMP_NE", SHMEM_CMP_NE);

    setIntField(env, c, obj, "BARRIER_SYNC_SIZE",
                        SHMEM_BARRIER_SYNC_SIZE);
    setIntField(env, c, obj, "BCAST_SYNC_SIZE",
                        SHMEM_BCAST_SYNC_SIZE);
    setIntField(env, c, obj, "COLLECT_SYNC_SIZE",
                        SHMEM_COLLECT_SYNC_SIZE);
    setIntField(env, c, obj, "REDUCE_SYNC_SIZE",
                        SHMEM_REDUCE_SYNC_SIZE);
    setIntField(env, c, obj, "REDUCE_MIN_WRKDATA_SIZE",
                        SHMEM_REDUCE_MIN_WRKDATA_SIZE);
    setIntField(env, c, obj, "SYNC_VALUE",
                        SHMEM_SYNC_VALUE);
}
