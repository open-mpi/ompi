#include "oshmem_config.h"

#include <stdio.h>
#include <stdlib.h>
#include <dlfcn.h>

#ifdef HAVE_TARGETCONDITIONALS_H
#include <TargetConditionals.h>
#endif

#include "shmem.h"
#include "shmem_PSync.h"
#include "shmemJava.h"

JNIEXPORT jlong JNICALL Java_shmem_PSync_newPSync(
        JNIEnv *env, jobject jthis, jint size)
{
    long *pSync = shmalloc(size * sizeof(long));
    int i;

    for(i = 0; i < size; i++)
        pSync[i] = _SHMEM_SYNC_VALUE;

    return (jlong)pSync;
}

JNIEXPORT void JNICALL Java_shmem_PSync_free(
        JNIEnv *env, jobject jthis, jlong handle)
{
    shfree((void*)handle);
}
