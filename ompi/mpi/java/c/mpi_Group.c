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
 * File         : mpi_Group.c
 * Headerfile   : mpi_Group.h 
 * Author       : Xinying Li
 * Created      : Thu Apr  9 12:22:15 1998
 * Revision     : $Revision: 1.3 $
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
#include "mpi_Group.h"
#include "mpiJava.h"

JNIEXPORT void JNICALL Java_mpi_Group_init(JNIEnv *env, jclass clazz)
{
    ompi_java_setStaticLongField(env, clazz,
            "nullHandle", (jlong)MPI_GROUP_NULL);

    ompi_java.GroupHandle = (*env)->GetFieldID(env, clazz, "handle", "J");
}

JNIEXPORT jlong JNICALL Java_mpi_Group_getEmpty(JNIEnv *env, jclass clazz)
{
    return (jlong)MPI_GROUP_EMPTY;
}

JNIEXPORT jint JNICALL Java_mpi_Group_getSize(
        JNIEnv *env, jobject jthis, jlong group)
{
    int size, rc;
    rc = MPI_Group_size((MPI_Group)group, &size);
    ompi_java_exceptionCheck(env, rc);
    return size;
}

JNIEXPORT jint JNICALL Java_mpi_Group_getRank(
        JNIEnv *env, jobject jthis, jlong group)
{
    int rank, rc;
    rc = MPI_Group_rank((MPI_Group)group, &rank);
    ompi_java_exceptionCheck(env, rc);
    return rank;
}

JNIEXPORT jlong JNICALL Java_mpi_Group_free(
        JNIEnv *env, jobject jthis, jlong handle)
{
    MPI_Group group = (MPI_Group)handle;
    int rc = MPI_Group_free(&group);
    ompi_java_exceptionCheck(env, rc);
    return (jlong)group;
}

JNIEXPORT jintArray JNICALL Java_mpi_Group_translateRanks(
        JNIEnv *env, jclass jthis, jlong group1,
        jintArray ranks1, jlong group2)
{
    jsize n = (*env)->GetArrayLength(env, ranks1);
    jintArray ranks2 = (*env)->NewIntArray(env,n);
    jint *jRanks1, *jRanks2;
    int  *cRanks1, *cRanks2;
    ompi_java_getIntArray(env, ranks1, &jRanks1, &cRanks1);
    ompi_java_getIntArray(env, ranks2, &jRanks2, &cRanks2);

    int rc = MPI_Group_translate_ranks((MPI_Group)group1, n, cRanks1,
                                       (MPI_Group)group2, cRanks2);
    ompi_java_exceptionCheck(env, rc);
    ompi_java_forgetIntArray(env, ranks1, jRanks1, cRanks1);
    ompi_java_releaseIntArray(env, ranks2, jRanks2, cRanks2);
    return ranks2;
}

JNIEXPORT jint JNICALL Java_mpi_Group_compare(
        JNIEnv *env, jclass jthis, jlong group1, jlong group2)
{
    int result, rc;
    rc = MPI_Group_compare((MPI_Group)group1, (MPI_Group)group2, &result);
    ompi_java_exceptionCheck(env, rc);
    return result;
}

JNIEXPORT jlong JNICALL Java_mpi_Group_union(
        JNIEnv *env, jclass jthis, jlong group1, jlong group2)
{
    MPI_Group newGroup;
    int rc = MPI_Group_union((MPI_Group)group1, (MPI_Group)group2, &newGroup);
    ompi_java_exceptionCheck(env, rc);
    return (jlong)newGroup;
}

JNIEXPORT jlong JNICALL Java_mpi_Group_intersection(
        JNIEnv *env, jclass jthis, jlong group1, jlong group2)
{
    MPI_Group newGroup;

    int rc = MPI_Group_intersection(
             (MPI_Group)group1, (MPI_Group)group2, &newGroup);

    ompi_java_exceptionCheck(env, rc);
    return (jlong)newGroup;
}

JNIEXPORT jlong JNICALL Java_mpi_Group_difference(
        JNIEnv *env, jclass jthis, jlong group1, jlong group2)
{
    MPI_Group newGroup;

    int rc = MPI_Group_difference(
             (MPI_Group)group1, (MPI_Group)group2, &newGroup);

    ompi_java_exceptionCheck(env, rc);
    return (jlong)newGroup;
}

JNIEXPORT jlong JNICALL Java_mpi_Group_incl(
        JNIEnv *env, jobject jthis, jlong group, jintArray ranks)
{
    jsize n = (*env)->GetArrayLength(env, ranks);
    jint *jRanks;
    int  *cRanks;
    ompi_java_getIntArray(env, ranks, &jRanks, &cRanks);

    MPI_Group newGroup;
    int rc = MPI_Group_incl((MPI_Group)group, n, cRanks, &newGroup);
    ompi_java_exceptionCheck(env, rc);

    ompi_java_forgetIntArray(env, ranks, jRanks, cRanks);
    return (jlong)newGroup;
}

JNIEXPORT jlong JNICALL Java_mpi_Group_excl(
        JNIEnv *env, jobject jthis, jlong group, jintArray ranks)
{
    jsize n = (*env)->GetArrayLength(env, ranks);
    jint *jRanks;
    int  *cRanks;
    ompi_java_getIntArray(env, ranks, &jRanks, &cRanks);

    MPI_Group newGroup;
    int rc = MPI_Group_excl((MPI_Group)group, n, cRanks, &newGroup);
    ompi_java_exceptionCheck(env, rc);

    ompi_java_forgetIntArray(env, ranks, jRanks, cRanks);
    return (jlong)newGroup;
}

JNIEXPORT jlong JNICALL Java_mpi_Group_rangeIncl(
        JNIEnv *env, jobject jthis, jlong group, jobjectArray ranges)
{
    int i;
    MPI_Group newGroup;
    jsize n = (*env)->GetArrayLength(env, ranges);
    int (*cRanges)[3] = (int(*)[3])calloc(n, sizeof(int[3]));

    for(i = 0; i < n; i++)
    {
        jintArray ri = (*env)->GetObjectArrayElement(env, ranges, i);
        jint *jri = (*env)->GetIntArrayElements(env, ri, NULL);
        cRanges[i][0] = jri[0];
        cRanges[i][1] = jri[1];
        cRanges[i][2] = jri[2];
        (*env)->ReleaseIntArrayElements(env, ri, jri, JNI_ABORT);
        (*env)->DeleteLocalRef(env, ri);
    }

    int rc = MPI_Group_range_incl((MPI_Group)group, n, cRanges, &newGroup);
    ompi_java_exceptionCheck(env, rc);
    free(cRanges);
    return (jlong)newGroup;
}

JNIEXPORT jlong JNICALL Java_mpi_Group_rangeExcl(
        JNIEnv *env, jobject jthis, jlong group, jobjectArray ranges)
{
    int i;
    MPI_Group newGroup;
    jsize n = (*env)->GetArrayLength(env, ranges);
    int (*cRanges)[3] = (int(*)[3])calloc(n, sizeof(int[3]));

    for(i = 0; i < n; i++)
    {
        jintArray ri = (*env)->GetObjectArrayElement(env, ranges, i);
        jint *jri = (*env)->GetIntArrayElements(env, ri, NULL);
        cRanges[i][0] = jri[0];
        cRanges[i][1] = jri[1];
        cRanges[i][2] = jri[2];
        (*env)->ReleaseIntArrayElements(env, ri, jri, JNI_ABORT);
        (*env)->DeleteLocalRef(env, ri);
    }

    int rc = MPI_Group_range_excl((MPI_Group)group, n, cRanges, &newGroup);
    ompi_java_exceptionCheck(env, rc);
    free(cRanges);
    return (jlong)newGroup;
}
