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
 * File         : mpi_Datatype.c
 * Headerfile   : mpi_Datatype.h
 * Author       : Sung-Hoon Ko, Xinying Li, Sang Lim, Bryan Carpenter
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
#include "mpi_Datatype.h"
#include "mpiJava.h"

MPI_Datatype Dts[] = { MPI_DATATYPE_NULL,  /* NULL */
                       MPI_UINT8_T,  /* BYTE */
                       MPI_UINT16_T, /* CHAR */
                       MPI_INT16_T,  /* SHORT */
                       MPI_UINT8_T,  /* BOOLEAN (let's hope Java is
                                        one byte..) */
                       MPI_INT32_T,  /* INT */
                       MPI_INT64_T,  /* LONG */
                       MPI_FLOAT,    /* FLOAT (let's hope it's the same!) */
                       MPI_DOUBLE,   /* DOUBLE (let's hoe it's the same!) */
                       MPI_PACKED,   /* PACKED */
                       MPI_2INT,
                       MPI_SHORT_INT,
                       MPI_LONG_INT,
                       MPI_FLOAT_INT,
                       MPI_DOUBLE_INT,
                       MPI_C_FLOAT_COMPLEX,
                       MPI_C_DOUBLE_COMPLEX
};

JNIEXPORT void JNICALL Java_mpi_Datatype_init(JNIEnv *e, jclass clazz)
{
    ompi_java.DatatypeHandle   = (*e)->GetFieldID(e, clazz, "handle",   "J");
    ompi_java.DatatypeBaseType = (*e)->GetFieldID(e, clazz, "baseType", "I");
    ompi_java.DatatypeBaseSize = (*e)->GetFieldID(e, clazz, "baseSize", "I");
}

JNIEXPORT jlong JNICALL Java_mpi_Datatype_getDatatype(
                        JNIEnv *e, jobject jthis, jint type)
{
    return (jlong)Dts[type];
}

JNIEXPORT void JNICALL Java_mpi_Datatype_getLbExtent(
        JNIEnv *env, jobject jthis, jlong type, jintArray jLbExt)
{
    MPI_Aint lb, extent;
    int rc = MPI_Type_get_extent((MPI_Datatype)type, &lb, &extent);
    ompi_java_exceptionCheck(env, rc);

    jint *lbExt = (*env)->GetIntArrayElements(env, jLbExt, NULL);
    lbExt[0] = (jint)lb;
    lbExt[1] = (jint)extent;
    (*env)->ReleaseIntArrayElements(env, jLbExt, lbExt, 0);
}

JNIEXPORT void JNICALL Java_mpi_Datatype_getTrueLbExtent(
        JNIEnv *env, jobject jthis, jlong type, jintArray jLbExt)
{
    MPI_Aint lb, extent;
    int rc = MPI_Type_get_true_extent((MPI_Datatype)type, &lb, &extent);
    ompi_java_exceptionCheck(env, rc);

    jint *lbExt = (*env)->GetIntArrayElements(env, jLbExt, NULL);
    lbExt[0] = (jint)lb;
    lbExt[1] = (jint)extent;
    (*env)->ReleaseIntArrayElements(env, jLbExt, lbExt, 0);
}

JNIEXPORT jint JNICALL Java_mpi_Datatype_getSize(
        JNIEnv *env, jobject jthis, jlong type)
{
    int rc, result;
    rc = MPI_Type_size((MPI_Datatype)type, &result);
    ompi_java_exceptionCheck(env, rc);
    return result;
}

JNIEXPORT void JNICALL Java_mpi_Datatype_commit(
        JNIEnv *env, jobject jthis, jlong handle)
{
    MPI_Datatype type = (MPI_Datatype)handle;
    int rc = MPI_Type_commit(&type);
    ompi_java_exceptionCheck(env, rc);
}

JNIEXPORT jlong JNICALL Java_mpi_Datatype_free(
        JNIEnv *env, jobject jthis, jlong handle)
{
    MPI_Datatype type = (MPI_Datatype)handle;

    if(type != MPI_DATATYPE_NULL)
    {
        int rc = MPI_Type_free(&type);
        ompi_java_exceptionCheck(env, rc);
    }

    return (jlong)type;
}

JNIEXPORT jlong JNICALL Java_mpi_Datatype_dup(
        JNIEnv *env, jobject jthis, jlong oldType)
{
    MPI_Datatype newType;
    int rc = MPI_Type_dup((MPI_Datatype)oldType, &newType);
    ompi_java_exceptionCheck(env, rc);
    return (jlong)newType;
}

JNIEXPORT jlong JNICALL Java_mpi_Datatype_getContiguous(
        JNIEnv *env, jclass clazz, jint count, jlong oldType)
{
    MPI_Datatype type;
    int rc = MPI_Type_contiguous(count, (MPI_Datatype)oldType, &type);
    ompi_java_exceptionCheck(env, rc);
    return (jlong)type;
}

JNIEXPORT jlong JNICALL Java_mpi_Datatype_getVector(
            JNIEnv *env, jclass clazz, jint count,
            jint blockLength, jint stride, jlong oldType)
{
    MPI_Datatype type;

    int rc = MPI_Type_vector(count, blockLength, stride,
                             (MPI_Datatype)oldType, &type);

    ompi_java_exceptionCheck(env, rc);
    return (jlong)type;
}

JNIEXPORT jlong JNICALL Java_mpi_Datatype_getHVector(
        JNIEnv *env, jclass clazz, jint count,
        jint blockLength, jint stride, jlong oldType)
{
    MPI_Datatype type;

    int rc = MPI_Type_hvector(count, blockLength, stride,
                              (MPI_Datatype)oldType, &type);

    ompi_java_exceptionCheck(env, rc);
    return (jlong)type;
}

JNIEXPORT jlong JNICALL Java_mpi_Datatype_getIndexed(
        JNIEnv *env, jclass clazz, jintArray blockLengths,
        jintArray disps, jlong oldType)
{
    MPI_Datatype type;
    int count = (*env)->GetArrayLength(env, blockLengths);

    jint *jBlockLengths, *jDispl;
    int  *cBlockLengths, *cDispl;
    ompi_java_getIntArray(env, blockLengths, &jBlockLengths, &cBlockLengths);
    ompi_java_getIntArray(env, disps, &jDispl, &cDispl);

    int rc = MPI_Type_indexed(count, cBlockLengths, cDispl,
                              (MPI_Datatype)oldType, &type);

    ompi_java_exceptionCheck(env, rc);
    ompi_java_forgetIntArray(env, blockLengths, jBlockLengths, cBlockLengths);
    ompi_java_forgetIntArray(env, disps, jDispl, cDispl);
    return (jlong)type;
}

JNIEXPORT jlong JNICALL Java_mpi_Datatype_getHIndexed(
        JNIEnv *env, jclass clazz, jintArray blockLengths,
        jintArray disps, jlong oldType)
{
    MPI_Datatype type;
    int count = (*env)->GetArrayLength(env, blockLengths);
    jint *jBlockLengths;
    int  *cBlockLengths;
    ompi_java_getIntArray(env, blockLengths, &jBlockLengths, &cBlockLengths);

    jint     *jDisps = (*env)->GetIntArrayElements(env, disps, NULL);
    MPI_Aint *cDisps = (MPI_Aint*)calloc(count, sizeof(MPI_Aint));
    int i;

    for(i = 0; i < count; i++)
        cDisps[i] = jDisps[i];

    int rc = MPI_Type_hindexed(count, cBlockLengths, cDisps,
                               (MPI_Datatype)oldType, &type);

    ompi_java_exceptionCheck(env, rc);
    free(cDisps);
    ompi_java_forgetIntArray(env, blockLengths, jBlockLengths, cBlockLengths);
    (*env)->ReleaseIntArrayElements(env, disps, jDisps, JNI_ABORT);
    return (jlong)type;
}

JNIEXPORT jlong JNICALL Java_mpi_Datatype_getStruct(
        JNIEnv *env, jclass clazz, jintArray blockLengths,
        jintArray disps, jobjectArray datatypes)
{
    int count = (*env)->GetArrayLength(env, blockLengths);
    jint *jBlockLengths;
    int  *cBlockLengths;
    ompi_java_getIntArray(env, blockLengths, &jBlockLengths, &cBlockLengths);

    jint     *jDisps = (*env)->GetIntArrayElements(env, disps, NULL);
    MPI_Aint *cDisps = (MPI_Aint*)calloc(count, sizeof(MPI_Aint));

    MPI_Datatype *cTypes = (MPI_Datatype*)calloc(count, sizeof(MPI_Datatype));
    int i;

    for(i = 0; i < count; i++)
    {
        cDisps[i] = jDisps[i];
        jobject type = (*env)->GetObjectArrayElement(env, datatypes, i);

        cTypes[i] = (MPI_Datatype)(*env)->GetLongField(
                    env, type, ompi_java.DatatypeHandle);

        (*env)->DeleteLocalRef(env, type);
    }

    MPI_Datatype type;
    int rc = MPI_Type_struct(count, cBlockLengths, cDisps, cTypes, &type);
    ompi_java_exceptionCheck(env, rc);

    free(cDisps);
    free(cTypes);
    ompi_java_forgetIntArray(env, blockLengths, jBlockLengths, cBlockLengths);
    (*env)->ReleaseIntArrayElements(env, disps, jDisps, JNI_ABORT);
    return (jlong)type;
}

JNIEXPORT jlong JNICALL Java_mpi_Datatype_getResized(
        JNIEnv *env, jclass clazz, jlong oldType, jint lb, jint extent)
{
    MPI_Datatype type;
    int rc = MPI_Type_create_resized((MPI_Datatype)oldType, lb, extent, &type);
    ompi_java_exceptionCheck(env, rc);
    return (jlong)type;
}

JNIEXPORT void JNICALL Java_mpi_Datatype_setName(
        JNIEnv *env, jobject jthis, jlong handle, jstring jname)
{
    const char *name = (*env)->GetStringUTFChars(env, jname, NULL);
    int rc = MPI_Type_set_name((MPI_Datatype)handle, (char*)name);
    ompi_java_exceptionCheck(env, rc);
    (*env)->ReleaseStringUTFChars(env, jname, name);
}

JNIEXPORT jstring JNICALL Java_mpi_Datatype_getName(
        JNIEnv *env, jobject jthis, jlong handle)
{
    char name[MPI_MAX_OBJECT_NAME];
    int len;
    int rc = MPI_Type_get_name((MPI_Datatype)handle, name, &len);

    if(ompi_java_exceptionCheck(env, rc))
        return NULL;

    return (*env)->NewStringUTF(env, name);
}

static int typeCopyAttr(MPI_Datatype oldType, int keyval, void *extraState,
                        void *attrValIn, void *attrValOut, int *flag)
{
    return ompi_java_attrCopy(attrValIn, attrValOut, flag);
}

static int typeDeleteAttr(MPI_Datatype oldType, int keyval,
                          void *attrVal, void *extraState)
{
    return ompi_java_attrDelete(attrVal);
}

JNIEXPORT jint JNICALL Java_mpi_Datatype_createKeyval_1jni(
                       JNIEnv *env, jclass clazz)
{
    int rc, keyval;
    rc = MPI_Type_create_keyval(typeCopyAttr, typeDeleteAttr, &keyval, NULL);
    ompi_java_exceptionCheck(env, rc);
    return keyval;
}

JNIEXPORT void JNICALL Java_mpi_Datatype_freeKeyval_1jni(
                       JNIEnv *env, jclass clazz, jint keyval)
{
    int rc = MPI_Type_free_keyval((int*)(&keyval));
    ompi_java_exceptionCheck(env, rc);
}

JNIEXPORT void JNICALL Java_mpi_Datatype_setAttr(
        JNIEnv *env, jobject jthis, jlong type, jint keyval, jbyteArray jval)
{
    void *cval = ompi_java_attrSet(env, jval);
    int rc = MPI_Type_set_attr((MPI_Datatype)type, keyval, cval);
    ompi_java_exceptionCheck(env, rc);
}

JNIEXPORT jobject JNICALL Java_mpi_Datatype_getAttr(
        JNIEnv *env, jobject jthis, jlong type, jint keyval)
{
    int flag;
    void *val;
    int rc = MPI_Type_get_attr((MPI_Datatype)type, keyval, &val, &flag);

    if(ompi_java_exceptionCheck(env, rc) || !flag)
        return NULL;

    return ompi_java_attrGet(env, val);
}

JNIEXPORT void JNICALL Java_mpi_Datatype_deleteAttr(
        JNIEnv *env, jobject jthis, jlong type, jint keyval)
{
    int rc = MPI_Type_delete_attr((MPI_Datatype)type, keyval);
    ompi_java_exceptionCheck(env, rc);
}
