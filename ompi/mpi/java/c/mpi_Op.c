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
 * File         : mpi_Op.c
 * Headerfile   : mpi_Op.h
 * Author       : Xinying Li, Bryan Carpenter
 * Created      : Thu Apr  9 12:22:15 1998
 * Revision     : $Revision: 1.7 $
 * Updated      : $Date: 2003/01/16 16:39:34 $
 * Copyright: Northeast Parallel Architectures Center
 *            at Syracuse University 1998
 */
#include "ompi_config.h"

#ifdef HAVE_TARGETCONDITIONALS_H
#include <TargetConditionals.h>
#endif

#include "mpi.h"
#include "mpi_Op.h"
#include "mpiJava.h"
#include "ompi/op/op.h"

JNIEXPORT void JNICALL Java_mpi_Op_init(JNIEnv *env, jclass clazz)
{
    ompi_java.OpHandle  = (*env)->GetFieldID(env, clazz, "handle", "J");
    ompi_java.OpCommute = (*env)->GetFieldID(env, clazz, "commute", "Z");

    ompi_java.OpCall = (*env)->GetMethodID(env, clazz, "call",
                       "(Ljava/lang/Object;Ljava/lang/Object;I)V");
}

JNIEXPORT void JNICALL Java_mpi_Op_getOp(JNIEnv *env, jobject jthis, jint type)
{
    static MPI_Op Ops[] = {
        MPI_OP_NULL, MPI_MAX, MPI_MIN, MPI_SUM,
        MPI_PROD, MPI_LAND, MPI_BAND, MPI_LOR, MPI_BOR, MPI_LXOR,
        MPI_BXOR, MPI_MINLOC, MPI_MAXLOC
    };
    (*env)->SetLongField(env,jthis, ompi_java.OpHandle, (jlong)Ops[type]);
}

static jobject setBooleanArray(JNIEnv *env, void *vec, int len)
{
    jobject obj = (*env)->NewBooleanArray(env, len);

    if(obj != NULL)
        (*env)->SetBooleanArrayRegion(env, obj, 0, len, vec);
    
    return obj;
}

static void getBooleanArray(JNIEnv *env, jobject obj, void *vec, int len)
{
    (*env)->GetBooleanArrayRegion(env, obj, 0, len, vec);
}

static void opIntercept(void *invec, void *inoutvec, int *count,
                        MPI_Datatype *datatype, int baseType,
                        void *jnienv, void *object)
{
    JNIEnv  *env  = jnienv;
    jobject jthis = object;
    jobject jin, jio;

    MPI_Aint extent;
    int rc = MPI_Type_extent(*datatype, &extent);

    if(ompi_java_exceptionCheck(env, rc))
        return;

    int len = (*count) * extent;

    if(baseType == 4)
    {
        jin = setBooleanArray(env, invec, len);
        jio = setBooleanArray(env, inoutvec, len);
    }
    else
    {
        jin = (*env)->NewDirectByteBuffer(env, invec, len);
        jio = (*env)->NewDirectByteBuffer(env, inoutvec, len);
    }

    if((*env)->ExceptionCheck(env))
        return;

    (*env)->CallVoidMethod(env, jthis, ompi_java.OpCall, jin, jio, *count);

    if(baseType == 4)
        getBooleanArray(env, jio, inoutvec, len);

    (*env)->DeleteLocalRef(env, jin);
    (*env)->DeleteLocalRef(env, jio);
}

MPI_Op ompi_java_op_getHandle(JNIEnv *env, jobject jOp, jlong hOp, int baseType)
{
    MPI_Op op = (MPI_Op)hOp;

    if(op == NULL)
    {
        /* It is an uninitialized user Op. */
        int commute = (*env)->GetBooleanField(
                      env, jOp, ompi_java.OpCommute);

        int rc = MPI_Op_create((MPI_User_function*)opIntercept, commute, &op);

        if(ompi_java_exceptionCheck(env, rc))
            return NULL;

        (*env)->SetLongField(env, jOp, ompi_java.OpHandle, (jlong)op);
        ompi_op_set_java_callback(op, env, jOp, baseType);
    }

    return op;
}

JNIEXPORT void JNICALL Java_mpi_Op_free(JNIEnv *env, jobject jthis)
{
    MPI_Op op = (MPI_Op)((*env)->GetLongField(env, jthis, ompi_java.OpHandle));

    if(op != NULL && op != MPI_OP_NULL)
    {
        int rc = MPI_Op_free(&op);
        ompi_java_exceptionCheck(env, rc);
        ((*env)->SetLongField(env,jthis,ompi_java.OpHandle,(long)MPI_OP_NULL));
    }
}

JNIEXPORT jboolean JNICALL Java_mpi_Op_isNull(JNIEnv *env, jobject jthis)
{
    MPI_Op op = (MPI_Op)((*env)->GetLongField(env, jthis, ompi_java.OpHandle));
    return op == NULL || op == MPI_OP_NULL ? JNI_TRUE : JNI_FALSE;
}
