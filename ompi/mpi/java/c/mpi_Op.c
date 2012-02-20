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

/*
 * Class:     mpi_Op
 * Method:    init
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_mpi_Op_init(JNIEnv *env, jclass thisClass) 
{
    ompi_java.OphandleID=(*env)->GetFieldID(env,thisClass,"handle","J");
}

/*
 * Class:     mpi_Op
 * Method:    GetOp
 * Signature: (I)J
 */
JNIEXPORT void JNICALL Java_mpi_Op_GetOp(JNIEnv *env, jobject jthis, jint type) 
{
    static MPI_Op Ops[] = {
        MPI_OP_NULL, MPI_MAX, MPI_MIN, MPI_SUM,
        MPI_PROD, MPI_LAND, MPI_BAND, MPI_LOR, MPI_BOR, MPI_LXOR,
        MPI_BXOR, MPI_MINLOC, MPI_MAXLOC
    };
    (*env)->SetLongField(env,jthis, ompi_java.OphandleID, (jlong)Ops[type]);
}

/*
 * Class:     mpi_Op
 * Method:    free
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_mpi_Op_free(JNIEnv *env, jobject jthis)
{
    MPI_Op op;
    op=(MPI_Op)((*env)->GetLongField(env,jthis,ompi_java.OphandleID));
    if(op != MPI_OP_NULL)
        MPI_Op_free(&op);
}

