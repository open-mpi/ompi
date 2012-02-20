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
 * File         : mpi_Errhandler.c
 * Headerfile   : mpi_Errhandler.h 
 * Author       : Bryan Carpenter
 * Created      : 1999
 * Revision     : $Revision: 1.2 $
 * Updated      : $Date: 2001/08/07 16:36:15 $
 * Copyright: Northeast Parallel Architectures Center
 *            at Syracuse University 1998
 */
#include "ompi_config.h"

#ifdef HAVE_TARGETCONDITIONALS_H
#include <TargetConditionals.h>
#endif

#include  <mpi.h>
#include "mpi_Errhandler.h"
#include "mpiJava.h"

jfieldID ErrhandleID;

/*
 * Class:     mpi_Errhandler
 * Method:    init
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_mpi_Errhandler_init(JNIEnv *env, jclass thisClass)
{
    ompi_java.ErrhandleID = (*env)->GetFieldID(env,thisClass,"handle","J");                      
}

/*
 * Class:     mpi_Errhandler
 * Method:    GetErrhandler
 * Signature: (I)V
 */
JNIEXPORT void JNICALL Java_mpi_Errhandler_GetErrhandler(JNIEnv *env, jobject jthis, jint type)
{
    switch (type) {
    case 0:
        (*env)->SetLongField(env,jthis, ompi_java.ErrhandleID, (jlong)MPI_ERRORS_RETURN);
    case 1:
        (*env)->SetLongField(env,jthis, ompi_java.ErrhandleID, (jlong)MPI_ERRORS_ARE_FATAL);
    }
}


