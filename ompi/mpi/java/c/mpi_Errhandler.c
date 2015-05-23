/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/*
 * This file is almost a complete re-write for Open MPI compared to the
 * original mpiJava package. Its license and copyright are listed below.
 * See <path to ompi/mpi/java/README> for more information.
 */
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

JNIEXPORT jlong JNICALL Java_mpi_Errhandler_getFatal(JNIEnv *env, jclass clazz)
{
    return (jlong)MPI_ERRORS_ARE_FATAL;
}

JNIEXPORT jlong JNICALL Java_mpi_Errhandler_getReturn(JNIEnv *env, jclass clazz)
{
    return (jlong)MPI_ERRORS_RETURN;
}
