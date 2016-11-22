/*
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 *
 * This file is almost a complete re-write for Open MPI compared to the
 * original mpiJava package. Its license and copyright are listed below.
 * See <path to ompi/mpi/java/README> for more information.
 *
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *
 * File         : Version.java
 * Author       : Nathaniel Graham
 * Created      : Thu Jul  30 09:34 2015
 */

#include "ompi_config.h"

#include <stdlib.h>
#ifdef HAVE_TARGETCONDITIONALS_H
#include <TargetConditionals.h>
#endif

#include "mpi.h"
#include "mpi_Count.h"
#include "mpiJava.h"

JNIEXPORT void JNICALL Java_mpi_Count_initCount(JNIEnv *env, jclass jthis)
{
	jclass c = (*env)->FindClass(env, "mpi/Count");
	ompi_java.CountClass = (*env)->NewGlobalRef(env, c);
	ompi_java.CountInit = (*env)->GetMethodID(env, ompi_java.CountClass, "<init>", "(J)V");

	(*env)->DeleteLocalRef(env, c);
}
