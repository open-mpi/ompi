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
 * File         : MPIException.java
 * Author       : Bryan Carpenter
 * Created      : Tue Sep 14 13:03:57 EDT 1999
 * Revision     : $Revision: 1.1 $
 * Updated      : $Date: 1999/09/14 22:01:52 $
 * Copyright: Northeast Parallel Architectures Center
 *            at Syracuse University 1999
 */

package mpi;

/**
 * Signals that an MPI exception of some sort has occurred.
 * <p>The Java binding of the MPI operation {@code MPI_Error_string} is the
 * method {@code getMessage}, which is inherited from the class Exception.
 */
public final class MPIException extends Exception
{
private int errorCode, errorClass;

protected MPIException(int code, int clazz, String message)
{
    super(message);
    errorCode  = code;
    errorClass = clazz;
}

/**
 * Creates an exception.
 * @param message message associated to the exception
 */
public MPIException(String message)
{
    super(message);
}

/**
 * Creates an exception:
 * @param cause cause associated to the exception
 */
public MPIException(Throwable cause)
{
    super(cause);
    setStackTrace(cause.getStackTrace());
}

/**
 * Gets the MPI error code.
 * @return error code
 */
public int getErrorCode()
{
    return errorCode;
}

/**
 * Gets the MPI error class.
 * @return error class
 */
public int getErrorClass()
{
    return errorClass;
}

} // MPIException
