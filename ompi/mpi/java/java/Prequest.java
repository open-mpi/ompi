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
 * File         : Prequest.java
 * Author       : Sang Lim, Xinying Li, Bryan Carpenter
 * Created      : Thu Apr  9 12:22:15 1998
 * Revision     : $Revision: 1.11 $
 * Updated      : $Date: 2001/10/22 21:07:55 $
 * Copyright: Northeast Parallel Architectures Center
 *            at Syracuse University 1998
 */

package mpi;

/**
 * Persistent request object.
 */
public final class Prequest extends Request
{
	/**
	 * Constructor used by {@code sendInit}, etc.
	 * @param handle	Handle for the Prequest object
	 */
	protected Prequest(long handle)
	{
		super(handle);
	}

	/**
	 * Activate a persistent communication request.
	 * <p>Java binding of the MPI operation {@code MPI_START}.
	 * The communication is completed by using the request in
	 * one of the {@code wait} or {@code test} operations.
	 * On successful completion the request becomes inactive again.
	 * It can be reactivated by a further call to {@code Start}.
	 * @throws MPIException Signals that an MPI exception of some sort has occurred.
	 */
	public void start() throws MPIException
	{
		handle = start(handle);
	}

	private native long start(long request) throws MPIException;

	/**
	 * Activate a list of communication requests.
	 * <p>Java binding of the MPI operation {@code MPI_STARTALL}.
	 * @param requests array of requests
	 * @throws MPIException Signals that an MPI exception of some sort has occurred.
	 */
	public static void startAll(Prequest[] requests) throws MPIException
	{
		MPI.check();
		long[] r = getHandles(requests);
		startAll(r);
		setHandles(requests, r);
	}

	private native static void startAll(long[] requests) throws MPIException;

} // Prequest
