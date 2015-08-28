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
 * File         : Intercomm.java
 * Author       : Xinying Li
 * Created      : Thu Apr  9 12:22:15 1998
 * Revision     : $Revision: 1.5 $
 * Updated      : $Date: 1999/09/14 20:50:11 $
 * Copyright: Northeast Parallel Architectures Center
 *            at Syracuse University 1998
 */

package mpi;

/**
 * This class represents intercommunicators.
 */
public final class Intercomm extends Comm
{
	protected Intercomm(long handle)
	{
		super(handle);
	}

	protected Intercomm(long[] commRequest)
	{
		super(commRequest);
	}

	/**
	 * Duplicates this communicator.
	 * <p>Java binding of {@code MPI_COMM_DUP}.
	 * <p>It is recommended to use {@link #dup} instead of {@link #clone}
	 * because the last can't throw an {@link mpi.MPIException}.
	 * @return copy of this communicator
	 */
	@Override public Intercomm clone()
	{
		try
		{
			return dup();
		}
		catch(MPIException e)
		{
			throw new RuntimeException(e.getMessage());
		}
	}

	/**
	 * Duplicates this communicator.
	 * <p>Java binding of {@code MPI_COMM_DUP}.
	 * @return copy of this communicator
	 * @throws MPIException Signals that an MPI exception of some sort has occurred.
	 */
	@Override public Intercomm dup() throws MPIException
	{
		MPI.check();
		return new Intercomm(dup(handle));
	}

	/**
	 * Duplicates this communicator.
	 * <p>Java binding of {@code MPI_COMM_IDUP}.
	 * <p>The new communicator can't be used before the operation completes.
	 * The request object must be obtained calling {@link #getRequest}.
	 * @return copy of this communicator
	 * @throws MPIException Signals that an MPI exception of some sort has occurred.
	 */
	@Override public Intercomm iDup() throws MPIException
	{
		MPI.check();
		return new Intercomm(iDup(handle));
	}

	/**
	* Duplicates this communicator with the info object used in the call.
	* <p>Java binding of {@code MPI_COMM_DUP_WITH_INFO}.
	* @param info	info object to associate with the new communicator
	* @return copy of this communicator
	* @throws MPIException Signals that an MPI exception of some sort has occurred.
	*/
	@Override public Intercomm dupWithInfo(Info info) throws MPIException
	{
	    MPI.check();
	    return new Intercomm(dupWithInfo(handle, info.handle));
	}

	// Inter-Communication

	/**
	 * Size of remote group.
	 * <p>Java binding of the MPI operation {@code MPI_COMM_REMOTE_SIZE}.
	 * @return number of process in remote group of this communicator
	 * @throws MPIException Signals that an MPI exception of some sort has occurred.
	 */
	public int getRemoteSize() throws MPIException
	{
		MPI.check();
		return getRemoteSize_jni();
	}

	private native int getRemoteSize_jni() throws MPIException;

	/**
	 * Return the remote group.
	 * <p>Java binding of the MPI operation {@code MPI_COMM_REMOTE_GROUP}.
	 * @return remote group of this communicator
	 * @throws MPIException Signals that an MPI exception of some sort has occurred.
	 */
	public Group getRemoteGroup() throws MPIException
	{
		MPI.check();
		return new Group(getRemoteGroup_jni());
	}

	private native long getRemoteGroup_jni();

	/**
	 * Creates an intracommuncator from an intercommunicator
	 * <p>Java binding of the MPI operation {@code MPI_INTERCOMM_MERGE}.
	 * @param high true if the local group has higher ranks in combined group
	 * @return new intra-communicator
	 * @throws MPIException Signals that an MPI exception of some sort has occurred.
	 */
	public Intracomm merge(boolean high) throws MPIException
	{
		MPI.check();
		return new Intracomm(merge_jni(high));
	}

	private native long merge_jni(boolean high);

	/**
	 * Java binding of {@code MPI_COMM_GET_PARENT}.
	 * @return the parent communicator
	 * @throws MPIException Signals that an MPI exception of some sort has occurred.
	 */
	public static Intercomm getParent() throws MPIException
	{
		MPI.check();
		return new Intercomm(getParent_jni());
	}

	private native static long getParent_jni() throws MPIException;

} // Intercomm
