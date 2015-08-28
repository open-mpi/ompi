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
 * File         : Status.java
 * Author       : Sang Lim, Sung-Hoon Ko, Xinying Li, Bryan Carpenter
 * Created      : Thu Apr  9 12:22:15 1998
 * Revision     : $Revision: 1.15 $
 * Updated      : $Date: 2003/01/16 16:39:34 $
 * Copyright: Northeast Parallel Architectures Center
 *            at Syracuse University 1998
 */

package mpi;

/**
 * This class represents {@code MPI_Status}.
 */
public final class Status
{
	protected final long[] data;

	static
	{
		init();
	}

	private static native void init();

	/**
	 * Status objects must be created only by the MPI methods.
	 */
	protected Status()
	{
		data = new long[6];
	}

	/**
	 * Returns the number of received entries.
	 * <p>Java binding of the MPI operation {@code MPI_GET_COUNT}.
	 * @param datatype datatype of each item in receive buffer
	 * @return number of received entries
	 * @throws MPIException Signals that an MPI exception of some sort has occurred.
	 */
	public int getCount(Datatype datatype) throws MPIException
	{
		MPI.check();
		int  i = 0;
		int  source    = (int)data[i++];
		int  tag       = (int)data[i++];
		int  error     = (int)data[i++];
		int  cancelled = (int)data[i++];
		long ucount    = data[i++];
		return getCount(source, tag, error, cancelled, ucount, datatype.handle);
	}

	private native int getCount(
			int source, int tag, int error,
			int cancelled, long ucount, long datatype) throws MPIException;

	/**
	 * Tests if the communication was cancelled.
	 * <p>Java binding of the MPI operation {@code MPI_TEST_CANCELLED}.
	 * @return true if the operation was succesfully cancelled, false otherwise
	 * @throws MPIException Signals that an MPI exception of some sort has occurred.
	 */
	public boolean isCancelled() throws MPIException
	{
		MPI.check();
		int  i = 0;
		int  source    = (int)data[i++];
		int  tag       = (int)data[i++];
		int  error     = (int)data[i++];
		int  cancelled = (int)data[i++];
		long ucount    = data[i++];
		return isCancelled(source, tag, error, cancelled, ucount);
	}

	private native boolean isCancelled(
			int source, int tag, int error, int cancelled, long ucount)
					throws MPIException;

	/**
	 * Retrieves the number of basic elements from status.
	 * <p>Java binding of the MPI operation {@code MPI_GET_ELEMENTS}.
	 * @param datatype datatype used by receive operation
	 * @return number of received basic elements
	 * @throws MPIException Signals that an MPI exception of some sort has occurred.
	 */
	public int getElements(Datatype datatype) throws MPIException
	{
		MPI.check();
		int  i = 0;
		int  source    = (int)data[i++];
		int  tag       = (int)data[i++];
		int  error     = (int)data[i++];
		int  cancelled = (int)data[i++];
		long ucount    = data[i++];
		return getElements(source, tag, error, cancelled, ucount, datatype.handle);
	}

	private native int getElements(
			int source, int tag, int error,
			int cancelled, long ucount, long datatype) throws MPIException;

	/**
	 * Retrieves the number of basic elements from status.
	 * <p>Java binding of the MPI operation {@code MPI_GET_ELEMENTS_X}.
	 * @param datatype datatype used by receive operation
	 * @return number of received basic elements
	 * @throws MPIException Signals that an MPI exception of some sort has occurred.
	 */
	public Count getElementsX(Datatype datatype) throws MPIException
	{
		MPI.check();
		int  i = 0;
		int  source    = (int)data[i++];
		int  tag       = (int)data[i++];
		int  error     = (int)data[i++];
		int  cancelled = (int)data[i++];
		long ucount    = data[i++];
		return getElementsX(source, tag, error, cancelled, ucount, datatype.handle);
	}

	private native Count getElementsX(
			int source, int tag, int error,
			int cancelled, long ucount, long datatype) throws MPIException;

	/**
	 * Sets the number of basic elements for this status object.
	 * <p>Java binding of the MPI operation {@code MPI_STATUS_SET_ELEMENTS}.
	 * @param datatype 	datatype used by receive operation
	 * @param count		number of elements to associate with the status
	 * @throws MPIException Signals that an MPI exception of some sort has occurred.
	 */
	public void setElements(Datatype datatype, int count) throws MPIException
	{
		MPI.check();
		int  i = 0;
		int  source    = (int)data[i++];
		int  tag       = (int)data[i++];
		int  error     = (int)data[i++];
		int  cancelled = (int)data[i++];
		long ucount    = data[i++];
		data[4] = setElements(source, tag, error, cancelled, ucount, datatype.handle, count);
	}

	private native int setElements(
			int source, int tag, int error,
			int cancelled, long ucount, long datatype, int count) throws MPIException;

	/**
	 * Sets the number of basic elements for this status object.
	 * <p>Java binding of the MPI operation {@code MPI_STATUS_SET_ELEMENTS_X}.
	 * @param datatype 	datatype used by receive operation
	 * @param count		number of elements to associate with the status
	 * @throws MPIException Signals that an MPI exception of some sort has occurred.
	 */
	public void setElementsX(Datatype datatype, Count count) throws MPIException
	{
		MPI.check();
		int  i = 0;
		int  source    = (int)data[i++];
		int  tag       = (int)data[i++];
		int  error     = (int)data[i++];
		int  cancelled = (int)data[i++];
		long ucount    = data[i++];
		data[4] = setElementsX(source, tag, error, cancelled, ucount, datatype.handle, count.getCount());
	}

	private native long setElementsX(
			int source, int tag, int error,
			int cancelled, long ucount, long datatype, long count) throws MPIException;

	/**
	 * Sets the cancelled flag.
	 * <p>Java binding of the MPI operation {@code MPI_STATUS_SET_CANCELLED}.
	 * @param flag	if true indicates request was cancelled
	 * @throws MPIException Signals that an MPI exception of some sort has occurred.
	 */
	public void setCancelled(boolean flag) throws MPIException
	{
		MPI.check();
		int  i = 0;
		int  source    = (int)data[i++];
		int  tag       = (int)data[i++];
		int  error     = (int)data[i++];
		int  cancelled = (int)data[i++];
		long ucount    = data[i++];

		if(flag) {
			setCancelled(source, tag, error, cancelled, ucount, 1);
			data[3] = 1;
		} else {
			setCancelled(source, tag, error, cancelled, ucount, 0);
			data[3] = 0;
		}

	}

	private native void setCancelled(
			int source, int tag, int error,
			int cancelled, long ucount, int flag) throws MPIException;

	/**
	 * Returns the "source" of message.
	 * <p>Java binding of the MPI value {@code MPI_SOURCE}.
	 * @return source of message
	 */
	public int getSource()
	{
		return (int)data[0];
	}

	/**
	 * Returns the "tag" of message.
	 * <p>Java binding of the MPI value {@code MPI_TAG}.
	 * @return tag of message
	 */
	public int getTag()
	{
		return (int)data[1];
	}

	/**
	 * Returns the {@code MPI_ERROR} of message.
	 * @return error of message.
	 */
	public int getError()
	{
		return (int)data[2];
	}

	/**
	 * Returns the index of message.
	 * @return index of message.
	 */
	public int getIndex()
	{
		return (int)data[5];
	}

} // Status
