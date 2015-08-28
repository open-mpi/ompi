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
 * IMPLEMENTATION DETAILS
 *
 * All methods with buffers that can be direct or non direct have
 * a companion argument 'db' which is true if the buffer is direct.
 *
 * Checking if a buffer is direct is faster in Java than C.
 */

package mpi;

import java.nio.*;
import static mpi.MPI.assertDirectBuffer;

/**
 * This class represents {@code MPI_Message}.
 */
public final class Message
{
	protected long handle;
	private static long NULL, NO_PROC;

	static
	{
		init();
	}

	private static native void init();

	/**
	 * Creates a {@code MPI_MESSAGE_NULL}.
	 */
	public Message()
	{
		handle = NULL;
	}

	/**
	 * Tests if the message is {@code MPI_MESSAGE_NULL}.
	 * @return true if the message is {@code MPI_MESSAGE_NULL}.
	 */
	public boolean isNull()
	{
		return handle == NULL;
	}

	/**
	 * Tests if the message is {@code MPI_MESSAGE_NO_PROC}.
	 * @return true if the message is {@code MPI_MESSAGE_NO_PROC}.
	 */
	public boolean isNoProc()
	{
		return handle == NO_PROC;
	}

	/**
	 * Java binding of {@code MPI_MPROBE}.
	 * @param source rank of the source
	 * @param tag    message tag
	 * @param comm   communicator
	 * @return status object
	 * @throws MPIException Signals that an MPI exception of some sort has occurred.
	 */
	public Status mProbe(int source, int tag, Comm comm) throws MPIException
	{
		MPI.check();
		Status status = new Status();
		handle = mProbe(source, tag, comm.handle, status.data);
		return status;
	}

	private native long mProbe(int source, int tag, long comm, long[] status)
			throws MPIException;

	/**
	 * Java binding of {@code MPI_IMPROBE}.
	 * @param source rank of the source
	 * @param tag    message tag
	 * @param comm   communicator
	 * @return status object if there is a message, {@code null} otherwise
	 * @throws MPIException Signals that an MPI exception of some sort has occurred.
	 */
	public Status imProbe(int source, int tag, Comm comm) throws MPIException
	{
		MPI.check();
		return imProbe(source, tag, comm.handle);
	}

	private native Status imProbe(int source, int tag, long comm)
			throws MPIException;

	/**
	 * Java binding of {@code MPI_MRECV}.
	 * @param buf   receive buffer
	 * @param count number of elements in receve buffer
	 * @param type  datatype of each receive buffer element
	 * @return status object
	 * @throws MPIException Signals that an MPI exception of some sort has occurred.
	 */
	public Status mRecv(Object buf, int count, Datatype type) throws MPIException
	{
		MPI.check();
		int off = 0;
		boolean db = false;
		Status status = new Status();

		if(buf instanceof Buffer && !(db = ((Buffer)buf).isDirect()))
		{
			off = type.getOffset(buf);
			buf = ((Buffer)buf).array();
		}

		handle = mRecv(handle, buf, db, off, count,
				type.handle, type.baseType, status.data);

		return status;
	}

	private native long mRecv(
			long message, Object buf, boolean db, int offset, int count,
			long type, int baseType, long[] status) throws MPIException;

	/**
	 * Java binding of {@code MPI_IMRECV}.
	 * @param buf   receive buffer
	 * @param count number of elements in receve buffer
	 * @param type  datatype of each receive buffer element
	 * @return request object
	 * @throws MPIException Signals that an MPI exception of some sort has occurred.
	 */
	public Request imRecv(Buffer buf, int count, Datatype type)
			throws MPIException
	{
		MPI.check();
		assertDirectBuffer(buf);
		Request req = new Request(imRecv(handle, buf, count, type.handle));
		req.addRecvBufRef(buf);
		return req;
	}

	private native long imRecv(long message, Object buf, int count, long type)
			throws MPIException;

} // Message
