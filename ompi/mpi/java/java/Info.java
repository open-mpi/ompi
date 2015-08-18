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
 */

package mpi;

/**
 * This class represents {@code MPI_Info}.
 */
public final class Info implements Freeable, Cloneable
{
	protected long handle;
	protected static final long NULL = getNull();

	/**
	 * Java binding of the MPI operation {@code MPI_INFO_CREATE}.
	 * @throws MPIException Signals that an MPI exception of some sort has occurred.
	 */
	public Info() throws MPIException
	{
		MPI.check();
		handle = create();
	}

	protected Info(long handle)
	{
		this.handle = handle;
	}

	private native long create();

	protected static Info newEnv()
	{
		return new Info(getEnv());
	}

	private native static long getEnv();
	private native static long getNull();

	/**
	 * Java binding of the MPI operation {@code MPI_INFO_SET}.
	 * @param key   key
	 * @param value value
	 * @throws MPIException Signals that an MPI exception of some sort has occurred.
	 */
	public void set(String key, String value) throws MPIException
	{
		MPI.check();
		set(handle, key, value);
	}

	private native void set(long handle, String key, String value)
			throws MPIException;

	/**
	 * Java binding of the MPI operation {@code MPI_INFO_SET}.
	 * @param key key
	 * @return value or {@code null} if key is not defined
	 * @throws MPIException Signals that an MPI exception of some sort has occurred.
	 */
	public String get(String key) throws MPIException
	{
		MPI.check();
		return get(handle, key);
	}

	private native String get(long handle, String key) throws MPIException;

	/**
	 * Java binding of the MPI operation {@code MPI_INFO_SET}.
	 * @param key key
	 * @throws MPIException Signals that an MPI exception of some sort has occurred.
	 */
	public void delete(String key) throws MPIException
	{
		MPI.check();
		delete(handle, key);
	}

	private native void delete(long handle, String key) throws MPIException;

	/**
	 * Java binding of the MPI operation {@code MPI_INFO_GET_NKEYS}.
	 * @return number of defined keys
	 * @throws MPIException Signals that an MPI exception of some sort has occurred.
	 */
	public int size() throws MPIException
	{
		MPI.check();
		return size(handle);
	}

	private native int size(long handle) throws MPIException;

	/**
	 * Java binding of the MPI operation {@code MPI_INFO_GET_NTHKEY}.
	 * @param i key number
	 * @return key
	 * @throws MPIException Signals that an MPI exception of some sort has occurred.
	 */
	public String getKey(int i) throws MPIException
	{
		MPI.check();
		return getKey(handle, i);
	}

	private native String getKey(long handle, int i) throws MPIException;

	/**
	 * Java binding of the MPI operation {@code MPI_INFO_DUP}.
	 * <p>It is recommended to use {@link #dup} instead of {@link #clone}
	 * because the last can't throw an {@link mpi.MPIException}.
	 * @return info object
	 */
	@Override public Info clone()
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
	 * Java binding of the MPI operation {@code MPI_INFO_DUP}.
	 * @return info object
	 * @throws MPIException Signals that an MPI exception of some sort has occurred.
	 */
	public Info dup() throws MPIException
	{
		MPI.check();
		return new Info(dup(handle));
	}

	private native long dup(long handle) throws MPIException;

	/**
	 * Java binding of the MPI operation {@code MPI_INFO_FREE}.
	 * @throws MPIException Signals that an MPI exception of some sort has occurred.
	 */
	@Override public void free() throws MPIException
	{
		MPI.check();
		handle = free(handle);
	}

	private native long free(long handle) throws MPIException;

	/**
	 * Tests if the info object is {@code MPI_INFO_NULL} (has been freed).
	 * @return true if the info object is {@code MPI_INFO_NULL}, false otherwise.
	 */
	public boolean isNull()
	{
		return isNull(handle);
	}

	private native boolean isNull(long handle);

} // Info
