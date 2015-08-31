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

package mpi;

import java.nio.*;

/**
 * This class represents {@code MPI_Win}.
 */
public final class Win implements Freeable
{
	private long handle;
	public static final int WIN_NULL = 0;
	public static final int FLAVOR_PRIVATE = 0;
	public static final int FLAVOR_SHARED = 1;

	/**
	 * Java binding of {@code MPI_WIN_CREATE}.
	 * @param base     initial address of window
	 * @param size     size of window (buffer elements)
	 * @param dispUnit local unit size for displacements (buffer elements)
	 * @param info     info object
	 * @param comm     communicator
	 * @throws MPIException Signals that an MPI exception of some sort has occurred.
	 */
	public Win(Buffer base, int size, int dispUnit, Info info, Comm comm)
			throws MPIException
	{
		if(!base.isDirect())
			throw new IllegalArgumentException("The buffer must be direct.");

		int baseSize;

		if(base instanceof ByteBuffer)
			baseSize = 1;
		else if(base instanceof CharBuffer || base instanceof ShortBuffer)
			baseSize = 2;
		else if(base instanceof IntBuffer || base instanceof FloatBuffer)
			baseSize = 4;
		else if(base instanceof LongBuffer || base instanceof DoubleBuffer)
			baseSize = 8;
		else
			throw new AssertionError();

		int sizeBytes = size * baseSize,
				dispBytes = dispUnit * baseSize;

		handle = createWin(base, sizeBytes, dispBytes, info.handle, comm.handle);
	}

	private native long createWin(
			Buffer base, int size, int dispUnit, long info, long comm)
					throws MPIException;

	/**
	 * Java binding of {@code MPI_WIN_ALLOCATE} and {@code MPI_WIN_ALLOCATE_SHARED}.
	 * @param size     size of window (buffer elements)
	 * @param dispUnit 	local unit size for displacements (buffer elements)
	 * @param info     	info object
	 * @param comm     	communicator
	 * @param base     	initial address of window
	 * @param flavor	FLAVOR_PRIVATE or FLAVOR_SHARED
	 * @throws MPIException Signals that an MPI exception of some sort has occurred.
	 */
	public Win(int size, int dispUnit, Info info, Comm comm, Buffer base, int flavor)
			throws MPIException
	{
		if(!base.isDirect())
			throw new IllegalArgumentException("The buffer must be direct.");

		int baseSize;

		if(base instanceof ByteBuffer)
			baseSize = 1;
		else if(base instanceof CharBuffer || base instanceof ShortBuffer)
			baseSize = 2;
		else if(base instanceof IntBuffer || base instanceof FloatBuffer)
			baseSize = 4;
		else if(base instanceof LongBuffer || base instanceof DoubleBuffer)
			baseSize = 8;
		else
			throw new AssertionError();

		int sizeBytes = size * baseSize,
				dispBytes = dispUnit * baseSize;

		if(flavor == 0) {
			handle = allocateWin(sizeBytes, dispBytes, info.handle, comm.handle, base);
		} else if(flavor == 1) {
			handle = allocateSharedWin(sizeBytes, dispBytes, info.handle, comm.handle, base);
		}
	}

	private native long allocateWin(
			int size, int dispUnit, long info, long comm, Buffer base)
					throws MPIException;

	private native long allocateSharedWin(
			int size, int dispUnit, long info, long comm, Buffer base)
					throws MPIException;

	/**
	 * Java binding of {@code MPI_WIN_CREATE_DYNAMIC}.
	 * @param info     info object
	 * @param comm     communicator
	 * @throws MPIException Signals that an MPI exception of some sort has occurred.
	 */
	public Win(Info info, Comm comm)
			throws MPIException
	{
		handle = createDynamicWin(info.handle, comm.handle);
	}

	private native long createDynamicWin(
			long info, long comm)
					throws MPIException;

	private int getBaseType(Datatype orgType, Datatype targetType)
	{
		int baseType = orgType.baseType;

		if(baseType != targetType.baseType)
		{
			throw new IllegalArgumentException(
					"Both datatype arguments must be constructed "+
					"from the same predefined datatype.");
		}

		return baseType;
	}

	/**
	 * Java binding of {@code MPI_WIN_ATTACH}.
	 * @param base     initial address of window
	 * @param size     size of window (buffer elements)
	 * @throws MPIException Signals that an MPI exception of some sort has occurred.
	 */
	public void attach(Buffer base, int size) throws MPIException
	{
		MPI.check();
		if(!base.isDirect())
			throw new IllegalArgumentException("The buffer must be direct.");

		int baseSize;

		if(base instanceof ByteBuffer)
			baseSize = 1;
		else if(base instanceof CharBuffer || base instanceof ShortBuffer)
			baseSize = 2;
		else if(base instanceof IntBuffer || base instanceof FloatBuffer)
			baseSize = 4;
		else if(base instanceof LongBuffer || base instanceof DoubleBuffer)
			baseSize = 8;
		else
			throw new AssertionError();

		int sizeBytes = size * baseSize;

		attach(handle, base, sizeBytes);
	}

	private native void attach(long win, Buffer base, int size) throws MPIException;

	/**
	 * Java binding of {@code MPI_WIN_DETACH}.
	 * @param base     initial address of window
	 * @throws MPIException Signals that an MPI exception of some sort has occurred.
	 */
	public void detach(Buffer base) throws MPIException
	{
		MPI.check();
		if(!base.isDirect())
			throw new IllegalArgumentException("The buffer must be direct.");

		detach(handle, base);
	}

	private native void detach(long win, Buffer base) throws MPIException;

	/**
	 * Java binding of the MPI operation {@code MPI_GET_GROUP}.
	 * @return group of processes which share access to the window
	 * @throws MPIException Signals that an MPI exception of some sort has occurred.
	 */
	public Group getGroup() throws MPIException
	{
		MPI.check();
		return new Group(getGroup(handle));
	}

	private native long getGroup(long win) throws MPIException;

	/**
	 * Java binding of {@code MPI_PUT}.
	 * @param origin      origin buffer
	 * @param orgCount    number of entries in origin buffer
	 * @param orgType     datatype of each entry in origin buffer
	 * @param targetRank  rank of target
	 * @param targetDisp  displacement from start of window to target buffer
	 * @param targetCount number of entries in target buffer
	 * @param targetType  datatype of each entry in target buffer
	 * @throws MPIException Signals that an MPI exception of some sort has occurred.
	 */
	public void put(Buffer origin, int orgCount, Datatype orgType,
			int targetRank, int targetDisp, int targetCount,
			Datatype targetType)
					throws MPIException
	{
		MPI.check();

		if(!origin.isDirect())
			throw new IllegalArgumentException("The origin must be direct buffer.");

		put(handle, origin, orgCount, orgType.handle,
				targetRank, targetDisp, targetCount, targetType.handle,
				getBaseType(orgType, targetType));
	}

	private native void put(
			long win, Buffer origin, int orgCount, long orgType,
			int targetRank, int targetDisp, int targetCount, long targetType,
			int baseType) throws MPIException;

	/**
	 * Java binding of {@code MPI_GET}.
	 * @param origin      origin buffer
	 * @param orgCount    number of entries in origin buffer
	 * @param orgType     datatype of each entry in origin buffer
	 * @param targetRank  rank of target
	 * @param targetDisp  displacement from start of window to target buffer
	 * @param targetCount number of entries in target buffer
	 * @param targetType  datatype of each entry in target buffer
	 * @throws MPIException Signals that an MPI exception of some sort has occurred.
	 */
	public void get(Buffer origin, int orgCount, Datatype orgType,
			int targetRank, int targetDisp, int targetCount,
			Datatype targetType)
					throws MPIException
	{
		MPI.check();

		if(!origin.isDirect())
			throw new IllegalArgumentException("The origin must be direct buffer.");

		get(handle, origin, orgCount, orgType.handle,
				targetRank, targetDisp, targetCount, targetType.handle,
				getBaseType(orgType, targetType));
	}

	private native void get(
			long win, Buffer origin, int orgCount, long orgType,
			int targetRank, int targetDisp, int targetCount, long targetType,
			int baseType) throws MPIException;

	/**
	 * Java binding of {@code MPI_ACCUMULATE}.
	 * @param origin      origin buffer
	 * @param orgCount    number of entries in origin buffer
	 * @param orgType     datatype of each entry in origin buffer
	 * @param targetRank  rank of target
	 * @param targetDisp  displacement from start of window to target buffer
	 * @param targetCount number of entries in target buffer
	 * @param targetType  datatype of each entry in target buffer
	 * @param op          reduce operation
	 * @throws MPIException Signals that an MPI exception of some sort has occurred.
	 */
	public void accumulate(Buffer origin, int orgCount, Datatype orgType,
			int targetRank, int targetDisp, int targetCount,
			Datatype targetType, Op op)
					throws MPIException
	{
		MPI.check();

		if(!origin.isDirect())
			throw new IllegalArgumentException("The origin must be direct buffer.");

		accumulate(handle, origin, orgCount, orgType.handle,
				targetRank, targetDisp, targetCount, targetType.handle,
				op, op.handle, getBaseType(orgType, targetType));
	}

	private native void accumulate(
			long win, Buffer origin, int orgCount, long orgType,
			int targetRank, int targetDisp, int targetCount, long targetType,
			Op jOp, long hOp, int baseType) throws MPIException;

	/**
	 * Java binding of {@code MPI_WIN_FENCE}.
	 * @param assertion program assertion
	 * @throws MPIException Signals that an MPI exception of some sort has occurred.
	 */
	public void fence(int assertion) throws MPIException
	{
		MPI.check();
		fence(handle, assertion);
	}

	private native void fence(long win, int assertion) throws MPIException;

	/**
	 * Java binding of the MPI operation {@code MPI_WIN_START}.
	 * @param group     group of target processes
	 * @param assertion program assertion
	 * @throws MPIException Signals that an MPI exception of some sort has occurred.
	 */
	public void start(Group group, int assertion) throws MPIException
	{
		MPI.check();
		start(handle, group.handle, assertion);
	}

	private native void start(long win, long group, int assertion)
			throws MPIException;

	/**
	 * Java binding of the MPI operation {@code MPI_WIN_COMPLETE}.
	 * @throws MPIException Signals that an MPI exception of some sort has occurred.
	 */
	public void complete() throws MPIException
	{
		MPI.check();
		complete(handle);
	}

	private native void complete(long win) throws MPIException;

	/**
	 * Java binding of the MPI operation {@code MPI_WIN_POST}.
	 * @param group     group of origin processes
	 * @param assertion program assertion
	 * @throws MPIException Signals that an MPI exception of some sort has occurred.
	 */
	public void post(Group group, int assertion) throws MPIException
	{
		MPI.check();
		post(handle, group.handle, assertion);
	}

	private native void post(long win, long group, int assertion)
			throws MPIException;

	/**
	 * Java binding of the MPI operation {@code MPI_WIN_WAIT}.
	 * @throws MPIException Signals that an MPI exception of some sort has occurred.
	 */
	public void waitFor() throws MPIException
	{
		MPI.check();
		waitFor(handle);
	}

	private native void waitFor(long win) throws MPIException;

	/**
	 * Java binding of the MPI operation {@code MPI_WIN_TEST}.
	 * @return true if success
	 * @throws MPIException Signals that an MPI exception of some sort has occurred.
	 */
	public boolean test() throws MPIException
	{
		MPI.check();
		return test(handle);
	}

	private native boolean test(long win) throws MPIException;

	/**
	 * Java binding of the MPI operation {@code MPI_WIN_LOCK}.
	 * @param lockType  either MPI.LOCK_EXCLUSIVE or MPI.LOCK_SHARED
	 * @param rank      rank of locked window
	 * @param assertion program assertion
	 * @throws MPIException Signals that an MPI exception of some sort has occurred.
	 */
	public void lock(int lockType, int rank, int assertion) throws MPIException
	{
		MPI.check();
		lock(handle, lockType, rank, assertion);
	}

	private native void lock(long win, int lockType, int rank, int assertion)
			throws MPIException;

	/**
	 * Java binding of the MPI operation {@code MPI_WIN_UNLOCK}.
	 * @param rank rank of window
	 * @throws MPIException Signals that an MPI exception of some sort has occurred.
	 */
	public void unlock(int rank) throws MPIException
	{
		MPI.check();
		unlock(handle, rank);
	}

	private native void unlock(long win, int rank) throws MPIException;

	/**
	 * Java binding of the MPI operation {@code MPI_WIN_SET_ERRHANDLER}.
	 * @param errhandler new MPI error handler for window
	 * @throws MPIException Signals that an MPI exception of some sort has occurred.
	 */
	public void setErrhandler(Errhandler errhandler) throws MPIException
	{
		MPI.check();
		setErrhandler(handle, errhandler.handle);
	}

	private native void setErrhandler(long win, long errhandler)
			throws MPIException;

	/**
	 * Java binding of the MPI operation {@code MPI_WIN_CALL_ERRHANDLER}.
	 * @param errorCode error code
	 * @throws MPIException Signals that an MPI exception of some sort has occurred.
	 */
	public void callErrhandler(int errorCode) throws MPIException
	{
		callErrhandler(handle, errorCode);
	}

	private native void callErrhandler(long handle, int errorCode)
			throws MPIException;

	/**
	 * Create a new attribute key.
	 * <p>Java binding of the MPI operation {@code MPI_WIN_CREATE_KEYVAL}.
	 * @return attribute key for future access
	 * @throws MPIException Signals that an MPI exception of some sort has occurred.
	 */
	public static int createKeyval() throws MPIException
	{
		MPI.check();
		return createKeyval_jni();
	}

	private static native int createKeyval_jni() throws MPIException;

	/**
	 * Frees an attribute key.
	 * <p>Java binding of the MPI operation {@code MPI_WIN_FREE_KEYVAL}.
	 * @param keyval attribute key
	 * @throws MPIException Signals that an MPI exception of some sort has occurred.
	 */
	public static void freeKeyval(int keyval) throws MPIException
	{
		MPI.check();
		freeKeyval_jni(keyval);
	}

	private static native void freeKeyval_jni(int keyval) throws MPIException;

	/**
	 * Stores attribute value associated with a key.
	 * <p>Java binding of the MPI operation {@code MPI_WIN_SET_ATTR}.
	 * @param keyval attribute key
	 * @param value  attribute value
	 * @throws MPIException Signals that an MPI exception of some sort has occurred.
	 */
	public void setAttr(int keyval, Object value) throws MPIException
	{
		MPI.check();
		setAttr(handle, keyval, MPI.attrSet(value));
	}

	private native void setAttr(long win, int keyval, byte[] value)
			throws MPIException;

	/**
	 * Retrieves attribute value by key.
	 * <p>Java binding of the MPI operation {@code MPI_WIN_GET_ATTR}.
	 * @param keyval attribute key
	 * @return attribute value or null if no attribute is associated with the key.
	 * @throws MPIException Signals that an MPI exception of some sort has occurred.
	 */
	public Object getAttr(int keyval) throws MPIException
	{
		MPI.check();
		Object obj = getAttr(handle, keyval);
		return obj instanceof byte[] ? MPI.attrGet((byte[])obj) : obj;
	}

	private native Object getAttr(long win, int keyval) throws MPIException;

	/**
	 * Deletes an attribute value associated with a key.
	 * <p>Java binding of the MPI operation {@code MPI_WIN_DELETE_ATTR}.
	 * @param keyval attribute key
	 * @throws MPIException Signals that an MPI exception of some sort has occurred.
	 */
	public void deleteAttr(int keyval) throws MPIException
	{
		MPI.check();
		deleteAttr(handle, keyval);
	}

	private native void deleteAttr(long win, int keyval) throws MPIException;

	/**
	 * Java binding of {@code MPI_WIN_FREE}.
	 * @throws MPIException Signals that an MPI exception of some sort has occurred.
	 */
	@Override public void free() throws MPIException
	{
		MPI.check();
		handle = free(handle);
	}

	private native long free(long win) throws MPIException;

	/**
	 * Java binding of the MPI operation {@code MPI_WIN_GET_INFO}.
	 * @return Info	Info object associated with this window
	 * @throws MPIException Signals that an MPI exception of some sort has occurred.
	 */
	public Info getInfo() throws MPIException
	{
		MPI.check();
		return new Info(getInfo(handle));
	}

	private native long getInfo(long win)
			throws MPIException;

	/**
	 * Java binding of the MPI operation {@code MPI_WIN_SET_INFO}.
	 * @param info the new info
	 * @throws MPIException Signals that an MPI exception of some sort has occurred.
	 */
	public void setInfo(Info info) throws MPIException
	{
		MPI.check();
		setInfo(handle, info.handle);
	}

	private native void setInfo(long win, long info)
			throws MPIException;

	/**
	 * <p>Java binding of the MPI operation {@code MPI_RPUT}.
	 * @param origin_addr   	initial address of origin buffer
	 * @param origin_count 		number of entries in origin buffer
	 * @param origin_datatype  	datatype of each entry in origin buffer
	 * @param target_rank		rank of target
	 * @param target_disp		displacement from start of window to target buffer
	 * @param target_count		number of entries in target buffer
	 * @param target_datatype	datatype of each entry in target buffer
	 * @return RMA request
	 * @throws MPIException Signals that an MPI exception of some sort has occurred.
	 */
	public final Request rPut(Buffer origin_addr, int origin_count,
			Datatype origin_datatype, int target_rank, int target_disp,
			int target_count, Datatype target_datatype)
					throws MPIException
	{
		if(!origin_addr.isDirect())
			throw new IllegalArgumentException("The origin must be direct buffer.");
		Request req = new Request(rPut(handle, origin_addr, origin_count,
				origin_datatype.handle, target_rank, target_disp,
				target_count, target_datatype.handle, getBaseType(origin_datatype, target_datatype)));
		req.addSendBufRef(origin_addr);
		return req;
	}

	private native long rPut(long win, Buffer origin_addr, int origin_count,
			long origin_datatype, int target_rank, int target_disp,
			int target_count, long target_datatype, int baseType)
					throws MPIException;

	/**
	 * Java binding of {@code MPI_RGET}.
	 * @param origin      	origin buffer
	 * @param orgCount    	number of entries in origin buffer
	 * @param orgType     	datatype of each entry in origin buffer
	 * @param targetRank  	rank of target
	 * @param targetDisp  	displacement from start of window to target buffer
	 * @param targetCount 	number of entries in target buffer
	 * @param targetType  	datatype of each entry in target buffer
	 * @return RMA request
	 * @throws MPIException Signals that an MPI exception of some sort has occurred.
	 */
	public final Request rGet(Buffer origin, int orgCount, Datatype orgType,
			int targetRank, int targetDisp, int targetCount,
			Datatype targetType)
					throws MPIException
	{
		MPI.check();

		if(!origin.isDirect())
			throw new IllegalArgumentException("The origin must be direct buffer.");
		Request req = new Request(rGet(handle, origin, orgCount, orgType.handle,
				targetRank, targetDisp, targetCount, targetType.handle,
				getBaseType(orgType, targetType)));
		req.addRecvBufRef(origin);
		return req;
	}

	private native long rGet(
			long win, Buffer origin, int orgCount, long orgType,
			int targetRank, int targetDisp, int targetCount, long targetType,
			int baseType) throws MPIException;

	/**
	 * Java binding of {@code MPI_RACCUMULATE}.
	 * @param origin      origin buffer
	 * @param orgCount    number of entries in origin buffer
	 * @param orgType     datatype of each entry in origin buffer
	 * @param targetRank  rank of target
	 * @param targetDisp  displacement from start of window to target buffer
	 * @param targetCount number of entries in target buffer
	 * @param targetType  datatype of each entry in target buffer
	 * @param op          reduce operation
	 * @return RMA request
	 * @throws MPIException Signals that an MPI exception of some sort has occurred.
	 */
	public Request rAccumulate(Buffer origin, int orgCount, Datatype orgType,
			int targetRank, int targetDisp, int targetCount,
			Datatype targetType, Op op)
					throws MPIException
	{
		MPI.check();

		if(!origin.isDirect())
			throw new IllegalArgumentException("The origin must be direct buffer.");
		Request req = new Request(rAccumulate(handle, origin, orgCount, orgType.handle,
				targetRank, targetDisp, targetCount, targetType.handle,
				op, op.handle, getBaseType(orgType, targetType)));
		req.addSendBufRef(origin);
		return req;
	}

	private native long rAccumulate(
			long win, Buffer origin, int orgCount, long orgType,
			int targetRank, int targetDisp, int targetCount, long targetType,
			Op jOp, long hOp, int baseType) throws MPIException;

	/**
	 * Java binding of {@code MPI_GET_ACCUMULATE}.
	 * @param origin      	origin buffer
	 * @param orgCount    	number of entries in origin buffer
	 * @param orgType		datatype of each entry in origin buffer
	 * @param resultAddr   	result buffer
	 * @param resultCount	number of entries in result buffer
	 * @param resultType   	datatype of each entry in result buffer
	 * @param targetRank  	rank of target
	 * @param targetDisp  	displacement from start of window to target buffer
	 * @param targetCount 	number of entries in target buffer
	 * @param targetType  	datatype of each entry in target buffer
	 * @param op          	reduce operation
	 * @throws MPIException Signals that an MPI exception of some sort has occurred.
	 */

	public void getAccumulate(Buffer origin, int orgCount, Datatype orgType,
			Buffer resultAddr, int resultCount, Datatype resultType,
			int targetRank, int targetDisp, int targetCount,
			Datatype targetType, Op op)
					throws MPIException
	{
		MPI.check();

		if(!origin.isDirect())
			throw new IllegalArgumentException("The origin must be direct buffer.");

		getAccumulate(handle, origin, orgCount, orgType.handle,
				resultAddr, resultCount, resultType.handle,
				targetRank, targetDisp, targetCount, targetType.handle,
				op, op.handle, getBaseType(orgType, targetType));
	}

	private native void getAccumulate(
			long win, Buffer origin, int orgCount, long orgType,
			Buffer resultAddr, int resultCount, long resultType,
			int targetRank, int targetDisp, int targetCount, long targetType,
			Op jOp, long hOp, int baseType) throws MPIException;

	/**
	 * Java binding of {@code MPI_RGET_ACCUMULATE}.
	 * @param origin      	origin buffer
	 * @param orgCount    	number of entries in origin buffer
	 * @param orgType		datatype of each entry in origin buffer
	 * @param resultAddr   	result buffer
	 * @param resultCount	number of entries in result buffer
	 * @param resultType   	datatype of each entry in result buffer
	 * @param targetRank  	rank of target
	 * @param targetDisp  	displacement from start of window to target buffer
	 * @param targetCount 	number of entries in target buffer
	 * @param targetType  	datatype of each entry in target buffer
	 * @param op          	reduce operation
	 * @return RMA request
	 * @throws MPIException Signals that an MPI exception of some sort has occurred.
	 */

	public Request rGetAccumulate(Buffer origin, int orgCount, Datatype orgType,
			Buffer resultAddr, int resultCount, Datatype resultType,
			int targetRank, int targetDisp, int targetCount,
			Datatype targetType, Op op)
					throws MPIException
	{
		MPI.check();

		if(!origin.isDirect())
			throw new IllegalArgumentException("The origin must be direct buffer.");
		Request req = new Request(rGetAccumulate(handle, origin, orgCount, orgType.handle,
				resultAddr, resultCount, resultType.handle,
				targetRank, targetDisp, targetCount, targetType.handle,
				op, op.handle, getBaseType(orgType, targetType)));
		req.addRecvBufRef(origin);
		return req;
	}

	private native long rGetAccumulate(
			long win, Buffer origin, int orgCount, long orgType,
			Buffer resultAddr, int resultCount, long resultType,
			int targetRank, int targetDisp, int targetCount, long targetType,
			Op jOp, long hOp, int baseType) throws MPIException;

	/**
	 * Java binding of the MPI operation {@code MPI_WIN_LOCK_ALL}.
	 * @param assertion program assertion
	 * @throws MPIException Signals that an MPI exception of some sort has occurred.
	 */
	public void lockAll(int assertion) throws MPIException
	{
		MPI.check();
		lockAll(handle, assertion);
	}

	private native void lockAll(long win, int assertion)
			throws MPIException;

	/**
	 * Java binding of the MPI operation {@code MPI_WIN_UNLOCK_ALL}.
	 * @throws MPIException Signals that an MPI exception of some sort has occurred.
	 */
	public void unlockAll() throws MPIException
	{
		MPI.check();
		unlockAll(handle);
	}

	private native void unlockAll(long win) throws MPIException;

	/**
	 * Java binding of the MPI operation {@code MPI_WIN_SYNC}.
	 * @throws MPIException Signals that an MPI exception of some sort has occurred.
	 */
	public void sync() throws MPIException
	{
		MPI.check();
		sync(handle);
	}

	private native void sync(long win) throws MPIException;

	/**
	 * Java binding of the MPI operation {@code MPI_WIN_FLUSH}.
	 * @param targetRank	rank of target window
	 * @throws MPIException Signals that an MPI exception of some sort has occurred.
	 */
	public void flush(int targetRank) throws MPIException
	{
		MPI.check();
		flush(handle, targetRank);
	}

	private native void flush(long win, int targetRank) throws MPIException;

	/**
	 * Java binding of the MPI operation {@code MPI_WIN_FLUSH_ALL}.
	 * @throws MPIException Signals that an MPI exception of some sort has occurred.
	 */
	public void flushAll() throws MPIException
	{
		MPI.check();
		flushAll(handle);
	}

	private native void flushAll(long win) throws MPIException;

	/**
	 * Java binding of {@code MPI_COMPARE_AND_SWAP}.
	 * @param origin      	origin buffer
	 * @param compareAddr   compare buffer
	 * @param resultAddr   	result buffer
	 * @param targetType  	datatype of each entry in target buffer
	 * @param targetRank  	rank of target
	 * @param targetDisp  	displacement from start of window to target buffer
	 * @throws MPIException Signals that an MPI exception of some sort has occurred.
	 */

	public void compareAndSwap(Buffer origin, Buffer compareAddr, Buffer resultAddr,
			Datatype targetType, int targetRank, int targetDisp)
					throws MPIException
	{
		MPI.check();

		if(!origin.isDirect())
			throw new IllegalArgumentException("The origin must be direct buffer.");

		compareAndSwap(handle, origin, compareAddr, resultAddr,
				targetType.handle, targetRank, targetDisp);
	}

	private native void compareAndSwap(
			long win, Buffer origin, Buffer compareAddr, Buffer resultAddr,
			long targetType, int targetRank, int targetDisp) throws MPIException;

	/**
	 * Java binding of {@code MPI_FETCH_AND_OP}.
	 * @param origin      	origin buffer
	 * @param resultAddr   	result buffer
	 * @param dataType  	datatype of entry in origin, result, and target buffers
	 * @param targetRank  	rank of target
	 * @param targetDisp  	displacement from start of window to target buffer
	 * @param op          	reduce operation
	 * @throws MPIException Signals that an MPI exception of some sort has occurred.
	 */

	public void fetchAndOp(Buffer origin, Buffer resultAddr, Datatype dataType,
			int targetRank, int targetDisp, Op op)
					throws MPIException
	{
		MPI.check();

		if(!origin.isDirect())
			throw new IllegalArgumentException("The origin must be direct buffer.");

		fetchAndOp(handle, origin, resultAddr, dataType.handle, targetRank,
				targetDisp, op, op.handle, getBaseType(dataType, dataType));
	}

	private native void fetchAndOp(
			long win, Buffer origin, Buffer resultAddr, long targetType, int targetRank,
			int targetDisp, Op jOp, long hOp, int baseType) throws MPIException;

	/**
	 * Java binding of the MPI operation {@code MPI_WIN_FLUSH_LOCAL}.
	 * @param targetRank	rank of target window
	 * @throws MPIException Signals that an MPI exception of some sort has occurred.
	 */

	public void flushLocal(int targetRank) throws MPIException
	{
		MPI.check();
		flushLocal(handle, targetRank);
	}

	private native void flushLocal(long win, int targetRank) throws MPIException;

	/**
	 * Java binding of the MPI operation {@code MPI_WIN_FLUSH_LOCAL_ALL}.
	 * @throws MPIException Signals that an MPI exception of some sort has occurred.
	 */

	public void flushLocalAll() throws MPIException
	{
		MPI.check();
		flushLocalAll(handle);
	}

	private native void flushLocalAll(long win) throws MPIException;

	/**
	 * Java binding of the MPI operation {@code MPI_WIN_GET_NAME}.
	 * @return the name associated with this window
	 * @throws MPIException Signals that an MPI exception of some sort has occurred.
	 */
	public String getName() throws MPIException
	{
		MPI.check();
		return getName(handle);
	}

	private native String getName(long handle) throws MPIException;

	/**
	 * Java binding of the MPI operation {@code MPI_WIN_SET_NAME}.
	 * @param name	the name to associate with this window
	 * @throws MPIException Signals that an MPI exception of some sort has occurred.
	 */
	public void setName(String name) throws MPIException
	{
		MPI.check();
		setName(handle, name);
	}

	private native void setName(long handle, String name) throws MPIException;

} // Win
