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
 * Struct class for {@link MPI#FLOAT_INT} datatype.
 */
public final class FloatInt extends Struct
{
	private final int iOff, iSize;

	/**
	 * The struct object will be created only in MPI class.
	 * @param intOff	int offset
	 * @param intSize	int size
	 * @see MPI#floatInt
	 */
	protected FloatInt(int intOff, int intSize)
	{
		int fOff = addFloat();
		assert fOff == 0;

		iSize = intSize;
		setOffset(intOff);

		switch(iSize)
		{
		case 4: iOff = addInt();  break;
		case 8: iOff = addLong(); break;
		default: throw new AssertionError("Unsupported int size: "+ iSize);
		}

		assert(intOff == iOff);
	}

	/**
	 * Creates a Data object.
	 * @return new Data object.
	 */
	@Override protected Data newData()
	{
		return new Data();
	}

	/**
	 * Class for reading/writing data in a struct stored in a byte buffer.
	 */
	public final class Data extends Struct.Data
	{
		/**
		 * Gets the float value.
		 * @return float value
		 */
		public float getValue()
		{
			return getFloat(0);
		}

		/**
		 * Gets the int value.
		 * @return int value
		 */
		public int getIndex()
		{
			switch(iSize)
			{
			case 4: return getInt(iOff);
			case 8: return (int)getLong(iOff);
			default: throw new AssertionError();
			}
		}

		/**
		 * Puts the float value.
		 * @param v float value
		 */
		public void putValue(float v)
		{
			putFloat(0, v);
		}

		/**
		 * Puts the int value.
		 * @param v int value
		 */
		public void putIndex(int v)
		{
			switch(iSize)
			{
			case 4: putInt(iOff, v);  break;
			case 8: putLong(iOff, v); break;
			default: throw new AssertionError();
			}
		}
	} // Data

} // FloatInt
