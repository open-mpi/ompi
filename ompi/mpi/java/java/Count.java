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
 * File         : Count.java
 * Author       : Nathaniel Graham
 * Created      : Thu Jul  29 17:13 2015
 */

package mpi;

/**
 * This class represents {@code MPI_Count}.
 */
public final class Count implements Comparable
{
	private long count;

	static
	{
		System.loadLibrary("mpi_java");
		initCount();
	}

	private static native void initCount();

	public Count(long count)
	{
		this.count = count;
	}

	/**
	 * Gets value associated with this Count object.
	 * @return Count value
	 */
	public long getCount()
	{
		return this.count;
	}

	/**
	 * Sets the value associated with this Count object.
	 * @param count	the value to set for this count object
	 */
	public void setCount(long count)
	{
		this.count = count;
	}

	@Override
	public boolean equals(Object obj)
	{
		if(obj instanceof Count) {
			if(this.count == ((Count)obj).getCount()) {
				return true;
			}
		}
		return false;
	}

	public int compareTo(Object obj)
	{
		if(obj instanceof Count) {
			if(this.count - ((Count)obj).getCount() > 0) {
				return 1;
			} else if(this.count - ((Count)obj).getCount() == 0) {
				return 0;
			}
		}
		return -1;
	}
} // Count
