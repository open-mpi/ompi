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
 * File         : CartParms.java
 * Author       : Xinying Li
 * Created      : Thu Apr  9 12:22:15 1998
 * Revision     : $Revision: 1.1 $
 * Updated      : $Date: 1998/08/26 18:49:50 $
 * Copyright: Northeast Parallel Architectures Center
 *            at Syracuse University 1998
 */

package mpi;

/**
 * Cartesian topology information associated with a communicator.
 */
public final class CartParms
{
/** Number of processes for each cartesian dimension. */
private final int[] dims;

/** Periodicity (true/false) for each cartesian dimension. */
private final boolean[] periods;

/** Coordinates of calling process in cartesian structure. */
private final int[] coords;

/**
 * Constructs a cartesian topology information object.
 * @param dims    number of processes for each cartesian dimension.
 * @param periods periodicity (true/false) for each cartesian dimension.
 * @param coords  coordinates of calling process in cartesian structure.
 */
protected CartParms(int[] dims, boolean[] periods, int[] coords)
{
    this.dims    = dims;
    this.periods = periods;
    this.coords  = coords;
}

/**
 * Returns the number of dimensions.
 * @return number of dimensions.
 */
public int getDimCount()
{
    return dims.length;
}

/**
 * Returns the number of processes for a cartesian dimension.
 * @param i cartesian dimension.
 * @return number of processes for a cartesian dimension.
 */
public int getDim(int i)
{
    return dims[i];
}

/**
 * Returns the periodicity (true/false) for a cartesian dimension.
 * @param i cartesian dimension.
 * @return periodicity for a cartesian dimension.
 */
public boolean getPeriod(int i)
{
    return periods[i];
}

/**
 * Returns the coordinate of calling process for a cartesian dimension.
 * @param i cartesian dimension.
 * @return coordinate of calling process for a cartesian dimension.
 */
public int getCoord(int i)
{
    return coords[i];
}

} // CartParms
