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
 * File         : ShiftParms.java
 * Author       : Xinying Li
 * Created      : Thu Apr  9 12:22:15 1998
 * Revision     : $Revision: 1.1 $
 * Updated      : $Date: 1998/08/26 18:50:05 $
 * Copyright: Northeast Parallel Architectures Center
 *            at Syracuse University 1998
 */

package mpi;

/**
 * Source and destination ranks for "shift" communication.
 */
public final class ShiftParms
{
private final int rankSource;
private final int rankDest;

protected ShiftParms(int rankSource, int rankDest)
{
    this.rankSource = rankSource;
    this.rankDest   = rankDest;
}

/**
 * Gets the source rank.
 * @return source rank
 */
public int getRankSource()
{
    return rankSource;
}

/**
 * Gets the destination rank.
 * @return destination rank
 */
public int getRankDest()
{
    return rankDest;
}

} // ShiftParms
