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
 * File         : Freeable.java
 * Author       : Bryan Carpenter
 * Created      : Wed Jan 15 23:14:43 EST 2003
 * Revision     : $Revision: 1.1 $
 * Updated      : $Date: 2003/01/16 16:39:34 $
 */

package mpi;

/**
 * Objects freeables must be freed calling the method free.
 */
public interface Freeable
{
    /**
     * Frees a freeable object.
     * @throws MPIException 
     */
    void free() throws MPIException;
}

