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
 * File         : MPIException.java
 * Author       : Bryan Carpenter
 * Created      : Tue Sep 14 13:03:57 EDT 1999
 * Revision     : $Revision: 1.1 $
 * Updated      : $Date: 1999/09/14 22:01:52 $
 * Copyright: Northeast Parallel Architectures Center
 *            at Syracuse University 1999
 */

package mpi;

public class MPIException extends Exception {
  public MPIException() {super() ;}
  public MPIException(String message) {super(message) ;}
}

