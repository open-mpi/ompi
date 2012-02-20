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
package mpi;

class Maxloc extends User_function{
  public void Call(Object invec, int inoffset, Object outvec, int outoffset,
                   int count, Datatype datatype){

    // *** should work also for derived datatypes with following as
    //     as bases ? ***

    if(datatype == MPI.SHORT2) {
      short [] in_array = (short[])invec;
      short [] out_array = (short[])outvec;

      int indisp  = inoffset ;
      int outdisp = outoffset ;
      for (int i = 0; i < count; i++, indisp += 2, outdisp += 2) {  
        short inval  = in_array  [indisp] ;
        short outval = out_array [outdisp] ;

        if(inval > outval) {
          out_array [outdisp    ] = inval ;
          out_array [outdisp + 1] = in_array [outdisp + 1] ;
        }
        else if(inval == outval) {
          short inloc = in_array [indisp + 1] ;

          if(inloc < out_array [outdisp + 1])
            out_array [outdisp + 1] = inloc ;
        }
      }
    }
    else if(datatype == MPI.INT2) {
      int [] in_array = (int[])invec;
      int [] out_array = (int[])outvec;

      int indisp  = inoffset ;
      int outdisp = outoffset ;
      for (int i = 0; i < count; i++, indisp += 2, outdisp += 2){
        
        int inval  = in_array  [indisp] ;
        int outval = out_array [outdisp] ;

        if(inval > outval) {
          out_array [outdisp    ] = inval ;
          out_array [outdisp + 1] = in_array [outdisp + 1] ;
        }
        else if(inval == outval) {
          int inloc = in_array [indisp + 1] ;

          if(inloc < out_array [outdisp + 1])
            out_array [outdisp + 1] = inloc ;
        }
      }
    }
    else if(datatype == MPI.LONG2) {
      long [] in_array = (long[])invec;
      long [] out_array = (long[])outvec;

      int indisp  = inoffset ;
      int outdisp = outoffset ;
      for (int i = 0; i < count; i++, indisp += 2, outdisp += 2){
        
        long inval  = in_array  [indisp] ;
        long outval = out_array [outdisp] ;

        if(inval > outval) {
          out_array [outdisp    ] = inval ;
          out_array [outdisp + 1] = in_array [outdisp + 1] ;
        }
        else if(inval == outval) {
          long inloc = in_array [indisp + 1] ;

          if(inloc < out_array [outdisp + 1])
            out_array [outdisp + 1] = inloc ;
        }
      }
    }
    else if(datatype == MPI.FLOAT2) {
      float [] in_array = (float[])invec;
      float [] out_array = (float[])outvec;

      int indisp  = inoffset ;
      int outdisp = outoffset ;
      for (int i = 0; i < count; i++, indisp += 2, outdisp += 2){
        
        float inval  = in_array  [indisp] ;
        float outval = out_array [outdisp] ;

        if(inval > outval) {
          out_array [outdisp    ] = inval ;
          out_array [outdisp + 1] = in_array [outdisp + 1] ;
        }
        else if(inval == outval) {
          float inloc = in_array [indisp + 1] ;

          if(inloc < out_array [outdisp + 1])
            out_array [outdisp + 1] = inloc ;
        }
      }
    }
    else if(datatype == MPI.DOUBLE2) {
      double [] in_array = (double[])invec;
      double [] out_array = (double[])outvec;

      int indisp  = inoffset ;
      int outdisp = outoffset ;
      for (int i = 0; i < count; i++, indisp += 2, outdisp += 2){
        
        double inval  = in_array  [indisp] ;
        double outval = out_array [outdisp] ;

        if(inval > outval) {
          out_array [outdisp    ] = inval ;
          out_array [outdisp + 1] = in_array [outdisp + 1] ;
        }
        else if(inval == outval) {
          double inloc = in_array [indisp + 1] ;

          if(inloc < out_array [outdisp + 1])
            out_array [outdisp + 1] = inloc ;
        }
      }
    }
    else {
      System.out.println("MPI.MAXLOC: invalid datatype") ;
      try {
        MPI.COMM_WORLD.Abort(1);
      }
      catch(MPIException e) {}
    }
  }
}
