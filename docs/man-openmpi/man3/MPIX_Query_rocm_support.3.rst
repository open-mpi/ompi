.. _mpix_query_rocm_support:


MPIX_Query_rocm_support
=======================

.. include_body

**MPIX_Query_rocm_support** - Returns 1 if there is AMD ROCm aware support
and 0 if there is not.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>
   #include <mpi-ext.h>

   int MPIX_Query_rocm_support(void)


Fortran Syntax
^^^^^^^^^^^^^^

There is no Fortran binding for this function.


C++ Syntax
^^^^^^^^^^

There is no C++ binding for this function.


DESCRIPTION
-----------

This routine return 1 if MPI library is build with ROCm and runtime
supports ROCm buffers. This routine must be called after MPI is
initialized by a call to :ref:`MPI_Init` or :ref:`MPI_Init_thread`.


Examples
^^^^^^^^

::


   #include <stdio.h>
   #include "mpi.h"

   #include "mpi-ext.h" /* Needed for ROCm-aware check */

   int main(int argc, char *argv[])
   {

       MPI_Init(&argc, &argv);

       if (MPIX_Query_rocm_support()) {
           printf("This MPI library has ROCm-aware support.);
       } else {
           printf("This MPI library does not have ROCm-aware support.);
       }
       MPI_Finalize();

       return 0;
   }
