.. _mpix_query_rocm_support:


MPIX_Query_rocm_support
=======================

.. include_body

**MPIX_Query_rocm_support** - Returns 1 if there is AMD ROCm-aware support
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

This function is part of an :ref:`Open MPI extension
<ompi-features-extentions-label>`; it is not part of standard MPI.

This routine returns 1 if both the MPI library was built with the AMD
ROCm library and the runtime supports ROCm buffers.  Otherwise, it
returns 0.  This routine must be called after MPI is initialized,
e.g., by a call to :ref:`MPI_Init(3) <MPI_Init>` or
:ref:`MPI_Init_thread(3) <MPI_Init_thread>`.

Including the Open MPI-specific file ``<mpi-ext.h>`` will define the C
preprocessor macro ``OMPI_HAVE_MPI_EXT`` to ``1``.  Otherwise, it will
be undefined.  This macro can be used by applications as a sentinel to
know whether ``<mpi-ext.h>`` has been included or not.

The Open MPI ROCm extension is built by default (regardless of whether
or not Open MPI was built with ROCm support), but *could* have been
disabled by an administrative action.  It is therefore safest for
applications to check that the preprocessor macro
``OMPI_HAVE_MPI_EXT_ROCM`` is defined and is set to 1 to know whether
the ``MPIX_Query_rocm_support()`` function is available.  Checking for
this macro also protects the use of this function when compiling the
application with older versions of Open MPI or other MPI
implementations that do not have this function.


EXAMPLES
^^^^^^^^

.. code-block:: c

   #include <stdio.h>
   #include <mpi.h>
   #include <mpi-ext.h> /* Needed for ROCm-aware check */

   int main(int argc, char *argv[])
   {
       MPI_Init(&argc, &argv);

       bool happy = false;
   #if defined(OMPI_HAVE_MPI_EXT_ROCM) && OMPI_HAVE_MPI_EXT_ROCM
       happy = (bool) MPIX_Query_rocm_support();
   #endif

       if (happy) {
           printf("This Open MPI installation has ROCm-aware support.\n");
       } else {
           printf("This Open MPI installation does not have ROCm-aware support.\n");
       }

       MPI_Finalize();
       return 0;
   }

.. seealso::
   * :ref:`MPIX_Query_cuda_support`
