Support for Memory-kind Info Objects
====================================

`MPI version 4.1. <https://www.mpi-forum.org/docs/mpi-4.1/mpi41-report.pdf>`_
introduced the notion of memory allocation kinds, which allow an
application to specify what memory types it plans to use, and to query
what memory types are supported by the MPI library in a portable
manner. In addition, the application can place restrictions on certain
objects such as creating a separate communicator for using with
host-memory and a communicator that will be used with device memory
only. This approach allows the MPI library to perform certain
optimizations, such as bypassing checking the memory-type of buffer
pointers. Please refer to the MPI specification as well as the `Memory
Allocation Kinds Side Document
<https://www.mpi-forum.org/docs/sidedocs/mem-alloc10.pdf>`_ for more
details and examples.

Open MPI starting from version 6.0.0 supports the following values for the memory allocation kind Info object:

* mpi
* system
* cuda:device
* cuda:host
* cuda:managed
* level_zero:device
* level_zero:host
* level_zero:shared
* rocm:device
* rocm:host
* rocm:managed

.. note:: Support for accelerator memory allocation kind info objects
          will depend on the accelerator support compiled into Open
          MPI.


Passing memory-kind info to mpiexec
===================================

The following example demonstrates how to pass memory allocation kind
information to Open MPI at application launch:

.. code:: sh

   # Specify that the application will use system, MPI, and CUDA device memory
   shell$ mpiexec --memory-allocation-kinds system,mpi,cuda:device -n 64 ./<my_executable>

Asserting usage of memory kind when creating a Communicator
===========================================================

The following code-snipplet demonstrates how to assert that a
communicator will only be used for ROCm device buffers:

.. code:: c
 
  MPI_Info info_assert;
  MPI_Info_create (&info_assert);
  char assert_key[] = "mpi_assert_memory_alloc_kinds";
  char assert_value[] = "rocm:device";
  MPI_Info_set (info_assert, assert_key, assert_value);

  MPI_Comm comm_dup
  MPI_Comm_dup_with_info (MPI_COMM_WORLD, info_assert, &comm_dup);
  ...
