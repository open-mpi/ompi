Selecting an Accelerator Device before calling MPI_Init
=======================================================

A common problem when using accelerators arises when selecting which
GPU should be used by an MPI process. The decision is often based by
the rank of that process in ``MPI_COMM_WORLD``. The rank of a process
can however only be retrieved after the MPI library has correctly
initialized. On the other hand, the accelerator resources initialized
during ``MPI_Init`` can have some associations with the `current`
device, which will be the default device used by a particular
eco-system if not set to a different value.

To circumvent this circular problem, applications are encouraged to
make use of the environment variable ``OMPI_COMM_WORLD_LOCAL_RANK``
that is set by Open MPI at launch time and can be retrieved before
``MPI_Init``. An example code sample using the HIP programming model
looks as follows:

.. code-block:: c

   int num_devices;
   hipGetDeviceCount(&num_devices);
   assert (num_devices > 0);

   char* ompi_local_rank = getenv("OMPI_COMM_WORLD_LOCAL_RANK");
   if (nullptr != ompi_local_rank) {
	hipSetDevice(atoi(ompi_local_rank) % num_devices);
   }

   MPI_Init (&argc, &argv);
   ...


.. note:: Open MPI currently assumes that an MPI processes is using a
          single accelerator device. Certain software stacks might be
          able to support multiple GPUs per rank.



