CUDA
====

CUDA-aware support means that the MPI library can send and receive GPU
buffers directly, without the application first staging them into host
memory.  Open MPI automatically detects that a buffer passed to an MPI
routine is a CUDA device pointer and handles it appropriately; this
detection relies on CUDA's Unified Virtual Addressing (UVA), which lets
the library determine whether a given pointer refers to device or host
memory.

CUDA support is updated continuously, and different levels of support
exist in different versions, so we recommend using the latest release
of Open MPI for the best support.  Open MPI offers two flavors of CUDA
support, described below; you may build with either or both.

Building Open MPI with CUDA-aware support
-----------------------------------------

Regardless of which flavor of CUDA support you plan to use, configure
Open MPI with the ``--with-cuda=<path-to-cuda>`` option to build in
CUDA support.  The configure script searches the given path for
``libcuda.so``; if it cannot be found, also pass
``--with-cuda-libdir``, for example:

.. code-block:: sh

   shell$ ./configure --with-cuda=/usr/local/cuda \
          --with-cuda-libdir=/usr/local/cuda/lib64/stubs <other configure params>

Support via UCX
^^^^^^^^^^^^^^^

Using `UCX <https://openucx.org/>`_ is the preferred mechanism.  Since
UCX provides the CUDA support in this configuration, it is important
that UCX itself is built with CUDA support.  To check whether your UCX
was built with CUDA support, run:

.. code-block:: sh

   shell$ ucx_info -v

and look for ``--with-cuda`` in the reported configure line.  If you
need to build UCX yourself to include CUDA support, see the UCX
documentation for `building UCX with Open MPI
<https://openucx.readthedocs.io/en/master/running.html#openmpi-with-ucx>`_.
A typical build looks like:

.. code-block:: sh

   # Configure UCX with CUDA support
   shell$ ./configure --prefix=/path/to/ucx-cuda-install \
          --with-cuda=/usr/local/cuda --with-gdrcopy=/usr

   # Configure Open MPI to use that UCX
   shell$ ./configure --with-cuda=/usr/local/cuda \
          --with-ucx=/path/to/ucx-cuda-install <other configure params>

Internal Open MPI CUDA support
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Open MPI also provides its own internal CUDA support, used by the
CUDA-ized components such as the ``smcuda`` shared-memory BTL.  This is
enabled by the same ``--with-cuda`` option.

Running on hosts without CUDA
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Open MPI supports building with CUDA libraries and then running on
systems that have neither CUDA libraries nor CUDA hardware.  For
releases v5.0.2 and newer, no special steps are required.

For the v5.0.0 and v5.0.1 releases only, you must build the
CUDA-dependent components as DSOs to get this behavior, using the
``--enable-mca-dso`` option.  This affects the ``smcuda`` shared-memory
and ``uct`` BTLs, as well as the ``rgpusm`` and ``gpusm`` rcache
components:

.. code-block:: sh

   shell$ ./configure --with-cuda=/usr/local/cuda \
          --enable-mca-dso=btl-smcuda,rcache-rgpusm,rcache-gpusm,accelerator-cuda \
          <other configure params>

Building with the NVIDIA compilers
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

With CUDA 6.5 and later, CUDA-aware Open MPI builds with the NVIDIA
compilers without anything special.  With CUDA 7.0 and 7.5, some
additional compiler flags are required:

.. code-block:: sh

   # For NVIDIA compilers version 15.9 and later
   shell$ ./configure --with-wrapper-cflags=-ta:tesla

   # For earlier NVIDIA compiler versions
   shell$ ./configure CFLAGS=-D__LP64__ \
          --with-wrapper-cflags="-D__LP64__ -ta:tesla"

Verifying that CUDA support was built
-------------------------------------

Use :ref:`ompi_info(1) <man1-ompi_info>` to confirm that a given Open
MPI installation was built with CUDA support:

.. code-block:: sh

   # List the MPI extensions that were built
   shell$ ompi_info | grep "MPI extensions"
          MPI extensions: affinity, cuda, ftmpi, rocm

   # Query the CUDA support MCA parameter directly
   shell$ ompi_info --parsable --all | grep mpi_built_with_cuda_support:value
          mca:mpi:base:param:mpi_built_with_cuda_support:value:true

Detecting CUDA-aware support at compile and run time
----------------------------------------------------

The ``cuda`` MPI extension provides both a compile-time and a run-time
check, and you can use whichever is more convenient.  Both require
including the Open MPI-specific header ``<mpi-ext.h>``:

* The compile-time check is the ``MPIX_CUDA_AWARE_SUPPORT`` macro.
* The run-time check is the :ref:`MPIX_Query_cuda_support(3)
  <mpix_query_cuda_support>` function.

The extension is built by default, whether or not Open MPI itself was
built with CUDA-aware support; when support is absent, the run-time
check simply returns ``0``.  The following program illustrates both
checks:

.. code-block:: c

   /*
    * Program that shows the use of the CUDA-aware macro and run-time check.
    */
   #include <stdio.h>
   #include "mpi.h"

   #if !defined(OPEN_MPI) || !OPEN_MPI
   #error This source code uses an Open MPI-specific extension
   #endif

   /* Needed for MPIX_Query_cuda_support(), below */
   #include "mpi-ext.h"

   int main(int argc, char *argv[])
   {
       MPI_Init(&argc, &argv);

       printf("Compile time check:\n");
   #if defined(MPIX_CUDA_AWARE_SUPPORT) && MPIX_CUDA_AWARE_SUPPORT
       printf("This MPI library has CUDA-aware support.\n");
   #elif defined(MPIX_CUDA_AWARE_SUPPORT) && !MPIX_CUDA_AWARE_SUPPORT
       printf("This MPI library does not have CUDA-aware support.\n");
   #else
       printf("This MPI library cannot determine if there is CUDA-aware support.\n");
   #endif /* MPIX_CUDA_AWARE_SUPPORT */

       printf("Run time check:\n");
   #if defined(MPIX_CUDA_AWARE_SUPPORT)
       if (1 == MPIX_Query_cuda_support()) {
           printf("This MPI library has CUDA-aware support.\n");
       } else {
           printf("This MPI library does not have CUDA-aware support.\n");
       }
   #else /* !defined(MPIX_CUDA_AWARE_SUPPORT) */
       printf("This MPI library cannot determine if there is CUDA-aware support.\n");
   #endif /* MPIX_CUDA_AWARE_SUPPORT */

       MPI_Finalize();
       return 0;
   }

Running applications that pass CUDA buffers
-------------------------------------------

Open MPI detects and enables its CUDA-capable components at run time
with no additional ``mpirun`` parameters; an application may simply pass
CUDA device buffers to MPI routines.  CUDA-aware support is available
in the following transports:

* The UCX (``ucx``) PML.
* The PSM2 (``psm2``) MTL with the CM (``cm``) PML.
* The OFI (``ofi``) MTL with the CM (``cm``) PML.
* The CUDA-ized shared-memory (``smcuda``) and TCP (``tcp``) BTLs with
  the OB1 (``ob1``) PML.

Both contiguous and non-contiguous derived datatypes are supported.
Non-contiguous datatypes currently carry high overhead, however,
because copying the pieces of the buffer into an intermediate buffer
requires many separate device-to-device copies.

.. _faq-cuda-mpi-apis-cuda-label:

CUDA-aware MPI APIs
-------------------

The following MPI operations accept CUDA device buffers:

* ``MPI_Allgather``, ``MPI_Allgatherv``
* ``MPI_Allreduce``
* ``MPI_Alltoall``, ``MPI_Alltoallv``, ``MPI_Alltoallw``
* ``MPI_Bcast``
* ``MPI_Bsend``, ``MPI_Bsend_init``
* ``MPI_Exscan``
* ``MPI_Gather``, ``MPI_Gatherv``
* ``MPI_Get``, ``MPI_Put``
* ``MPI_Ibsend``
* ``MPI_Irecv``, ``MPI_Isend``, ``MPI_Irsend``, ``MPI_Issend``
* ``MPI_Recv``, ``MPI_Recv_init``
* ``MPI_Reduce``, ``MPI_Reduce_scatter``, ``MPI_Reduce_scatter_block``
* ``MPI_Rsend``, ``MPI_Rsend_init``
* ``MPI_Scan``
* ``MPI_Scatter``, ``MPI_Scatterv``
* ``MPI_Send``, ``MPI_Send_init``
* ``MPI_Sendrecv``
* ``MPI_Ssend``, ``MPI_Ssend_init``
* ``MPI_Win_create``

The following operations do *not* currently accept CUDA device buffers:

* ``MPI_Accumulate``, ``MPI_Get_Accumulate``
* ``MPI_Compare_and_swap``, ``MPI_Fetch_and_op``
* The non-blocking collectives ``MPI_Iallgather``, ``MPI_Iallgatherv``,
  ``MPI_Iallreduce``, ``MPI_Ialltoall``, ``MPI_Ialltoallv``,
  ``MPI_Ialltoallw``, ``MPI_Ibcast``, and ``MPI_Iexscan``
* ``MPI_Rget``, ``MPI_Rput``

.. note:: These lists reflect known support and may vary between Open
   MPI versions and transports.  The set of CUDA-aware operations when
   using the UCX PML is essentially the same as above, with the
   additional restriction that UCX's one-sided component does not
   support CUDA buffers: all one-sided operations (for example,
   ``MPI_Put``, ``MPI_Get``, ``MPI_Accumulate``), all window creation
   calls (for example, ``MPI_Win_create``), and all non-blocking
   reduction collectives are not CUDA-aware when using UCX.

Transport-specific notes
------------------------

CUDA-aware UCX
^^^^^^^^^^^^^^

When both UCX and Open MPI are built with CUDA support, selecting the
UCX PML is sufficient to use it.  For example, to run ``osu_latency``
from the `OSU benchmarks <https://mvapich.cse.ohio-state.edu/benchmarks>`_
with CUDA buffers:

.. code-block:: sh

   shell$ mpirun -n 2 --mca pml ucx \
       -x UCX_TLS=rc,sm,cuda_copy,gdr_copy,cuda_ipc ./osu_latency D D

OFI / libfabric
^^^^^^^^^^^^^^^

When running over Libfabric, the OFI MTL checks whether any provider
can handle GPU (or other accelerator) memory through the
``hmem``-related flags.  If a CUDA-capable provider is available, the
OFI MTL sends GPU buffers directly through Libfabric's API after
registering the memory; otherwise, the buffers are automatically copied
to host memory before being transferred.

PSM2 / Omni-Path
^^^^^^^^^^^^^^^^^^

CUDA-aware support is present in the PSM2 MTL.  When running CUDA-aware
Open MPI on Cornelis Networks Omni-Path, the PSM2 MTL automatically sets
the ``PSM2_CUDA`` environment variable so that PSM2 handles GPU
buffers.  If you want to use host buffers with a CUDA-aware Open MPI, it
is recommended to set ``PSM2_CUDA`` to ``0`` in the environment.  PSM2
also supports NVIDIA GPUDirect; to enable it, set ``PSM2_GPUDIRECT`` to
``1``.  These variables must be set before ``MPI_Init()`` is called, for
example:

.. code-block:: sh

   shell$ mpirun -x PSM2_CUDA=1 -x PSM2_GPUDIRECT=1 --mca mtl psm2 ./mpi_hello

GPUDirect support on Omni-Path requires a PSM2 library and ``hfi1``
driver with CUDA support; the minimum required PSM2 version is `PSM2
10.2.175 <https://github.com/01org/opa-psm2/releases/tag/PSM2_10.2-175>`_.

When binding processes to GPUs on Omni-Path, each process should select
a specific GPU (within the same NUMA node as the CPU the process runs
on) before calling ``MPI_Init()`` using ``cudaChooseDevice()``,
``cudaSetDevice()``, and similar; use the ``mpirun`` binding options
(such as ``--bind-to core``) to keep processes from migrating between
NUMA nodes.  See :ref:`Selecting a GPU close to the process
<faq-cuda-mpi-cuda-numa-issues-label>`.

.. note:: The Cornelis Networks Omni-Path / PSM2 details above may be
   dated.  For current guidance, consult the PSM2 and Omni-Path
   documentation from `Cornelis Networks
   <https://customercenter.cornelisnetworks.com/>`_.

Selecting a CUDA device
-----------------------

Open MPI requires some CUDA resources for internal use.  When possible,
these are allocated lazily, the first time they are needed |mdash| for
example, CUDA IPC memory handles are created when a transfer first
requires them.  ``MPI_Init()`` and most communicator operations do not
create any CUDA resources (this is guaranteed at least for
``MPI_Comm_rank`` and ``MPI_Comm_size`` on ``MPI_COMM_WORLD``).

This is not always the case, however.  When using PSM2 or the
``smcuda`` BTL (with the OB1 PML), it is not feasible to delay the
allocation, so those CUDA resources are allocated during
``MPI_Init()``.

In all cases, the CUDA device must be selected *before* the first MPI
call that requires a CUDA resource.  When CUDA resources are initialized
lazily, you may use the communicator operations above to determine rank
information and select a GPU accordingly:

.. code-block:: c

   int local_rank = -1;
   {
       MPI_Comm local_comm;
       MPI_Comm_split_type(MPI_COMM_WORLD, MPI_COMM_TYPE_SHARED, rank,
                           MPI_INFO_NULL, &local_comm);
       MPI_Comm_rank(local_comm, &local_rank);
       MPI_Comm_free(&local_comm);
   }
   int num_devices = 0;
   cudaGetDeviceCount(&num_devices);
   cudaSetDevice(local_rank % num_devices);

Open MPI's internal CUDA resources are released during
``MPI_Finalize()``, so it is an application error to call
``cudaDeviceReset()`` before ``MPI_Finalize()``.

For a general treatment of selecting an accelerator device before
``MPI_Init()`` (using the ``OMPI_COMM_WORLD_LOCAL_RANK`` environment
variable), see :doc:`initialize`.

.. _faq-cuda-mpi-cuda-numa-issues-label:

Selecting a GPU close to the process
------------------------------------

On a node with multiple GPUs, you may want each process to use the GPU
closest to the NUMA node on which it is running.  One way to do this is
with the ``hwloc`` library.  The following C snippet determines the CPU
the process is running on and then looks for the closest GPU; note that
several GPUs may be equidistant.

.. code-block:: c

   /**
    * Test program to show the use of hwloc to select the GPU closest to the CPU
    * that the MPI program is running on.  Note that this works even without
    * any libpciaccess or libpci support as it keys off the NVIDIA vendor ID.
    * There may be other ways to implement this but this is one way.
    */
   #include <assert.h>
   #include <stdio.h>
   #include "cuda.h"
   #include "mpi.h"
   #include "hwloc.h"

   #define ABORT_ON_ERROR(func) \
     { CUresult res; \
       res = func; \
       if (CUDA_SUCCESS != res) { \
           printf("%s returned error=%d\n", #func, res); \
           abort(); \
       } \
     }
   static hwloc_topology_t topology = NULL;
   static int gpuIndex = 0;
   static hwloc_obj_t gpus[16] = {0};

   /**
    * This function searches for all the GPUs that are hanging off a NUMA
    * node.  It walks through each of the PCI devices and looks for ones
    * with the NVIDIA vendor ID.  It then stores them into an array.
    * Note that there can be more than one GPU on the NUMA node.
    */
   static void find_gpus(hwloc_topology_t topology, hwloc_obj_t parent, hwloc_obj_t child) {
       hwloc_obj_t pcidev;
       pcidev = hwloc_get_next_child(topology, parent, child);
       if (NULL == pcidev) {
           return;
       } else if (0 != pcidev->arity) {
           /* This device has children so need to look recursively at them */
           find_gpus(topology, pcidev, NULL);
           find_gpus(topology, parent, pcidev);
       } else {
           if (pcidev->attr->pcidev.vendor_id == 0x10de) {
               gpus[gpuIndex++] = pcidev;
           }
           find_gpus(topology, parent, pcidev);
       }
   }

   int main(int argc, char *argv[])
   {
       int rank, retval, length;
       char procname[MPI_MAX_PROCESSOR_NAME+1];
       const unsigned long flags = HWLOC_TOPOLOGY_FLAG_IO_DEVICES | HWLOC_TOPOLOGY_FLAG_IO_BRIDGES;
       hwloc_cpuset_t newset;
       hwloc_obj_t node, bridge;
       char pciBusId[16];
       CUdevice dev;
       char devName[256];

       MPI_Init(&argc, &argv);
       MPI_Comm_rank(MPI_COMM_WORLD, &rank);
       if (MPI_SUCCESS != MPI_Get_processor_name(procname, &length)) {
           strcpy(procname, "unknown");
       }

       /* Now decide which GPU to pick.  This requires hwloc to work properly.
        * We first see which CPU we are bound to, then try and find a GPU nearby.
        */
       retval = hwloc_topology_init(&topology);
       assert(retval == 0);
       retval = hwloc_topology_set_flags(topology, flags);
       assert(retval == 0);
       retval = hwloc_topology_load(topology);
       assert(retval == 0);
       newset = hwloc_bitmap_alloc();
       retval = hwloc_get_last_cpu_location(topology, newset, 0);
       assert(retval == 0);

       /* Get the object that contains the cpuset */
       node = hwloc_get_first_largest_obj_inside_cpuset(topology, newset);

       /* Climb up from that object until we find the HWLOC_OBJ_NODE */
       while (node->type != HWLOC_OBJ_NODE) {
           node = node->parent;
       }

       /* Now look for the HWLOC_OBJ_BRIDGE.  All PCI busses hanging off the
        * node will have one of these */
       bridge = hwloc_get_next_child(topology, node, NULL);
       while (bridge->type != HWLOC_OBJ_BRIDGE) {
           bridge = hwloc_get_next_child(topology, node, bridge);
       }

       /* Now find all the GPUs on this NUMA node and put them into an array */
       find_gpus(topology, bridge, NULL);

       ABORT_ON_ERROR(cuInit(0));
       /* Now select the first GPU that we find */
       if (gpus[0] == 0) {
           printf("No GPU found\n");
       } else {
           sprintf(pciBusId, "%.2x:%.2x:%.2x.%x", gpus[0]->attr->pcidev.domain, gpus[0]->attr->pcidev.bus,
           gpus[0]->attr->pcidev.dev, gpus[0]->attr->pcidev.func);
           ABORT_ON_ERROR(cuDeviceGetByPCIBusId(&dev, pciBusId));
           ABORT_ON_ERROR(cuDeviceGetName(devName, 256, dev));
           printf("rank=%d (%s): Selected GPU=%s, name=%s\n", rank, procname, pciBusId, devName);
       }

       MPI_Finalize();
       return 0;
   }

Run-time tuning and debugging
-----------------------------

CUDA IPC in the shared-memory BTL
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

By default, the ``smcuda`` BTL uses CUDA IPC where possible to move GPU
data quickly between GPUs on the same node and PCI root complex.  A few
MCA parameters control this behavior.  You can disable CUDA IPC
entirely:

.. code-block:: sh

   shell$ mpirun --mca btl_smcuda_use_cuda_ipc 0 ...

CUDA IPC is assumed to be possible when two ranks run on the same GPU;
this too can be disabled:

.. code-block:: sh

   shell$ mpirun --mca btl_smcuda_use_cuda_ipc_same_gpu 0 ...

To see whether CUDA IPC is being enabled between two GPUs, turn on some
verbosity:

.. code-block:: sh

   shell$ mpirun --mca btl_smcuda_cuda_ipc_verbose 100 ...

The ``smcuda`` BTL holds on to CUDA IPC registrations even after a
transfer completes, because the registration calls are expensive.  To
limit how much memory is registered, use the
``mpool_rgpusm_rcache_size_limit`` MCA parameter (in bytes); when the
cache reaches this size, the least-recently-used entries are evicted to
make room:

.. code-block:: sh

   shell$ mpirun --mca mpool_rgpusm_rcache_size_limit 1000000 ...

Alternatively, the cache can empty itself entirely when the limit is
reached:

.. code-block:: sh

   shell$ mpirun --mca mpool_rgpusm_rcache_empty_cache 1 ...

Verbose output
^^^^^^^^^^^^^^

Additional CUDA debugging output is available at run time.  The
``opal_cuda_verbose`` parameter has a single verbosity level:

.. code-block:: sh

   shell$ mpirun --mca opal_cuda_verbose 10 ...

The ``mpi_common_cuda_verbose`` parameter provides more detailed
information about CUDA-aware activities and accepts a range of values.
There is normally no need to use these unless you are diagnosing a
problem:

.. code-block:: sh

   shell$ mpirun --mca mpi_common_cuda_verbose 10 ...    # some detail
   shell$ mpirun --mca mpi_common_cuda_verbose 20 ...    # more detail
   shell$ mpirun --mca mpi_common_cuda_verbose 100 ...   # most detail

Developing CUDA-aware applications
----------------------------------

Developing CUDA-aware applications is a broad topic, beyond the scope of
this document.  Such applications often must account for
machine-specific details, including the number of GPUs per node and how
the GPUs are connected to the CPUs and to each other.  With some
transports there are additional run-time decisions to make about which
CPU cores are used with which GPUs.

A good place to start is the `NVIDIA CUDA Toolkit Documentation
<https://docs.nvidia.com/cuda/>`_, including the `Programming Guide
<https://docs.nvidia.com/cuda/cuda-c-programming-guide/>`_ and the `Best
Practices Guide
<https://docs.nvidia.com/cuda/cuda-c-best-practices-guide/>`_.  For
examples of CUDA-aware MPI applications, the `NVIDIA developer blog
<https://github.com/NVIDIA-developer-blog/code-samples/tree/master/posts/cuda-aware-mpi-example>`_
and the `OSU Micro-Benchmarks
<https://mvapich.cse.ohio-state.edu/benchmarks/>`_ are good references.
