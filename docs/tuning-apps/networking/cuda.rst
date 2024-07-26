CUDA
====

.. error:: TODO This section needs to be converted from FAQ Q&A style
           to regular documentation style.

How do I build Open MPI with CUDA-aware support?
------------------------------------------------

CUDA-aware support means that the MPI library can send and receive GPU
buffers directly.  CUDA support is being continuously updated so
different levels of support exist in different versions.  We recommend
you use the latest version of Open MPI for best support.

Open MPI offers two flavors of CUDA support:

#. Via `UCX <https://openucx.org/>`_.

   This is the preferred mechanism.  Since UCX will be providing the
   CUDA support, it is important to ensure that UCX itself is built
   with CUDA support.

   To see if your ucx was built with CUDA support run the following
   command:

   .. code-block:: sh

      # Check if ucx was built with CUDA support
      shell$ ucx_info -v

      # configured with: --build=powerpc64le-redhat-linux-gnu --host=powerpc64le-redhat-linux-gnu --program-prefix= --disable-dependency-tracking --prefix=/usr --exec-prefix=/usr --bindir=/usr/bin --sbindir=/usr/sbin --sysconfdir=/etc --datadir=/usr/share --includedir=/usr/include --libdir=/usr/lib64 --libexecdir=/usr/libexec --localstatedir=/var --sharedstatedir=/var/lib --mandir=/usr/share/man --infodir=/usr/share/info --disable-optimizations --disable-logging --disable-debug --disable-assertions --enable-mt --disable-params-check --enable-cma --without-cuda --without-gdrcopy --with-verbs --with-cm --with-knem --with-rdmacm --without-rocm --without-xpmem --without-ugni --without-java

   If you need to build ucx yourself to include CUDA support, please
   see the UCX documentation for `building ucx with Open MPI: <https://openucx.readthedocs.io/en/master/running.html#openmpi-with-ucx>`_

   It should look something like:

   .. code-block:: sh

      # Configure UCX this way
      shell$ ./configure --prefix=/path/to/ucx-cuda-install --with-cuda=/usr/local/cuda --with-gdrcopy=/usr

      # Configure Open MPI this way
      shell$ ./configure --with-cuda=/usr/local/cuda --with-ucx=/path/to/ucx-cuda-install <other configure params>

#. Via internal Open MPI CUDA support

Regardless of which flavor of CUDA support (or both) you plan to use,
Open MPI should be configured using the ``--with-cuda=<path-to-cuda>``
configure option to build CUDA support into Open MPI. The configure
script will automatically search the path given for ``libcuda.so``. If it cannot
be found, please also pass ``--with-cuda-libdir``. For example:
``--with-cuda=<path-to-cuda> --with-cuda-libdir=/usr/local/cuda/lib64/stubs``.

Open MPI supports building with CUDA libraries and running on systems
without CUDA libraries or hardware. In order to take advantage of
this functionality, when compiling, you have to specify the CUDA
dependent components to be built as DSOs using the
``--enable-mca-dso=<comma-delimited-list-of-cuda-components.``
configure option.

This affects the ``smcuda`` shared memory and ``uct`` BTLs, as well
as the ``rgpusm`` and ``gpusm`` rcache components.

An example configure command would look like the following:

   .. code-block:: sh

      # Configure Open MPI this way
      shell$ ./configure --with-cuda=/usr/local/cuda \
             --enable-mca-dso=btl-smcuda,rcache-rgpusm,rcache-gpusm,accelerator-cuda <other configure params>

/////////////////////////////////////////////////////////////////////////

How do I verify that Open MPI has been built with CUDA support?
---------------------------------------------------------------

Verify that Open MPI has been built with cuda using ``ompi_info``
with one of the following commands.

.. code-block:: sh

   # Use ompi_info to verify cuda support in Open MPI
   shell$ ompi_info | grep "MPI extensions"
          MPI extensions: affinity, cuda, pcollreq
   shell$ ompi_info --parsable --all | grep mpi_built_with_cuda_support:value
          mca:mpi:base:param:mpi_built_with_cuda_support:value:true

/////////////////////////////////////////////////////////////////////////

How do I run Open MPI with applications that pass CUDA buffers to MPI?
----------------------------------------------------------------------

Open MPI will detect and enable CUDA enabled components at runtime with
no additional mpirun parameters.

/////////////////////////////////////////////////////////////////////////

How do I build Open MPI with CUDA-aware support using PGI?
----------------------------------------------------------

With CUDA 6.5, you can build all versions of CUDA-aware Open MPI
without doing anything special.  However, with CUDA 7.0 and CUDA 7.5,
you need to pass in some specific compiler flags for things to work
correctly.  Add the following to your configure line.

.. code-block:: sh

   # For PGI 15.9 and later (Also called NVCC):
   shell$ ./configure --with-wrapper-cflags=-ta:tesla

   # For earlier versions of PGI:
   shell$ ./configure CFLAGS=-D__LP64__ --with-wrapper-cflags="-D__LP64__ -ta:tesla"

/////////////////////////////////////////////////////////////////////////

What kind of CUDA support exists in Open MPI?
---------------------------------------------

CUDA-aware support is defined as Open MPI automatically detecting that
the argument pointer being passed to an MPI routine is a CUDA device
memory pointer.

See :ref:`this FAQ entry <faq-cuda-mpi-apis-cuda-label>`
for more details on which APIs are CUDA-aware.


.. error:: CUDA 4.0 is SUPER OLD!  End users dont care about the
   differences between cuda-aware, cuda-ipc, gpu-direct, and gpu-direct-rdma

Open MPI depends on various features of CUDA 4.0, so one needs to have
at least the CUDA 4.0 driver and toolkit.  The new features of
interest are the Unified Virtual Addressing (UVA) so that all pointers
within a program have unique addresses.  In addition, there is a new
API that allows one to determine if a pointer is a CUDA device pointer
or host memory pointer.  This API is used by the library to decide
what needs to be done with each buffer.  In addition, CUDA 4.1 also
provides the ability to register host memory with the CUDA driver,
which can improve performance.  CUDA 4.1 also added CUDA IPC support
for fast communication between GPUs on the same node.

Note that derived datatypes |mdash| both contiguous and non-contiguous
|mdash| are supported.  However, the non-contiguous datatypes
currently have high overhead because of the many calls to the CUDA
function ``cuMemcpy()`` to copy all the pieces of the buffer into the
intermediate buffer.

CUDA-aware support is available in:

* The UCX (``ucx``) PML
* The PSM2 (``psm2``) MTL with the CM (``cm``) PML.
* The OFI (``ofi``) MTL with the CM (``cm``) PML.
* Both CUDA-ized shared memory (``smcuda``) and TCP (``tcp``) BTLs
  with the OB1 (``ob1``) PML.
* The HCOLL (``hcoll``) COLL

/////////////////////////////////////////////////////////////////////////

PSM2 support for CUDA
---------------------

CUDA-aware support is present in PSM2 MTL.  When running CUDA-aware
Open MPI on Cornelis Networks Omni-Path, the PSM2 MTL will automatically set
``PSM2_CUDA`` environment variable which enables PSM2 to handle GPU
buffers.  If the user wants to use host buffers with a CUDA-aware Open
MPI, it is recommended to set ``PSM2_CUDA`` to ``0`` in the execution
environment. PSM2 also has support for the NVIDIA GPUDirect support
feature. To enable this, users will need to set ``PSM2_GPUDIRECT``
to ``1`` in the execution environment.

Note: The PSM2 library and ``hfi1`` driver with CUDA support are requirements
to use GPUDirect support on Cornelis Networks Omni-Path. The minimum
PSM2 build version required is `PSM2 10.2.175
<https://github.com/01org/opa-psm2/releases/tag/PSM2_10.2-175>`_.

For more information refer to the `Cornelis Networks Customer Center
<https://customercenter.cornelisnetworks.com/>`_.

/////////////////////////////////////////////////////////////////////////

OFI support for CUDA
---------------------

CUDA-aware support is present in OFI MTL.  When running CUDA-aware
Open MPI over Libfabric, the OFI MTL will check if there are any
providers capable of handling GPU (or other accelerator) memory
through the ``hmem``-related flags. If a CUDA-capable provider is
available, the OFI MTL will directly send GPU buffers through
Libfabric's API after registering the memory. If there are no
CUDA-capable providers available, the buffers will automatically
be copied to host buffers before being transferred through
Libfabric's API.

/////////////////////////////////////////////////////////////////////////

Can I get additional CUDA debug-level information at run-time?
--------------------------------------------------------------

Yes, by enabling some vebosity flags.

* The ``opal_cuda_verbose`` parameter has only one level of verbosity:

  .. code-block::

     shell$ mpirun --mca opal_cuda_verbose 10 ...


* The ``mpi_common_cuda_verbose`` parameter provides additional
  information about CUDA-aware related activities.  This can be set to
  a variety of different values.  There is really no need to use these
  unless you have strange problems:

  .. code-block:: sh

     # A bunch of CUDA debug information
     shell$ mpirun --mca mpi_common_cuda_verbose 10 ...
     # Even more CUDA debug information
     shell$ mpirun --mca mpi_common_cuda_verbose 20 ...
     # Yet more CUDA debug information
     shell$ mpirun --mca mpi_common_cuda_verbose 100 ...

* The ``smcuda`` BTL has three MCA parameters related to the use of
  CUDA IPC.  By default, CUDA IPC is used where possible.  But the
  user can now turn it off if they prefer.

  .. code-block:: sh

     shell$ mpirun --mca btl_smcuda_use_cuda_ipc 0 ...

  In addition, it is assumed that CUDA IPC is possible when running on
  the same GPU, and this is typically true.  However, there is the
  ability to turn it off.

  .. code-block:: sh

     shell$ mpirun --mca btl_smcuda_use_cuda_ipc_same_gpu 0 ...

  Last, to get some insight into whether CUDA IPC is being used, you
  can turn on some verbosity that shows whether CUDA IPC gets enabled
  between two GPUs.

  .. code-block:: sh

     shell$ mpirun --mca btl_smcuda_cuda_ipc_verbose 100 ...

/////////////////////////////////////////////////////////////////////////

.. _faq-cuda-mpi-cuda-numa-issues-label:

NUMA Node Issues
----------------

When running on a node that has multiple GPUs, you may want to select
the GPU that is closest to the NUMA node on which your process is
running.  One way to do this is to make use of the ``hwloc`` library.
The following is a C code snippet that can be used in your application
to select a GPU that is close.  It will determine on which CPU it is
running and then look for the closest GPU.  There could be multiple
GPUs that are the same distance away.  This is dependent on having
``hwloc`` somewhere on your system.

.. code-block:: c

   /**
    * Test program to show the use of hwloc to select the GPU closest to the CPU
    * that the MPI program is running on.  Note that this works even without
    * any libpciaccess or libpci support as it keys off the NVIDIA vendor ID.
    * There may be other ways to implement this but this is one way.
    * January 10, 2014
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

/////////////////////////////////////////////////////////////////////////

How do I develop CUDA-aware Open MPI applications?
--------------------------------------------------

Developing CUDA-aware applications is a complex topic, and beyond the
scope of this document. CUDA-aware applications often have to take
machine-specific considerations into account, including the number of
GPUs installed on each node and how the GPUs are connected to the CPUs
and to each other. Often, when using a particular transport layer
(such as OPA/PSM2) there will be run-time decisions to make about
which CPU cores will be used with which GPUs.

A good place to start is the `NVIDIA CUDA Toolkit Documentation
<https://docs.nvidia.com/cuda/>`_ including the `Programming Guide
<https://docs.nvidia.com/cuda/cuda-c-programming-guide/>`_ and the
`Best Practices Guide
<https://docs.nvidia.com/cuda/cuda-c-best-practices-guide/>`_.  For
examples of how to write CUDA-aware MPI applications, the `NVIDIA
developers blog
<https://github.com/NVIDIA-developer-blog/code-samples/tree/master/posts/cuda-aware-mpi-example>`_
offers examples and the `OSU Micro-Benchmarks
<https://mvapich.cse.ohio-state.edu/benchmarks/>`_ offer an excellent
example of how to write CUDA-aware MPI applications.

/////////////////////////////////////////////////////////////////////////

.. _faq-cuda-mpi-apis-cuda-label:

Which MPI APIs work with CUDA-aware?
------------------------------------

* MPI_Allgather
* MPI_Allgatherv
* MPI_Allreduce
* MPI_Alltoall
* MPI_Alltoallv
* MPI_Alltoallw
* MPI_Bcast
* MPI_Bsend
* MPI_Bsend_init
* MPI_Exscan
* MPI_Ibsend
* MPI_Irecv
* MPI_Isend
* MPI_Irsend
* MPI_Issend
* MPI_Gather
* MPI_Gatherv
* MPI_Get
* MPI_Put
* MPI_Rsend
* MPI_Rsend_init
* MPI_Recv
* MPI_Recv_init
* MPI_Reduce
* MPI_Reduce_scatter
* MPI_Reduce_scatter_block
* MPI_Scan
* MPI_Scatter
* MPI_Scatterv
* MPI_Send
* MPI_Send_init
* MPI_Sendrecv
* MPI_Ssend
* MPI_Ssend_init
* MPI_Win_create

.. FIXME: We need to verify the above list.

/////////////////////////////////////////////////////////////////////////

Which MPI APIs do NOT work with CUDA-aware?
-------------------------------------------

* MPI_Accumulate
* MPI_Compare_and_swap
* MPI_Fetch_and_op
* MPI_Get_Accumulate
* MPI_Iallgather
* MPI_Iallgatherv
* MPI_Iallreduce
* MPI_Ialltoall
* MPI_Ialltoallv
* MPI_Ialltoallw
* MPI_Ibcast
* MPI_Iexscan
* MPI_Rget
* MPI_Rput

.. FIXME: We need to verify the above list.

/////////////////////////////////////////////////////////////////////////

How do I use CUDA-aware UCX for Open MPI?
-----------------------------------------

Example of running ``osu_latency`` from the `OSU benchmarks
<https://mvapich.cse.ohio-state.edu/benchmarks>`_ with CUDA buffers
using Open MPI and UCX CUDA support:

.. code-block::

   shell$ mpirun -n 2 --mca pml ucx \
       -x UCX_TLS=rc,sm,cuda_copy,gdr_copy,cuda_ipc ./osu_latency D D

/////////////////////////////////////////////////////////////////////////

Which MPI APIs work with CUDA-aware UCX?
----------------------------------------

* MPI_Send
* MPI_Bsend
* MPI_Ssend
* MPI_Rsend
* MPI_Isend
* MPI_Ibsend
* MPI_Issend
* MPI_Irsend
* MPI_Send_init
* MPI_Bsend_init
* MPI_Ssend_init
* MPI_Rsend_init
* MPI_Recv
* MPI_Irecv
* MPI_Recv_init
* MPI_Sendrecv
* MPI_Bcast
* MPI_Gather
* MPI_Gatherv
* MPI_Allgather
* MPI_Reduce
* MPI_Reduce_scatter
* MPI_Reduce_scatter_block
* MPI_Allreduce
* MPI_Scan
* MPI_Exscan
* MPI_Allgatherv
* MPI_Alltoall
* MPI_Alltoallv
* MPI_Alltoallw
* MPI_Scatter
* MPI_Scatterv
* MPI_Iallgather
* MPI_Iallgatherv
* MPI_Ialltoall
* MPI_Iialltoallv
* MPI_Ialltoallw
* MPI_Ibcast
* MPI_Iexscan

.. FIXME: We need to verify the above list.  These _SHOULD_ be the same
   as above.

/////////////////////////////////////////////////////////////////////////

Which MPI APIs do NOT work with CUDA-aware UCX?
-----------------------------------------------

* All one-sided operations such as MPI_Put, MPI_Get, MPI_Accumulate,
  MPI_Rget, MPI_Rput, MPI_Get_Accumulate, MPI_Fetch_and_op,
  MPI_Compare_and_swap, etc
* All window creation calls such as MPI_Win_create
* All non-blocking reduction collectives like MPI_Ireduce,
  MPI_Iallreduce, etc

.. FIXME: Checking with nVidia.  This may be more of an issue of OSC_UCX
   not supporting CUDA, though perhaps it's just performance.

/////////////////////////////////////////////////////////////////////////

Can I tell at compile time or runtime whether I have CUDA-aware support?
------------------------------------------------------------------------

There is both a compile time check and a run-time check available.
You can use whichever is the most convenient for your program.  To
access them, you need to include ``mpi-ext.h``. Note that
``mpi-ext.h`` is specific to Open MPI. The following program shows an
example of using the CUDA-aware macro and run-time check.

.. code-block:: c

   /*
    * Program that shows the use of CUDA-aware macro and runtime check.
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
       printf("This MPI library has CUDA-aware support.\n", MPIX_CUDA_AWARE_SUPPORT);
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

/////////////////////////////////////////////////////////////////////////

How do I limit how much CUDA IPC memory is held in the registration cache?
--------------------------------------------------------------------------

As mentioned earlier, the Open MPI library will make use of CUDA IPC support where
possible to move the GPU data quickly between GPUs that are on the same node and
same PCI root complex. The library holds on to registrations even after the data
transfer is complete as it is expensive to make some of the CUDA IPC registration
calls. If you want to limit how much memory is registered, you can use the
``mpool_rgpusm_rcache_size_limit`` MCA parameter. For example, this sets the limit
to 1000000 bytes:

.. code-block::

   shell$ mpirun --mca mpool_rgpusm_rcache_size_limit 1000000 ...

When the cache reaches this size, it will kick out the least recently used until
it can fit the new registration in.

There also is the ability to have the cache empty itself out when the
limit is reached:

.. code-block::

   shell$ mpirun --mca mpool_rgpusm_rcache_empty_cache 1 ...

/////////////////////////////////////////////////////////////////////////

What are some guidelines for using CUDA and Open MPI with Omni-Path?
--------------------------------------------------------------------

When developing CUDA-aware Open MPI applications for OPA-based fabrics, the
PSM2 transport is preferred and a CUDA-aware version of PSM2 is provided with
all versions of the Cornelis Networks Omni-Path OPXS software suite.

.. error:: TODO Are Intel/OPA references still correct?

The PSM2 library provides a number of settings that will govern how it
will interact with CUDA, including ``PSM2_CUDA`` and ``PSM2_GPUDIRECT``,
which should be set in the environment before ``MPI_Init()`` is called. For
example:

.. code-block::

   shell$ mpirun -x PSM2_CUDA=1 -x PSM2_GPUDIRECT=1 --mca mtl psm2 mpi_hello

In addition, each process of the application should select a specific
GPU card to use before calling ``MPI_Init()``, by using
``cudaChooseDevice()``, ``cudaSetDevice()`` and similar. The chosen
GPU should be within the same NUMA node as the CPU the MPI process is
running on. You will also want to use the ``mpirun``
``--bind-to-core`` or ``--bind-to-socket`` option to ensure that MPI
processes do not move between NUMA nodes. See the section on
:ref:`NUMA Node Issues <faq-cuda-mpi-cuda-numa-issues-label>`, for
more information.

For more information see the *Cornelis Networks Performance Scaled Messaging 2
(PSM2) Programmer's Guide* and the *Cornelis Networks Omni-Path Performance
Tuning Guide*, which can be found in the `Cornelis Networks Customer Center
<https://customercenter.cornelisnetworks.com/>`_.

.. error:: TODO Are Intel/OPA references still correct?

/////////////////////////////////////////////////////////////////////////

When do I need to select a CUDA device?
---------------------------------------

"mpi-cuda-dev-selection"

OpenMPI requires CUDA resources allocated for internal use.  These
are allocated lazily when they are first needed, e.g. CUDA IPC mem handles
are created when a communication routine first requires them during a
transfer.  So, the CUDA device needs to be selected before the first MPI
call requiring a CUDA resource. MPI_Init and most communicator related
operations do not create any CUDA resources (guaranteed for MPI_Init,
MPI_Comm_rank, MPI_Comm_size, MPI_Comm_split_type and MPI_Comm_free).  It
is thus possible to use those routines to query rank information and use
those to select a GPU, e.g. using

.. code-block:: c

    int local_rank = -1;
    {
        MPI_Comm local_comm;
        MPI_Comm_split_type(MPI_COMM_WORLD, MPI_COMM_TYPE_SHARED, rank, MPI_INFO_NULL, &local_comm);
        MPI_Comm_rank(local_comm, &local_rank);
        MPI_Comm_free(&local_comm);
    }
    int num_devices = 0;
    cudaGetDeviceCount(&num_devices);
    cudaSetDevice(local_rank % num_devices);

MPI internal CUDA resources are released during MPI_Finalize. Thus it is an
application error to call cudaDeviceReset before MPI_Finalize is called.


/////////////////////////////////////////////////////////////////////////

How do I enable CUDA support in HCOLL collective component
----------------------------------------------------------

HCOLL component supports CUDA GPU buffers for the following
collectives:

MPI_Allreduce
MPI_Bcast
MPI_Allgather
MPI_Ibarrier
MPI_Ibcast
MPI_Iallgather
MPI_Iallreduce

To enable CUDA GPU buffer support in these collectives pass the
following environment variables via mpirun:

.. code-block::

   shell$ mpirun -x HCOLL_GPU_ENABLE=1 -x HCOLL_ENABLE_NBC=1 ..

See `nVidia HCOLL documentation <https://docs.nvidia.com/networking/display/HPCXv29/HCOLL>`_
for more information.
