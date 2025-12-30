ROCm
====

ROCm is the name of the software stack used by AMD GPUs. It includes
the ROCm Runtime (ROCr), the HIP programming model, and numerous
numerical and machine learning libraries tuned for the AMD Instinct and Radeon
accelerators. More information can be found at the following
`AMD webpages <https://rocm.docs.amd.com/en/latest/>`_


Building Open MPI with ROCm support
-----------------------------------

ROCm-aware support means that the MPI library can send and receive
data from AMD GPU device buffers directly. Starting from Open MPI
v6.0.0 ROCm support is available directly within Open MPI for single
node scenarios, and through UCX or libfabric for multi-node scenarios.


Compiling Open MPI with ROCm support
------------------------------------

Compiling Open MPI with ROCm support requires setting the
``--with-rocm=<rocm-path>`` option at configure time:

.. code-block:: sh

 # Configure Open MPI with ROCm support
 shell$ cd ompi
 shell$ ./configure --with-rocm=/opt/rocm \
        <other configure params>


/////////////////////////////////////////////////////////////////////////

Checking that Open MPI has been built with ROCm support
-------------------------------------------------------

Verify that Open MPI has been built with ROCm using the
:ref:`ompi_info(1) <man1-ompi_info>` command:

.. code-block:: sh

   # Use ompi_info to verify ROCm support in Open MPI
   shell$ ./ompi_info | grep "MPI extensions"
          MPI extensions: affinity, cuda, ftmpi, rocm

/////////////////////////////////////////////////////////////////////////

Runtime querying of ROCm support in Open MPI
--------------------------------------------

Querying the availability of ROCm support in Open MPI at runtime is
possible through the memory allocation kind info object, see ::ref::`memkind`
page for details.

In addition, starting with Open MPI v5.0.0 :ref:`MPIX_Query_rocm_support(3)
<mpix_query_rocm_support>` is available as an extension to check
the availability of ROCm support in the library. To use the
function, the code needs to include ``mpi-ext.h``. Note that
``mpi-ext.h`` is an Open MPI specific header file.


.. _sm-rocm-options-label:

/////////////////////////////////////////////////////////////////////////

Running single node jobs with ROCm support
------------------------------------------

The user has multiple options for running an Open MPI job with GPU support
in a single node scenario:

* the default shared memory component ``btl/sm`` has support for
  accelerators, will use however by default a bounce buffer on the CPU
  for data transfers. Hence, while this works, it will not be able to
  take advantage of the high-speed GPU-to-GPU InfinityFabric
  interconnect (if available).

* to use the high-speed GPU-to-GPU interconnect within a node, the user has to
  enable the accelerator single-copy component (``smsc/accelerator``), e.g.:

.. code-block:: sh

  # Enable the smsc/accelerator component
  shell$ mpirun --mca smsc_accelerator_priority 80 -n 64 ./<my_executable>

* Alternatively, the user can replace the default shared memory
  component ``btl/sm`` with the ``btl/smcuda`` component, which has
  been extended to support ROCm devices. While this approach supports
  communication over a high-speed GPU-to-GPU interconnect, it does not
  support single-copy data transfers for host-memory through
  e.g. ``xpmem`` or ``cma``.  Hence, the performance of host-memory
  based data transfers might be lower than with the default ``btl/sm``
  component. Example:

.. code-block:: sh

  # Use btl/smcuda instead of btl/sm for communication
  shell$ mpirun --mca btl smcuda,tcp,self -n 64 ./<my_executable>

/////////////////////////////////////////////////////////////////////////

ROCm support in Open MPI with UCX
---------------------------------

In this configuration, UCX will provide the ROCm support, and hence it
is important to ensure that UCX itself is built with ROCm support. Both,
inter- and intra-node communication will be executed through UCX.

To see if your UCX library was built with ROCm support, run the
following command:

.. code-block:: sh

   # Check if ucx was built with ROCm support
   shell$ ucx_info -v

   # configured with: --with-rocm=/opt/rocm --enable-mt

If you need to build the UCX library yourself to include ROCm support,
please see the UCX documentation for `building UCX with Open MPI:
<https://openucx.readthedocs.io/en/master/running.html#openmpi-with-ucx>`_

It should look something like:

.. code-block:: sh

   # Configure UCX with ROCm support
   shell$ cd ucx
   shell$ ./configure --prefix=/path/to/ucx-rocm-install \
                      --with-rocm=/opt/rocm

   # Configure Open MPI with UCX and ROCm support
   shell$ cd ompi
   shell$ ./configure --with-rocm=/opt/rocm    \
          --with-ucx=/path/to/ucx-rocm-install \
          <other configure params>

/////////////////////////////////////////////////////////////////////////

Using ROCm-aware UCX with Open MPI
----------------------------------

If UCX and Open MPI have been configured with ROCm support, specifying
the UCX pml component is sufficient to take advantage of the ROCm
support in the libraries. For example, the command to execute the
``osu_latency`` benchmark from the `OSU benchmarks
<https://mvapich.cse.ohio-state.edu/benchmarks>`_ with ROCm buffers
using Open MPI and UCX ROCm support is something like this:

.. code-block:: sh

   shell$ mpirun -n 2 --mca pml ucx \
           ./osu_latency D D

.. note:: some additional configure flags are required to compile the
          OSU benchmark to support ROCm buffers. Please refer to the
          `UCX ROCm instructions
          <https://github.com/openucx/ucx/wiki/Build-and-run-ROCM-UCX-OpenMPI>`_
          for details.

/////////////////////////////////////////////////////////////////////////

ROCm support in Open MPI with libfabric
---------------------------------------

Some network interconnects are supported through the libfabric library.
Configuring libfabric and Open MPI with ROCm support looks something like:

.. code-block:: sh

   # Configure libfabric with ROCm support
   shell$ cd libfabric
   shell$ ./configure --prefix=/path/to/ofi-rocm-install \
                      --with-rocr=/opt/rocm

   # Configure Open MPI with libfabric and ROCm support
   shell$ cd ompi
   shell$ ./configure --with-rocm=/opt/rocm    \
          --with-ofi=/path/to/ofi-rocm-install \
          <other configure params>

/////////////////////////////////////////////////////////////////////////


Using ROCm-aware libfabric with Open MPI
----------------------------------------

There are two mechanism for using libfabric and Open MPI with ROCm support.

* Specifying the ``mtl/ofi`` component is sufficient to take advantage
  of the ROCm support in the libraries. In this case, both intra- and
  inter-node communication will be performed by the libfabric library. In
  order to ensure that the application will make use of the shared
  memory provider for intra-node communication and the network
  interconnect specific provider for inter-node communication, the
  user might have to request using the ``linkX`` provider, e.g.:

.. code-block:: sh

   # Force using the ofi mtl component
   mpirun --mca pml cm --mca mtl ofi                             \
          --mca opal_common_ofi_provider_include "shm+cxi:lnx"   \
          -n 64 ./<my_executable>

* Alternatively, the user can use the ``btl/ofi`` component, in which
  case the intra-node communication will use the Open MPI shared
  memory mechanisms (see <_sm-rocm-options-label>), and use
  libfabric only for inter-node scenarios.

.. code-block:: sh

   # Use the ofi btl for inter-node and sm btl
   # for intra-node communication
   mpirun --mca pml ob1 --mca btl ofi,sm,tcp,self   \
          --mca smsc_accelerator_priority 80        \
          -n 64 ./<my_executable>


/////////////////////////////////////////////////////////////////////////

Collective component supporting ROCm device memory
--------------------------------------------------


The ``coll/accelerator`` component supports collective operations on
ROCm device buffers for many commonly used collective
operations. The component works by copying data into a temporary host
buffer, executing the collective operation on the host buffer, and
copying the result back to the device buffer at completion. This
component will lead to adequate performance for short to medium data
sizes, but performance is often suboptimal especially for large reduction
operations.

The `UCC <https://github.com/openucx/ucc>`_ based collective component
in Open MPI can be configured and compiled to include ROCm support,
and will typically lead to significantly better performance for large
reductions.

An example for configure UCC and Open MPI with ROCm is shown below:

.. code-block:: sh

   # Configure and compile UCC with ROCm support
   shell$ cd ucc
   shell$ ./configure --with-rocm=/opt/rocm                \
                      --with-ucx=/path/to/ucx-rocm-install \
                      --prefix=/path/to/ucc-rocm-install
   shell$ make -j && make install

   # Configure and compile Open MPI with UCX, UCC, and ROCm support
   shell$ cd ompi
   shell$ ./configure --with-rocm=/opt/rocm                \
                      --with-ucx=/path/to/ucx-rocm-install \
                      --with-ucc=/path/to/ucc-rocm-install

To use the UCC component in an application requires setting some
additional parameters:

.. code-block::

   shell$ mpirun --mca pml ucx --mca osc ucx \
                 --mca coll_ucc_enable 1     \
                 --mca coll_ucc_priority 100 -np 64 ./my_mpi_app

.. note:: Using the UCC library for collective operations in Open MPI
          requires using the UCX library, and hence cannot be deployed
          e.g. when using libfabric.
