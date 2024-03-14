ROCm
====

ROCm is the name of the software stack used by AMD GPUs. It includes
the ROCm Runtime (ROCr), the HIP programming model, and numerous
numerical and machine learning libraries tuned for the AMD Instinct
accelerators. More information can be found at the following
`AMD webpages <https://www.amd.com/en/graphics/servers-solutions-rocm>`_


Building Open MPI with ROCm support
-----------------------------------

ROCm-aware support means that the MPI library can send and receive
data from AMD GPU device buffers directly. As of today, ROCm support
is available through UCX. While other communication transports might
work as well, UCX is the only transport formally supported in Open MPI
|ompi_ver| for ROCm devices.

Since UCX will be providing the ROCm support, it is important to
ensure that UCX itself is built with ROCm support.

To see if your UCX library was built with ROCm support, run the
following command:

.. code-block:: sh

 # Check if ucx was built with ROCm support
 shell$ ucx_info -v

 # configured with: --with-rocm=/opt/rocm --without-knem --without-cuda

If you need to build the UCX library yourself to include ROCm support,
please see the UCX documentation for `building UCX with Open MPI:
<https://openucx.readthedocs.io/en/master/running.html#openmpi-with-ucx>`_

It should look something like:

.. code-block:: sh

   # Configure UCX with ROCm support
   shell$ cd ucx
   shell$ ./configure --prefix=/path/to/ucx-rocm-install \
                     --with-rocm=/opt/rocm --without-knem

   # Configure Open MPI with UCX and ROCm support
   shell$ cd ompi
   shell$ ./configure --with-rocm=/opt/rocm    \
          --with-ucx=/path/to/ucx-rocm-install \
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


Using ROCm-aware UCX with Open MPI
--------------------------------------------------------------------------

If UCX and Open MPI have been configured with ROCm support, specifying
the UCX pml component is sufficient to take advantage of the ROCm
support in the libraries. For example, the command to execute the
``osu_latency`` benchmark from the `OSU benchmarks
<https://mvapich.cse.ohio-state.edu/benchmarks>`_ with ROCm buffers
using Open MPI and UCX ROCm support is something like this:

.. code-block::

   shell$ mpirun -n 2 --mca pml ucx \
           ./osu_latency -d rocm D D

Note: some additional configure flags are required to compile the OSU
benchmark to support ROCm buffers. Please refer to the `UCX ROCm
instructions
<https://github.com/openucx/ucx/wiki/Build-and-run-ROCM-UCX-OpenMPI>`_
for details.


/////////////////////////////////////////////////////////////////////////

Runtime querying of ROCm support in Open MPI
--------------------------------------------

Starting with Open MPI v5.0.0 :ref:`MPIX_Query_rocm_support(3)
<mpix_query_rocm_support>` is available as an extension to check
the availability of ROCm support in the library. To use the
function, the code needs to include ``mpi-ext.h``. Note that
``mpi-ext.h`` is an Open MPI specific header file.

/////////////////////////////////////////////////////////////////////////

Collective component supporting ROCm device memory
--------------------------------------------------

The `UCC <https://github.com/openucx/ucc>`_ based collective component
in Open MPI can be configured and compiled to include ROCm support.

An example for configure UCC and Open MPI with ROCm is shown below:

.. code-block::

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
   
To use the UCC component in an applicatin requires setting some
additional parameters:

.. code-block::

   shell$ mpirun --mca pml ucx --mca osc ucx \
                 --mca coll_ucc_enable 1     \
                 --mca coll_ucc_priority 100 -np 64 ./my_mpi_app
