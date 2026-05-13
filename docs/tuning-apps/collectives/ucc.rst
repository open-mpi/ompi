The ``ucc`` Component
=====================

The ``ucc`` collective component uses the `Unified Collective
Communication (UCC) library <https://github.com/openucx/ucc/>`_ to
offload selected MPI collective operations to UCC.  This component is
useful on systems where UCC has been configured for the target transport
or accelerator environment.

Building with UCC
-----------------

Open MPI must be configured with UCC support:

.. code-block:: sh

   shell$ ./configure --with-ucc=/path/to/ucc-install

If UCC support is explicitly requested and the UCC headers and library
cannot be found, ``configure`` aborts.  The ``ucc`` component is disabled
when Open MPI is configured with progress thread support, because the UCC
driver does not currently support progress threads.

Enabling the Component
----------------------

The component is not enabled by default.  Enable it at run time and give
it a high enough priority to be selected:

.. code-block:: sh

   shell$ mpirun --mca coll_ucc_enable 1 \
                 --mca coll_ucc_priority 100 \
                 -np 64 ./my_mpi_app

The ``ucc`` component is considered only for intracommunicators whose
size is at least ``coll_ucc_np``.  The default value of ``coll_ucc_np``
is ``2``.

UCC Layers and Protocols
------------------------

For each MPI communicator selected for UCC, Open MPI creates a UCC
``team``: the UCC group object used to initialize and execute collective
operations.  Inside UCC, collective implementations are selected through
two kinds of layers:

* Collective layers (CLs), such as ``basic`` and ``hier``, decide how a
  collective is decomposed.
* Team layers (TLs), such as ``ucp``, ``self``, ``cuda``, ``nccl``,
  ``rccl``, ``sharp``, and ``mlx5``, provide the underlying transport or
  accelerator implementation.

For example, the ``ucp`` TL uses UCX/UCP transports such as InfiniBand,
RoCE, and shared memory; ``sharp`` uses SHARP in-network collective
offload; and ``nccl`` or ``rccl`` can be used for GPU collectives on
CUDA or ROCm memory.

The ``basic`` CL is the general-purpose layer.  The ``hier`` CL can use
system hierarchy when it is available; for example, it may split work
across ``NODE`` and ``NET`` subgroups, plus the ``FULL`` group, and then
pipeline phases through different TLs.  A typical hierarchical protocol
could use an intra-node reduction, an inter-node operation such as
SHARP, and an intra-node broadcast.

The exact CLs, TLs, and algorithms available depend on how UCC was
built.  Use UCC's own tools to inspect the installed library:

.. code-block:: sh

   shell$ ucc_info -s   # Show available CLs and TLs
   shell$ ucc_info -A   # Show supported collective algorithms
   shell$ ucc_info -caf # Show UCC configuration variables

Open MPI's ``coll_ucc_cls`` MCA parameter is passed to UCC as its
``CLS`` setting.  It can be used to restrict team creation to specific
UCC collective layers, for example:

.. code-block:: sh

   shell$ mpirun --mca coll_ucc_enable 1 \
                 --mca coll_ucc_cls hier \
                 ./my_mpi_app

For lower-level TL tuning, use UCC environment variables such as
``UCC_TL_<NAME>_TUNE`` or a UCC configuration file.  UCC scores TLs
based on factors including the collective type, message size, memory
type, and team size.

Selecting Collective Operations
-------------------------------

Use ``coll_ucc_cts`` to choose which collective operations the component
should provide.  By default, the component enables all supported blocking
and nonblocking operations.

.. code-block:: sh

   shell$ mpirun --mca coll_ucc_enable 1 \
                 --mca coll_ucc_cts allreduce,iallreduce,bcast,ibcast \
                 ./my_mpi_app

Prefix the value with ``^`` to start from all supported operations and
disable specific operations from that set:

.. code-block:: sh

   shell$ mpirun --mca coll_ucc_enable 1 \
                 --mca coll_ucc_cts ^alltoall,ialltoall \
                 ./my_mpi_app

The supported operation names are:

* ``barrier``, ``bcast``, ``allreduce``, ``alltoall``, ``alltoallv``,
  ``allgather``, ``allgatherv``, ``reduce``, ``gather``, ``gatherv``,
  ``reduce_scatter_block``, ``reduce_scatter``, ``scatterv``, and
  ``scatter``
* ``ibarrier``, ``ibcast``, ``iallreduce``, ``ialltoall``,
  ``ialltoallv``, ``iallgather``, ``iallgatherv``, ``ireduce``,
  ``igather``, ``igatherv``, ``ireduce_scatter_block``,
  ``ireduce_scatter``, ``iscatterv``, and ``iscatter``

The aliases ``colls_b``, ``colls_i`` (or ``colls_nb``), and ``colls_p``
select all blocking, nonblocking, and persistent collective operations,
respectively.  Individual persistent collective operations can be
selected by adding the ``_init`` suffix to the blocking operation name,
for example ``allreduce_init``.

Other MCA Parameters
--------------------

.. list-table::
   :header-rows: 1
   :widths: 30 15 55

   * - Parameter
     - Default
     - Description
   * - ``coll_ucc_enable``
     - ``0``
     - Enable or disable the component.
   * - ``coll_ucc_priority``
     - ``10``
     - Component selection priority.
   * - ``coll_ucc_verbose``
     - ``0``
     - Verbosity level for component logging.
   * - ``coll_ucc_np``
     - ``2``
     - Minimum communicator size for enabling the component.
   * - ``coll_ucc_cls``
     - UCC default
     - Comma-separated list of UCC collective layers to use for team
       creation, passed to UCC as ``CLS``.
   * - ``coll_ucc_cts``
     - All supported blocking and nonblocking operations
     - Comma-separated list of UCC collective types to enable.

Verifying Selection
-------------------

Use ``coll_base_verbose`` to check which collective component Open MPI
selects for each operation:

.. code-block:: sh

   shell$ mpirun --mca coll_ucc_enable 1 \
                 --mca coll_ucc_priority 100 \
                 --mca coll_base_verbose 20 \
                 ./my_mpi_app

See :doc:`components` for more details about interpreting collective
component selection output.
