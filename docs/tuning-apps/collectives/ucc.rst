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
   * - ``coll_ucc_derived_sharp``
     - ``false``
     - Allow SHARP on communicators other than ``MPI_COMM_WORLD`` that
       do not set the ``ompi_comm_coll_ucc_sharp`` info key.  See
       :ref:`label-coll-ucc-sharp`.
   * - ``coll_ucc_max_domains``
     - ``0`` (unlimited)
     - Maximum number of live UCC contexts per process.  A communicator
       that would need another context uses the previous collective
       components instead.

.. _label-coll-ucc-sharp:

SHARP on Derived Communicators
------------------------------

When the UCC library includes the SHARP transport (``tl/sharp``), Open
MPI uses SHARP for ``MPI_COMM_WORLD`` only.  Every UCC context that may
use SHARP gets a second context over the same processes in which
``tl/sharp`` is disabled, and communicators that do not ask for SHARP
use it, so they take no SHARP resources and avoid the cost of creating
a SHARP team.  For communicators derived from ``MPI_COMM_WORLD`` this
is one extra UCC context per process.  Without ``tl/sharp`` in the UCC
library nothing changes, no extra context is created and the info key
below has no effect.

To use SHARP on another communicator, create it with the info key
``ompi_comm_coll_ucc_sharp`` set to ``true``, for example with
``MPI_Comm_dup_with_info``, ``MPI_Comm_idup_with_info``,
``MPI_Comm_split_type`` or ``MPI_Comm_create_from_group``.  The accepted
values are ``true``, ``false``, ``yes``, ``no``, ``1`` and ``0``;
``false`` keeps SHARP off, and any other value (for example
``default``) selects the default described above.

* The key is read when the communicator is created; changing it later
  with ``MPI_Comm_set_info`` has no effect.
* A communicator without the key whose group is identical to its
  parent's (``MPI_Comm_dup``, ``MPI_Comm_idup``) inherits an explicit
  value from the parent.  Splits and other derived communicators do
  not, so to use SHARP on the result of ``MPI_Comm_split``, duplicate
  it with ``MPI_Comm_dup_with_info``.
* The key must be set with the same value on all processes of the
  communicator, or on none of them; otherwise the program is erroneous.
  With ``tl/sharp`` present, processes that all set the key but with
  different values do not use UCC for that communicator.
* The key has no effect on ``MPI_COMM_WORLD``, whose transports are
  selected through the UCC environment (for example
  ``UCC_CL_BASIC_TLS``).  A Sessions communicator created from the
  ``mpi://WORLD`` process set counts as derived: it gets no SHARP
  unless it sets the key.

Setting ``coll_ucc_derived_sharp`` to ``true`` restores SHARP on every
communicator that does not set the key.  With ``coll_ucc_max_domains``
set to ``1`` there is no room for the second context, so communicators
other than ``MPI_COMM_WORLD`` that do not ask for SHARP use the previous
collective components.

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
