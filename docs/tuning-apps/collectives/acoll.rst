Working with the ``acoll`` Collective Component
===============================================

Introduction
------------

The ``acoll`` (AMD Collective) component is a high-performance MPI collective implementation optimized for AMD Zen-based processors. At present, ``acoll`` is optimized for the following commonly used collective algorithms:

- MPI_Allgather
- MPI_Allreduce
- MPI_Alltoall
- MPI_Barrier
- MPI_Bcast
- MPI_Gather
- MPI_Reduce

The component uses topology-aware algorithms that leverage subgroups, NUMA domains, and socket hierarchies to achieve optimal performance on AMD Zen architectures.

Enabling the acoll Component
-----------------------------

To enable the ``acoll`` component, set its priority higher than other collective components:

.. code-block:: sh

   shell$ mpirun <common mpi runtime parameters> --bind-to core \
                 --mca coll acoll,tuned,libnbc,basic --mca coll_acoll_priority 40 <executable>

The component will only activate for communicators meeting the minimum size threshold (default: 16 ranks). Use ``--bind-to core`` for optimal performance.

MCA Parameters
--------------

Core Configuration
~~~~~~~~~~~~~~~~~~

.. list-table::
   :header-rows: 1
   :widths: 30 15 55

   * - Parameter
     - Default
     - Description
   * - ``coll_acoll_priority``
     - 0
     - Component selection priority
   * - ``coll_acoll_comm_size_thresh``
     - 16
     - Minimum communicator size for which ``acoll`` component is enabled

Topology Parameters
~~~~~~~~~~~~~~~~~~~

.. list-table::
   :header-rows: 1
   :widths: 30 15 55

   * - Parameter
     - Default
     - Description
   * - ``coll_acoll_max_comms``
     - 10
     - Maximum number of nested layers of subcommunicators to be derived by ``acoll`` from a given communicator. For example, for a given parent communicator, child1 is derived out of parent, child2 is derived out of child1, so on till child10 is derived out of child9. No split of child10 is allowed.
   * - ``coll_acoll_force_numa``
     - -1
     - Force NUMA based subgroups to be used in hierarchical version of the broadcast collective. Default is auto-tuned, where NUMA based subgroup is enabled based on communicator size and message size. Force enable (1) or disable (0) NUMA-based communicator split; -1 for auto.
   * - ``coll_acoll_bcast_socket``
     - -1
     - Enable (1) or disable (0) socket based hierarchy rather than node based hierarchy in broadcast and allreduce collectives. The default value is -1, where the hierarchy is pre-configured based on message size and communicator size.

Runtime Optimization Parameters
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. list-table::
   :header-rows: 1
   :widths: 30 15 55

   * - Parameter
     - Default
     - Description
   * - ``coll_acoll_use_dynamic_rules``
     - 0
     - Dynamically select algorithms for the broadcast collective. If set to (1), uses the hierarchical algorithm specified by the command line arguments ``coll_acoll_bcast_lin0``, ``coll_acoll_bcast_lin1``, ``coll_acoll_bcast_lin2``.
   * - ``coll_acoll_disable_shmbcast``
     - 0
     - If set to (1), disables shared-memory data copy based broadcast collective.
   * - ``coll_acoll_bcast_lin0``
     - 0
     - Choose binomial (0) or flat tree (1) based topology for the first stage of hierarchical broadcast collective.
   * - ``coll_acoll_bcast_lin1``
     - 0
     - Choose binomial (0) or flat tree (1) based topology for the second stage of hierarchical broadcast collective.
   * - ``coll_acoll_bcast_lin2``
     - 0
     - Choose binomial (0) or flat tree (1) based topology for the third stage of hierarchical broadcast collective.
   * - ``coll_acoll_bcast_nonsg``
     - 0
     - If set (1), uses 2 stages instead of 3 stages in hierarchical broadcast collective.
   * - ``coll_acoll_barrier_algo``
     - 0
     - Barrier algorithm selection for the intra-node case: shared-memory hierarchical algorithm (0), shared-memory flat algorithm (1), non-shared memory algorithm (any other value). This parameter is ignored for multinode cases.
   * - ``coll_acoll_alltoall_split_factor``
     - 0
     - Factor that specifies the amount of parallelism to go for in parallel-split alltoall algorithm. Set it to 0 (default) to use pre-configured value that is set based on communicator size, message size and mapping pattern; 2, 4, 8, 16, 32, 64 are supported values.
   * - ``coll_acoll_alltoall_psplit_msg_thres``
     - 0
     - Message size (bytes) threshold below which parallel-split alltoall is enabled. Default is 0 that uses a pre-configured value.
   * - ``coll_acoll_without_smsc``
     - 0
     - Disable Shared Memory Single Copy (SMSC) based algorithms when set to 1. This is applicable to allreduce and reduce collectives.
   * - ``coll_acoll_smsc_use_sr_buf``
     - 1
     - Use application send/recv buffers for SMSC registration instead of temporary buffers. This parameter is applicable when SMSC is enabled using ``coll_acoll_without_smsc``.
   * - ``coll_acoll_smsc_buffer_size``
     - 4MB for each rank
     - Maximum size (bytes) for temporary SMSC buffers (default: 4 MB). This parameter is applicable when SMSC is enabled and ``coll_acoll_smsc_use_sr_buf`` is set to 0.
   * - ``coll_acoll_reserve_memory_for_algo``
     - 1
     - If set to 0, disable allocation of reserved memory for use in ``acoll``.
   * - ``coll_acoll_reserve_memory_size_for_algo``
     - 4MB for each rank
     - Use to specify the amount of reserved memory to be allocated in ``acoll``.


Usage Examples
--------------

Basic Usage
~~~~~~~~~~~

Enable acoll with default settings:

.. code-block:: sh

   shell$ mpirun --mca coll acoll,tuned,libnbc,basic \
                 --mca coll_acoll_priority 40 ./my_app


Multi-Node Configuration
~~~~~~~~~~~~~~~~~~~~~~~~~

For multi-node runs, enable dynamic rules:

.. code-block:: sh

   shell$ mpirun --mca coll acoll,tuned,libnbc,basic \
                 --mca coll_acoll_priority 40 \
                 --mca coll_acoll_bcast_lin0 0 \
                 --mca coll_acoll_bcast_lin1 1 \
                 --mca coll_acoll_bcast_lin2 1 \
                 --mca coll_acoll_use_dynamic_rules 1 ./my_app

If ``coll_acoll_bcast_lin0``, ``coll_acoll_bcast_lin1``, and ``coll_acoll_bcast_lin2`` are not specified when ``coll_acoll_use_dynamic_rules`` is passed as 1, default value (0) will be used for each of these parameters.

Disabling Shared Memory Single Copy (SMSC)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If SMSC causes issues, disable it:

.. code-block:: sh

   shell$ mpirun --mca coll acoll,tuned,libnbc,basic \
                 --mca coll_acoll_priority 40 \
                 --mca coll_acoll_without_smsc 1 ./my_app

Alltoall Tuning
~~~~~~~~~~~~~~~

Enable parallel split algorithm for alltoall with split factor 4:

.. code-block:: sh

   shell$ mpirun --mca coll acoll,tuned,libnbc,basic \
                 --mca coll_acoll_priority 40 \
                 --mca coll_acoll_alltoall_split_factor 4 ./my_app
