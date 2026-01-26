Available Collective Components
===============================

Open MPI's ``coll`` framework provides a number of components
implementing collective communication, each of which target a
different environment or scenario.  Some of these components may not
be available depending on how Open MPI was compiled and what hardware
is available on the system.  A run-time decision based on each
component's self reported priority, selects which component will be
used.  These priorities may be adjusted on the command line or with
any of the other usual ways of setting MCA variables, giving us a way
to influence or override component selection.  In the end, which of
the available components is selected depends on a number of factors
such as the underlying hardware and the whether or not a specific
collective is provided by the component as not all components
implement all collectives.  However, there is always a fallback
``basic`` component that steps in and takes over when another component
fails to provide an implementation. 

The following provides a list of components and their primary target scenario:

 - ``han`` : component providing hierarchical algorithms.
 - ``libnbc``: component providing non-blocking collective operations
   based on a modified version of the libNBC library.
 - ``self``: component providing single-process collective algorithms.
 - ``tuned``: component providing fine grained mechanisms to switch
   between algorithms for each operation and message size. See :doc:`tuned` for
   more details.
 - ``ucc``: component using the `UCC library <https://github.com/openucx/ucc/>`_
   for collective operations.
 - ``xhc``: shared memory collective component using XPMEM for data transfers.
 - ``acoll``: collective component tuned for AMD Zen architectures. See :doc:`acoll` for
   more details.
 - ``accelerator``: component providing host-proxy algorithms for some
   collective operations using device buffers.
 - ``ftagree``: component providing fault-tolerant collective operations.
 - ``inter``: component providing collective operaitons for inter-communicators.
 - ``basic``: component providing basic algorithms, used as a fall-back component.
 - ``sync``: component used in scenarios where some nodes can be
   overrun with messages. This component can be used to insert
   synchronization points every *n-th* execution of a collective
   operations.
 - ``portals4``: component targetting portals4 networks.

Different component can and will be used for different collective
operations, since no component is providing implementations for all
operations defined in the MPI specification.

Displaying collective component selection
-----------------------------------------

Open MPI 6.0.x provides a mechanism to display which component has
been selected for a particular communication and communicator by setting the verbosity level
of the *coll_base_verbose* mca variable.

Specifically, setting *coll_base_verbose* to certain values will
influence which functions are precisely being displayed:

 - values between *1 - 19*: will print the selected component for
   blocking and non-blocking collectives assigned to MPI_COMM_WORLD,
   but not for persistent collective operations
 - value *20*: will print the selected component for all blocking and
   non-blocking collectives for all communicators, but not the
   persistent collectives
 - values larger than *20*: will print the selected component for all
   communicators and all collective operations

Example:

.. code-block:: sh

    shell$ mpiexec --mca coll_base_verbose 10 -n 4 ./<executable>
    ...
    coll:base:comm_select: communicator MPI_COMM_WORLD rank 1 allgather -> accelerator
    coll:base:comm_select: communicator MPI_COMM_WORLD rank 1 allgatherv -> tuned
    coll:base:comm_select: communicator MPI_COMM_WORLD rank 1 allreduce -> accelerator
    coll:base:comm_select: communicator MPI_COMM_WORLD rank 1 alltoall -> accelerator
    coll:base:comm_select: communicator MPI_COMM_WORLD rank 1 alltoallv -> tuned
    coll:base:comm_select: communicator MPI_COMM_WORLD rank 1 alltoallw -> basic
    coll:base:comm_select: communicator MPI_COMM_WORLD rank 1 barrier -> tuned
    coll:base:comm_select: communicator MPI_COMM_WORLD rank 1 bcast -> accelerator
    coll:base:comm_select: communicator MPI_COMM_WORLD rank 1 exscan -> accelerator
    coll:base:comm_select: communicator MPI_COMM_WORLD rank 1 gather -> tuned
    coll:base:comm_select: communicator MPI_COMM_WORLD rank 1 gatherv -> basic
    coll:base:comm_select: communicator MPI_COMM_WORLD rank 1 reduce -> accelerator
    coll:base:comm_select: communicator MPI_COMM_WORLD rank 1 reduce_scatter_block -> accelerator
    coll:base:comm_select: communicator MPI_COMM_WORLD rank 1 reduce_scatter -> accelerator
    coll:base:comm_select: communicator MPI_COMM_WORLD rank 1 scan -> accelerator
    coll:base:comm_select: communicator MPI_COMM_WORLD rank 1 scatter -> tuned
    coll:base:comm_select: communicator MPI_COMM_WORLD rank 1 scatterv -> basic
    coll:base:comm_select: communicator MPI_COMM_WORLD rank 1 neighbor_allgather -> basic
    coll:base:comm_select: communicator MPI_COMM_WORLD rank 1 neighbor_allgatherv -> basic
    coll:base:comm_select: communicator MPI_COMM_WORLD rank 1 neighbor_alltoall -> basic
    coll:base:comm_select: communicator MPI_COMM_WORLD rank 1 neighbor_alltoallv -> basic
    coll:base:comm_select: communicator MPI_COMM_WORLD rank 1 neighbor_alltoallw -> basic
    coll:base:comm_select: communicator MPI_COMM_WORLD rank 1 reduce_local -> accelerator
    coll:base:comm_select: communicator MPI_COMM_WORLD rank 1 iallgather -> libnbc
    coll:base:comm_select: communicator MPI_COMM_WORLD rank 1 iallgatherv -> libnbc
    coll:base:comm_select: communicator MPI_COMM_WORLD rank 1 iallreduce -> libnbc
    coll:base:comm_select: communicator MPI_COMM_WORLD rank 1 ialltoall -> libnbc
    coll:base:comm_select: communicator MPI_COMM_WORLD rank 1 ialltoallv -> libnbc
    coll:base:comm_select: communicator MPI_COMM_WORLD rank 1 ialltoallw -> libnbc

 
.. note:: While this output can provide valuable information, it might
   not always accurately reflect which component executes the
   operation, since some components have built-in logic to call the
   next component in the priority list if certain conditions are not
   met. For example, the `accelerator` collective component will use
   this mechanism to hand-off the execution of the operation to the
   next component in the priority list if the collective operation
   invoked does not use device buffers.
   
