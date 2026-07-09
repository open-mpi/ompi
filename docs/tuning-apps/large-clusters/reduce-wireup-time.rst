Reducing wireup time
====================

Open MPI's run-time uses an *out-of-band* (OOB) communication
subsystem to pass control messages during the launch, initialization,
and termination stages of a job. These messages allow ``mpirun`` to
tell its daemons which processes to launch, and allow the daemons in
turn to forward stdio to ``mpirun``, update ``mpirun`` on process
status, and so on.

.. note:: Since Open MPI 5.0, the run-time environment |mdash|
   including the OOB subsystem and the ``mpirun`` launcher |mdash| is
   provided by :ref:`PRRTE <label-running-role-of-pmix-and-prte>`, not
   by Open MPI itself.

The OOB uses TCP sockets, but the daemons do *not* all connect back to
``mpirun``. Instead, PRRTE arranges them in a *radix tree*: each daemon
connects only to its parent in that tree, and traffic bound for
``mpirun`` is relayed up the tree hop by hop. The fan-out of the tree
is set by the ``rml_base_radix`` PRTE MCA parameter, which defaults to
64 |mdash| so no matter how large the job is, only the first 64 daemons
connect directly to ``mpirun``, and each daemon below them accepts at
most 64 connections from its own children.

Similarly, when the ``ssh`` launcher is used, the daemons are also
*launched* through a tree: each daemon ``ssh``\ s the next level of
daemons into existence, rather than ``mpirun`` launching every daemon
itself. (This can be disabled by setting the ``plm_ssh_no_tree_spawn``
PRTE MCA parameter to 1, but there is rarely a reason to do so on a
large cluster.)

Together, these keep the connection and launch load on the node where
``mpirun`` resides bounded rather than growing with the job size.
``mpirun`` additionally services the connections it does receive on a
dedicated listener thread, so remote daemons get a prompt response.
This behavior is built in and requires no tuning.

PMIx "Instant On"
-----------------

A larger contributor to startup cost at scale is the exchange of
per-process communication endpoint information (the "modex") that MPI
processes historically performed during ``MPI_Init`` in order to wire
up their point-to-point connections. As the job size grows, this global
exchange grows with it.

Open MPI, PRRTE, and PMIx support the PMIx *Instant On* capability,
which moves this work out of ``MPI_Init``. Rather than having the
processes exchange endpoint information among themselves at run time,
the launcher (PRRTE) and the fabric/network software collect the
necessary network addressing information as part of the launch and
pre-position it in each process's PMIx data store before the process
starts. Each process can then look up any peer's endpoint information
locally, so no global wireup exchange is required |mdash| the processes
start already "wired up."

Instant On is provided cooperatively by PMIx, PRRTE, and the
network/fabric components. Whether |mdash| and how much of |mdash| it
is available therefore depends on the interconnect and its software
stack, rather than on a single user-settable MCA parameter. See the
:ref:`PMIx and PRRTE section <label-running-role-of-pmix-and-prte>` for
more information about these projects.

For applications with sparse communication patterns, you can also
reduce or eliminate the up-front modex with the
``pmix_base_async_modex`` MCA parameter, which defers endpoint lookups
until first message. See :doc:`reduce-startup-time` for details on that
and other launch-time options.
