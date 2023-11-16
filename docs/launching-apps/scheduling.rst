Scheduling processes across hosts
=================================

Open MPI provides many options for scheduling application processes across
hosts, including oversubscribing processes to processors.  This section
describes how to define that mapping.

Scheduling overview
-------------------

If you are not oversubscribing your hosts
(i.e., trying to run more processes than slots available on that
host), scheduling is pretty simple and occurs either on a by-slot or
by-node round robin schedule.  If you're oversubscribing, the issue
gets much more complicated |mdash| keep reading.

The more complete answer is: Open MPI schedules processes to nodes by
asking two questions from each application on the ``mpirun`` command
line:

#. *How many* processes should be launched?
#. *Where* should those processes be launched?

The "how many" question is directly answered with the ``-n`` switch
to ``mpirun``.  If ``-n`` is not specified on the ``mpirun`` command
line, its value is the sum of the slots on all the nodes.

The "where" question is a little more complicated, and depends on
three factors:

#. The final node list (e.g., after ``-hostname`` / ``--host``
   exclusionary or inclusionary processing)
#. The scheduling policy (which applies to all applications in a
   single job)
#. The default and maximum number of slots on each host

Open MPI currently supports two scheduling policies: by slot and by
node:

#. *By slot:* This is the default scheduling policy, but can also be
   explicitly requested by using either the ``--map-by slot`` option
   to ``mpirun`` or by setting the MCA parameter
   ``rmaps_default_mapping_policy`` to the string ``slot``.

   In this mode, Open MPI will schedule processes on a node until all
   of its default slots are exhausted before proceeding to the next
   node.  In MPI terms, this means that Open MPI tries to maximize the
   number of adjacent ranks in ``MPI_COMM_WORLD`` on the same host
   without oversubscribing that host.

   For example:

   .. code-block::

      shell$ cat my-hosts
      node0 slots=2 max_slots=20
      node1 slots=2 max_slots=20
      shell$ mpirun --hostfile my-hosts -n 8 --map-by slot hello | sort
      Hello World I am rank 0 of 8 running on node0
      Hello World I am rank 1 of 8 running on node0
      Hello World I am rank 2 of 8 running on node1
      Hello World I am rank 3 of 8 running on node1
      Hello World I am rank 4 of 8 running on node0
      Hello World I am rank 5 of 8 running on node0
      Hello World I am rank 6 of 8 running on node1
      Hello World I am rank 7 of 8 running on node1

#. *By node:* This policy can be requested either by using the
   ``--map-by node`` option to ``mpirun`` or by setting the MCA parameter
   ``rmaps_default_mapping_policy`` to the string "node".

   In this mode, Open MPI will schedule a single process on each node
   in a round-robin fashion (looping back to the beginning of the node
   list as necessary) until all processes have been scheduled.  Nodes
   are skipped once their default slot counts are exhausted.

   For example:

   .. code-block::

      shell$ cat my-hosts
      node0 slots=2 max_slots=20
      node1 slots=2 max_slots=20
      shell$ mpirun --hostname my-hosts -n 8 --map-by node hello | sort
      Hello World I am rank 0 of 8 running on node0
      Hello World I am rank 1 of 8 running on node1
      Hello World I am rank 2 of 8 running on node0
      Hello World I am rank 3 of 8 running on node1
      Hello World I am rank 4 of 8 running on node0
      Hello World I am rank 5 of 8 running on node1
      Hello World I am rank 6 of 8 running on node0
      Hello World I am rank 7 of 8 running on node1

In both policies, if the default slot count is exhausted on all nodes
while there are still processes to be scheduled, Open MPI will trigger
an oversubscription condition.

If ``:OVERSUBSCRIBE`` is added as a modifier to the ``--map-by``
option (e.g., ``mpirun --map-by node:OVERSUBSCRIBE ...`` |mdash| :ref:`see
this section <running-mpi-apps-oversubscribing-label>` for more
details), Open MPI will continue to loop through the list of nodes
again and try to schedule one more process to each node until all
processes are scheduled.  Nodes are skipped in this process if their
maximum slot count is exhausted.  If the maximum slot count is
exhausted on all nodes while there are still processes to be
scheduled, Open MPI will abort without launching any processes.

If ``:OVERSUBSCRIBE`` is *not* specified and an oversubscription
condition occurs, Open MPI will abort without launching any processes.

.. _running-scheduling-hostfile-option-label:

Scheduling with the --hostfile option
-------------------------------------

The ``--hostfile`` option to ``mpirun`` takes a filename that lists
hosts on which to launch MPI processes.

.. important:: The hosts listed in a hostfile have *nothing* to do
               with which network interfaces are used for MPI
               communication.  They are *only* used to specify on
               which hosts to launch MPI processes.

Hostfiles are simple text files with hosts specified, one per line.
Each host can also specify a default and maximum number of *slots* to
be used on that host (i.e., the maximum number of processes that will
be launched on that node).  Comments are also supported, and blank
lines are ignored.  For example:

.. code-block::

   # This is an example hostfile.  Comments begin with #.
   #
   # Since no slots are specified, the number of slots defaults to the
   # number of processor cores available on the machine.
   foo.example.com

   # We want to allow launching a maximum of 2 processes on this host
   # (e.g., potentially because it has two processor cores):
   bar.example.com slots=2

Slots are discussed in much more detail :ref:`in this section
<running-scheduling-slots-label>`.

Hostfiles works in two different ways:

#. *Exclusionary:* If a list of hosts to run on has been provided by
   another source (e.g., by a hostfile or a batch scheduler such as
   Slurm, PBS/Torque, SGE, etc.), the hosts provided by the hostfile
   must be in the already-provided host list.  If the
   hostfile-specified nodes are *not* in the already-provided host
   list, ``mpirun`` will abort without launching anything.

   In this case, hostfiles act like an exclusionary filter |mdash|
   they limit the scope of where processes will be scheduled from the
   original list of hosts to produce a final list of hosts.

   For example, say that a scheduler job contains hosts ``node01``
   through ``node04``.  If you run:

   .. code-block::

      shell$ cat my_hosts
      node03
      shell$ mpirun -n 1 --hostfile my_hosts hostname

   This will run a single copy of ``hostname`` on the host ``node03``.

   However, presuming your job was allocated only to ``node03`` and
   you run the following:

   .. code-block::

      shell$ cat my_hosts
      node17
      shell$ mpirun -n 1 --hostfile my_hosts hostname

   This is an error (because ``node17`` is not allocated to your job),
   and ``mpirun`` will abort.

   Finally, note that in exclusionary mode, processes will *only* be
   executed on the hostfile-specified hosts, If this ends up causing
   an oversubscription situation, ``mpirun`` will abort by default.

#. *Inclusionary:* If a list of hosts has *not* been provided by
   another source, then the hosts provided by the ``--hostfile``
   option will be used as the original and final host list.

   In this case, ``--hostfile`` acts as an inclusionary agent; all
   ``--hostfile``-supplied hosts become available for scheduling
   processes.  For example (assume that you are *not* in a scheduling
   environment where a list of nodes is being transparently supplied):

   .. code-block::

      shell$ cat my_hosts
      node01.example.com slots=1
      node02.example.com slots=1
      node03.example.com slots=1
      shell$ mpirun -n 3 --hostfile my_hosts hostname

   This will launch a single copy of ``hostname`` on the hosts
   ``node01.example.com``, ``node02.example.com``, and
   ``node03.example.com``.

Note, too, that ``--hostfile`` is essentially a per-application switch.
Hence, if you specify multiple applications (as in an MPMD job),
``--hostfile`` can be specified multiple times:

.. code-block::

   shell$ cat hostfile_1
   node01.example.com
   shell$ cat hostfile_2
   node02.example.com
   shell$ mpirun -n 1 --hostfile hostfile_1 hostname : -n 1 --hostfile hostfile_2 uptime
   node01.example.com
    06:11:45 up 1 day,  2:32,  0 users,  load average: 21.65, 20.85, 19.84

Notice that ``hostname`` was launched on ``node01.example.com`` and
``uptime`` was launched on ``node02.example.com``.

.. _running-scheduling-host-option-label:

Scheduling with the --host option
---------------------------------

The ``--host`` option to ``mpirun`` takes a comma-delimited list of
hosts on which to run.  For example:

.. code-block::

   shell$ mpirun -n 3 --host a,b,c hostname

Will launch *one* copy of ``hostname`` on each of hosts ``a``, ``b``,
and ``c``.  Specifically: each host defaults to 1 slot, unless
specified by the ``:N`` suffix.  For example:

.. code-block::

   shell$ mpirun --host a,b:2,c:3 hostname

Will launch one copy of ``hostname`` on ``a``, two copies of
``hostname`` on ``b``, and three copies of ``hostname`` and ``c``.

Slots are discussed in much more detail :ref:`in this section
<running-scheduling-slots-label>`.

.. important:: The hosts specified by the ``--host`` option have
               *nothing* to do with which network interfaces are used
               for MPI communication.  They are *only* used to specify
               on which hosts to launch MPI processes.

``--host`` works in two different ways:

#. *Exclusionary:* If a list of hosts to run on has been provided by
   another source (e.g., by a hostfile or a batch scheduler such as
   Slurm, PBS/Torque, SGE, etc.), the hosts provided by the ``--host``
   option must be in the already-provided host list.  If the
   ``--host``-specified nodes are *not* in the already-provided host
   list, ``mpirun`` will abort without launching anything.

   In this case, the ``--host`` option acts like an exclusionary
   filter |mdash| it limits the scope of where processes will be
   scheduled from the original list of hosts to produce a final list
   of hosts.

   For example, say that the hostfile ``my_hosts`` contains the hosts
   ``node1`` through ``node4``.  If you run:

   .. code-block::

      shell$ mpirun -n 1 --hostfile my_hosts --host node3 hostname

   This will run a single copy of ``hostname`` on the host ``node3``.
   However, if you run:

   .. code-block::

      shell$ mpirun -n 1 --hostfile my_hosts --host node17 hostname

   This is an error (because ``node17`` is not listed in
   ``my_hosts``); ``mpirun`` will abort.

   Finally, note that in exclusionary mode, processes will *only* be
   executed on the ``--host``-specified hosts.  If this ends up
   causing an oversubscription situation, ``mpirun`` will abort by
   default.

#. *Inclusionary:* If a list of hosts has *not* been provided by
   another source, then the hosts provided by the ``--host`` option
   will be used as the original and final host list.

   In this case, ``--host`` acts as an inclusionary agent; all
   ``--host``-supplied hosts become available for scheduling
   processes.  For example (assume that you are *not* in a scheduling
   environment where a list of nodes is being transparently supplied):

   .. code-block::

      shell$ mpirun -n 3 --host a,b,c hostname

   This will launch a single copy of ``hostname`` on the hosts ``a``,
   ``b``, and ``c``.

Note, too, that ``--host`` is essentially a per-application switch.
Hence, if you specify multiple applications (as in an MPMD job),
``--host`` can be specified multiple times:

.. code-block::

   shell$ mpirun -n 1 --host a hostname : -n 1 --host b uptime

This will launch ``hostname`` on host ``a`` and ``uptime`` on host ``b``.

.. _running-scheduling-slots-label:

Process Slots
-------------

*Slots* are Open MPI's representation of how many processes can be
launched on a given host.

Open MPI maintains the number of slots for each host in a given
parallel job, and |mdash| by default |mdash| will not let you launch
more processes on a host than it has slots.

.. important:: It is common to set the number of slots on a host to be
               less than or equal to the number of processor cores on
               that host.

               **But it is important to realize that Open MPI's concept
               of slots is actually unrelated to the number of
               physical processor cores on a host.**

               Specifically: the number of slots on a host can be less
               than, equal to, or more than the number of processor
               cores on a host.

If you wish to run more processes on a host than it has slots,
:ref:`see the section on oversubscription
<running-mpi-apps-oversubscribing-label>`.

Calculating the number of slots
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The number of slots on a host depends on a few factors:

#. If the host is specified by a job scheduler (e.g., Slurm,
   PBS/Torque, etc.), the job scheduler specifies the number of slots
   for that host.

#. If the host is specified in a hostfile:

   #. If the ``slots`` parameter is specified, that value is used for
      the number of slots on that host.
   #. Otherwise:

      #. If ``--map-by :HWTCPUS`` was specified, the number of slots
         defaults to the number of hardware threads on that host.
      #. Otherwise, the number of slots defaults to the number of
         processor cores on that host.

#. If the host is specified via the ``--host`` command line option:

   #. If the ``:N`` suffix is specified, ``N`` is used for the number
      of slots on that host.
   #. Otherwise, the number of slots defaults to 1.
   #. If the same host name is specified multiple times, the slots
      value for that host is increased by ``N`` if ``:N`` is
      specified, or increased by 1 if ``:N`` is not specified.

.. caution:: The exact scheme used to determine the number of slots
             has varied between different major versions of Open MPI.
             The scheme described above is relevant for Open MPI
             |ompi_series|.

Max slot counts, however, are rarely specified by schedulers.  The max
slot count for each node will default to "infinite" if it is not
provided (meaning that Open MPI will oversubscribe the node if you ask
it to |mdash| see more on oversubscribing in :ref:`this section
<running-mpi-apps-oversubscribing-label>`).

Here are some examples, all from unscheduled environments:

#. Use a hostfile and specify the ``slots`` parameter.

   .. code-block:: sh

      shell$ cat my-hostfile
      node01.example.come slots=4
      shell$ mpirun --hostfile my-hostfile hostname
      node01
      node01
      node01
      node01

   This launched 4 processes because ``slots=4`` was specified in the
   hostfile.

#. Use a hostfile and do *not* specify the ``slots`` parameter (assume
   that ``node01.example.com`` has 2 processor cores):

   .. code-block:: sh

      shell$ cat my-hostfile
      node01.example.come
      shell$ mpirun --hostfile my-hostfile hostname
      node01
      node01

   This launched 2 processes because ``slots`` was not specified, and
   ``node02`` has 2 processor cores.

#. Use ``--host``:

   .. code-block:: sh

      shell$ mpirun --host node01.example.com hostname
      node01

   This launched 1 processes because ``--host`` with no ``:N`` suffix
   increments the slot count for that host by 1.

#. Use ``--host`` with a ``:N`` suffix:

   .. code-block:: sh

      shell$ mpirun --host node01.example.com:2 hostname
      node01
      node01

   This launched 2 processes because ``:2`` was specified on the
   command line.

#. Use ``--host`` with a ``:N`` suffix, and mention the host multiple times:

   .. code-block:: sh

      shell$ mpirun --host node01.example.com:2,node01.example.com hostname
      node01
      node01
      node01

   This launched 3 processes because ``:2`` was specified on the
   command line, and then ``node01.example.com`` was specified an
   additional time, incrementing the slot count for that host to 3.

..  _running-mpi-apps-oversubscribing-label:

Oversubscribing nodes
---------------------

Running more MPI processes than processors are available on a node is called
*oversubscribing* the node.   Open MPI can oversubscribe nodes,
but it very much matters *how* you do it.

Specifically: it is critical that Open MPI *knows* that you are
oversubscribing the node, or **severe** performance degradation can
result.

.. important:: Here is a good general rule to follow: **never specify
               a number of slots that is more than the available
               number of processors.**

For example, if you want to run 4 processes on a host with 2 processor
cores, then indicate that you only have 2 slots but want to run 4
processes.  For example:

.. code-block:: sh

   # In a hostfile, the number of slots will default to the number of
   # processor cores on the host
   shell$ cat my-hostfile
   localhost
   shell$ mpirun -n 4 --hostfile my-hostfile a.out

Specifically: we strongly suggest that you do **NOT** have a hostfile
that contains ``slots=4`` (because there are only two available
processor cores).

That being said, the above command will fail, because you are trying
to run 4 processes but there are only 2 slots available.  You must
specifically tell Open MPI that it is ok to oversubscribe via
``--map-by :OVERSUBSCRIBE``:

.. code-block:: sh

   shell$ cat my-hostfile
   # For the purposes of this example, explicitly tell Open MPI
   # that we have 2 slots on the host.
   localhost slots=2
   shell$ mpirun -n 4 --hostfile my-hostfile --map-by :OVERSUBSCRIBE a.out

The reason you should tell Open MPI whether you're oversubscribing or
not (i.e., never specify a ``slots`` value more than the number of
processor cores available) is because Open MPI basically runs its
message passing progression engine in two modes: *aggressive* and
*degraded*.

#. *Degraded:* When Open MPI thinks that it is in an oversubscribed
   mode (i.e., more processes are running than there are processor
   cores available), MPI processes will automatically run in
   *degraded* mode and frequently yield the processor to its peers,
   thereby allowing all processes to make progress.

   .. note:: Be sure to see :ref:`this FAQ entry
             <tuning-using-paffinity-label>` that describes how
             degraded mode affects processor and memory
             affinity.

#. *Aggressive:* When Open MPI thinks that it is in an exactly- or
   under-subscribed mode (i.e., the number of running processes is
   equal to or less than the number of available processor cores), MPI
   processes will automatically run in *aggressive* mode, meaning that
   they will never voluntarily give up the processor to other
   processes.  With some network transports, this means that Open MPI
   will spin in tight loops attempting to make message passing
   progress, effectively causing other processes to not get any CPU
   cycles (and therefore never make any progress).

For example, on a node with a two processor cores:

.. code-block::

   shell$ cat my-hostfile
   localhost slots=4
   shell$ mpirun -n 4 --hostfile my-hostfile a.out

This would cause all 4 MPI processes to run in *aggressive* mode
because Open MPI thinks that there are 4 available processor cores to
use.  This is actually a lie (there are only 2 processor core |mdash|
not 4), and can cause extremely bad performance.

Forcing aggressive or degraded performance mode
-----------------------------------------------

The MCA parameter ``mpi_yield_when_idle`` controls whether an MPI
process runs in Aggressive or Degraded performance mode.  Setting it
to 0 forces Aggressive mode; setting it to 1 forces Degraded mode (see
:ref:`this FAQ entry <label-running-setting-mca-param-values>` to see how
to set MCA parameters).

Note that this value *only* affects the behavior of MPI processes when
they are blocking in MPI library calls.  It does not affect behavior
of non-MPI processes, nor does it affect the behavior of a process
that is not inside an MPI library call.

Open MPI normally sets this parameter automatically and
users are cautioned against setting this parameter unless you are
really, absolutely, positively sure of what you are doing.
