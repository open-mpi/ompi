.. -*- rst -*-

   Copyright (c) 2022-2024 Nanook Consulting  All rights reserved.
   Copyright (c) 2023      Jeffrey M. Squyres.  All rights reserved.

   $COPYRIGHT$

   Additional copyrights may follow

   $HEADER$

.. The following line is included so that Sphinx won't complain
   about this file not being directly included in some toctree

Examples
========

Listed here are the subset of command line options that will be used
in the process mapping/ranking/binding examples below.

Specifying Host Nodes
---------------------

Use one of the following options to specify which hosts (nodes) within
the PRRTE DVM environment to run on.

.. code::

    --host <host1,host2,...,hostN>

    # or

    --host <host1:X,host2:Y,...,hostN:Z>

* List of hosts on which to invoke processes. After each hostname a
  colon (``:``) followed by a positive integer can be used to specify
  the number of slots on that host (``:X``, ``:Y``, and ``:Z``). The
  default is ``1``.

.. code::

    --hostfile <hostfile>

* Provide a hostfile to use.

Process Mapping / Ranking / Binding Options
-------------------------------------------

* ``-c #``, ``-n #``, ``--n #``, ``--np <#>``: Run this many copies of
  the program on the given nodes. This option indicates that the
  specified file is an executable program and not an application
  context. If no value is provided for the number of copies to execute
  (i.e., neither the ``-np`` nor its synonyms are provided on the
  command line), ``prun`` will automatically execute a copy of the
  program on each process slot (see below for description of a
  "process slot"). This feature, however, can only be used in the SPMD
  model and will return an error (without beginning execution of the
  application) otherwise.

  .. note:: These options specify the number of processes to launch.
            None of the options imply a particular binding policy
            |mdash| e.g., requesting ``N`` processes for each package
            does not imply that the processes will be bound to the
            package.

* ``--map-by <object>``: Map to the specified object. Supported
  objects include:

  * ``slot``
  * ``hwthread``
  * ``core`` (default)
  * ``l1cache``
  * ``l2cache``
  * ``l3cache``
  * ``numa``
  * ``package``
  * ``node``
  * ``seq``
  * ``ppr``
  * ``rankfile``
  * ``pe-list``

  Any object can include qualifiers by adding a colon (``:``) and any
  colon-delimited combination of one or more of the following to the
  ``--map-by`` options:

  * ``PE=n`` bind ``n`` processing elements to each process (can not
    be used in combination with rankfile or pe-list directives)

    .. error:: JMS Several of the options below refer to ``pe-list``.
               Is this option supposed to be ``PE-LIST=n``, not
               ``PE=n``?

  * ``SPAN`` load balance the processes across the allocation (cannot
    be used in combination with ``slot``, ``node``, ``seq``, ``ppr``,
    ``rankfile``, or ``pe-list`` directives)

  * ``OVERSUBSCRIBE`` allow more processes on a node than processing
    elements

  * ``NOOVERSUBSCRIBE`` means ``!OVERSUBSCRIBE``

  * ``NOLOCAL`` do not launch processes on the same node as ``prun``

  * ``HWTCPUS`` use hardware threads as CPU slots

  * ``CORECPUS`` use cores as CPU slots (default)

  * ``INHERIT`` indicates that a child job (i.e., one spawned from
    within an application) shall inherit the placement policies of the
    parent job that spawned it.

  * ``NOINHERIT`` means ``!INHERIT``

  * ``FILE=<path>`` (path to file containing sequential or rankfile
    entries).

  * ``ORDERED`` only applies to the PE-LIST option to indicate that
    procs are to be bound to each of the specified CPUs in the order
    in which they are assigned (i.e., the first proc on a node shall
    be bound to the first CPU in the list, the second proc shall be
    bound to the second CPU, etc.)

  ``ppr`` policy example: ``--map-by ppr:N:<object>`` will launch
  ``N`` times the number of objects of the specified type on each
  node.

  .. note:: Directives and qualifiers are case-insensitive and can be
            shortened to the minimum number of characters to uniquely
            identify them. Thus, ``L1CACHE`` can be given as
            ``l1cache`` or simply as ``L1``.

* ``--rank-by <object>``: This assigns ranks in round-robin fashion
  according to the specified object. The default follows the mapping
  pattern. Supported rank-by objects include:

  * ``slot``
  * ``node``
  * ``fill``
  * ``span``

  There are no qualifiers for the ``--rank-by`` directive.

* ``--bind-to <object>``: This binds processes to the specified
  object. See defaults in Quick Summary.  Supported bind-to objects
  include:

  * ``none``
  * ``hwthread``
  * ``core``
  * ``l1cache``
  * ``l2cache``
  * ``l3cache``
  * ``numa``
  * ``package``

  Any object can include qualifiers by adding a colon (``:``) and any
  colon-delimited combination of one or more of the following to the
  ``--bind-to`` options:

  * ``overload-allowed`` allows for binding more than one process in
    relation to a CPU

  * ``if-supported`` if binding to that object is supported on this
    system.

Specifying Host Nodes
---------------------

Host nodes can be identified on the command line with the ``--host``
option or in a hostfile.

For example, assuming no other resource manager or scheduler is
involved:

.. code::

    prun --host aa,aa,bb ./a.out

This launches two processes on node ``aa`` and one on ``bb``.

.. code::

    prun --host aa ./a.out

This launches one process on node ``aa``.

.. code::

    prun --host aa:5 ./a.out

This launches five processes on node ``aa``.

Or, consider the hostfile:

.. code::

    $ cat myhostfile
    aa slots=2
    bb slots=2
    cc slots=2

Here, we list both the host names (``aa``, ``bb``, and ``cc``) but
also how many "slots" there are for each. Slots indicate how many
processes can potentially execute on a node. For best performance, the
number of slots may be chosen to be the number of cores on the node or
the number of processor sockets.

If the hostfile does not provide slots information, the PRRTE DVM will
attempt to discover the number of cores (or hwthreads, if the
``:HWTCPUS`` qualifier to the ``--map-by`` option is set) and set the
number of slots to that value.

Examples using the hostfile above with and without the ``--host``
option:

.. code::

    prun --hostfile myhostfile ./a.out

This will launch two processes on each of the three nodes.

.. code::

    prun --hostfile myhostfile --host aa ./a.out

This will launch two processes, both on node ``aa``.

.. code::

    prun --hostfile myhostfile --host dd ./a.out

This will find no hosts to run on and abort with an error. That is, the
specified host ``dd`` is not in the specified hostfile.

When running under resource managers (e.g., SLURM, Torque, etc.), PRTE
will obtain both the hostnames and the number of slots directly from
the resource manger. The behavior of ``--host`` in that environment
will behave the same as if a hostfile was provided (since it is
provided by the resource manager).


Specifying Number of Processes
------------------------------

As we have just seen, the number of processes to run can be set using
the hostfile. Other mechanisms exist.

The number of processes launched can be specified as a multiple of the
number of nodes or processor sockets available. Consider the hostfile
below for the examples that follow.

.. code::

   $ cat myhostfile
   aa
   bb

For example:

.. code::

   prun --hostfile myhostfile --map-by ppr:2:package ./a.out

This launches processes 0-3 on node ``aa`` and process 4-7 on node
``bb``, where ``aa`` and ``bb`` are both dual-package nodes. The
``--map-by ppr:2:package`` option also turns on the ``--bind-to
package`` option, which is discussed in a later section.

.. code::

   prun --hostfile myhostfile --map-by ppr:2:node ./a.out

This launches processes 0-1 on node ``aa`` and processes 2-3 on node
``bb``.

.. code::

   prun --hostfile myhostfile --map-by ppr:1:node ./a.out

This launches one process per host node.

Another alternative is to specify the number of processes with the
``--np`` option. Consider now the hostfile:

.. code::

   $ cat myhostfile
   aa slots=4
   bb slots=4
   cc slots=4

With this hostfile:

.. code::

   prun --hostfile myhostfile --np 6 ./a.out

This will launch processes 0-3 on node ``aa`` and processes 4-5 on
node ``bb``.  The remaining slots in the hostfile will not be used
since the ``-np`` option indicated that only 6 processes should be
launched.


Mapping Processes to Nodes Using Policies
-----------------------------------------

The examples above illustrate the default mapping of process processes
to nodes. This mapping can also be controlled with various
``prun`` / ``prterun`` options that describe mapping policies.

.. code::

   $ cat myhostfile
   aa slots=4
   bb slots=4
   cc slots=4

Consider the hostfile above, with ``--np 6``:

.. list-table::
   :header-rows: 1

   * - Command
     - Ranks on ``aa``
     - Ranks on ``bb``
     - Ranks on ``cc``

   * - ``prun``
     - 0 1 2 3
     - 4 5
     -

   * - ``prun --map-by node``
     - 0 3
     - 1 4
     - 2 5

   * - ``prun --map-by node:NOLOCAL``
     -
     - 0 2 4
     - 1 3 5

The ``--map-by node`` option will load balance the processes across
the available nodes, numbering each process by node in a round-robin
fashion.

The ``:NOLOCAL`` qualifier to ``--map-by`` prevents any processes from
being mapped onto the local host (in this case node ``aa``). While
``prun`` typically consumes few system resources, the ``:NOLOCAL``
qualifier can be helpful for launching very large jobs where ``prun``
may actually need to use noticeable amounts of memory and/or
processing time.

Just as ``--np`` can specify fewer processes than there are slots, it
can also oversubscribe the slots. For example, with the same hostfile:

.. code::

   prun --hostfile myhostfile --np 14 ./a.out

This will produce an error since the default ``:NOOVERSUBSCRIBE``
qualifier to ``--map-by`` prevents oversubscription.

To oversubscribe the nodes you can use the ``:OVERSUBSCRIBE``
qualifier to ``--map-by``:

.. code::

   prun --hostfile myhostfile --np 14 --map-by :OVERSUBSCRIBE ./a.out

This will launch processes 0-5 on node ``aa``, 6-9 on ``bb``, and
10-13 on ``cc``.

Limits to oversubscription can also be specified in the hostfile
itself with the ``max_slots`` field:

.. code::

    $ cat myhostfile
    aa slots=4 max_slots=4
    bb         max_slots=8
    cc slots=4

The ``max_slots`` field specifies such a limit. When it does, the
``slots`` value defaults to the limit. Now:

.. code::

   prun --hostfile myhostfile --np 14 --map-by :OVERSUBSCRIBE ./a.out

This causes the first 12 processes to be launched as before, but the
remaining two processes will be forced onto node cc. The other two
nodes are protected by the hostfile against oversubscription by this
job.

Using the ``:NOOVERSUBSCRIBE`` qualifier to ``--map-by`` option can be
helpful since the PRTE DVM currently does not get ``max_slots`` values
from the resource manager.

Of course, ``--np`` can also be used with the ``--host`` option. For
example,

.. code::

   prun --host aa,bb --np 8 ./a.out

This will produce an error since the default ``:NOOVERSUBSCRIBE``
qualifier to ``--map-by`` prevents oversubscription.

.. code::

   prun --host aa,bb --np 8 --map-by :OVERSUBSCRIBE ./a.out

This launches 8 processes. Since only two hosts are specified, after
the first two processes are mapped, one to ``aa`` and one to ``bb``,
the remaining processes oversubscribe the specified hosts evenly.

.. code::

   prun --host aa:2,bb:6 --np 8 ./a.out

This launches 8 processes. Processes 0-1 on node ``aa`` since it has 2
slots and processes 2-7 on node ``bb`` since it has 6 slots.

And here is a MIMD example:

.. code::

   prun --host aa --np 1 hostname : --host bb,cc --np 2 uptime

This will launch process 0 running ``hostname`` on node ``aa`` and
processes 1 and 2 each running ``uptime`` on nodes ``bb`` and ``cc``,
respectively.
