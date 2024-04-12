.. -*- rst -*-

   Copyright (c) 2022-2024 Nanook Consulting  All rights reserved.
   Copyright (c) 2023      Jeffrey M. Squyres.  All rights reserved.

   $COPYRIGHT$

   Additional copyrights may follow

   $HEADER$

.. The following line is included so that Sphinx won't complain
   about this file not being directly included in some toctree

Fundamentals
============

The mapping of processes to nodes can be defined not just with general
policies but also, if necessary, using arbitrary mappings that cannot
be described by a simple policy. Supported directives, given on the
command line via the ``--map-by`` option, include:

* ``SEQ``: (often accompanied by the ``file=<path>`` qualifier)
  assigns one process to each node specified in the file. The
  sequential file is to contain an entry for each desired process, one
  per line of the file.

* ``RANKFILE``: (often accompanied by the ``file=<path>`` qualifier)
  assigns one process to the node/resource specified in each entry of
  the file, one per line of the file.

For example, using the hostfile below:

.. code::

   $ cat myhostfile
   aa slots=4
   bb slots=4
   cc slots=4

The command below will launch three processes, one on each of nodes
``aa``, ``bb``, and ``cc``, respectively. The slot counts don't
matter; one process is launched per line on whatever node is listed on
the line.

.. code::

   $ prun --hostfile myhostfile --map-by seq ./a.out

Impact of the ranking option is best illustrated by considering the
following hostfile and test cases where each node contains two
packages (each package with two cores). Using the ``--map-by
ppr:2:package`` option, we map two processes onto each package and
utilize the ``--rank-by`` option as show below:

.. code::

   $ cat myhostfile
   aa
   bb

.. list-table::
   :header-rows: 1

   * - Command
     - Ranks on ``aa``
     - Ranks on ``bb``

   * - ``--rank-by core``
     - 0 1 ! 2 3
     - 4 5 ! 6 7

   * - ``--rank-by package``
     - 0 2 ! 1 3
     - 4 6 ! 5 7

   * - ``--rank-by package:SPAN``
     - 0 4 ! 1 5
     - 2 6 ! 3 7

Ranking by slot provides the identical result as ranking by core in
this case |mdash| a simple progression of ranks across each
node. Ranking by package does a round-robin ranking across packages
within each node until all processes have been assigned a rank, and
then progresses to the next node.  Adding the ``:SPAN`` qualifier to
the ranking directive causes the ranking algorithm to treat the entire
allocation as a single entity |mdash| thus, the process ranks are
assigned across all packages before circling back around to the
beginning.

The binding operation restricts the process to a subset of the CPU
resources on the node.

The processors to be used for binding can be identified in terms of
topological groupings |mdash| e.g., binding to an l3cache will bind
each process to all processors within the scope of a single L3 cache
within their assigned location. Thus, if a process is assigned by the
mapper to a certain package, then a ``--bind-to l3cache`` directive
will cause the process to be bound to the processors that share a
single L3 cache within that package.

To help balance loads, the binding directive uses a round-robin method,
binding a process to the first available specified object type within
the object where the process was mapped. For example, consider the case
where a job is mapped to the package level, and then bound to core. Each
package will have multiple cores, so if multiple processes are mapped to
a given package, the binding algorithm will assign each process located
to a package to a unique core in a round-robin manner.

Binding can only be done to the mapped object or to a resource located
within that object.

An object is considered completely consumed when the number of
processes bound to it equals the number of CPUs within it. Unbound
processes are not considered in this computation. Additional
processes cannot be mapped to consumed objects unless the
OVERLOAD qualifier is provided via the "--bind-to" command
line option.

Default process mapping/ranking/binding policies can also be set with MCA
parameters, overridden by the command line options when provided. MCA
parameters can be set on the ``prte`` command line when starting the
DVM (or in the ``prterun`` command line for a single-execution job), but
also in a system or user ``mca-params.conf`` file or as environment
variables, as described in the MCA section below. Some examples include:

.. list-table::
   :header-rows: 1

   * - ``prun`` option
     - MCA parameter key
     - Value

   * - ``--map-by core``
     - ``rmaps_default_mapping_policy``
     - ``core``

   * - ``--map-by package``
     - ``rmaps_default_mapping_policy``
     - ``package``

   * - ``--rank-by core``
     - ``rmaps_default_ranking_policy``
     - ``core``

   * - ``--bind-to core``
     - ``hwloc_default_binding_policy``
     - ``core```

   * - ``--bind-to package``
     - ``hwloc_default_binding_policy``
     - ``package``

   * - ``--bind-to none``
     - ``hwloc_default_binding_policy``
     - ``none``
