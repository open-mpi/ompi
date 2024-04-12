.. -*- rst -*-

   Copyright (c) 2022-2024 Nanook Consulting  All rights reserved.
   Copyright (c) 2023 Jeffrey M. Squyres.  All rights reserved.

   $COPYRIGHT$

   Additional copyrights may follow

   $HEADER$

.. The following line is included so that Sphinx won't complain
   about this file not being directly included in some toctree

Processes are mapped based on one of the following directives as
applied at the job level:

* ``SLOT`` assigns procs to each node up to the number of available
  slots on that node before moving to the next node in the
  allocation

* ``HWTHREAD`` assigns a proc to each hardware thread on a node in a
  round-robin manner up to the number of available slots on that
  node before moving to the next node in the allocation

* ``CORE`` (default) assigns a proc to each core on a node in a
  round-robin manner up to the number of available slots on that
  node before moving to the next node in the allocation

* ``L1CACHE`` assigns a proc to each L1 cache on a node in a
  round-robin manner up to the number of available slots on that
  node before moving to the next node in the allocation

* ``L2CACHE`` assigns a proc to each L2 cache on a node in a
  round-robin manner up to the number of available slots on that
  node before moving to the next node in the allocation

* ``L3CACHE`` assigns a proc to each L3 cache on a node in a
  round-robin manner up to the number of available slots on that
  node before moving to the next node in the allocation

* ``NUMA`` assigns a proc to each NUMA region on a node in a
  round-robin manner up to the number of available slots on that
  node before moving to the next node in the allocation

* ``PACKAGE`` assigns a proc to each package on a node in a
  round-robin manner up to the number of available slots on that
  node before moving to the next node in the allocation

* ``NODE`` assigns processes in a round-robin fashion to all nodes
  in the allocation, with the number assigned to each node capped
  by the number of available slots on that node

* ``SEQ`` (often accompanied by the file=<path> qualifier) assigns
  one process to each node specified in the file. The sequential
  file is to contain an entry for each desired process, one per
  line of the file.

* ``PPR:N``:resource maps N procs to each instance of the specified
  resource type in the allocation

* ``RANKFILE`` (often accompanied by the file=<path> qualifier) assigns
  one process to the node/resource specified in each entry of the
  file, one per line of the file.

* ``PE-LIST=a,b`` assigns procs to each node in the allocation based on
  the ORDERED qualifier. The list is comprised of comma-delimited
  ranges of CPUs to use for this job. If the ORDERED qualifier is not
  provided, then each node will be assigned procs up to the number of
  available slots, capped by the availability of the specified CPUs.
  If ORDERED is given, then one proc will be assigned to each of the
  specified CPUs, if available, capped by the number of slots on each
  node and the total number of specified processes. Providing the
  OVERLOAD qualifier to the "bind-to" option removes the check on
  availability of the CPU in both cases.

Any directive can include qualifiers by adding a colon (``:``) and any
combination of one or more of the following (delimited by colons) to
the ``--map-by`` option (except where noted):

* ``PE=n`` bind n CPUs to each process (can not be used in combination
  with rankfile or pe-list directives)

* ``SPAN`` load balance the processes across the allocation by treating
  the allocation as a single "super-node" (can not be used in
  combination with ``slot``, ``node``, ``seq``, ``ppr``, ``rankfile``, or
  ``pe-list`` directives)

* ``OVERSUBSCRIBE`` allow more processes on a node than processing elements

* ``NOOVERSUBSCRIBE`` means ``!OVERSUBSCRIBE``

* ``NOLOCAL`` do not launch processes on the same node as ``prun``

* ``HWTCPUS`` use hardware threads as CPU slots

* ``CORECPUS`` use cores as CPU slots (default)

* ``INHERIT`` indicates that a child job (i.e., one spawned from within
  an application) shall inherit the placement policies of the parent job
  that spawned it.

* ``NOINHERIT`` means ```!INHERIT``

* ``FILE=<path>`` (path to file containing sequential or rankfile entries).

* ``ORDERED`` only applies to the ``PE-LIST`` option to indicate that
  procs are to be bound to each of the specified CPUs in the order in
  which they are assigned (i.e., the first proc on a node shall be
  bound to the first CPU in the list, the second proc shall be bound
  to the second CPU, etc.)

.. note:: Directives and qualifiers are case-insensitive and can be
          shortened to the minimum number of characters to uniquely
          identify them. Thus, ``L1CACHE`` can be given as ``l1cache`` or
          simply as ``L1``.

The type of CPU (core vs hwthread) used in the mapping algorithm
is determined as follows:

* by user directive on the command line via the HWTCPUS qualifier to
  the ``--map-by`` directive

* by setting the ``rmaps_default_mapping_policy`` MCA parameter to
  include the ``HWTCPUS`` qualifier. This parameter sets the default
  value for a PRRTE DVM |mdash| qualifiers are carried across to DVM jobs
  started via ``prun`` unless overridden by the user's command line

* defaults to CORE in topologies where core CPUs are defined, and to
  hwthreads otherwise.

If your application uses threads, then you probably want to ensure that
you are either not bound at all (by specifying ``--bind-to none``), or
bound to multiple cores using an appropriate binding level or specific
number of processing elements per application process via the ``PE=#``
qualifier to the ``--map-by`` command line directive.

A more detailed description of the mapping, ranking, and binding
procedure can be obtained via the ``--help placement`` option.
