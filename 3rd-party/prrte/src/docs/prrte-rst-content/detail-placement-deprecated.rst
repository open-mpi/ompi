.. -*- rst -*-

   Copyright (c) 2022-2024 Nanook Consulting  All rights reserved.
   Copyright (c) 2023 Jeffrey M. Squyres.  All rights reserved.

   $COPYRIGHT$

   Additional copyrights may follow

   $HEADER$

.. The following line is included so that Sphinx won't complain
   about this file not being directly included in some toctree

Deprecated options
==================

These deprecated options will be removed in a future release.

.. list-table::
   :header-rows: 1
   :widths: 20 20 30

   * - Deprecated Option
     - Replacement
     - Description

   * - ``--bind-to-core``
     - ``--bind-to core``
     - Bind processes to cores


   * - ``--bind-to-socket``
     - ``--bind-to package``
     - Bind processes to processor sockets

   * - ``--bycore``
     - ``--map-by core``
     - Map processes by core

   * - ``--bynode``
     - ``--map-by node``
     - Launch processes one per node, cycling by node in a round-robin
       fashion. This spreads processes evenly among nodes and assigns
       ranks in a round-robin, "by node" manner.

   * - ``--byslot``
     - ``--map-by slot``
     - Map and rank processes round-robin by slot

   * - ``--cpus-per-proc <#perproc>``
     - `--map-by <obj>:PE=<#perproc>``
     - Bind each process to the specified number of CPUs

   * - ``--cpus-per-rank <#perrank>``
     - ``--map-by <obj>:PE=<#perrank>``
     - Alias for ``--cpus-per-proc``

   * - ``--display-allocation``
     - ``--display ALLOC``
     - Display the detected resource allocation

   * - ``-display-devel-map``
     - ``--display MAP-DEVEL``
     - Display a detailed process map (mostly intended for developers)
       just before launch.

   * - ``--display-map``
     - ``--display MAP``
     - Display a table showing the mapped location of each process
       prior to launch.

   * - ``--display-topo``
     - ``--display TOPO``
     - Display the topology as part of the process map (mostly
       intended for developers) just before launch.

   * - ``--do-not-launch``
     - ``--map-by :DONOTLAUNCH``
     - Perform all necessary operations to prepare to launch the
       application, but do not actually launch it (usually used to
       test mapping patterns).

   * - ``--do-not-resolve``
     - ``--map-by :DONOTRESOLVE``
     - Do not attempt to resolve interfaces |mdash| usually used to
       determine proposed process placement/binding prior to obtaining
       an allocation.

   * - ``-N <num>``
     - ``--map-by prr:<num>:node``
     - Launch ``num`` processes per node on all allocated nodes

   * - ``--nolocal``
     - ``--map-by :NOLOCAL``
     - Do not run any copies of the launched application on the same
       node as ``prun`` is running. This option will override listing
       the ``localhost`` with ``--host`` or any other host-specifying
       mechanism.

   * - ``--nooversubscribe``
     - ``--map-by :NOOVERSUBSCRIBE``
     - Do not oversubscribe any nodes; error (without starting any
       processes) if the requested number of processes would cause
       oversubscription. This option implicitly sets "max_slots" equal
       to the "slots" value for each node. (Enabled by default).

   * - ``--npernode <#pernode>``
     - ``--map-by ppr:<#pernode>:node``
     - On each node, launch this many processes

   * - ``--npersocket <#persocket>``
     - ``--map-by ppr:<#perpackage>:package``
     - On each node, launch this many processes times the number of
       processor sockets on the node. The ``--npersocket`` option also
       turns on the ``--bind-to socket`` option. The term ``socket``
       has been globally replaced with ``package``.

   * - ``--oversubscribe``
     - ``--map-by :OVERSUBSCRIBE``
     - Nodes are allowed to be oversubscribed, even on a managed
       system, and overloading of processing elements.

   * - ``--pernode``
     - ``--map-by ppr:1:node``
     - On each node, launch one process

   * - ``--ppr``
     - `--map-by ppr:<list>``
     - Comma-separated list of number of processes on a given resource type
       [default: ``none``].

   * - ``--rankfile <FILENAME>``
     - ``--map-by rankfile:FILE=<FILENAME>``
     - Use a rankfile for mapping/ranking/binding

   * - ``--report-bindings``
     - ``--display BINDINGS``
     - Report any bindings for launched processes

   * - ``--tag-output``
     - ``--output TAG``
     - Tag all output with ``[job,rank]``

   * - ``--timestamp-output``
     - ``--output TIMESTAMP``
     - Timestamp all application process output

   * - ``--use-hwthread-cpus``
     - ``--map-by :HWTCPUS``
     - Use hardware threads as independent CPUs

   * - ``--xml``
     - ``--output XML``
     - Provide all output in XML format
