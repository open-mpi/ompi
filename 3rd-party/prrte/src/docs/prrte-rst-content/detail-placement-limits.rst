.. -*- rst -*-

   Copyright (c) 2022-2024 Nanook Consulting  All rights reserved.
   Copyright (c) 2023      Jeffrey M. Squyres.  All rights reserved.

   $COPYRIGHT$

   Additional copyrights may follow

   $HEADER$

.. The following line is included so that Sphinx won't complain
   about this file not being directly included in some toctree

Overloading and Oversubscribing
===============================

This section explores the difference between the terms "overloading"
and "oversubscribing". Users are often confused by the difference
between these two scenarios. As such, this section provides a number
of scenarios to help illustrate the differences.

* ``--map-by :OVERSUBSCRIBE`` allow more processes on a node than
  allocated

* ``--bind-to <object>:overload-allowed`` allows for binding more than
  one process in relation to a CPU

The important thing to remember with *oversubscribing* is that it can
be defined separately from the actual number of CPUs on a node. This
allows the mapper to place more or fewer processes per node than
CPUs. By default, PRRTE uses cores to determine slots in the absence
of such information provided in the hostfile or by the resource
manager (except in the case of the ``--host`` as described in
the section on that command line option.

The important thing to remember with *overloading* is that it is
defined as binding more processes than CPUs. By default, PRRTE uses
cores as a means of counting the number of CPUs. However, the user can
adjust this. For example when using the ``:HWTCPUS`` qualifier to the
``--map-by`` option PRRTE will use hardware threads as a means of
counting the number of CPUs.

For the following examples consider a node with:

* 2 processor packages,
* 10 cores per package, and
* 8 hardware threads per core.

Consider the node from above with the hostfile below:

.. code::

   $ cat myhostfile
   node01 slots=32
   node02 slots=32

The ``slots`` token tells PRRTE that it can place up to 32 processes
before *oversubscribing* the node.

If we run the following:

.. code::

   prun --np 34 --hostfile myhostfile --map-by core --bind-to core hostname

It will return an error at the binding time indicating an
*overloading* scenario.

The mapping mechanism assigns 32 processes to ``node01`` matching the
``slots`` specification in the hostfile. The binding mechanism will bind
the first 20 processes to unique cores leaving it with 12 processes
that it cannot bind without overloading one of the cores (putting more
than one process on the core).

Using the ``overload-allowed`` qualifier to the ``--bind-to core``
option tells PRRTE that it may assign more than one process to a core.

If we run the following:

.. code::

   prun --np 34 --hostfile myhostfile --map-by core --bind-to core:overload-allowed hostname

This will run correctly placing 32 processes on ``node01``, and 2
processes on ``node02``. On ``node01`` two processes are bound to
cores 0-11 accounting for the overloading of those cores.

Alternatively, we could use hardware threads to give binding a lower
level CPU to bind to without overloading.

If we run the following:

.. code::

   prun --np 34 --hostfile myhostfile --map-by core:HWTCPUS --bind-to hwthread hostname

This will run correctly placing 32 processes on ``node01``, and 2
processes on ``node02``. On ``node01`` two processes are mapped to
cores 0-11 but bound to different hardware threads on those cores (the
logical first and second hardware thread). Thus no hardware threads
are overloaded at binding time.

In both of the examples above the node is not oversubscribed at
mapping time because the hostfile set the oversubscription limit to
``slots=32`` for each node. It is only after we exceed that limit that
PRRTE will throw an oversubscription error.

Consider next if we ran the following:

.. code::

   prun --np 66 --hostfile myhostfile --map-by core:HWTCPUS --bind-to hwthread hostname

This will return an error at mapping time indicating an
oversubscription scenario. The mapping mechanism will assign all of
the available slots (64 across 2 nodes) and be left two processes to
map. The only way to map those processes is to exceed the number of
available slots putting the job into an oversubscription scenario.

You can force PRRTE to oversubscribe the nodes by using the
``:OVERSUBSCRIBE`` qualifier to the ``--map-by`` option as seen in the
example below:

.. code::

   prun --np 66 --hostfile myhostfile \
       --map-by core:HWTCPUS:OVERSUBSCRIBE --bind-to hwthread hostname

This will run correctly placing 34 processes on ``node01`` and 32 on
``node02``.  Each process is bound to a unique hardware thread.

Overloading vs. Oversubscription: Package Example
-------------------------------------------------

Let's extend these examples by considering the package level.
Consider the same node as before, but with the hostfile below:

.. code::

   $ cat myhostfile
   node01 slots=22
   node02 slots=22

The lowest level CPUs are "cores" and we have 20 total (10 per
package).

If we run:

.. code::

   prun --np 20 --hostfile myhostfile --map-by package \
       --bind-to package:REPORT hostname

Then 10 processes are mapped to each package, and bound at the package
level.  This is not overloading since we have 10 CPUs (cores)
available in the package at the hardware level.

However, if we run:

.. code::

   prun --np 21 --hostfile myhostfile --map-by package \
       --bind-to package:REPORT hostname

Then 11 processes are mapped to the first package and 10 to the second
package.  At binding time we have an overloading scenario because
there are only 10 CPUs (cores) available in the package at the
hardware level. So the first package is overloaded.

Overloading vs. Oversubscription: Hardware Threads Example
----------------------------------------------------------

Similarly, if we consider hardware threads.

Consider the same node as before, but with the hostfile below:

.. code::

   $ cat myhostfile
   node01 slots=165
   node02 slots=165

The lowest level CPUs are "hwthreads" (because we are going to use the
``:HWTCPUS`` qualifier) and we have 160 total (80 per package).

If we re-run (from the package example) and add the ``:HWTCPUS``
qualifier:

.. code::

   prun --np 21 --hostfile myhostfile --map-by package:HWTCPUS \
       --bind-to package:REPORT hostname

Without the ``:HWTCPUS`` qualifier this would be overloading (as we
saw previously). The mapper places 11 processes on the first package
and 10 to the second package. The processes are still bound to the
package level. However, with the ``:HWTCPUS`` qualifier, it is not
overloading since we have 80 CPUs (hwthreads) available in the package
at the hardware level.

Alternatively, if we run:

.. code::

   prun --np 161 --hostfile myhostfile --map-by package:HWTCPUS \
       --bind-to package:REPORT hostname

Then 81 processes are mapped to the first package and 80 to the second
package.  At binding time we have an overloading scenario because
there are only 80 CPUs (hwthreads) available in the package at the
hardware level.  So the first package is overloaded.
