.. -*- rst -*-

   Copyright (c) 2022-2024 Nanook Consulting  All rights reserved.
   Copyright (c) 2023      Jeffrey M. Squyres.  All rights reserved.

   $COPYRIGHT$

   Additional copyrights may follow

   $HEADER$

.. The following line is included so that Sphinx won't complain
   about this file not being directly included in some toctree

Diagnostics
===========

PRRTE provides various diagnostic reports that aid the user in
verifying and tuning the mapping/ranking/binding for a specific job.

The ``:REPORT`` qualifier to the ``--bind-to`` command line option can
be used to report process bindings.

As an example, consider a node with:

* 2 processor packages,
* 4 cores per package, and
* 8 hardware threads per core.

In each of the examples below the binding is reported in a human readable
format.

.. code::

   $ prun --np 4 --map-by core --bind-to core:REPORT ./a.out
   [node01:103137] MCW rank 0 bound to package[0][core:0]
   [node01:103137] MCW rank 1 bound to package[0][core:1]
   [node01:103137] MCW rank 2 bound to package[0][core:2]
   [node01:103137] MCW rank 3 bound to package[0][core:3]

In the example above, processes are bound to successive cores on the
first package.

.. code::

   $ prun --np 4 --map-by package --bind-to package:REPORT ./a.out
   [node01:103115] MCW rank 0 bound to package[0][core:0-9]
   [node01:103115] MCW rank 1 bound to package[1][core:10-19]
   [node01:103115] MCW rank 2 bound to package[0][core:0-9]
   [node01:103115] MCW rank 3 bound to package[1][core:10-19]

In the example above, processes are bound to all cores on successive
packages in a round-robin fashion.

.. code::

   $ prun --np 4 --map-by package:PE=2 --bind-to core:REPORT ./a.out
   [node01:103328] MCW rank 0 bound to package[0][core:0-1]
   [node01:103328] MCW rank 1 bound to package[1][core:10-11]
   [node01:103328] MCW rank 2 bound to package[0][core:2-3]
   [node01:103328] MCW rank 3 bound to package[1][core:12-13]

The example above shows us that 2 cores have been bound per process.
The ``:PE=2`` qualifier states that 2 CPUs underneath the package
(which would be cores in this case) are mapped to each process.

.. code::

   $ prun --np 4 --map-by core:PE=2:HWTCPUS --bind-to :REPORT  hostname
   [node01:103506] MCW rank 0 bound to package[0][hwt:0-1]
   [node01:103506] MCW rank 1 bound to package[0][hwt:8-9]
   [node01:103506] MCW rank 2 bound to package[0][hwt:16-17]
   [node01:103506] MCW rank 3 bound to package[0][hwt:24-25]

The example above shows us that 2 hardware threads have been bound per
process.  In this case ``prun`` is directing the DVM to map by
hardware threads since we used the ``:HWTCPUS`` qualifier. Without
that qualifier this command would return an error since by default the
DVM will not map to resources smaller than a core.  The ``:PE=2``
qualifier states that 2 processing elements underneath the core (which
would be hardware threads in this case) are mapped to each process.

.. code::

   $ prun --np 4 --bind-to none:REPORT  hostname
   [node01:107126] MCW rank 0 is not bound (or bound to all available processors)
   [node01:107126] MCW rank 1 is not bound (or bound to all available processors)
   [node01:107126] MCW rank 2 is not bound (or bound to all available processors)
   [node01:107126] MCW rank 3 is not bound (or bound to all available processors)

Binding is turned off in the above example, as reported.
