.. -*- rst -*-

   Copyright (c) 2022-2024 Nanook Consulting  All rights reserved.
   Copyright (c) 2023      Jeffrey M. Squyres.  All rights reserved.

   $COPYRIGHT$

   Additional copyrights may follow

   $HEADER$

.. The following line is included so that Sphinx won't complain
   about this file not being directly included in some toctree

Definition of 'slot'
====================

The term "slot" is used extensively in the rest of this documentation.
A slot is an allocation unit for a process.  The number of slots on a
node indicate how many processes can potentially execute on that node.
By default, PRRTE will allow one process per slot.

If PRRTE is not explicitly told how many slots are available on a node
(e.g., if a hostfile is used and the number of slots is not specified
for a given node), it will determine a maximum number of slots for
that node in one of two ways:

#. Default behavior: By default, PRRTE will attempt to discover the
   number of processor cores on the node, and use that as the number
   of slots available.

#. When ``--use-hwthread-cpus`` is used: If ``--use-hwthread-cpus`` is
   specified on the command line, then PRRTE will attempt to discover
   the number of hardware threads on the node, and use that as the
   number of slots available.

This default behavior also occurs when specifying the ``--host``
option with a single host.  Thus, the command:

.. code:: sh

   shell$ prun --host node1 ./a.out

launches a number of processes equal to the number of cores on node
``node1``, whereas:

.. code:: sh

   shell$ prun --host node1 --use-hwthread-cpus ./a.out

launches a number of processes equal to the number of hardware
threads on ``node1``.

When PRRTE applications are invoked in an environment managed by a
resource manager (e.g., inside of a Slurm job), and PRRTE was built
with appropriate support for that resource manager, then PRRTE will
be informed of the number of slots for each node by the resource
manager.  For example:

.. code:: sh

   shell$ prun ./a.out

launches one process for every slot (on every node) as dictated by
the resource manager job specification.

Also note that the one-process-per-slot restriction can be overridden
in unmanaged environments (e.g., when using hostfiles without a
resource manager) if oversubscription is enabled (by default, it is
disabled).  Most parallel applications and HPC environments do not
oversubscribe; for simplicity, the majority of this documentation
assumes that oversubscription is not enabled.

Slots are not hardware resources
--------------------------------

Slots are frequently incorrectly conflated with hardware resources.
It is important to realize that slots are an entirely different metric
than the number (and type) of hardware resources available.

Here are some examples that may help illustrate the difference:

#. More processor cores than slots: Consider a resource manager job
   environment that tells PRRTE that there is a single node with 20
   processor cores and 2 slots available.  By default, PRRTE will
   only let you run up to 2 processes.

   Meaning: you run out of slots long before you run out of processor
   cores.

#. More slots than processor cores: Consider a hostfile with a single
   node listed with a ``slots=50`` qualification.  The node has 20
   processor cores.  By default, PRRTE will let you run up to 50
   processes.

   Meaning: you can run many more processes than you have processor
   cores.
