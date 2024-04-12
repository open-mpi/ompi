.. -*- rst -*-

   Copyright (c) 2022-2024 Nanook Consulting  All rights reserved.
   Copyright (c) 2023 Jeffrey M. Squyres.  All rights reserved.

   $COPYRIGHT$

   Additional copyrights may follow

   $HEADER$

.. The following line is included so that Sphinx won't complain
   about this file not being directly included in some toctree

Host syntax consists of a comma-delimited list of node names, each
entry optionally containing a ``:N`` extension indicating the number
of slots to assign to that entry:

.. code::

   --host node01:5,node02

In the absence of the slot extension, one slot will be assigned to the
node. Duplicate entries are aggregated and the number of slots
assigned to that node are summed together.

.. note:: A "slot" is the PRRTE term for an allocatable unit where we
   can launch a process. Thus, the number of slots equates to the
   maximum number of processes PRRTE may start on that node without
   oversubscribing it.
