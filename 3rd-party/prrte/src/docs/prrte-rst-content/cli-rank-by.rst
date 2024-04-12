.. -*- rst -*-

   Copyright (c) 2022-2024 Nanook Consulting  All rights reserved.
   Copyright (c) 2023 Jeffrey M. Squyres.  All rights reserved.

   $COPYRIGHT$

   Additional copyrights may follow

   $HEADER$

.. The following line is included so that Sphinx won't complain
   about this file not being directly included in some toctree

PRRTE automatically ranks processes for each job starting from zero.
Regardless of the algorithm used, rank assignments span applications
in the same job |mdash| i.e., a command line of

.. code::

  -n 3 app1 : -n 2 app2

will result in ``app1`` having three processes ranked 0-2 and ``app2``
having two processes ranked 3-4.

By default, process ranks are assigned in accordance with the mapping
directive |mdash| e.g., jobs that are mapped by-node will have the process
ranks assigned round-robin on a per-node basis. However, users can override
the default by specifying any of the following directives using the
``--rank-by`` command line option:

* ``SLOT`` assigns ranks to each process on a node in the order in
  which the mapper assigned them. This is the default behavior,
  but is provided as an explicit option to allow users to override
  any alternative default specified in the environment. When mapping
  to a specific resource type, procs assigned to a given instance
  of that resource on a node will be ranked on a per-resource basis
  on that node before moving to the next node.

* ``NODE`` assigns ranks round-robin on a per-node basis

* ``FILL`` assigns ranks to procs mapped to a particular resource type
  on each node, filling all ranks on that resource before moving to
  the next resource on that node. For example, procs mapped by
  ``L1cache`` would have all procs on the first ``L1cache`` ranked
  sequentially before moving to the second ``L1cache`` on the
  node. Once all procs on the node have been ranked, ranking would
  continue on the next node.

* ``SPAN`` assigns ranks round-robin to procs mapped to a particular
  resource type, treating the collection of resource instances
  spanning the entire allocation as a single "super node" before
  looping around for the next pass. Thus, ranking would begin with the
  first proc on the first ``L1cache`` on the first node, then the next
  rank would be assigned to the first proc on the second ``L1cache``
  on that node, proceeding across until the first proc had been ranked
  on all ``L1cache`` used by the job before circling around to rank
  the second proc on each object.

The ``rank-by`` command line option has no qualifiers.

.. note:: Directives are case-insensitive.  ``SPAN`` is the same as
          ``span``.

A more detailed description of the mapping, ranking, and binding
procedure can be obtained via the ``--help placement`` option.
