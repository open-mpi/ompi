.. -*- rst -*-

   Copyright (c) 2022-2024 Nanook Consulting  All rights reserved.
   Copyright (c) 2023      Jeffrey M. Squyres.  All rights reserved.

   $COPYRIGHT$

   Additional copyrights may follow

   $HEADER$

.. The following line is included so that Sphinx won't complain
   about this file not being directly included in some toctree

Overview
========

PRRTE provides a set of three controls for assigning process
locations and ranks:

#. Mapping: Assigns a default location to each process
#. Ranking: Assigns a unique integer rank value to each process
#. Binding: Constrains each process to run on specific processors

This section provides an overview of these three controls.  Unless
otherwise this behavior is shared by ``prun(1)`` (working with a PRRTE
DVM), and ``prterun(1)``. More detail about PRRTE process placement is
available in the following sections (using ``--help
placement-<section>``):

* ``examples``: some examples of the interactions between mapping,
  ranking, and binding options.

* ``fundamentals``: provides deeper insight into PRRTE's mapping,
  ranking, and binding options.

* ``limits``: explains the difference between *overloading* and
  *oversubscribing* resources.

* ``diagnostics``: describes options for obtaining various diagnostic
  reports that aid the user in verifying and tuning the placement for
  a specific job.

* ``rankfiles``: explains the format and use of the rankfile mapper
  for specifying arbitrary process placements.

* ``deprecated``: a list of deprecated options and their new
  equivalents.

* ``all``: outputs all the placement help except for the
  ``deprecated`` section.


Quick Summary
-------------

The two binaries that most influence process layout are ``prte(1)``
and ``prun(1)``.  The ``prte(1)`` process discovers the allocation,
establishes a Distributed Virtual Machine by starting a ``prted(1)``
daemon on each node of the allocation, and defines the efault
mapping/ranking/binding policies for all jobs.  The ``prun(1)`` process
defines the specific mapping/ranking/binding for a specific job. Most
of the command line controls are targeted to ``prun(1)`` since each job
has its own unique requirements.

``prterun(1)`` is just a wrapper around ``prte(1)`` for a single job
PRRTE DVM. It is doing the job of both ``prte(1)`` and ``prun(1)``,
and, as such, accepts the sum all of their command line arguments. Any
example that uses ``prun(1)`` can substitute the use of ``prterun(1)``
except where otherwise noted.

The ``prte(1)`` process attempts to automatically discover the nodes
in the allocation by querying supported resource managers. If a
supported resource manager is not present then ``prte(1)`` relies on a
hostfile provided by the user.  In the absence of such a hostfile it
will run all processes on the localhost.

If running under a supported resource manager, the ``prte(1)`` process
will start the daemon processes (``prted(1)``) on the remote nodes
using the corresponding resource manager process starter. If no such
starter is available then ``ssh`` (or ``rsh``) is used.

Minus user direction, PRRTE will automatically map processes in a
round-robin fashion by CPU, binding each process to its own CPU. The
type of CPU used (core vs hwthread) is determined by (in priority
order):

* user directive on the command line via the HWTCPUS qualifier to
  the ``--map-by`` directive

* setting the ``rmaps_default_mapping_policy`` MCA parameter to
  include the ``HWTCPUS`` qualifier. This parameter sets the default
  value for a PRRTE DVM |mdash| qualifiers are carried across to DVM
  jobs started via ``prun`` unless overridden by the user's command
  line

* defaulting to ``CORE`` in topologies where core CPUs are defined,
  and to ``hwthreads`` otherwise.

By default, the ranks are assigned in accordance with the mapping
directive |mdash| e.g., jobs that are mapped by-node will have the
process ranks assigned round-robin on a per-node basis.

PRRTE automatically binds processes unless directed not to do so by
the user. Minus direction, PRRTE will bind individual processes to
their own CPU within the object to which they were mapped. Should a
node become oversubscribed during the mapping process, and if
oversubscription is allowed, all subsequent processes assigned to that
node will *not* be bound.

.. include:: /prrte-rst-content/definitions-slots.rst

.. include:: /prrte-rst-content/definitions-pes.rst
