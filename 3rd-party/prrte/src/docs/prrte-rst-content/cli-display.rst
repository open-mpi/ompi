.. -*- rst -*-

   Copyright (c) 2022-2024 Nanook Consulting  All rights reserved.
   Copyright (c) 2023 Jeffrey M. Squyres.  All rights reserved.

   $COPYRIGHT$

   Additional copyrights may follow

   $HEADER$

.. The following line is included so that Sphinx won't complain
   about this file not being directly included in some toctree

The ``display`` command line directive must be accompanied by a
comma-delimited list of case-insensitive options indicating what
information about the job and/or allocation is to be displayed. The
full directive need not be provided |mdash| only enough characters are
required to uniquely identify the directive. For example, ``ALL`` is
sufficient to represent the ``ALLOCATION`` directive |mdash| while ``MAP``
can not be used to represent ``MAP-DEVEL`` (though ``MAP-D`` would
suffice).

Supported values include:

* ``ALLOCATION`` displays the detected hosts and slot assignments for
  this job

* ``BINDINGS`` displays the resulting bindings applied to processes in
  this job

* ``MAP`` displays the resulting locations assigned to processes in
  this job

* ``MAP-DEVEL`` displays a more detailed report on the locations
  assigned to processes in this job that includes local and node
  ranks, assigned bindings, and other data

* ``TOPO=LIST`` displays the topology of each node in the
  semicolon-delimited list that is allocated to the job

* ``CPUS[=LIST]`` displays the available CPUs on the provided
  semicolon-delimited list of nodes (defaults to all nodes)

The display command line directive can include qualifiers by adding a
colon (``:``) and any combination of one or more of the following
(delimited by colons):

* ``PARSEABLE`` directs that the output be provided in a format that
  is easily parsed by machines. Note that ``PARSABLE`` is also accepted as
  a typical spelling for the qualifier.

Provided qualifiers will apply to *all* of the display directives.
