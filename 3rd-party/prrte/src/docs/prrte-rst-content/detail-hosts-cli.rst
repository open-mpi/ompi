.. -*- rst -*-

   Copyright (c) 2022-2024 Nanook Consulting  All rights reserved.
   Copyright (c) 2023      Jeffrey M. Squyres.  All rights reserved.

   $COPYRIGHT$

   Additional copyrights may follow

   $HEADER$

.. The following line is included so that Sphinx won't complain
   about this file not being directly included in some toctree

Listing Hosts on the Command Line
=================================

Many PRRTE commands accept the ``--host`` CLI parameter.
``--host`` accepts a comma-delimited list of tokens of the form:

.. code::

   host[:slots]

The ``host`` token can be either:

* A name that resolves to an IP address, or
* An IP address

.. note:: The names and/or IP addresses of hosts are *only* used for
          identifying the target host on which to launch.  They are
          *not* used for determining which network interfaces are used
          by applications (e.g., MPI or other network-based
          applications).

          For network-based applications, consult their documentation
          for how to specify which network interfaces are used.

The optional integer ``:slots`` parameter tells PRRTE the maximum
number of slots to use on that host (see this section on definition
of the term ``slot`` for a description of what a "slot" is).

For example:

.. code::

   prterun --host node1:10,node2,node3:5 ...
