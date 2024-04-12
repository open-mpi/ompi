.. Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
                           University Research and Technology
                           Corporation.  All rights reserved.
   Copyright (c) 2004-2005 The University of Tennessee and The University
                           of Tennessee Research Foundation.  All rights
                           reserved.
   Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
                           University of Stuttgart.  All rights reserved.
   Copyright (c) 2004-2005 The Regents of the University of California.
                           All rights reserved.
   Copyright (c) 2019      Intel, Inc.  All rights reserved.
   Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
   Copyright (c) 2023      Jeffrey M. Squyres.  All rights reserved.
   Copyright (c) 2024      Nanook Consulting  All rights reserved.
   $COPYRIGHT$

   Additional copyrights may follow

   $HEADER$

   This is the US/English general help file for hostfile utilities.

[bogus section]

This section is not used by PRTE code.  But we have to put a RST
section title in this file somewhere, or Sphinx gets unhappy.  So we
put it in a section that is ignored by PRTE code.

Hello, world
------------

[not-all-mapped-alloc]

At least one of the requested hosts is not included in the current allocation.

.. code::

   Missing requested host: %s

Please check your allocation or your request.

[dash-host:relative-syntax]

A relative host was specified, but no prior allocation has been made.
Thus, there is no way to determine the proper host to be used.

.. code::

   --host: %s

Re-run this command with ``--help hosts`` for further information.

[dash-host:relative-node-not-found]

A relative host was specified, but was not found. The requested host was
specified with ``--host`` as:

.. code::

   Index: %d
   Syntax given: %s

Re-run this command with ``--help hosts`` for further information.

[dash-host:relative-node-out-of-bounds]

A relative host was specified, but the index given is beyond the number
of hosts in the current allocation:

.. code::

   Index: %d
   #hosts: %d

You could obtain a larger allocation or reduce the relative host index.

Re-run this command with ``--help hosts`` for further information.

[dash-host:invalid-relative-node-syntax]

A relative host was improperly specified |mdash| the value provided was.

.. code::

   --host: %s

You may have forgotten to preface a node with ``N`` or ``n``, or used the
``e`` or ``E`` to indicate empty nodes.

Re-run this command with ``--help hosts`` for further information.

[dash-host:not-enough-empty]

The requested number of empty hosts was not available |mdash| the
system was short by %d hosts.  Please recheck your allocation.

Re-run this command with ``--help hosts`` for further information.
