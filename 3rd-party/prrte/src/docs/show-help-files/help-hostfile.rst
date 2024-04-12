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
   Copyright (c) 2012      Los Alamos National Security, LLC
                           All rights reserved.
   Copyright (c) 2016      Research Organization for Information Science
                           and Technology (RIST). All rights reserved.
   Copyright (c) 2019-2020 Intel, Inc.  All rights reserved.
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

[no-hostfile]

PRTE was unable to open the hostfile:

.. code::

   %s

Check to make sure the path and filename are correct.

[port]

PRTE detected a bad parameter in the hostfile:

.. code::

   %s

The port parameter is less than 0:

.. code::

   port=%d

[slots]

PRTE detected a bad parameter in the hostfile:

.. code::

    %s

The slots parameter is less than 0:

.. code::

   slots=%d

[max_slots]

PRTE detected a bad parameter in the hostfile:

.. code::

   %s

The max_slots parameter is less than 0:

.. code::

   max_slots=%d

[max_slots_lt]

PRTE detected a bad parameter in the hostfile:

.. code::

   %s

The max_slots parameter is less than the slots parameter:

.. code::

   slots=%d
   max_slots=%d

[parse_error_string]

PRTE detected a parse error in the hostfile:

.. code::

   %s

It occured on line number %d on token %d:

.. code::

   %s

[parse_error_int]

PRTE detected a parse error in the hostfile:

.. code::

   %s

It occured on line number %d on token %d:

.. code::

   %d

[parse_error]

PRTE detected a parse error in the hostfile:

.. code::

   %s

It occured on line number %d on token %d.

[not-all-mapped-alloc]

Some of the requested hosts are not included in the current allocation.

The requested hosts were in this hostfile:

 .. code::

    %s

Please verify that you have specified the allocated resources properly in
the provided hostfile.

[hostfile:relative-syntax]

A relative host was specified, but no prior allocation has been made.
Thus, there is no way to determine the proper host to be used.

.. code::

   hostfile entry: %s

Re-run this command with ``--help hosts`` for further information.

[hostfile:relative-node-not-found]

A relative host was specified, but was not found. The requested host was
specified as:

.. code::

   Index: %d
   Syntax given: %s

This is most likely due to the relative index being out of bounds. You
could obtain a larger allocation or reduce the relative host index.

Re-run this command with ``--help hosts`` for further information.

[hostfile:invalid-relative-node-syntax]

A relative host was improperly specified |mdash| the value provided was.

.. code::

   hostfile entry: %s

You may have forgotten to preface a node with ``N`` or ``n``, or used the
``e`` or ``E`` to indicate empty nodes.

Re-run this command with ``--help hosts`` for further information.

[hostfile:not-enough-empty]

The requested number of empty hosts was not available |mdash| the
system was short by %d hosts.  Please recheck your allocation.

Re-run this command with ``--help hosts`` for further information.

[boards]

PRTE detected a bad parameter in the hostfile:

.. code::

   %s

The boards parameter is less than 0:

.. code::

   boards=%d

[sockets]

PRTE detected a bad parameter in the hostfile:

.. code::

   %s

The packages parameter is less than 0:

.. code::

   packages=%d

[cores]

PRTE detected a bad parameter in the hostfile:

.. code::

   %s

The cores parameter is less than 0:

.. code::

   cores=%d

[hostfile:extra-node-not-found]

A hostfile was provided that contains at least one node not
present in the allocation:

.. code::

   hostfile:  %s
   node:      %s

If you are operating in a resource-managed environment, then only
nodes that are in the allocation can be used in the hostfile. You may
find relative node syntax to be a useful alternative to specifying
absolute node names; re-run this command with ``--help hosts`` for
further information.

[slots-given]

A hostfile was provided that contains multiple definitions
of the slot count for at least one node:

.. code::

   hostfile:  %s
   node:      %s

You can either list a node multiple times, once for each slot,
or you can provide a single line that contains ``slot=N``. Mixing
the two methods is not supported.

Please correct the hostfile and try again.
