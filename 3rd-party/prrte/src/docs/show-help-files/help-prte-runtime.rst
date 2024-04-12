.. Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
                           University Research and Technology
                           Corporation.  All rights reserved.
   Copyright (c) 2004-2005 The University of Tennessee and The University
                           of Tennessee Research Foundation.  All rights
                           reserved.
   Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
                           University of Stuttgart.  All rights reserved.
   Copyright (c) 2004-2005 The Regents of the University of California.
                           All rights reserved.
   Copyright (c) 2019-2020 Intel, Inc.  All rights reserved.
   Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
   Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
   Copyright (c) 2023      Jeffrey M. Squyres.  All rights reserved.
   $COPYRIGHT$

   Additional copyrights may follow

   $HEADER$

   This is the US/English general help file for PRTE.

[bogus section]

This section is not used by PRTE code.  But we have to put a RST
section title in this file somewhere, or Sphinx gets unhappy.  So we
put it in a section that is ignored by PRTE code.

Hello, world
------------

[prte_init:startup:internal-failure]

It looks like ``prte_init()`` failed for some reason. There are many
reasons that can cause PRRTE to fail during ``prte_init()``, some of
which are due to configuration or environment problems.  This failure
appears to be an internal failure |mdash| here's some additional
information (which may only be relevant to a PRRTE developer):

.. code::

   %s failed
   --> Returned value %s (%d) instead of PRTE_SUCCESS

[prte:session:dir:prohibited]

The specified location for the temporary directories required by PRTE
is on the list of prohibited locations:

.. code::

   Location given: %s
   Prohibited locations: %s

If you believe this is in error, please contact your system administrator
to have the list of prohibited locations changed. Otherwise, please identify
a different location to be used (use ``-h`` to see the cmd line option), or
simply let the system pick a default location.

[prte:session:dir:nopwname]

PRTE was unable to obtain the username in order to create a path for
its required temporary directories.  This type of error is usually
caused by a transient failure of network-based authentication services
(e.g., LDAP failure due to network congestion), but can also be an
indication of system misconfiguration.

Please consult your system administrator about these issues and try
again.

[prte_nidmap:too_many_nodes]

An error occurred while trying to pack the information about the job. More nodes
have been found than the %d expected. Please check your configuration files such
as the mapping.

[prte_init:startup:num_daemons]

PRTE was unable to determine the number of nodes in your allocation. We
are therefore assuming a very large number to ensure you receive proper error
messages.

[failed-to-uncompress]

A compressed message was received that could not be
decompressed. This is most likely due to a missing
libz library on the receiving node:

.. code::

   node:  %s

Please ensure that the ``libz`` library is present on all
compute nodes.

[bootstrap-not-found]

Bootstrap startup was requested, but the required configuration
file was not found on at least one node:

.. code::

   node: %s
   file: %s

Please ensure the file has been installed in the correct location
on every node included in this PRRTE DVM.

[bootstrap-missing-field-name]

Bootstrap startup was requested, but the required configuration
file on at least one node is missing the field name on an entry:

.. code::

   node: %s
   file: %s
   line: %s

Please correct the line.

[bootstrap-missing-value]

Bootstrap startup was requested, but the required configuration
file on at least one node is missing a value on an entry:

.. code::

   node: %s
   file: %s
   line: %s

Please correct the line.

[bootstrap-missing-entry]

Bootstrap startup was requested, but the required configuration
file on at least one node is missing a required entry:

.. code::

   node:   %s
   file:   %s
   entry:  %s

Please provide the missing information.

[bootstrap-bad-nodelist]

Bootstrap startup was requested, but we were unable to parse
the regex of participating nodes on at least one node:

.. code::

   node:   %s
   file:   %s
   regex:  %s
   error:  %s

Please fix the error and try again.

[prte:session:dir:shared]

PRTE has detected that the head of the session directory
tree (where scratch files and shared memory backing storage
will be placed) resides on a shared file system:

.. code::

   Directory: %s
   File system type: %s

For performance reasons, it is strongly recommended that the
session directory be located on a local file system. This can
be controlled by setting the system temporary directory to be
used by PRTE using either the TMPDIR envar or the "prte_tmpdir_base"
MCA param.

If you need the temporary directory to be different
on remote nodes from the local one where %s is running (e.g.,
when a login node is being employed), then you can set the
local temporary directory using the "prte_local_tmpdir_base"
MCA param and the one to be used on all other nodes using the
"prte_remote_tmpdir_base" param.

This is only a warning advisory and your job will continue.
You can disable this warning in the future by setting the
"prte_silence_shared_fs" MCA param to "1".
