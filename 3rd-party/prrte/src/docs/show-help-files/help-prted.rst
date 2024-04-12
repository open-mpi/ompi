.. -*- rst -*-

   Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
   Copyright (c) 2023 Jeffrey M. Squyres.  All rights reserved.

   $COPYRIGHT$

   Additional copyrights may follow

   $HEADER$

[bogus section]

This section is not used by PRTE code.  But we have to put a RST
section title in this file somewhere, or Sphinx gets unhappy.  So we
put it in a section that is ignored by PRTE code.

Hello, world
------------

[usage]

%s (%s) %s

Usage: %s [OPTION]...

Start a PRTE Daemon

The following list of command line options are available. Note that
more detailed help for any option can be obtained by adding that
option to the help request as "--help <option>".

.. list-table::
   :header-rows: 2
   :widths: 20 45

   * -
     - General Options

   * - Option
     - Description

   * - ``-h`` | ``--help``
     - This help message

   * - ``-h`` | ``--help <arg0>``
     - Help for the specified option

   * - ``-v`` | ``--verbose``
     - Enable typical debug options

   * - ``-V`` | ``--version``
     - Print version and exit

.. list-table::
   :header-rows: 2
   :widths: 20 45

   * -
     - Debug Options

   * - Option
     - Description

   * - ``--debug``
     - Top-level PRTE debug switch (default: false). This option will
       be deprecated, use ``--debug-devel`` instead.

   * - ``--debug-daemons-file``
     - Enable debugging of any PRTE daemons used by this application,
       storing output in files

   * - ``--leave-session-attached``
     - Do not daemonize the ``prted``

   * - ``--test-suicide <arg0>``
     - Suicide instead of clean abort after specified delay

.. list-table::
   :header-rows: 2
   :widths: 20 45

   * -
     - DVM Options

   * - Option
     - Description

   * - ``--pmixmca <key> <value>``
     - Pass context-specific PMIx MCA parameters (``key`` is the
       parameter name; ``value`` is the parameter value)

   * - ``--prtemca <key> <value>``
     - Pass context-specific PRTE MCA parameters; they are considered
       global if ``--gmca`` is not used and only one context is
       specified (``key`` is the parameter name; ``value`` is the
       parameter value)

   * - ``--dvm-master-uri <uri>``
     - Specify the URI of the DVM master, or the name of the file
       (specified as ``file:filename``) that contains that info

   * - ``--parent-uri <uri>``
     - Specify the URI of the prted acting as the parent of this prted
       in a tree-based spawn operation

   * - ``--tree-spawn``
     - A tree-based spawn operation is in progress

   * - ``--bootstrap``
     - Self-construct the DVM based on a configuration file

.. list-table::
   :header-rows: 2
   :widths: 20 45

   * -
     - Specific Options

   * - Option
     - Description

   * - ``--set-sid``
     - Direct the DVM daemons to separate from the current session

   * - ``--system-server``
     - Start the DVM as the system server

   * - ``--pubsub-server <uri>``
     - Contact information for external PRRTE publish/lookup data
       server

Report bugs to %s

[version]

%s (%s) %s

Report bugs to %s

[dvm-master-uri]

Specify the URI of the DVM master, or the name of the file (specified
as ``file:filename``) that contains that info

[test-suicide]

Test DVM cleanup upon daemon failure by having one daemon suicide
after delay

[system-server]

Start the daemon as the system server on its node

[set-sid]

Direct the daemon to separate from the current session

[prtemca]

.. include:: /prrte-rst-content/cli-prtemca.rst

[pmixmca]

.. include:: /prrte-rst-content/cli-pmixmca.rst

[leave-session-attached]

.. include:: /prrte-rst-content/cli-leave-session-attached.rst

[parent-uri]

Specify the URI of the prted acting as the parent of this prted in a
tree-based spawn operation

[tree-spawn]

A tree-based spawn operation is in progress

[pubsub-server]

Contact information for external PRRTE publish/lookup data server

%s

[prted:environ]

PRTE Daemon was unable to set

.. code::

   %s = %s

in the environment. Returned value %d instead of PRTE_SUCCESS.

[prted:init-failure]

PRTE was unable to initialize properly.  The error occured while
attempting to %s.  Returned value %d instead of PRTE_SUCCESS.

[prted:cannot-bind]

A request was made to bind the PRTE daemons to
a core that does not exist on this node:

.. code::

   node:  %s
   cores: %s

The MCA param directing this behavior is prte_daemon_cores.
Please correct the request and try again.

[cwd]

A dynamic operation (%s) was requested that requires us to obtain
the current working directory. Unfortunately, an error was returned
when we attempted to obtain it:

.. code::

   error: %d

We are unable to complete the requested operation.

[bad-key]

A dynamic operation (%s) was requested that included an unrecognized
info key:

.. code::

   group: %s
   key:   %s

The operation will continue, but may not behave completely as expected.

[timedout]

A request has timed out and will therefore fail:

.. code::

   Operation:  %s

Your job may terminate as a result of this problem. You may want to
adjust the MCA parameter pmix_server_max_wait and try again. If this
occurred during a connect/accept operation, you can adjust that time
using the pmix_base_exchange_timeout parameter.

[noroom]

A request for an asynchronous runtime operation cannot be fulfilled
because of a lack of room in the tracking array:

.. code::

   Operation:  %s
   Number of rooms:  %d

This is usually caused by a large job that encounters significant
delays across the cluster when starting the application processes.
Your job may terminate as a result of this problem. You may want to
adjust the MCA parameter ``pmix_server_max_reqs`` and try again.

[noserver]

A publish/lookup server was provided, but we were unable to connect
to it |mdash| please check the connection info and ensure the server
is alive:

.. code::

  Connection:  %s

[mpir-debugger-detected]

PRRTE has detected that you have attached a debugger to this MPI
job, and that debugger is using the legacy "MPIR" method of
attachment.

Please note that PRRTE has deprecated the "MPIR" debugger
attachment method in favor of the new "PMIx" debugger attchment
mechanisms.

.. warning:: This means that future versions of PRRTE may not support
             the "MPIR" debugger attachment method at all.
             Specifically: the debugger you just attached may not work
             with future versions of PRTE.

You may wish to contact your debugger vendor to inquire about support
for PMIx-based debugger attachment mechanisms. Meantime, you can
disable this warning by setting the ``OMPI_MPIR_DO_NOT_WARN``
environment variable to 1.

[both-file-and-dir-set]

Both the ``output-directory`` and ``output-filename`` options have been
set:

.. code::

   Directory:      %s
   Filename:       %s

Only one of these can be set |mdash| please fix the options and try
again.

[min-pmix-violation]

PRRTE has detected that the PMIx library being used to run this
executable does not meet the minimum supported version:

  Min PMIx version: %0x
  Detected version: %0x

Please check your LD_LIBRARY_PATH and ensure we are pointed to
a version that meets the minimum requirement.
