   Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
   Copyright (c) 2022      IBM Corporation.  All rights reserved.
   Copyright (c) 2023      Jeffrey M. Squyres.  All rights reserved.
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
     - Top-level PRTE debug switch (default: ``false``) This option
       will be deprecated, use ``--debug-devel`` instead.

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
     - Pass context-specific PMIx MCA parameters; they are considered
       global if only one context is specified (``key`` is the
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

Specify the URI of the DVM master, or the name of the file (specified as
``file:filename``) that contains that info

[test-suicide]

Test DVM cleanup upon daemon failure by having one daemon suicide after delay

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
