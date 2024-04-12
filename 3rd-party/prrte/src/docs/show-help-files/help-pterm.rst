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

Terminate an instance of the PMIx Reference RTE (PRRTE) DVM

* General Options

.. list-table::
   :header-rows: 1
   :widths: 20 45

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

* Specific Options

.. list-table::
   :header-rows: 1
   :widths: 20 45

   * - Option
     - Description

   * - ``--pmixmca <key> <value>``
     - Pass context-specific PMIx MCA parameters (``key`` is the
       parameter name; ``value`` is the parameter value)

   * - ``--dvm-uri <uri>``
     - Specify the URI of the DVM master, or the name of the file
       (specified as ``file:filename``) that contains that info

   * - ``--num-connect-retries <num>``
     - Max number of times to try to connect (int)

   * - ``--pid <pid>```
     - PID of the daemon to which we should connect (integer PID or
       ``file:<filename>`` for file containing the PID

   * - ``--namespace <name>``
     - Namespace of the daemon we are to connect to

   * - ``--system-server-first``
     - First look for a system server and connect to it if found

   * - ``--system-server-only``
     - Connect only to a system-level server

   * - ``--wait-to-connect <seconds>``
     - Delay specified number of seconds before trying to connect

Report bugs to %s

[version]

%s (%s) %s

Report bugs to %s

[dvm-uri]

Specify the URI of the DVM master, or the name of the file (specified
as ``file:<filename>``) that contains that info

[num-connect-retries]

Max number of times to try to connect to the specified server (int)

[pid]

PID of the daemon to which we should connect (integer PID or
``file:<filename>`` for file containing the PID

[namespace]

Namespace of the daemon we are to connect to (char*)

[system-server-first]

First look for a system server and connect to it if found

[system-server-only]

Connect only to a system-level server - abort if one is not found

[wait-to-connect]

Delay specified number of seconds before trying to connect

[pmixmca]

.. include:: /prrte-rst-content/cli-pmixmca.rst

[no-args]

The %s command does not accept arguments other than those
specifically defined by the command. The following were
not recognized:

  Args: %s

Please see "%s --help" for a description of all accepted
command options.
