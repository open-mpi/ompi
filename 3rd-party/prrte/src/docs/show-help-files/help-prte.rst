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

[version]

%s (%s) %s

%s

[usage]

%s (%s) %s

Usage: %s [OPTION]...

Initiate an instance of the PMIx Reference RTE (PRRTE) DVM

The following list of command line options are available. Note that
more detailed help for any option can be obtained by adding that
option to the help request as ``--help <option>``.

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

   * - ``--debug-daemons``
     - Debug daemons - if not set, any "verbose" settings will be
       limited to the DVM controller to reduce clutter

   * - ``--debug-daemons-file``
     - Enable debugging of any PRRTE daemons used by this application,
       storing their verbose output in files

   * - ``--leave-session-attached``
     - Do not discard stdout/stderr of remote PRRTE daemons

   * - ``--display <arg0>``
     - Comma-delimited list of options for displaying information

.. list-table::
   :header-rows: 2
   :widths: 20 45

   * -
     - DVM Options

   * - Option
     - Description

   * - ``--runtime-options <options>``
     - Comma-delimited list of runtime directives for the job (e.g.,
       show progress reports on DVM startup for large systems)

   * - ``--default-hostfile <filename>``
     - Provide a default hostfile

   * - ``-H|--host <hostname>``
     - List of hosts to use for the DVM

   * - ``--hostfile <filename>``
     - Provide a hostfile

   * - ``--machinefile <filename>``
     - Provide a hostfile (synonym for ``--hostfile``)

   * - ``--pmixmca <key> <value>``
     - Pass PMIx MCA parameters

   * - ``--prtemca <key> <value>``
     - Pass PRTE MCA parameters to the DVM

   * - ``--show-progress``
     - Output a brief periodic report on DVM startup progress

   * - ``-x <var_name>[=value]``
     - Export an environment variable, optionally specifying a value

   * - ``--allow-run-as-root``
     - Allow execution as root (**STRONGLY DISCOURAGED**)

   * - ``--daemonize``
     - Daemonize the DVM daemons into the background

   * - ``--forward-signals <signals>``
     - Comma-delimited list of additional signals (names or integers)
       to forward

   * - ``--keepalive <arg0>``
     - Pipe to monitor - DVM will terminate upon closure

   * - ``--launch-agent <executable>``
     - Name of daemon executable used to start processes on remote
       nodes (default: ``prted``)

   * - ``--max-vm-size <size>``
     - Max number of daemons to start

   * - ``--no-ready-msg``
     - Do not print a "DVM ready" message

   * - ``--noprefix``
     - Disable automatic ``--prefix`` behavior

   * - ``--prefix <dir>``
     - Prefix to be used to look for RTE executables

   * - ``--report-pid <arg>``
     - Print out PID on stdout (``-``), stderr (``+``), or a file
       (anything else)

   * - ``--report-uri <arg>``
     - Print out URI on stdout (``-``), stderr (``+``), or a file
       (anything else)

   * - ``--set-sid``
     - Direct the DVM daemons to separate from the current session

   * - ``--singleton <id>``
     - ID of the singleton process that started us

   * - ``--system-server``
     - Start the DVM as the system server

   * - ``--tmpdir <dir>``
     - Set the filesystem root for the session directory tree

   * - ``--tune <arg0>``
     - File(s) containing MCA params for tuning DVM operations

   * - ``--timeout <seconds>``
     - Timeout DVM startup if time exceeds the specified number of
       seconds

Report bugs to %s

[prtemca]

.. include:: /prrte-rst-content/cli-prtemca.rst

[pmixmca]

.. include:: /prrte-rst-content/cli-pmixmca.rst

[tune]

.. include:: /prrte-rst-content/cli-tune.rst

[no-ready-msg]

Do not print a DVM ready message

[daemonize]

Daemonize the DVM daemons and controller into the background

[system-server]

Start the DVM controller and its daemons as the system server on their
nodes

[set-sid]

Direct the DVM (controller and daemons) to separate from the current
session

[report-pid]

Printout DVM controller's PID on stdout [-], stderr [+], or a file
[anything else]

[report-uri]

Printout DVM controller's URI on stdout [-], stderr [+], or a file
[anything else]

[test-suicide]

Test DVM cleanup upon daemon failure by having one daemon suicide
after delay

[default-hostfile]

Specify a default hostfile.

Also see ``--help hostfile``.

[singleton]

DVM is being started by a singleton process (i.e., one not started by
a DVM) - the argument must be the PMIx ID of the singleton process
that started us

[keepalive]

Pipe for DVM controller to monitor - DVM will terminate upon closure

[launch-agent]

Name of daemon executable used to start processes on remote nodes
(default: prted).  This is the executable the DVM controller shall
start on each remote node when establishing the DVM.

[max-vm-size]

Maximum number of daemons to start - sets the maximum size of the DVM.

[debug-daemons]

Debug daemon output enabled. This is a somewhat limited stream of
information normally used to simply confirm that the daemons
started. Includes leaving the output streams open.

[debug-daemons-file]

.. include:: /prrte-rst-content/cli-debug-daemons-file.rst

See the "Session directory" HTML documentation for additional details
about the PRRTE session directory.

[leave-session-attached]

.. include:: /prrte-rst-content/cli-leave-session-attached.rst

[tmpdir]

Define the root location for the PRRTE session directory tree

See the "Session directory" HTML documentation for additional details
about the PRRTE session directory.

[prefix]

.. include:: /prrte-rst-content/cli-prefix.rst

[noprefix]

.. include:: /prrte-rst-content/cli-noprefix.rst

[forward-signals]

.. include:: /prrte-rst-content/cli-forward-signals.rst

[allow-run-as-root]

.. include:: /prrte-rst-content/cli-allow-run-as-root.rst

[no-aggregate-help]

Do not aggregate help output from multiple processes. PRRTE defaults
to aggregating messages generated by its "help" subsystem so that
only one is printed out per topic (along with the number of processes
that reported the same issue). This is done to avoid users receiving a
flood of one-per-process error messages, all containing the identical
error report.  Setting this option turns off the aggregation, thereby
allowing the user to see duplicate reports from multiple processes.

[timeout]

Timeout DVM startup if time exceeds the specified number of
seconds. The DVM startup will abort after the specified interval.

[x]

.. include:: /prrte-rst-content/cli-x.rst

[show-progress]

Output a brief periodic report on DVM startup progress

[hostfile]

.. include:: /prrte-rst-content/cli-dvm-hostfile.rst

See the "Host specification" HTML documentation for details about the
format and content of hostfiles.

[machinefile]

Provide a hostfile.  This option is a synonym for ``--hostfile``; see
that option for more information.

[host]

.. include:: /prrte-rst-content/cli-dash-host.rst

[display]

.. include:: /prrte-rst-content/cli-display.rst
