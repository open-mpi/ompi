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
     - Debug daemons |mdash| if not set, any "verbose" settings will
       be limited to the prterun itself to reduce clutter

   * - ``--debug-daemons-file``
     - Enable debugging of any PRRTE daemons used by this application,
       storing their verbose output in files

   * - ``--display <arg0>``
     - Options for displaying information about the allocation and
       job.

   * - ``--spawn-timeout <seconds>``
     - Timeout the job if spawn takes more than the specified number
       of seconds

   * - ``--timeout <seconds>``
     - Timeout the job if execution is not complete after the
       specified number of seconds

   * - ``--get-stack-traces``
     - Get stack traces of all application procs on timeout

   * - ``--leave-session-attached``
     - Do not discard stdout/stderr of remote PRRTE daemons

   * - ``--report-state-on-timeout``
     - Report all job and process states upon timeout

   * - ``--stop-on-exec``
     - If supported, stop each specified process at start of execution

   * - ``--stop-in-init``
     - Direct the specified processes to stop in ``PMIx_Init``

   * - ``--stop-in-app``
     - Direct the specified processes to stop at an
       application-controlled location

   * - ``--do-not-launch``
     - Perform all necessary operations to prepare to launch the
       application, but do not actually launch it (usually used to
       test mapping patterns)

.. list-table::
   :header-rows: 2
   :widths: 20 45

   * -
     - Output Options

   * - Option
     - Description

   * - ``--output <arg0>``
     - Comma-delimited list of options that control how output is
       generated.

   * - ``--report-child-jobs-separately``
     - Return the exit status of the primary job only

   * - ``--xterm <ranks>``
     - Create a new xterm window for each of the comma-delimited
       ranges of application process ranks

.. list-table::
   :header-rows: 2
   :widths: 20 45

   * -
     - Input Options

   * - Option
     - Description

   * - ``--stdin <ranks>``
     - Specify application rank(s) to receive stdin [integer ranks,
       ``rank``, ``all``, ``none``] (default: ``0``, indicating
       rank 0)

.. list-table::
   :header-rows: 2
   :widths: 20 45

   * -
     - Placement Options

   * - Option
     - Description

   * - ``--map-by <type>``
     - Mapping Policy for job

   * - ``--rank-by <type>``
     - Ranking Policy for job

   * - ``--bind-to <type>``
     - Binding policy for job.

.. list-table::
   :header-rows: 2
   :widths: 20 45

   * -
     - Launch Options

   * - Option
     - Description

   * - ``--runtime-options <arg0>``
     - Comma-delimited list of runtime directives for the job (e.g.,
       do not abort if a process exits on non-zero status)

   * - ``-c`` | ``--np <num>``
     - Number of processes to run

   * - ``-n`` | ``--n <num>``
     - Number of processes to run

   * - ``-N`` | ``--npernode <num>``
     - Run designated number of processes on each node

   * - ``--personality <name>``
     - Specify the personality to be used

   * - ``-H`` | ``--host <hosts>``
     - List of hosts to invoke processes on

   * - ``--hostfile <file>``
     - Provide a hostfile

   * - ``--machinefile <file>``
     - Provide a hostfile (synonym for ``--hostfile``)

   * - ``--pmixmca <key> <value>``
     - Pass context-specific PMIx MCA parameters; they are considered
       global if only one context is specified (``key`` is the
       parameter name; ``value`` is the parameter value)

   * - ``--gpmixmca <key> <value>``
     - Pass global PMIx MCA parameters that are applicable to all
       contexts (``key`` is the parameter name; ``value`` is the
       parameter value)

   * - ``--preload-files <files>``
     - Preload the comma separated list of files to the remote
       machines current working directory before starting the remote
       process.

   * - ``--prtemca <key> <value>``
     - Pass context-specific PRRTE MCA parameters to the DVM

   * - ``--pset <name>``
     - User-specified name assigned to the processes in their given
       application

   * - ``-s`` | ``--preload-binary``
     - Preload the binary on the remote machine before starting the
       remote process.

   * - ``--set-cwd-to-session-dir``
     - Set the working directory of the started processes to their
       session directory

   * - ``--show-progress``
     - Output a brief periodic report on launch progress

   * - ``--wd <dir>``
     - Synonym for ``--wdir``

   * - ``--wdir <dir>``
     - Set the working directory of the started processes

   * - ``-x <name>``
     - Export an environment variable, optionally specifying a value

.. list-table::
   :header-rows: 2
   :widths: 20 45

   * -
     - Specific Options

   * - Option
     - Description

   * - ``--allow-run-as-root``
     - Allow execution as root (**STRONGLY DISCOURAGED**)

   * - ``--forward-signals <signals>``
     - Comma-delimited list of additional signals (names or integers) to forward
       to application processes [``none`` => forward nothing].

   * - ``--keepalive <arg0>``
     - Pipe to monitor |mdash| DVM will terminate upon closure

   * - ``--launch-agent <exe>``
     - Name of daemon executable used to start processes on remote
       nodes (default: ``prted``)

   * - ``--max-vm-size <num>``
     - Number of daemons to start

   * - ``--no-ready-msg``
     - Do not print a DVM ready message

   * - ``--noprefix``
     - Disable automatic ``--prefix`` behavior

   * - ``--prefix <dir>``
     - Prefix to be used to look for RTE executables

   * - ``--report-pid <arg0>``
     - Print out PID on stdout (``-``), stderr (``+``), or a file
       [anything else]

   * - ``--report-uri <arg0>``
     - Print out URI on stdout (``-``), stderr (``+``), or a file
       [anything else]

   * - ``--set-sid``
     - Direct the DVM daemons to separate from the current session

   * - ``--singleton <id>``
     - ID of the singleton process that started us

   * - ``--tmpdir <dir>``
     - Set the root for the session directory tree

   * - ``--tune <files>``
     - File(s) containing MCA params for tuning DVM operations

   * - ``--dvm <arg>``
     - Use a persistent DVM instead of instantiating independent runtime
       infrastructure. The argument indicates the PID, URI,
       file containing the URI, or namespace of the DVM.

Report bugs to %s

[dvm]

Utilize an existing persistent DVM instead of instantiating an
independent runtime infrastructure. This mimics the ``prun`` command,
but is provided as a convenience option for those wanting to embed the
``prterun`` command in a script that can be optionally used to run
either independently or under a persistent DVM.

.. include:: /prrte-rst-content/cli-dvm.rst

[prtemca]

.. include:: /prrte-rst-content/cli-prtemca.rst

[pmixmca]

.. include:: /prrte-rst-content/cli-pmixmca.rst

[gpmixmca]

Syntax: ``--gpmixmca <key> <value>``

where ``key`` is the parameter name and ``value`` is the parameter
value. The ``g`` prefix indicates that this PMIx parameter is to be
applied to _all_ application contexts and not just the one in which
the directive appears.

[tune]

.. include:: /prrte-rst-content/cli-tune.rst

[stream]

.. include:: /prrte-rst-content/cli-stream-buffering.rst

[system-server]

Start prterun and its daemons as the system server on their nodes

[set-sid]

Direct the DVM (controller and daemons) to separate from the current session

[report-pid]

Printout prterun's PID on stdout (``-``), stderr (``+``), or a file
(anything else).

[report-uri]

Printout prterun's URI on stdout (``-``), stderr (``+``), or a file
(anything else).

[test-suicide]

Test DVM cleanup upon daemon failure by having one daemon suicide
after delay

[singleton]

``prterun`` is being started by a singleton process (i.e., one not
started by prterun) |mdash| the argument must be the PMIx ID of the
singleton process that started us

[keepalive]

Pipe for prterun to monitor |mdash| job will terminate upon closure

[launch-agent]

Name of daemon executable used to start processes on remote nodes
(default: ``prted``).

This is the executable prterun shall start on each remote node when
establishing the DVM.

[max-vm-size]

Maximum number of daemons to start

[debug-daemons]

.. include:: /prrte-rst-content/cli-debug-daemons.rst

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

[report-child-jobs-separately]

Return the exit status of the primary job only

[timeout]

Timeout the job if execution time exceeds the specified number of
seconds

[report-state-on-timeout]

Report all job and process states upon timeout

[get-stack-traces]

Get stack traces of all application procs on timeout

[spawn-timeout]

Timeout the job if spawn takes more than the specified number of seconds

[np]

Specify number of application processes to be started

[n]
Specify number of application processes to be started

[N]

Specify number of application processes per node to be started

[app]

Provide an appfile; ignore all other command line options

[xterm]

Create a new xterm window and display output from the specified ranks
there.  Ranks are specified as a comma-delimited list of ranges
|mdash| e.g., ``1,3-6,9``, or as ``all``.

[stop-on-exec]

If supported, stop each process at start of execution

[stop-in-init]

Include the ``PMIX_DEBUG_STOP_IN_INIT`` attribute in the application's
job info directing that the specified ranks stop in PMIx_Init pending
release. Ranks are specified as a comma-delimited list of ranges
|mdash| e.g., ``1,3-6,9``, or as ``all``.

[stop-in-app]

Include the ``PMIX_DEBUG_STOP_IN_APP`` attribute in the application's
job info directing that the specified ranks stop at an application-determined
point pending release. Ranks are specified as a comma-delimited list of
ranges |mdash| e.g., ``1,3-6,9``, or as ``all``.

[x]

.. include:: /prrte-rst-content/cli-x.rst

[wdir]

Set the working directory of the started processes

[wd]

Synonym for --wdir

[set-cwd-to-session-dir]

Set the working directory of the started processes to their session
directory

[path]

Path to be used to look for executables to start processes

[show-progress]

Output a brief periodic report on launch progress

[pset]

User-specified name assigned to the processes in their given application

[hostfile]

.. include:: /prrte-rst-content/cli-launcher-hostfile.rst

See the "Host specification" HTML documentation for details about the
format and content of hostfiles.

[machinefile]

Provide a hostfile.  This option is a synonym for ``--hostfile``; see
that option for more information.

[host]

.. include:: /prrte-rst-content/cli-dash-host.rst

See the "Host specification" HTML documentation for details about the
format and content of hostfiles.

[personality]

.. include:: /prrte-rst-content/cli-personality.rst

[preload-files]

Syntax: ``--preload-files <files>``

Preload the comma separated list of files to the remote machines current
working directory before starting the remote process.

[preload-binary]

Syntax: ``--preload-binary``

Preload the binary on the remote machine before starting the
remote process.

[output]

.. include:: /prrte-rst-content/cli-output.rst

[stdin]

Specify procs to receive stdin [integer ranks, ``all``, ``none``]
(default: ``0``, indicating rank 0)

[map-by]

.. include:: /prrte-rst-content/cli-map-by.rst

[rank-by]

.. include:: /prrte-rst-content/cli-rank-by.rst

[bind-to]

.. include:: /prrte-rst-content/cli-bind-to.rst

[runtime-options]

.. include:: /prrte-rst-content/cli-runtime-options.rst

[rankfile]

Name of file to specify explicit task mapping
[display]

.. include:: /prrte-rst-content/cli-display.rst

[do-not-launch]

Perform all necessary operations to prepare to launch the application,
but do not actually launch it (usually used to test mapping patterns)

[mca]

.. include:: /prrte-rst-content/deprecated-mca.rst

[gmca]

.. include:: /prrte-rst-content/deprecated-gmca.rst

[xml]

.. include:: /prrte-rst-content/deprecated-xml.rst

[bind-to-core]

.. include:: /prrte-rst-content/deprecated-bind-to-core.rst

[tag-output]

.. include:: /prrte-rst-content/deprecated-tag-output.rst

[timestamp-output]

.. include:: /prrte-rst-content/deprecated-timestamp-output.rst

[output-directory]

.. include:: /prrte-rst-content/deprecated-output-directory.rst

[output-filename]

.. include:: /prrte-rst-content/deprecated-output-filename.rst

[merge-stderr-to-stdout]

.. include:: /prrte-rst-content/deprecated-merge-stderr-to-stdout.rst

[display-devel-map]

.. include:: /prrte-rst-content/deprecated-display-devel-map.rst

[display-topo]

.. include:: /prrte-rst-content/deprecated-display-topo.rst

[report-bindings]

.. include:: /prrte-rst-content/deprecated-report-bindings.rst

[display-devel-allocation]

.. include:: /prrte-rst-content/deprecated-display-devel-allocation.rst

[display-map]

.. include:: /prrte-rst-content/deprecated-display-map.rst

[display-allocation]

.. include:: /prrte-rst-content/deprecated-display-allocation.rst

[placement]

.. include:: /prrte-rst-content/detail-placement.rst

[placement-examples]

.. include:: /prrte-rst-content/detail-placement-examples.rst

[placement-rankfiles]

.. include:: /prrte-rst-content/detail-placement-rankfiles.rst

[placement-deprecated]

.. include:: /prrte-rst-content/detail-placement-deprecated.rst

[placement-diagnostics]

.. include:: /prrte-rst-content/detail-placement-diagnostics.rst

[placement-fundamentals]

.. include:: /prrte-rst-content/detail-placement-fundamentals.rst

[placement-limits]

.. include:: /prrte-rst-content/detail-placement-limits.rst

