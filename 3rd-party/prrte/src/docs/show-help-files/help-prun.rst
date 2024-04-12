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

Submit job to the PMIx Reference RTE

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

   * - ``--display <arg0>``
     - Comma-delimited list of options for displaying information
       about the allocation and job. Allowed values: ``allocation``,
       ``bind``, ``map``, ``map-devel``, ``topo``.

   * - ``--timeout <seconds>``
     - Timeout the job after the specified number of seconds

   * - ``--spawn-timeout <seconds>``
     - Timeout the job if spawn takes more than the specified number
       of seconds

   * - ``--get-stack-traces``
     - Get stack traces of all application procs on timeout

   * - ``--report-state-on-timeout``
     - Report all job and process states upon timeout

   * - ``--stop-on-exec``
     - If supported, stop each specified process at start of execution

   * - ``--stop-in-init``
     - Direct the specified processes to stop in ``PMIx_Init``

   * - ``--stop-in-app``
     - Direct the specified processes to stop at an
       application-controlled location

.. list-table::
   :header-rows: 2
   :widths: 20 45

   * -
     - Output Options

   * - Option
     - Description

   * - ``--output <args>``
     - Comma-delimited list of options that control how output is
       generated. Allowed values: ``tag``, ``tag-detailed``,
       ``tag-fullname``, ``timestamp``, ``xml``,
       ``merge-stderr-to-stdout``, ``dir=DIRNAME``,
       ``file=filename``. The ``dir`` option redirects output from
       application processes into
       ``DIRNAME/job/rank/std[out,err,diag]``. The file option
       redirects output from application processes into
       ``filename.rank``. In both cases, the provided name will be
       converted to an absolute path.  Supported qualifiers include
       ``NOCOPY`` (do not copy the output to the stdout/stderr
       streams).

   * - ``--report-child-jobs-separately``
     - Return the exit status of the primary job only

   * - ``--xterm <ranks>``
     - Create a new xterm window and display output from the specified
       ranks there

.. list-table::
   :header-rows: 2
   :widths: 20 45

   * -
     - Input Options

   * - Option
     - Description

   * - ``--stdin <ranks>``
     - Specify procs to receive stdin [``rank``, ``all``, ``none`, or
       comma-delimited list of integers] (default: ``0``, indicating
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
     - Binding policy for job

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

   * - ``-c`` | ``--np <num_procs>``
     - Number of processes to run

   * - ``-n`` | ``--n <num_procs>``
     - Number of processes to run

   * - ``-N <num_procs>``
     - Number of processes to run per node

   * - ``--app <filename>``
     - Provide an appfile; ignore all other command line options

   * - ``-H`` | ``--host <hosts>``
     - List of hosts to invoke processes on

   * - ``--hostfile <filename>``
     - Provide a hostfile

   * - ``--machinefile <filename>``
     - Provide a hostfile (synonym for ``--hostfile``)

   * - ``--path <path>``
     - PATH to be used to look for executables to start processes

   * - ``--pmixmca <key> <value>``
     - Pass context-specific PMIx MCA parameters; they are considered
       global if only one context is specified (``key`` is the
       parameter name; ``value`` is the parameter value)

   * - ``--gpmixmca <key> <value>``
     - Pass global PMIx MCA parameters that are applicable to all
       contexts (``key`` is the parameter name; ``value`` is the
       parameter value)

   * - ``--tune <filename>``
     - File(s) containing MCA params for tuning operations

   * - ``--preload-files <filenames>``
     - Preload the comma separated list of files to the remote
       machines current working directory before starting the remote
       process.

   * - ``--pset <name>``
     - User-specified name assigned to the processes in their given
       application

   * - ``--rankfile <filename>``
     - Name of file to specify explicit task mapping

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
       (e.g., ``-x foo`` exports the environment variable foo and takes
       its value from the current environment; ``-x foo=bar`` exports
       the environment variable name foo and sets its value to ``bar``
       in the started processes; ``-x foo*`` exports all current
       environmental variables starting with ``foo``)

.. list-table::
   :header-rows: 2
   :widths: 20 45

   * -
     - Specific Options

   * - Option
     - Description

   * - ``--do-not-connect``
     - Do not connect to a server

   * - ``--dvm-uri <uri>``
     - Specify the URI of the DVM master, or the name of the file
       (specified as ``file:filename``) that contains that info

   * - ``--namespace <name>``
     - Namespace of the daemon to which we should connect

   * - ``--pid <pid>``
     - PID of the daemon to which we should connect (integer PID or
       ``file:<filename>`` for file containing the PID

   * - ``--system-server-first``
     - First look for a system server and connect to it if found

   * - ``--system-server-only``
     - Connect only to a system-level server

   * - ``--tmpdir <dir>``
     - Set the root for the session directory tree

   * - ``--wait-to-connect <seconds>```
     - Delay specified number of seconds before trying to connect

   * - ``--num-connect-retries <num>```
     - Max number of times to try to connect

   * - ``--personality <name>``
     - Specify the personality to be used

   * - ``--allow-run-as-root``
     - Allow execution as root (**STRONGLY DISCOURAGED**)

   * - ``--forward-signals <signals>``
     - Comma-delimited list of additional signals (names or integers)
       to forward to application processes [``none`` => forward
       nothing]. Signals provided by default include ``SIGTSTP``,
       ``SIGUSR1``, ``SIGUSR2``, ``SIGABRT``, ``SIGALRM``, and
       ``SIGCONT``

Report bugs to %s

[dvm-uri]

Specify the URI of the DVM master, or the name of the file (specified as
``file:filename``) that contains that info

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

[tmpdir]

Define the root location for the PRRTE session directory tree

See the "Session directory" HTML documentation for additional details
about the PRRTE session directory.

[wait-to-connect]

Delay specified number of seconds before trying to connect

[hostfile]

.. include:: /prrte-rst-content/cli-launcher-hostfile.rst

See the "Host specification" HTML documentation for details about the
format and content of hostfiles.

[machinefile]

Provide a hostfile.  This option is a synonym for ``--hostfile``; see
that option for more information.

[host]

.. include:: /prrte-rst-content/cli-dash-host.rst

[parseable]

Output information (e.g., help messages) in machine-parseable
friendly format

[parsable]

Output information (e.g., help messages) in machine-parseable
friendly format

[np]

Specify number of application processes to be started

[no-ready-msg]

Do not print a DVM ready message

[daemonize]

Daemonize the DVM daemons into the background

[system-server]

Start the DVM as the system server

[set-sid]

Direct the DVM daemons to separate from the current session

[report-pid]

Printout PID on stdout [-], stderr [+], or a file [anything else]

[report-uri]

Printout URI on stdout [-], stderr [+], or a file [anything else]

[test-suicide]

Suicide instead of clean abort after delay

[singleton]

ID of the singleton process that started us

[keepalive]

Pipe to monitor - DVM will terminate upon closure

[map-by]

.. include:: /prrte-rst-content/cli-map-by.rst

[rank-by]

.. include:: /prrte-rst-content/cli-rank-by.rst

[bind-to]

.. include:: /prrte-rst-content/cli-bind-to.rst

[runtime-options]

.. include:: /prrte-rst-content/cli-runtime-options.rst

[display]

.. include:: /prrte-rst-content/cli-display.rst

[output]

.. include:: /prrte-rst-content/cli-output.rst

[rankfile]

Name of file to specify explicit task mapping

[do-not-launch]

Perform all necessary operations to prepare to launch the
application, but do not actually launch it (usually used
to test mapping patterns)

[display-devel-map]

Display a detailed process map (mostly intended for developers)
just before launch

[display-topo]

Display the topology as part of the process map (mostly intended
for developers) just before launch

[report-bindings]

Display process bindings to stderr

[display-devel-allocation]

Display a detailed list (mostly intended for developers) of the
allocation being used by this job

[display-map]

Display the process map just before launch

[display-allocation]

Display the allocation being used by this job

[enable-recovery]

Enable recovery from process failure [Default = disabled]

[max-restarts]

Max number of times to restart a failed process

[disable-recovery]

Disable recovery (resets all recovery options to off)

[continuous]

Job is to run until explicitly terminated

[personality]

Specify the personality to be used

[prte-server]

Specify the URI of the publish/lookup server, or the name of the file
(specified as ``file:<filename>`` that contains that info

[dvm-master-uri]

URI for the DVM master

[parent-uri]

URI for the parent if tree launch is enabled

[tree-spawn]

Tree-based spawn in progress

[daemonize]

Daemonize the DVM daemons into the background

[set-sid]

Direct the DVM daemons to separate from the current session

[prtemca]

.. include:: /prrte-rst-content/cli-prtemca.rst

[pmixmca]

.. include:: /prrte-rst-content/cli-pmixmca.rst

[debug-daemons-file]

.. include:: /prrte-rst-content/cli-debug-daemons-file.rst

See the "Session directory" HTML documentation for additional details
about the PRRTE session directory.

[leave-session-attached]

.. include:: /prrte-rst-content/cli-leave-session-attached.rst

[prun:executable-not-specified]

No executable was specified on the %s command line.

Aborting.

[prun:call-failed]

%s encountered a %s call failure.  This should not happen, and
usually indicates an error within the operating system itself.
Specifically, the following error occurred:

    %s

The only other available information that may be helpful is the errno
that was returned: %d.

[prun:proc-ordered-abort]

%s has exited due to process rank %lu with PID %lu on
node %s calling "abort". This may have caused other processes
in the application to be terminated by signals sent by %s
(as reported here).

[prun:proc-exit-no-sync]

%s has exited due to process rank %lu with PID %lu on
node %s exiting improperly. There are three reasons this could occur:

1. this process did not call "init" before exiting, but others in
the job did. This can cause a job to hang indefinitely while it waits
for all processes to call "init". By rule, if one process calls "init",
then ALL processes must call "init" prior to termination.

2. this process called "init", but exited without calling "finalize".
By rule, all processes that call "init" MUST call "finalize" prior to
exiting or it will be considered an "abnormal termination"

3. this process called "MPI_Abort" or "prte_abort" and the mca parameter
prte_create_session_dirs is set to false. In this case, the run-time cannot
detect that the abort call was an abnormal termination. Hence, the only
error message you will receive is this one.

This may have caused other processes in the application to be
terminated by signals sent by %s (as reported here).

You can avoid this message by specifying -quiet on the %s command line.

[prun:proc-aborted]

%s noticed that process rank %lu with PID %lu on node %s exited on signal %d.

[prun:proc-aborted-strsignal]

%s noticed that process rank %lu with PID %lu on node %s exited on signal %d (%s).

[prun:empty-prefix]

A prefix was supplied to %s that only contained slashes.

This is a fatal error; %s will now abort.  No processes were launched.

[prun:sys-limit-pipe]

%s was unable to launch the specified application as it encountered an error:

Error: system limit exceeded on number of pipes that can be open
Node: %s

when attempting to start process rank %lu.

This can be resolved by setting the mca parameter opal_set_max_sys_limits to 1,
increasing your limit descriptor setting (using limit or ulimit commands),
asking the system administrator for that node to increase the system limit, or
by rearranging your processes to place fewer of them on that node.

[prun:sys-limit-files]

%s was unable to launch the specified application as it encountered an error:

Error: system limit exceeded on number of files that can be open
Node: %s

when attempting to start process rank %lu.

This can be resolved by setting the mca parameter opal_set_max_sys_limits to 1,
increasing your limit descriptor setting (using limit or ulimit commands),
asking the system administrator for that node to increase the system limit, or
by rearranging your processes to place fewer of them on that node.

[prun:pipe-setup-failure]

%s was unable to launch the specified application as it encountered an error:

Error: pipe function call failed when setting up I/O forwarding subsystem
Node: %s

while attempting to start process rank %lu.

[prun:sys-limit-children]

%s was unable to launch the specified application as it encountered an error:

Error: system limit exceeded on number of processes that can be started
Node: %s

when attempting to start process rank %lu.

This can be resolved by either asking the system administrator for that node to
increase the system limit, or by rearranging your processes to place fewer of them
on that node.

[prun:failed-term-attrs]

%s was unable to launch the specified application as it encountered an error:

Error: reading tty attributes function call failed while setting up
I/O forwarding system
Node: %s

while attempting to start process rank %lu.

[prun:wdir-not-found]

%s was unable to launch the specified application as it could not
find the specified working directory:

Working directory: %s
Node: %s

while attempting to start process rank %lu.

[prun:wdir-not-accessible]

%s was unable to launch the specified application as it lacks
permissions to change to the specified working directory:

Working directory: %s
Node: %s

while attempting to start process rank %lu.

[prun:exe-not-found]

%s was unable to find the specified executable file, and therefore
did not launch the job.  This error was first reported for process
rank %lu; it may have occurred for other processes as well.

NOTE: A common cause for this error is misspelling a %s command
      line parameter option (remember that %s interprets the first
      unrecognized command line token as the executable).

Node:       %s
Executable: %s

[prun:exe-not-accessible]

%s was unable to launch the specified application as it lacked
permissions to execute an executable:

Executable: %s
Node: %s

while attempting to start process rank %lu.

[prun:pipe-read-failure]

%s was unable to launch the specified application as it encountered an error:

Error: reading from a pipe function call failed while spawning a local process
Node: %s

while attempting to start process rank %lu.

[prun:proc-failed-to-start]

%s was unable to start the specified application as it encountered an
error:

Error code: %d
Error name: %s
Node: %s

when attempting to start process rank %lu.

[prun:proc-failed-to-start-no-status]

%s was unable to start the specified application as it encountered an
error on node %s. More information may be available above.

[prun:ompi-server-filename-bad]

%s was unable to parse the filename where contact info for the
prte-server was to be found. The option we were given was:

--prte-server %s

This appears to be missing the required ':' following the
keyword "file". Please remember that the correct format for this
command line option is:

--prte-server file:path-to-file

where path-to-file can be either relative to the cwd or absolute.

[prun:ompi-server-filename-missing]

%s was unable to parse the filename where contact info for the
prte-server was to be found. The option we were given was:

--prte-server %s

This appears to be missing a filename following the ':'. Please
remember that the correct format for this command line option is:

--prte-server file:path-to-file

where path-to-file can be either relative to the cwd or absolute.

[prun:ompi-server-filename-access]

%s was unable to access the filename where contact info for the
prte-server was to be found. The option we were given was:

--prte-server %s

Please remember that the correct format for this command line option is:

--prte-server file:path-to-file

where path-to-file can be either relative to the cwd or absolute, and that
you must have read access permissions to that file.

[prun:ompi-server-file-bad]

%s was unable to read the prte-server's contact info from the
given filename. The filename we were given was:

FILE: %s

Please remember that the correct format for this command line option is:

--prte-server file:path-to-file

where path-to-file can be either relative to the cwd or absolute, and that
the file must have a single line in it that contains the PRTE
uri for the prte-server. Note that this is *not* a standard uri, but
a special format used internally by PRTE for communications. It can
best be generated by simply directing the prte-server to put its
uri in a file, and then giving %s that filename.


[prun:multiple-paffinity-schemes]

Multiple processor affinity schemes were specified (can only specify
one):

Slot list: %s
prte_paffinity_alone:  true

Please specify only the one desired method.

[prun:invalid-node-rank]

An invalid node rank was obtained - this is probably something
that should be reported to the PRRTE developers.

[prun:invalid-local-rank]

An invalid local rank was obtained - this is probably something
that should be reported to the PRRTE developers.

[prun:invalid-phys-cpu]

An invalid physical processor id was returned when attempting to
set processor affinity - please check to ensure that your system
supports such functionality. If so, then this is probably something
that should be reported to the PRRTE developers.

[prun:topo-not-supported]

An attempt was made to bind a process to a specific hardware topology
mapping (e.g., binding to a package) but the operating system does not
support such topology-aware actions.  Talk to your local system
administrator to find out if your system can support topology-aware
functionality (e.g., Linux Kernels newer than v2.6.18).

Systems that do not support processor topology-aware functionality
cannot use "bind to package" and other related functionality.

  Local host:        %s
  Action attempted:  %s %s
  Application name:  %s

[prun:not-enough-resources]

Not enough %s were found on the local host to meet the requested
binding action:

  Local host:        %s
  Action requested:  %s
  Application name:  %s

Please revise the request and try again.

[prun:invalid-slot-list-range]

A slot list was provided that exceeds the boundaries on available
resources:

  Local host: %s
  Slot list:  %s

Please check your boundaries and try again.

[prun:proc-comm-failed]

A critical communication path was lost to:

  My name:      %s
  Process name: %s
  Node:         %s

[prun:proc-mem-exceeded]

A process exceeded memory limits:

  Process name: %s
  Node:         %s

[prun:proc-stalled]

One or more processes appear to have stalled - a monitored file
failed to show the required activity.

[prun:proc-sensor-exceeded]

One or more processes have exceeded a specified sensor limit, but
no further info is available.

[prun:proc-heartbeat-failed]

%s failed to receive scheduled heartbeat communications from a remote
process:

  Process name: %s
  Node:         %s

[prun:non-zero-exit]

%s detected that one or more processes exited with non-zero status, thus causing
the job to be terminated. The first process to do so was:

  Process name: %s
  Exit code:    %d

[prun:negative-nprocs]

%s has detected that one or more applications was given a negative
number of processes to run:

  Application:  %s
  Num procs:    %d

Please correct this value and try again.

[prun:timeout]

The user-provided time limit for job execution has been reached:

  Timeout: %d seconds

The job will now be aborted.  Please check your code and/or
adjust/remove the job execution time limit (as specified by --timeout
command line option or MPIEXEC_TIMEOUT environment variable).

[prun:conflict-env-set]

ERROR: You have attempted to pass environment variables to PRTE
with both the "-x" method and by setting the MCA parameter "mca_base_env_list".

PRTE does not support mixing these two methods.  Please choose one
method and try launching your job again.

Your job will now abort.

[prun:pmix-failed]

The call to pmix_init_server() failed. This may be due to your
system's restriction for Unix's socket's path-length.

   prte_proc_session_dir: %s

Please try to set TMPDIR to something short (like /tmp) or change
Your computer's name (see uname -n).

[prun:timeoutconflict]

Conflicting requests for timeout were given:

  --timeout command line option: %d
  MPIEXEC_TIMEOUT envar:         %s

Only one method should be provided, or else they must agree. Please
correct and retry.

[prun:stop-on-exec]

%s was unable to stop the executable at first instruction:

  Error:     %s
  Nodename:  %s
  Rank:      %lu

[use-pterm]

Use of %s to terminate the PRTE DVM has been deprecated. Please use
the "pterm" tool instead in the future.

[bad-pause-for-tool]

%s detected the presence of the PMIX_LAUNCHER_PAUSE_FOR_TOOL environmental
variable, but the value of the variable is in an improper form:

  PMIX_LAUNCHER_PAUSE_FOR_TOOL:  %s

The variable must be of the form (nspace:rank) of the tool requesting that
%s pause for it to connect. Please reset the value of the variable and try
again.

[prun:executable-incorrectly-given]

The %s command was given with an application specified. %s is only used
to start the persistent DVM - it cannot be used with an application.

[bad-option-input]

%s was given an option that expected a string argument:

  option: %s
  argument: %s
  expected: %s

Please correct the option and try again.

[file-open-error]

%s was unable to open the specified file provided as an option:

  option: %s
  argument: %s
  file: %s

Please correct the option and try again.

[bad-file]

%s was unable to read the necessary info from the provided file:

  option: %s
  argument: %s
  file: %s

Please correct the option or the file and try again.

[bad-dvm-option]

The --dvm <arg> option was provided, but the argument
is one not recognized:

  arg: %s

Please see "%s --help dvm" for a list of recognized
options and examples

[appfile-failure]

The --app <arg> option was provided, but the specified
file was not found or could not be opened:

  file: %s

Please correct the option and try again.
