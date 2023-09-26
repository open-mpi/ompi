.. _man1-mpirun:
.. _man1-mpiexec:


mpirun / mpiexec
================

.. include_body

mpirun, mpiexec |mdash| Execute serial and parallel jobs in Open MPI.

.. note:: ``mpirun`` and ``mpiexec`` are synonyms for each other.
          Indeed, they are symbolic links to the same executable.
          Using either of the names will produce the exact same
          behavior.

SYNOPSIS
--------

Single Process Multiple Data (SPMD) Model:

.. code::

   mpirun [ options ] <program> [ <args> ]

Multiple Instruction Multiple Data (MIMD) Model:

.. code::

   mpirun [ global_options ]
          [ local_options1 ] <program1> [ <args1> ] :
          [ local_options2 ] <program2> [ <args2> ] :
          ... :
          [ local_optionsN ] <programN> [ <argsN> ]

Note that in both models, invoking ``mpirun`` via an absolute path
name is equivalent to specifying the ``--prefix`` option with a
``<dir>`` value equivalent to the directory where ``mpirun`` resides,
minus its last subdirectory.  For example:

.. code:: sh

   shell$ /usr/local/bin/mpirun ...

is equivalent to

.. code:: sh

   shell$ mpirun --prefix /usr/local

QUICK SUMMARY
-------------

If you are simply looking for how to run an MPI application, you
probably want to use a command line of the following form:

.. code:: sh

   shell$ mpirun [ -n X ] [ --hostfile <filename> ]  <program>

This will run ``X`` copies of ``<program>`` in your current run-time
environment (if running under a supported resource manager, Open MPI's
``mpirun`` will usually automatically use the corresponding resource
manager process starter, as opposed to ``ssh`` (for example), which
require the use of a hostfile, or will default to running all ``X``
copies on the localhost), scheduling (by default) in a round-robin
fashion by CPU slot.  See the rest of this documentation for more
details.

Please note that ``mpirun`` automatically binds processes to hardware
resources. Three binding patterns are used in the absence of any
further directives (See :ref:`map/rank/bind defaults
<man1-mpirun-map-rank-bind-defaults>` for more details):

* **Bind to core**:     when the number of processes is <= 2
* **Bind to package**:  when the number of processes is > 2
* **Bind to none**:     when oversubscribed

If your application uses threads, then you probably want to ensure
that you are either not bound at all (by specifying ``--bind-to none``),
or bound to multiple cores using an appropriate binding level or
specific number of processing elements per application process.

OPEN MPI'S USE OF PRRTE
-----------------------

Open MPI uses the PMIx Reference Runtime Environment (PRRTE) as the
main engine for launching, monitoring, and terminating MPI processes.

Much of the documentation below is directly imported from PRRTE.  As
such, it frequently refers to PRRTE concepts and command line options.
Except where noted, these concepts and command line argument are all
applicable to Open MPI as well.  Open MPI extends the available PRRTE
command line options, and also slightly modifies the PRRTE's default
behaviors in a few cases.  These will be specifically described in the
docuemtnation below.

COMMAND LINE OPTIONS
--------------------

The core of Open MPI's ``mpirun`` processing is performed via the
`PRRTE <https://docs.prrte.org/>`_.  Specifically: ``mpirun`` is
effectively a wrapper around ``prterun``, but ``mpirun``'s CLI options
are slightly different than PRRTE's CLI commands.

.. include:: /schizo-ompi-rst-content/schizo-ompi-cli.rstxt

OPTIONS (OLD / HARD-CODED CONTENT -- TO BE AUDITED
--------------------------------------------------

.. admonition:: This is old content
   :class: error

   This is the old section of manually hard-coded content.  It should
   probably be read / audited and see what we want to keep and what we
   want to discard.

   Feel free to refer to https://docs.prrte.org/ rather than
   replicating content here (e.g., for the definition of a slot and
   other things).

mpirun will send the name of the directory where it was invoked on the
local node to each of the remote nodes, and attempt to change to that
directory.  See the "Current Working Directory" section below for
further details.

* ``<program>``: The program executable. This is identified as the
  first non-recognized argument to mpirun.

* ``<args>``: Pass these run-time arguments to every new process.
  These must always be the last arguments to mpirun. If an app context
  file is used, ``<args>`` will be ignored.

* ``-h``, ``--help``: Display help for this command

* ``-q``, ``--quiet``: Suppress informative messages from orterun
  during application execution.

* ``-v``, ``--verbose``:` Be verbose

* ``-V``, ``--version``: Print version number.  If no other arguments
  are given, this will also cause orterun to exit.

* ``-N <num>``: Launch num processes per node on all allocated nodes
  (synonym for ``--npernode``).

* ``--display-map``: Display a table showing the mapped location of
  each process prior to launch.

* ``--display-allocation``: Display the detected resource allocation.

* ``--output-proctable``: Output the debugger proctable after launch.

* ``--dvm``: Create a persistent distributed virtual machine (DVM).

* ``--max-vm-size <size>``: Number of daemons to start.

Use one of the following options to specify which hosts (nodes) of the
cluster to run on. Note that as of the start of the v1.8 release,
mpirun will launch a daemon onto each host in the allocation (as
modified by the following options) at the very beginning of execution,
regardless of whether or not application processes will eventually be
mapped to execute there. This is done to allow collection of hardware
topology information from the remote nodes, thus allowing us to map
processes against known topology. However, it is a change from the
behavior in prior releases where daemons were only launched after
mapping was complete, and thus only occurred on nodes where
application processes would actually be executing.

* ``-H``, ``--host <host1,host2,...,hostN>``: list of hosts on which to
  invoke processes.

* ``--hostfile <hostfile>``: Provide a hostfile to use.

* ``--default-hostfile <hostfile>``: Provide a default hostfile.

* ``--machinefile <machinefile>``: Synonym for ``--hostfile``.

* ``--cpu-set <list>``: Restrict launched processes to the specified
  logical CPUs on each node (comma-separated list). Note that the
  binding options will still apply within the specified envelope
  |mdash| e.g., you can elect to bind each process to only one CPU
  within the specified CPU set.

The following options specify the number of processes to launch. Note
that none of the options imply a particular binding policy |mdash| e.g.,
requesting N processes for each package does not imply that the
processes will be bound to the package.

* ``-n``, ``--n``, ``-c``, ``-np <#>``: Run this many copies of the
  program on the given nodes.  This option indicates that the
  specified file is an executable program and not an application
  context. If no value is provided for the number of copies to execute
  (i.e., neither the ``-n`` nor its synonyms are provided on the
  command line), Open MPI will automatically execute a copy of the
  program on each process slot (see PRRTE's `defintion of "slot"
  <https://docs.prrte.org/en/latest/placement/overview.html#definition-of-slot>`_
  for description of a "process slot"). This feature, however, can
  only be used in the SPMD model and will return an error (without
  beginning execution of the application) otherwise.

  .. note:: The ``-n`` option is the preferred option to be used to specify the
            number of copies of the program to be executed, but the alternate
            options are also accepted.


* ``--map-by ppr:N:<object>``: Launch N times the number of objects of
  the specified type on each node.

* ``--npersocket <#persocket>``: On each node, launch this many
  processes times the number of processor sockets on the node.
  The -npersocket option also turns on the ``--bind-to-socket``
  option.  (deprecated in favor of ``--map-by ppr:n:package``)

* ``--npernode <#pernode>``: On each node, launch this many processes.
  (deprecated in favor of ``--map-by ppr:n:node``).

* ``--pernode``: On each node, launch one process |mdash| equivalent to
  ``--npernode 1``.  (deprecated in favor of ``--map-by ppr:1:node``)

To map processes:

* ``--map-by <object>``: Map to the specified object, defaults to
  ``package``. Supported options include ``slot``, ``hwthread``, ``core``,
  ``L1cache``, ``L2cache``, ``L3cache``, ``package``, ``numa``,
  ``node``, ``seq``, ``rankfile``, ``pe-list=#``, and ``ppr``.
  Any object can include modifiers by adding a ``:`` and any combination
  of the following:

    * ``pe=n``: bind ``n`` processing elements to each proc
    * ``span``: load balance the processes across the allocation
    * ``oversubscribe``: allow more processes on a node than processing elements
    * ``nooversubscribe``: do *not* allow more processes on a node than processing elements (default)
    * ``nolocal``: do not place processes on the same host as the ``mpirun`` process
    * ``hwtcpus``: use hardware threads as CPU slots for mapping
    * ``corecpus``: use processor cores as CPU slots for mapping (default)
    * ``file=filename``: used with ``rankfile``; use ``filename`` to specify the file to use
    * ``ordered``: used with ``pe-list`` to bind each process to one of the specified processing elements

  .. note:: ``socket`` is also accepted as an alias for ``package``.

* ``--bycore``: Map processes by core (deprecated in favor of
  ``--map-by core``).

* ``--byslot``: Map and rank processes round-robin by slot (deprecated
  in favor of ``--map-by slot``).

* ``--nolocal``: Do not run any copies of the launched application on
  the same node as orterun is running.  This option will override
  listing the localhost with ``--host`` or any other host-specifying
  mechanism. Alias for ``--map-by :nolocal``.

* ``--nooversubscribe``: Do not oversubscribe any nodes; error
  (without starting any processes) if the requested number of
  processes would cause oversubscription.  This option implicitly sets
  "max_slots" equal to the "slots" value for each node. (Enabled by
  default). Alias for ``--map-by :nooversubscribe``.

* ``--oversubscribe``: Nodes are allowed to be oversubscribed, even on
  a managed system, and overloading of processing elements.
  Alias for ``--map-by :oversubscribe``.

* ``--bynode``: Launch processes one per node, cycling by node in a
  round-robin fashion.  This spreads processes evenly among nodes and
  assigns MPI_COMM_WORLD ranks in a round-robin, "by node" manner.
  (deprecated in favor of ``--map-by node``)

* ``--cpu-list <cpus>``: Comma-delimited list of processor IDs to
  which to bind processes [default=``NULL``].  Processor IDs are
  interpreted as hwloc logical core IDs.

  .. note:: You can run Run the hwloc ``lstopo(1)`` command to see a
            list of available cores and their logical IDs.

To order processes' ranks in MPI_COMM_WORLD:

* ``--rank-by <mode>``: Rank in round-robin fashion according to the
  specified mode, defaults to slot. Supported options include
  ``slot``, ``node``, ``fill``, and ``span``.

For process binding:

* ``--bind-to <object>``: Bind processes to the specified object,
  defaults to ``core``.  Supported options include ``slot``,
  ``hwthread``, ``core``, ``l1cache``, ``l2cache``, ``l3cache``,
  ``package``, ``numa``, and ``none``.

* ``--cpus-per-proc <#perproc>``: Bind each process to the specified
  number of cpus.  (deprecated in favor of ``--map-by <obj>:PE=n``)

* ``--cpus-per-rank <#perrank>``: Alias for ``--cpus-per-proc``.
  (deprecated in favor of ``--map-by <obj>:PE=n``)

* ``--bind-to-core`` Bind processes to cores (deprecated in favor of
  ``--bind-to core``)

* ``--bind-to-socket``: Bind processes to processor sockets
  (deprecated in favor of ``--bind-to package``)

* ``--report-bindings``: Report any bindings for launched processes.

For rankfiles:

* ``--rankfile <rankfile>``: Provide a rankfile file.
  (deprecated in favor of ``--map-by rankfile:file=FILE``)

To manage standard I/O:

* ``--output-filename <filename>``: Redirect the stdout, stderr, and
  stddiag of all processes to a process-unique version of the
  specified filename. Any directories in the filename will
  automatically be created.  Each output file will consist of
  ``filename.id``, where the ``id`` will be the processes' rank in
  MPI_COMM_WORLD, left-filled with zero's for correct ordering in
  listings. A relative path value will be converted to an absolute
  path based on the cwd where mpirun is executed. Note that this will
  not work on environments where the file system on compute nodes
  differs from that where :ref:`mpirun(1) <man1-mpirun>` is
  executed.

* ``--stdin <rank>``: The MPI_COMM_WORLD rank of the process that is
  to receive stdin.  The default is to forward stdin to MPI_COMM_WORLD
  rank 0, but this option can be used to forward stdin to any
  process. It is also acceptable to specify none, indicating that no
  processes are to receive stdin.

* ``--merge-stderr-to-stdout``: Merge stderr to stdout for each
  process.

* ``--tag-output``: Tag each line of output to stdout, stderr, and
  stddiag with ``[jobid, MCW_rank]<stdxxx>`` indicating the process
  jobid and MPI_COMM_WORLD rank of the process that generated the
  output, and the channel which generated it.

* ``--timestamp-output``: Timestamp each line of output to stdout,
  stderr, and stddiag.

* ``--xml``: Provide all output to stdout, stderr, and stddiag in an
  XML format.

* ``--xml-file <filename>`` Provide all output in XML format to the
  specified file.

* ``--xterm <ranks>``: Display the output from the processes
  identified by their MPI_COMM_WORLD ranks in separate xterm
  windows. The ranks are specified as a comma-separated list of
  ranges, with a ``-1`` indicating all. A separate window will be created
  for each specified process.

  .. note:: xterm will normally terminate the window upon termination
            of the process running within it. However, by adding a
            ``!`` to the end of the list of specified ranks, the
            proper options will be provided to ensure that xterm keeps
            the window open after the process terminates, thus
            allowing you to see the process' output.  Each xterm
            window will subsequently need to be manually closed.
            Note: In some environments, xterm may require that the
            executable be in the user's path, or be specified in
            absolute or relative terms. Thus, it may be necessary to
            specify a local executable as ``./my_mpi_app`` instead of just
            ``my_mpi_app``. If xterm fails to find the executable, ``mpirun``
            will hang, but still respond correctly to a ctrl-C.  If
            this happens, please check that the executable is being
            specified correctly and try again.

To manage files and runtime environment:

* ``--path <path>``: ``<path>`` that will be used when attempting to
  locate the requested executables.  This is used prior to using the
  local ``PATH`` environment variable setting.

* ``--prefix <dir>``: Prefix directory that will be used to set the
  ``PATH`` and ``LD_LIBRARY_PATH`` on the remote node before invoking
  Open MPI or the target process.  See the :ref:`Remote Execution
  <man1-mpirun-remote-execution>` section, below.

* ``--noprefix``: Disable the automatic ``--prefix`` behavior

* ``--preload-binary``: Copy the specified executable(s) to remote
  machines prior to starting remote processes. The executables will be
  copied to the Open MPI session directory and will be deleted upon
  completion of the job.

* ``--preload-files <files>``: Preload the comma-separated list of
  files to the current working directory of the remote machines where
  processes will be launched prior to starting those processes.

* ``--set-cwd-to-session-dir``: Set the working directory of the
  started processes to their session directory.

* ``--wd <dir>``: Synonym for ``-wdir``.

* ``--wdir <dir>``: Change to the directory ``<dir>`` before the
  user's program executes.  See the :ref:`Current Working Directory
  <man1-mpirun-current-working-directory>` section for notes on
  relative paths.  Note: If the ``--wdir`` option appears both on the
  command line and in an application context, the context will take
  precedence over the command line. Thus, if the path to the desired
  wdir is different on the backend nodes, then it must be specified as
  an absolute path that is correct for the backend node.

* ``-x <env>``: Export the specified environment variables to the
  remote nodes before executing the program.  Only one environment
  variable can be specified per ``-x`` option.  Existing environment
  variables can be specified or new variable names specified with
  corresponding values.  For example:

  .. code:: sh

     shell$ mpirun -x DISPLAY -x OFILE=/tmp/out ...

  The parser for the ``-x`` option is not very sophisticated; it does
  not even understand quoted values.  Users are advised to set
  variables in the environment, and then use ``-x`` to export (not
  define) them.

Setting MCA parameters:

* ``--gmca <key> <value>``: Pass global MCA parameters that are
  applicable to all contexts.  ``<key>`` is the parameter name;
  ``<value>`` is the parameter value.

* ``--mca <key> <value>``: Send arguments to various MCA modules.  See
  the :ref:`Setting MCA Parameters
  <man1-mpirun-setting-mca-parameters>` section for mode details.

* ``--tune <tune_file>``: Specify a tune file to set arguments for
  various MCA modules and environment variables.  See the :ref:`
  Setting MCA parameters and environment variables from file
  <man1-mpirun-setting-mca-params-from-file>`. ``--am <arg>`` is an alias for ``--tune <arg>``.

For debugging:

* ``--get-stack-traces``: When paired with the ``--timeout`` option,
  ``mpirun`` will obtain and print out stack traces from all launched
  processes that are still alive when the timeout expires.  Note that
  obtaining stack traces can take a little time and produce a lot of
  output, especially for large process-count jobs.

* ``--timeout <seconds>``: The maximum number of seconds that
  ``mpirun`` will run.  After this many seconds, ``mpirun`` will abort
  the launched job and exit with a non-zero exit status.  Using
  ``--timeout`` can be also useful when combined with the
  ``--get-stack-traces`` option.

There are also other options:

* ``--allow-run-as-root``: Allow ``mpirun`` to run when executed by
  the root user (``mpirun`` defaults to aborting when launched as the
  root user).  Be sure to see the :ref:`Running as root
  <man1-mpirun-running-as-root>` section for more detail.

* ``--app <appfile>``: Provide an appfile, ignoring all other command
  line options.

* ``--continuous``: Job is to run until explicitly terminated.

* ``--disable-recovery``: Disable recovery (resets all recovery
  options to off).

* ``--do-not-launch``: Perform all necessary operations to prepare to
  launch the application, but do not actually launch it.

* ``--enable-recovery``: Enable recovery from process failure (default:
  disabled)

* ``--leave-session-attached``: Do not detach back-end daemons used by
  this application. This allows error messages from the daemons as
  well as the underlying environment (e.g., when failing to launch a
  daemon) to be output.

* ``--max-restarts <num>``: Max number of times to restart a failed
  process.

* ``--personality <list>``: Comma-separated list of programming model,
  languages, and containers being used (default=``ompi``).

* ``--ppr <list>``: Comma-separated list of number of processes on a
  given resource type (default: none). Alias for ``--map-by ppr:N:OBJ``.

* ``--report-child-jobs-separately``: Return the exit status of the
  primary job only.

* ``--report-events <URI>``: Report events to a tool listening at the
  specified URI.

* ``--report-pid <channel>``: Print out ``mpirun``'s PID during
  startup. The channel must be either a ``-`` to indicate that the PID
  is to be output to stdout, a ``+`` to indicate that the PID is to be
  output to stderr, or a filename to which the PID is to be written.

* ``--report-uri <channel>``: Print out ``mpirun``'s URI during
  startup. The channel must be either a ``-`` to indicate that the URI
  is to be output to stdout, a ``+`` to indicate that the URI is to be
  output to stderr, or a filename to which the URI is to be written.

* ``--show-progress``: Output a brief periodic report on launch
  progress.

* ``--terminate``: Terminate the DVM.

* ``--use-hwthread-cpus``: Use hardware threads as independent CPUs.

  Note that if a number of slots is not provided to Open MPI (e.g.,
  via the ``slots`` keyword in a hostfile or from a resource manager
  such as Slurm), the use of this option changes the default
  calculation of number of slots on a node.  See the PRRTE's
  `defintion of "slot"
  <https://docs.prrte.org/en/latest/placement/overview.html#definition-of-slot>`_
  for more details.

  Also note that the use of this option changes the Open MPI's
  definition of a "processor element" from a processor core to a
  hardware thread.  See
  PRRTE's `defintion of a "processor element"
  <https://docs.prrte.org/en/latest/placement/overview.html#definition-of-pe>`_
  for more details.

The following options are useful for developers; they are not
generally useful to most Open MPI users:

* ``--debug-daemons``: Enable debugging of the run-time daemons used
  by this application.

* ``--debug-daemons-file``: Enable debugging of the run-time daemons
  used by this application, storing output in files.

* ``--display-devel-map``: Display a more detailed table showing the
  mapped location of each process prior to launch.

* ``--display-topo``: Display the topology as part of the process map
  just before launch.

* ``--launch-agent``: Name of the executable that is to be used to
  start processes on the remote nodes. The default is ``PRRTEd``. This
  option can be used to test new daemon concepts, or to pass options
  back to the daemons without having mpirun itself see them. For
  example, specifying a launch agent of ``PRRTEd -mca odls_base_verbose
  5`` allows the developer to ask the ``PRRTEd`` for debugging output
  without clutter from ``mpirun`` itself.

* ``--report-state-on-timeout``: When paired with the ``--timeout``
  command line option, report the run-time subsystem state of each
  process when the timeout expires.

There may be other options listed with ``mpirun --help``.

Environment Variables
^^^^^^^^^^^^^^^^^^^^^

.. admonition:: This is old, hard-coded content
   :class: error

   Is this content still current / accurate?  Should it be updated and
   retained, or removed?

* ``MPIEXEC_TIMEOUT``: Synonym for the ``--timeout`` command line option.

DESCRIPTION
-----------

.. admonition:: This is old, hard-coded content
   :class: error

   Is this content still current / accurate?  Should it be updated and
   retained, or removed?

One invocation of ``mpirun`` starts an MPI application running under Open
MPI. If the application is single process multiple data (SPMD), the
application can be specified on the ``mpirun`` command line.

If the application is multiple instruction multiple data (MIMD),
comprising of multiple programs, the set of programs and argument can
be specified in one of two ways: Extended Command Line Arguments, and
Application Context.

An application context describes the MIMD program set including all
arguments in a separate file.  This file essentially contains multiple
mpirun command lines, less the command name itself.  The ability to
specify different options for different instantiations of a program is
another reason to use an application context.

Extended command line arguments allow for the description of the
application layout on the command line using colons (``:``) to
separate the specification of programs and arguments. Some options are
globally set across all specified programs (e.g., ``--hostfile``),
while others are specific to a single program (e.g., ``-n``).

Specifying Host Nodes
^^^^^^^^^^^^^^^^^^^^^

.. admonition:: This is old, hard-coded content
   :class: error

   Is this content still current / accurate?  Should it be updated and
   retained, or removed?

Host nodes can be identified on the ``mpirun`` command line with the
``--host`` option or in a hostfile.

For example:

.. code:: sh

   shell$ mpirun -H aa,aa,bb ./a.out

Launches two processes on node ``aa`` and one on ``bb``.

Or, consider the hostfile:

.. code:: sh

   shell$ cat myhostfile
   aa slots=2
   bb slots=2
   cc slots=2

Here, we list both the host names (``aa``, ``bb``, and ``cc``) but
also how many slots there are for each.

.. code:: sh

   shell$ mpirun --hostfile myhostfile ./a.out

will launch two processes on each of the three nodes.

.. code:: sh

   shell$ mpirun --hostfile myhostfile --host aa ./a.out

will launch two processes, both on node ``aa``.

.. code:: sh

   shell$ mpirun --hostfile myhostfile --host dd ./a.out

will find no hosts to run on and will abort with an error.  That is,
the specified host ``dd`` is not in the specified hostfile.

When running under resource managers (e.g., Slurm, Torque, etc.), Open
MPI will obtain both the hostnames and the number of slots directly
from the resource manager.

Specifying Number of Processes
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. admonition:: This is old, hard-coded content
   :class: error

   Is this content still current / accurate?  Should it be updated and
   retained, or removed?

As we have just seen, the number of processes to run can be set using the
hostfile.  Other mechanisms exist.

The number of processes launched can be specified as a multiple of the
number of nodes or processor packages available.  For example,

.. code:: sh

   shell$ mpirun -H aa,bb --map-by ppr:2:package ./a.out

launches processes 0-3 on node ``aa`` and process 4-7 on node ``bb``
(assuming ``aa`` and ``bb`` both contain 4 slots each).

.. code:: sh

   shell$ mpirun -H aa,bb --map-by ppr:2:node ./a.out

launches processes 0-1 on node ``aa`` and processes 2-3 on node ``bb``.

.. code:: sh

   shell$ mpirun -H aa,bb --map-by ppr:1:node ./a.out

launches one process per host node.

.. code:: sh

   mpirun -H aa,bb --pernode ./a.out

is the same as ``--map-by ppr:1:node`` and ``--npernode 1``.

Another alternative is to specify the number of processes with the ``-n``
option.  Consider now the hostfile:

.. code:: sh

   shell$ cat myhostfile
   aa slots=4
   bb slots=4
   cc slots=4

Now run with ``myhostfile``:

.. code:: sh

   shell$ mpirun --hostfile myhostfile -n 6 ./a.out

will launch processes 0-3 on node ``aa`` and processes 4-5 on node
``bb``.  The remaining slots in the hostfile will not be used since
the ``-n`` option indicated that only 6 processes should be launched.

Mapping Processes to Nodes: Using Policies
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. admonition:: This is old, hard-coded content
   :class: error

   Is this content still current / accurate?  Should it be updated and
   retained, or removed?

The examples above illustrate the default mapping of process processes
to nodes.  This mapping can also be controlled with various ``mpirun``
options that describe mapping policies.

Consider the same hostfile as above, again with ``-n 6``.  The table
below lists a few ``mpirun`` variations, and shows which
MPI_COMM_WORLD ranks end up on which node:

.. list-table::
   :header-rows: 1

   * - Command
     - Node ``aa``
     - Node ``bb``
     - Node ``cc``

   * - ``mpirun``
     - 0 1 2 3
     - 4 5
     -

   * - ``mpirun --map-by node``
     - 0 3
     - 1 4
     - 2 5

   * - ``mpirun --nolocal``
     -
     - 0 1 2 3
     - 4 5

The ``--map-by node`` option will load balance the processes across the
available nodes, numbering each process in a round-robin fashion.

The ``--nolocal`` option prevents any processes from being mapped onto
the local host (in this case node ``aa``).  While ``mpirun`` typically
consumes few system resources, ``--nolocal`` can be helpful for
launching very large jobs where mpirun may actually need to use
noticeable amounts of memory and/or processing time.

Just as ``-n`` can specify fewer processes than there are slots, it
can also oversubscribe the slots.  For example, with the same
hostfile:

.. code:: sh

   shell$ mpirun --hostfile myhostfile -n 14 ./a.out

will launch processes 0-3 on node ``aa``, 4-7 on ``bb``, and 8-11 on
``cc``.  It will then add the remaining two processes to whichever
nodes it chooses.

One can also specify limits to oversubscription.  For example, with the
same hostfile:

.. code:: sh

   shell$ mpirun --hostfile myhostfile -n 14 --nooversubscribe ./a.out

will produce an error since ``--nooversubscribe`` prevents
oversubscription.

Limits to oversubscription can also be specified in the hostfile
itself:

.. code:: sh

   shell$ cat myhostfile
   aa slots=4 max_slots=4
   bb         max_slots=4
   cc slots=4

The ``max_slots`` field specifies such a limit.  When it does, the slots
value defaults to the limit.  Now:

.. code:: sh

   shell$ mpirun --hostfile myhostfile -n 14 ./a.out

causes the first 12 processes to be launched as before, but the
remaining two processes will be forced onto node ``cc``.  The other
two nodes are protected by the hostfile against oversubscription by
this job.

Using the ``--nooversubscribe`` option can be helpful since Open MPI
currently does not get ``max_slots`` values from the resource manager.

Of course, ``-n`` can also be used with the ``-H`` or ``-host``
option.  For example:

.. code:: sh

   shell$ mpirun -H aa,bb -n 8 ./a.out

launches 8 processes.  Since only two hosts are specified, after the
first two processes are mapped, one to ``aa`` and one to ``bb``, the
remaining processes oversubscribe the specified hosts.

And here is a MIMD example:

.. code:: sh

   shell$ mpirun -H aa -n 1 hostname : -H bb,cc -n 2 uptime

will launch process 0 running hostname on node ``aa`` and processes 1
and 2 each running uptime on nodes ``bb`` and ``cc``, respectively.

.. _man1-mpirun-map-rank-bind:

Mapping, Ranking, and Binding: Oh My!
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. admonition:: This is old, hard-coded content
   :class: error

   Is this content still current / accurate?  Should it be updated and
   retained, or removed?

Open MPI employs a three-phase procedure for assigning process locations
and ranks:

#. **Mapping**: Assigns a default location to each process
#. **Ranking**: Assigns an MPI_COMM_WORLD rank value to each process
#. **Binding**: Constrains each process to run on specific processors

The mapping step is used to assign a default location to each process
based on the mapper being employed. Mapping by slot, node, and
sequentially results in the assignment of the processes to the node
level. In contrast, mapping by object, allows the mapper to assign the
process to an actual object on each node.

Note that the location assigned to the process is independent of where
it will be bound |mdash| the assignment is used solely as input to the
binding algorithm.

The mapping of process processes to nodes can be defined not just with
general policies but also, if necessary, using arbitrary mappings that
cannot be described by a simple policy.  One can use the "sequential
mapper," which reads the hostfile line by line, assigning processes to
nodes in whatever order the hostfile specifies.  Use the ``---map-by seq`` option.  For example, using the same hostfile as before:

.. code:: sh

   shell$ mpirun -hostfile myhostfile --map-by seq ./a.out

will launch three processes, one on each of nodes ``aa``, ``bb``, and ``cc``,
respectively.  The slot counts don't matter; one process is launched
per line on whatever node is listed on the line.

Another way to specify arbitrary mappings is with a rankfile, which
gives you detailed control over process binding as well.  Rankfiles
are discussed :ref:`below <man1-mpirun-rankfiles>`.

The second phase focuses on the ranking of the process within the
job's MPI_COMM_WORLD.  Open MPI separates this from the mapping
procedure to allow more flexibility in the relative placement of MPI
processes. This is best illustrated by considering the following
cases where we used the ``--np 8 --map-by ppr:2:package --host aa:4,bb:4`` option:

.. list-table::
   :header-rows: 1

   * - Option
     - Node ``aa``
     - Node ``bb``

   * - ``--rank-by fill`` (i.e., dense packing) Default
     - 0 1 | 2 3
     - 4 5 | 6 7

   * - ``--rank-by span`` (i.e., sparse or load balanced packing)
     - 0 4 | 1 5
     - 2 6 | 3 7

   * - ``--rank-by node``
     - 0 2 | 4 6
     - 1 3 | 5 7

Ranking by ``fill`` assigns MCW ranks in a simple progression across each
node. Ranking by ``span`` and by ``slot`` provide the identical
result |mdash| a round-robin progression of the packages across all nodes
before returning to the first package on the first node. Ranking by
``node`` assigns MCW ranks iterating first across nodes then by package.

The binding phase actually binds each process to a given set of
processors. This can improve performance if the operating system is
placing processes suboptimally.  For example, it might oversubscribe
some multi-core processor packages, leaving other packages idle; this
can lead processes to contend unnecessarily for common resources.  Or,
it might spread processes out too widely; this can be suboptimal if
application performance is sensitive to interprocess communication
costs.  Binding can also keep the operating system from migrating
processes excessively, regardless of how optimally those processes
were placed to begin with.

The processors to be used for binding can be identified in terms of
topological groupings |mdash| e.g., binding to an ``l3cache`` will
bind each process to all processors within the scope of a single L3
cache within their assigned location. Thus, if a process is assigned
by the mapper to a certain package, then a ``--bind-to l3cache``
directive will cause the process to be bound to the processors that
share a single L3 cache within that package.

Alternatively, processes can be mapped and bound to specified cores using
the ``--map-by pe-list=`` option. For example, ``--map-by pe-list=0,2,5``
will map three processes all three of which will be bound to logical cores
``0,2,5``. If you intend to bind each of the three processes to different
cores then the ``:ordered`` qualifier can be used like
``--map-by pe-list=0,2,5:ordered``. In this example, the first process
on a node will be bound to CPU 0, the second process on the node will
be bound to CPU 2, and the third process on the node will be bound to
CPU 5.

Finally, ``--report-bindings`` can be used to report bindings.

As an example, consider a node with two processor packages, each
comprised of four cores, and each of those cores contains one hardware
thread.  The ``--report-bindings`` option shows the binding of each process in a
descriptive manner. Below are some examples.

.. code::

   shell$ mpirun --np 4 --report-bindings --map-by core --bind-to core
   [...] Rank 0 bound to package[0][core:0]
   [...] Rank 1 bound to package[0][core:1]
   [...] Rank 2 bound to package[0][core:2]
   [...] Rank 3 bound to package[0][core:3]

In the above case, the processes bind to successive cores.

.. code::

   shell$ mpirun --np 4 --report-bindings --map-by package --bind-to package
   [...] Rank 0 bound to package[0][core:0-3]
   [...] Rank 1 bound to package[0][core:0-3]
   [...] Rank 2 bound to package[1][core:4-7]
   [...] Rank 3 bound to package[1][core:4-7]

In the above case, processes bind to all cores on successive packages.
The processes cycle through the processor packages in a
round-robin fashion as many times as are needed. By default, the processes
are ranked in a ``fill`` manner.

.. code::

   shell$ mpirun --np 4 --report-bindings --map-by package --bind-to package --rank-by span
   [...] Rank 0 bound to package[0][core:0-3]
   [...] Rank 1 bound to package[1][core:4-7]
   [...] Rank 2 bound to package[0][core:0-3]
   [...] Rank 3 bound to package[1][core:4-7]

The above case demonstrates the difference
in ranking when the ``span`` qualifier is used instead of the default.

.. code::

   shell$ mpirun --np 4 --report-bindings --map-by slot:PE=2 --bind-to core
   [...] Rank 0 bound to package[0][core:0-1]
   [...] Rank 1 bound to package[0][core:2-3]
   [...] Rank 2 bound to package[0][core:4-5]
   [...] Rank 3 bound to package[0][core:6-7]

In the above case, the output shows us that 2 cores have been bound per
process.  Specifically, the mapping by ``slot`` with the ``PE=2`` qualifier
indicated that each slot (i.e., process) should consume two processor
elements.  By default, Open MPI defines "processor element" as "core",
and therefore the ``--bind-to core`` caused each process to be bound to
both of the cores to which it was mapped.

.. code::

   shell$ mpirun --np 4 --report-bindings --map-by slot:PE=2 --use-hwthread-cpus
   [...]] Rank 0 bound to package[0][hwt:0-1]
   [...]] Rank 1 bound to package[0][hwt:2-3]
   [...]] Rank 2 bound to package[0][hwt:4-5]
   [...]] Rank 3 bound to package[0][hwt:6-7]

In the above case, we replace the ``--bind-to core`` with
``--use-hwthread-cpus``. The ``--use-hwthread-cpus`` is converted into
``--bind-to hwthread`` and tells the ``--report-bindings`` output to show the
hardware threads to which a process is bound. In this case, processes are
bound to 2 hardware threads per process.

.. code::

   shell$ mpirun --np 4 --report-bindings --bind-to none
   [...] Rank 0 is not bound (or bound to all available processors)
   [...] Rank 1 is not bound (or bound to all available processors)
   [...] Rank 2 is not bound (or bound to all available processors)
   [...] Rank 3 is not bound (or bound to all available processors)

In the above case, binding is turned off and are reported as such.

Open MPI's support for process binding depends on the underlying
operating system.  Therefore, certain process binding options may not
be available on every system.

Process binding can also be set with MCA parameters.  Their usage is
less convenient than that of ``mpirun`` options.  On the other hand,
MCA parameters can be set not only on the mpirun command line, but
alternatively in a system or user ``mca-params.conf`` file or as
environment variables, as described in the :ref:`Setting MCA
Parameters <man1-mpirun-setting-mca-parameters>`. These are MCA parameters for
the PRRTE runtime so the command line argument ``--PRRTEmca`` must be used to
pass the MCA parameter key/value pair. Alternatively, the MCA parameter key/
value pair may be specific on the command line by prefixing the key with
``PRRTE_MCA_``. Some examples include:

.. list-table::
   :header-rows: 1

   * - Option
     - PRRTE MCA parameter key
     - Value

   * - ``--map-by core``
     - ``rmaps_default_mapping_policy``
     - ``core``

   * - ``--map-by package``
     - ``rmaps_default_mapping_policy``
     - ``package``

   * - ``--rank-by fill``
     - ``rmaps_default_ranking_policy``
     - ``fill``

   * - ``--bind-to core``
     - ``hwloc_default_binding_policy``
     - ``core``

   * - ``--bind-to package``
     - ``hwloc_default_binding_policy``
     - ``package``

   * - ``--bind-to none``
     - ``hwloc_default_binding_policy``
     - ``none``

.. _man1-mpirun-map-rank-bind-defaults:

Defaults for Mapping, Ranking, and Binding
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. admonition:: This is old, hard-coded content
   :class: error

   Is this content still current / accurate?  Should it be updated and
   retained, or removed?

If the user does not specify each of ``--map-by``, ``--rank-by``, and ``--bind-to`` option then the default values are as follows:

* If no options are specified then

    * If the number of processes is less than or equal to 2, then:

        * ``--map-by`` is ``core``
        * ``--bind-to`` is ``core``
        * ``--rank-by`` is ``span``
        * Result: ``--map-by core --bind-to core --rank-by span``

    * Otherwise:

        * ``--map-by`` is ``package``
        * ``--bind-to`` is ``package``
        * ``--rank-by`` is ``fill``
        * Result: ``--map-by package --bind-to package --rank-by fill``

* If only ``--map-by OBJ`` (where ``OBJ`` is something like ``core``) is specified, then:

    * ``--map-by`` specified ``OBJ``
    * ``--bind-to`` uses the same ``OBJ`` as ``--map-by``
    * ``--rank-by`` defaults to ``fill``
    * Result: ``--map-by OBJ --bind-to OBJ --rank-by fill``

* If only ``--bind-to OBJ`` (where ``OBJ`` is something like ``core``) is specified, then:

    * ``--map-by`` is either ``core`` or ``package`` depending on the number of processes
    * ``--bind-to`` specified ``OBJ``
    * ``--rank-by`` defaults to ``fill``
    * Result: ``--map-by OBJ --bind-to OBJ --rank-by fill``

* If ``--map-by OBJ1 --bind-to OBJ2``, then:

    * ``--map-by`` specified ``OBJ1``
    * ``--bind-to`` specified ``OBJ2``
    * ``--rank-by`` defaults to ``fill``
    * Result: ``--map-by OBJ2 --bind-to OBJ2 --rank-by fill``


Consider 2 identical hosts (``hostA`` and ``hostB``) with 2 packages (denoted by ``[]``) each with 8 cores (denoted by ``/../``) and 2 hardware threads per core (denoted by a ``.``).

Default of ``--map-by core --bind-to core --rank-by span`` when the number of processes is less than or equal to 2.

.. code::

  shell$ mpirun --np 2 --host hostA:4,hostB:2 ./a.out
  R0  hostA  [BB/../../../../../../..][../../../../../../../..]
  R1  hostA  [../BB/../../../../../..][../../../../../../../..]

Default of ``--map-by package --bind-to package --rank-by fill`` when the number of processes is greater than 2.

.. code::

  shell$ mpirun --np 4 --host hostA:4,hostB:2 ./a.out
  R0  hostA  [BB/BB/BB/BB/BB/BB/BB/BB][../../../../../../../..]
  R1  hostA  [BB/BB/BB/BB/BB/BB/BB/BB][../../../../../../../..]
  R2  hostA  [../../../../../../../..][BB/BB/BB/BB/BB/BB/BB/BB]
  R3  hostA  [../../../../../../../..][BB/BB/BB/BB/BB/BB/BB/BB]

If only ``--map-by OBJ`` is specified, then it implies ``--bind-to OBJ --rank-by fill``. The example below results in ``--map-by hwthread --bind-to hwthread --rank-by fill``

.. code::

  shell$ mpirun --np 4 --map-by hwthread --host hostA:4,hostB:2 ./a.out
  R0  hostA  [B./../../../../../../..][../../../../../../../..]
  R1  hostA  [.B/../../../../../../..][../../../../../../../..]
  R0  hostA  [../B./../../../../../..][../../../../../../../..]
  R1  hostA  [../.B/../../../../../..][../../../../../../../..]

If only ``--bind-to OBJ`` is specified, then ``--map-by`` is determined by the number of processes and ``--rank-by fill``. The example below results in ``--map-by package --bind-to core --rank-by fill``

.. code::

  shell$ mpirun --np 4 --bind-to core --host hostA:4,hostB:2 ./a.out
  R0  hostA  [BB/../../../../../../..][../../../../../../../..]
  R1  hostA  [../BB/../../../../../..][../../../../../../../..]
  R2  hostA  [../../../../../../../..][BB/../../../../../../..]
  R3  hostA  [../../../../../../../..][../BB/../../../../../..]

The mapping pattern might be better seen if we change the default ``--rank-by`` from ``fill`` to ``span``. First, the processes are mapped by package iterating between the two marking a core at a time. Next, the processes are ranked in a spanning manner that load balances them across the object they were mapped against. Finally, the processes are bound to the core that they were mapped againast.

.. code::

  shell$ mpirun --np 4 --bind-to core --rank-by span --host hostA:4,hostB:2 ./a.out
  R0  hostA  [BB/../../../../../../..][../../../../../../../..]
  R1  hostA  [../../../../../../../..][BB/../../../../../../..]
  R2  hostA  [../BB/../../../../../..][../../../../../../../..]
  R3  hostA  [../../../../../../../..][../BB/../../../../../..]


.. _man1-mpirun-rankfiles:

Rankfiles
^^^^^^^^^

.. admonition:: This is old, hard-coded content
   :class: error

   Is this content still current / accurate?  Should it be updated and
   retained, or removed?

Rankfiles are text files that specify detailed information about how
individual processes should be mapped to nodes, and to which
processor(s) they should be bound.  Each line of a rankfile specifies
the location of one process (for MPI jobs, the process' "rank" refers
to its rank in MPI_COMM_WORLD).  The general form of each line in the
rankfile is:

.. code::

   rank <N>=<hostname> slot=<slot list>

For example:

.. code::

   shell$ cat myrankfile
   rank 0=aa slot=1:0-2
   rank 1=bb slot=0:0,1
   rank 2=cc slot=2-3
   shell$ mpirun -H aa,bb,cc,dd --map-by rankfile:file=myrankfile ./a.out

Means that:

* Rank 0 runs on node aa, bound to logical package 1, cores 0-2.
* Rank 1 runs on node bb, bound to logical package 0, cores 0 and 1.
* Rank 2 runs on node cc, bound to logical cores 2 and 3.

Note that only logicical processor locations are supported. By default, the values specifed are assumed to be cores. If you intend to specify specific hardware threads then you must add the ``:hwtcpus`` qualifier to the ``--map-by`` command line option (e.g., ``--map-by rankfile:file=myrankfile:hwtcpus``).

If the binding specification overlaps between any two ranks then an error occurs. If you intend to allow processes to share the same logical processing unit then you must pass the ``--bind-to :overload-allowed`` command line option to tell the runtime to ignore this check.

The hostnames listed above are "absolute," meaning that actual
resolveable hostnames are specified.  However, hostnames can also be
specified as "relative," meaning that they are specified in relation
to an externally-specified list of hostnames (e.g., by ``mpirun``'s
``--host`` argument, a hostfile, or a job scheduler).

The "relative" specification is of the form ``+n<X>``, where X is an
integer specifying the Xth hostname in the set of all available
hostnames, indexed from 0.  For example:

.. code::

   shell$ cat myrankfile
   rank 0=+n0 slot=1:0-2
   rank 1=+n1 slot=0:0,1
   rank 2=+n2 slot=2-3
   shell$ mpirun -H aa,bb,cc,dd --map-by rankfile:file=myrankfile ./a.out

All package/core slot locations are specified as logical indexes.

.. note:: The Open MPI v1.6 series used physical indexes. Starting in Open MPI v5.0 only logicial indexes are supported and the ``rmaps_rank_file_physical`` MCA parameter is no longer recognized.

You can use tools such as Hwloc's `lstopo(1)` to find the logical
indexes of package and cores.

Application Context or Executable Program?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. admonition:: This is old, hard-coded content
   :class: error

   Is this content still current / accurate?  Should it be updated and
   retained, or removed?

To distinguish the two different forms, mpirun looks on the command
line for ``--app`` option.  If it is specified, then the file named on
the command line is assumed to be an application context.  If it is
not specified, then the file is assumed to be an executable program.

Locating Files
^^^^^^^^^^^^^^

.. admonition:: This is old, hard-coded content
   :class: error

   Is this content still current / accurate?  Should it be updated and
   retained, or removed?

If no relative or absolute path is specified for a file, Open MPI will
first look for files by searching the directories specified by the
``--path`` option.  If there is no ``--path`` option set or if the
file is not found at the ``--path`` location, then Open MPI will
search the user's ``PATH`` environment variable as defined on the
source node(s).

If a relative directory is specified, it must be relative to the
initial working directory determined by the specific starter used. For
example when using the ssh starter, the initial directory is ``$HOME``
by default.  Other starters may set the initial directory to the
current working directory from the invocation of ``mpirun``.

.. _man1-mpirun-current-working-directory:

Current Working Directory
^^^^^^^^^^^^^^^^^^^^^^^^^

.. admonition:: This is old, hard-coded content
   :class: error

   Is this content still current / accurate?  Should it be updated and
   retained, or removed?

The ``--wdir`` ``mpirun`` option (and its synonym, ``--wd``) allows
the user to change to an arbitrary directory before the program is
invoked.  It can also be used in application context files to specify
working directories on specific nodes and/or for specific
applications.

If the ``--wdir`` option appears both in a context file and on the
command line, the context file directory will override the command
line value.

If the ``-wdir`` option is specified, Open MPI will attempt to change
to the specified directory on all of the remote nodes. If this fails,
``mpirun`` will abort.

If the ``-wdir`` option is not specified, Open MPI will send the
directory name where ``mpirun`` was invoked to each of the remote
nodes.  The remote nodes will try to change to that directory.  If
they are unable (e.g., if the directory does not exist on that node),
then Open MPI will use the default directory determined by the
starter.

All directory changing occurs before the user's program is invoked; it
does not wait until :ref:`MPI_INIT(3) <mpi_init>` is called.

Standard I/O
^^^^^^^^^^^^

.. admonition:: This is old, hard-coded content
   :class: error

   Is this content still current / accurate?  Should it be updated and
   retained, or removed?

Open MPI directs UNIX standard input to ``/dev/null`` on all processes
except the MPI_COMM_WORLD rank 0 process. The MPI_COMM_WORLD rank 0
process inherits standard input from ``mpirun``.

.. note:: The node that invoked ``mpirun`` need not be the same as the
          node where the MPI_COMM_WORLD rank 0 process resides. Open
          MPI handles the redirection of ``mpirun``'s standard input
          to the rank 0 process.

Open MPI directs UNIX standard output and error from remote nodes to
the node that invoked ``mpirun`` and prints it on the standard
output/error of ``mpirun``.  Local processes inherit the standard
output/error of ``mpirun`` and transfer to it directly.

Thus it is possible to redirect standard I/O for Open MPI applications
by using the typical shell redirection procedure on ``mpirun``.  For
example:

.. code:: sh

   shell$ mpirun -n 2 my_app < my_input > my_output

Note that in this example only the MPI_COMM_WORLD rank 0 process will
receive the stream from ``my_input`` on stdin.  The stdin on all the other
nodes will be tied to ``/dev/null``.  However, the stdout from all nodes
will be collected into the ``my_output`` file.

Signal Propagation
^^^^^^^^^^^^^^^^^^

.. admonition:: This is old, hard-coded content
   :class: error

   Is this content still current / accurate?  Should it be updated and
   retained, or removed?

When ``mpirun`` receives a SIGTERM and SIGINT, it will attempt to kill
the entire job by sending all processes in the job a SIGTERM, waiting
a small number of seconds, then sending all processes in the job a
SIGKILL.

SIGUSR1 and SIGUSR2 signals received by ``mpirun`` are propagated to all
processes in the job.

A SIGTSTOP signal to ``mpirun`` will cause a SIGSTOP signal to be sent
to all of the programs started by ``mpirun`` and likewise a SIGCONT
signal to ``mpirun`` will cause a SIGCONT sent.

Other signals are not currently propagated by ``mpirun``.

Process Termination / Signal Handling
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. admonition:: This is old, hard-coded content
   :class: error

   Is this content still current / accurate?  Should it be updated and
   retained, or removed?

During the run of an MPI application, if any process dies abnormally
(either exiting before invoking :ref:`MPI_FINALIZE(3) <mpi_finalize>`,
or dying as the result of a signal), ``mpirun`` will print out an
error message and kill the rest of the MPI application.

User signal handlers should probably avoid trying to cleanup MPI state
(Open MPI is currently not async-signal-safe; see
:ref:`MPI_INIT_THREAD(3) <mpi_init_thread>` for details about
MPI_THREAD_MULTIPLE and thread safety).  For example, if a
segmentation fault occurs in :ref:`MPI_SEND(3) <mpi_send>` (perhaps
because a bad buffer was passed in) and a user signal handler is
invoked, if this user handler attempts to invoke :ref:`MPI_FINALIZE(3)
<mpi_finalize>`, Bad Things could happen since Open MPI was already
"in" MPI when the error occurred.  Since ``mpirun`` will notice that the
process died due to a signal, it is probably not necessary (and
safest) for the user to only clean up non-MPI state.

Process Environment
^^^^^^^^^^^^^^^^^^^

.. admonition:: This is old, hard-coded content
   :class: error

   Is this content still current / accurate?  Should it be updated and
   retained, or removed?

Processes in the MPI application inherit their environment from the
PRRTE daemon upon the node on which they are running.  The
environment is typically inherited from the user's shell.  On remote
nodes, the exact environment is determined by the boot MCA module
used.  The rsh launch module, for example, uses either rsh/ssh to
launch the PRRTE daemon on remote nodes, and typically executes one
or more of the user's shell-setup files before launching the PRRTE
daemon.  When running dynamically linked applications which require
the ``LD_LIBRARY_PATH`` environment variable to be set, care must be
taken to ensure that it is correctly set when booting Open MPI.

See the :ref:`Remote Execution <man1-mpirun-remote-execution>` section
for more details.

.. _man1-mpirun-remote-execution:

Remote Execution
^^^^^^^^^^^^^^^^

.. admonition:: This is old, hard-coded content
   :class: error

   Is this content still current / accurate?  Should it be updated and
   retained, or removed?

Open MPI requires that the ``PATH`` environment variable be set to
find executables on remote nodes (this is typically only necessary in
rsh- or ssh-based environments |mdash| batch/scheduled environments
typically copy the current environment to the execution of remote
jobs, so if the current environment has ``PATH`` and/or
``LD_LIBRARY_PATH`` set properly, the remote nodes will also have it
set properly).  If Open MPI was compiled with shared library support,
it may also be necessary to have the ``LD_LIBRARY_PATH`` environment
variable set on remote nodes as well (especially to find the shared
libraries required to run user MPI applications).

However, it is not always desirable or possible to edit shell startup
files to set ``PATH`` and/or ``LD_LIBRARY_PATH``.  The ``--prefix``
option is provided for some simple configurations where this is not
possible.

The ``--prefix`` option takes a single argument: the base directory on
the remote node where Open MPI is installed.  Open MPI will use this
directory to set the remote ``PATH`` and ``LD_LIBRARY_PATH`` before
executing any Open MPI or user applications.  This allows running Open
MPI jobs without having pre-configured the ``PATH`` and
``LD_LIBRARY_PATH`` on the remote nodes.

Open MPI adds the basename of the current node's ``$bindir`` (the
directory where Open MPI's executables were installed) to the prefix
and uses that to set the ``PATH`` on the remote node.  Similarly, Open
MPI adds the basename of the current node's ``$libdir`` (the directory
where Open MPI's libraries were installed) to the prefix and uses that
to set the ``LD_LIBRARY_PATH`` on the remote node.  For example:

* Local bindir: ``/local/node/directory/bin``
* Local libdir: ``/local/node/directory/lib64``

If the following command line is used:

.. code:: sh

   shell$ mpirun --prefix /remote/node/directory

Open MPI will add ``/remote/node/directory/bin`` to the ``PATH`` and
``/remote/node/directory/lib64`` to the ``LD_LIBRARY_PATH`` on the
remote node before attempting to execute anything.

The ``--prefix`` option is not sufficient if the installation paths on
the remote node are different than the local node (e.g., if ``/lib``
is used on the local node, but ``/lib64`` is used on the remote node),
or if the installation paths are something other than a subdirectory
under a common prefix.

Note that executing ``mpirun`` via an absolute pathname is equivalent
to specifying ``--prefix`` without the last subdirectory in the
absolute pathname to ``mpirun``.  For example:

.. code:: sh

   shell$ /usr/local/bin/mpirun ...

is equivalent to

.. code:: sh

   shell$ mpirun --prefix /usr/local

Exported Environment Variables
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. admonition:: This is old, hard-coded content
   :class: error

   Is this content still current / accurate?  Should it be updated and
   retained, or removed?

All environment variables that are named in the form ``OMPI_*`` will
automatically be exported to new processes on the local and remote
nodes.  Environmental parameters can also be set/forwarded to the new
processes using the MCA parameter ``mca_base_env_list``. The ``-x``
option to mpirun has been deprecated, but the syntax of the MCA param
follows that prior example. While the syntax of the ``-x`` option and
MCA param allows the definition of new variables, note that the parser
for these options are currently not very sophisticated |mdash| it does
not even understand quoted values.  Users are advised to set variables
in the environment and use the option to export them; not to define
them.

.. _man1-mpirun-setting-mca-parameters:

Setting MCA Parameters
^^^^^^^^^^^^^^^^^^^^^^

.. admonition:: This is old, hard-coded content
   :class: error

   Is this content still current / accurate?  Should it be updated and
   retained, or removed?

The ``--mca`` switch allows the passing of parameters to various MCA
(Modular Component Architecture) modules.  MCA modules have direct
impact on MPI programs because they allow tunable parameters to be set
at run time (such as which BTL communication device driver to use,
what parameters to pass to that BTL, etc.).

The ``--mca`` switch takes two arguments: ``<key>`` and ``<value>``.
The ``<key>`` argument generally specifies which MCA module will
receive the value.  For example, the ``<key>`` ``btl`` is used to
select which BTL to be used for transporting MPI messages.  The
``<value>`` argument is the value that is passed.  For example:

.. code:: sh

   shell$ mpirun --mca btl tcp,self -n 1 my_mpi_app

This tells Open MPI to use the ``tcp`` and ``self`` BTLs, and to run a
single copy of ``my_mpi_app`` an allocated node.

.. code:: sh

   shell$ mpirun --mca btl self -n 1 my_mpi_app

Tells Open MPI to use the ``self`` BTL, and to run a single copy of
``my_mpi_app`` an allocated node.

The ``--mca`` switch can be used multiple times to specify different
<key> and/or ``<value>`` arguments.  If the same ``<key>`` is
specified more than once, the ``<value>``s are concatenated with a
comma (``,``) separating them.

Note that the ``--mca`` switch is simply a shortcut for setting
environment variables.  The same effect may be accomplished by setting
corresponding environment variables before running ``mpirun``.  The form
of the environment variables that Open MPI sets is:

.. code:: sh

   OMPI_MCA_<key>=<value>

Thus, the ``--mca`` switch overrides any previously set environment
variables.  The ``--mca`` settings similarly override MCA parameters
set in the ``$OPAL_PREFIX/etc/openmpi-mca-params.conf`` or
``$HOME/.openmpi/mca-params.conf`` file.

Unknown ``<key>`` arguments are still set as environment variable --
they are not checked (by mpirun) for correctness.  Illegal or
incorrect ``<value>`` arguments may or may not be reported |mdash| it
depends on the specific MCA module.

To find the available component types under the MCA architecture, or
to find the available parameters for a specific component, use the
ompi_info command.  See the :ref:`ompi_info(1) <man1-ompi_info>` man
page for detailed information on this command.

.. _man1-mpirun-setting-mca-params-from-file:

Setting MCA parameters and environment variables from file
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. admonition:: This is old, hard-coded content
   :class: error

   Is this content still current / accurate?  Should it be updated and
   retained, or removed?

The ``--tune`` command line option and its synonym ``--mca``
``mca_base_envar_file_prefix`` allows a user to set MCA parameters and
environment variables with the syntax described below.  This option
requires a single file or list of files separated by "," to follow.

A valid line in the file may contain zero or more ``-x`` or
``--mca``. The following patterns are supported:

* ``--mca var val``
* ``--mca var "val"``
* ``-x var=val``
* ``-x var``

If any argument is duplicated in the file, the last value read will be
used.

MCA parameters and environment specified on the command line
have higher precedence than variables specified in the file.

.. _man1-mpirun-running-as-root:

Running as root
^^^^^^^^^^^^^^^

.. admonition:: This is old, hard-coded content
   :class: error

   Is this content still current / accurate?  Should it be updated and
   retained, or removed?

.. warning:: The Open MPI team **strongly** advises against executing
             ``mpirun`` as the root user.  MPI applications should be
             run as regular (non-root) users.

``mpirun`` will refuse to run as root by default.

To override this default, you can add the ``--allow-run-as-root``
option to the mpirun command line, or you can set the environmental
parameters ``OMPI_ALLOW_RUN_AS_ROOT=1`` and
``OMPI_ALLOW_RUN_AS_ROOT_CONFIRM=1``.  Note that it takes setting two
environment variables to effect the same behavior as
``--allow-run-as-root`` in order to stress the Open MPI team's strong
advice against running as the root user.

After extended discussions with communities who use containers (where
running as the root user is the default), there was a persistent
desire to be able to enable root execution of ``mpirun`` via an
environmental control (vs. the existing ``--allow-run-as-root``
command line parameter).  The compromise of using two environment
variables was reached: it allows root execution via an environmental
control, but it conveys the Open MPI team's strong recommendation
against this behavior.

Exit status
^^^^^^^^^^^

.. admonition:: This is old, hard-coded content
   :class: error

   Is this content still current / accurate?  Should it be updated and
   retained, or removed?

There is no standard definition for what ``mpirun`` should return as
an exit status. After considerable discussion, we settled on the
following method for assigning the ``mpirun`` exit status (note: in
the following description, the "primary" job is the initial
application started by mpirun |mdash| all jobs that are spawned by
that job are designated "secondary" jobs):

* If all processes in the primary job normally terminate with exit
  status 0, ``mpirun`` returns 0.

* If one or more processes in the primary job normally terminate with
  non-zero exit status, ``mpirun`` returns the exit status of the
  process with the lowest MPI_COMM_WORLD rank to have a non-zero
  status.

* If all processes in the primary job normally terminate with exit
  status 0, and one or more processes in a secondary job normally
  terminate with non-zero exit status, ``mpirun``:

  #. Returns the exit status of the process with the lowest
     MPI_COMM_WORLD rank in the lowest jobid to have a non-zero
     status, and
  #. Outputs a message summarizing the exit status of the primary and
     all secondary jobs.

* If the command line option ``--report-child-jobs-separately`` is
  set, we will return *only* the exit status of the primary job. Any
  non-zero exit status in secondary jobs will be reported solely in a
  summary print statement.

By default, the job will abort when any process terminates with
non-zero status. The MCA parameter ``--PRRTEmca state_base_error_non_zero_exit``
can be set to "false" (or "0") to cause Open MPI to not abort a job if
one or more processes return a non-zero status. In that situation the
Open MPI records and notes that processes exited with non-zero
termination status to report the appropriate exit status of ``mpirun`` (per
bullet points above).

EXAMPLES
--------

.. admonition:: This is old, hard-coded content
   :class: error

   Is this content still current / accurate?  Should it be updated and
   retained, or removed?

Be sure also to see the examples throughout the sections above.

.. code:: sh

   shell$ mpirun -n 4 --mca btl tcp,sm,self prog1

Run 4 copies of ``prog1`` using the ``tcp``, ``sm`` (shared memory),
and ``self`` (process loopback) BTL's for the transport of MPI
messages.


RETURN VALUE
------------

.. admonition:: This is old, hard-coded content
   :class: error

   Is this content still current / accurate?  Should it be updated and
   retained, or removed?

``mpirun`` returns 0 if all processes started by mpirun exit after
calling :ref:`MPI_FINALIZE(3) <mpi_finalize>`.  A non-zero value is
returned if an internal error occurred in mpirun, or one or more
processes exited before calling :ref:`MPI_FINALIZE(3) <mpi_finalize>`.
If an internal error occurred in mpirun, the corresponding error code
is returned.  In the event that one or more processes exit before
calling :ref:`MPI_FINALIZE(3) <mpi_finalize>`, the return value of
the MPI_COMM_WORLD rank of the process that mpirun first notices died
before calling :ref:`MPI_FINALIZE(3) <mpi_finalize>` will be
returned.  Note that, in general, this will be the first process that
died but is not guaranteed to be so.

If the ``--timeout`` command line option is used and the timeout
expires before the job completes (thereby forcing mpirun to kill the
job) mpirun will return an exit status equivalent to the value of
ETIMEDOUT (which is typically 110 on Linux and OS X systems).


.. seealso::
   :ref:`MPI_INIT(3) <mpi_init>`,
   :ref:`MPI_INIT_THREAD(3) <mpi_init_thread>`,
   :ref:`MPI_FINALIZE(3) <mpi_finalize>`,
   :ref:`ompi_info(1) <man1-ompi_info>`
